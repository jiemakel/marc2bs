package fi.sange.marc2bs

import java.io.FileOutputStream
import java.io.{ File, FileInputStream }
import java.net.URLEncoder
import java.util.regex.Pattern
import scala.Array.canBuildFrom
import scala.Option.option2Iterable
import scala.collection.JavaConversions.{ asJavaCollection, asScalaBuffer, mapAsScalaMap }
import scala.language.implicitConversions
import scala.util.Try
import org.marc4j.{ MarcReader, MarcXmlReader }
import org.marc4j.marc.{ ControlField, DataField, Record }
import org.w3.banana.{ FOAFPrefix, PointedGraph, RDFWriter, Turtle }
import org.w3.banana.diesel.{ ObjectList, toPointedGraphW }
import org.w3.banana.jena.ImmutableJenaGraph
import com.hp.hpl.jena.shared.PrefixMapping
import com.typesafe.config.{ Config, ConfigFactory, ConfigValue, ConfigValueFactory }
import com.typesafe.scalalogging.slf4j.Logging
import fi.sange.marc2bs.ops.{ anyW, graphW }
import scalaz.Scalaz._
import scopt.OptionParser
import java.util.UUID
import org.marc4j.MarcStreamReader
import org.w3.banana.DCTPrefix
import dispatch.Defaults._
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration
import org.apache.jena.riot.{ RDFFormat, RDFDataMgr }
import java.text.Normalizer

case class Params(
  sparqlEndpoint: BookSampoSPARQLEndpoint = null,
  datasourceIRI: PointedGraph[Rdf] = null,
  filteredKeywords: Seq[String] = Seq.empty,
  languageMap: Map[String, String] = Map.empty,
  languageURIMap: Map[String, Rdf#URI] = Map.empty,
  yklTypeKeywordMap: Map[String, Map[String, Rdf#URI]] = Map.empty,
  yklTypeMap: Map[String, Rdf#URI] = Map.empty,
  hkljTypeKeywordMap: Map[String, Map[String, Rdf#URI]] = Map.empty,
  hkljTypeMap: Map[String, Rdf#URI] = Map.empty,
  collectionPartTypeMap: Map[Rdf#URI, Rdf#URI] = Map.empty,
  actorRoleMap: Map[String, Rdf#URI] = Map.empty,
  files: Seq[String] = Seq.empty,
  blacklist: Seq[String] = Seq.empty)

object ImportMarcToBookSampoSPARQL extends Logging {

  implicit def anyToConfigValue(ar: AnyRef): ConfigValue = ConfigValueFactory.fromAnyRef(ar)

  implicit def marcReaderToIterator(mr: MarcReader): Iterator[Record] = new Iterator[Record] {
    override def hasNext: Boolean = mr.hasNext
    override def next: Record = mr.next
  }

  class NormalizableString(val s: String) {
    def normalize = Normalizer.normalize(s, Normalizer.Form.NFC)
  }

  implicit def stringToNormalizableString(s: String) = new NormalizableString(s)

  import ops._

  private val p1 = "^[^a-zA-Z0-9åäöÅÄÖ]*(.*?(?: [a-zA-Z0-9åäöÅÄÖ].)?)[^a-zA-Z0-9åäöÅÄÖ]*$".r
  private val numbers = "[0-9]+".r
  private val isbn = """[0-9\-X]+""".r

  def fix(source: String): String = {
    p1.findFirstMatchIn(source).map(_.group(1)).getOrElse(source)
  }

  def normalize(source: String): String = {
    Normalizer.normalize(source, Normalizer.Form.NFC)
  }

  def filesAt(blacklist: Seq[String])(f: File): Iterable[File] = {
    if (blacklist.exists(entry => f.getAbsolutePath().startsWith(entry)) || new File(f.getAbsolutePath() + ".processed").exists) Seq() else if (f.isDirectory) f.listFiles flatMap filesAt(blacklist) else Seq(f)
  }

  def getField(field: String, subfield: Char, ind1: Option[Char] = None, ind2: Option[Char] = None)(implicit record: Record): Seq[String] = {
    record.getVariableFields(field).filter(field => ind1.forall(field.asInstanceOf[DataField].getIndicator1==_) && ind2.forall(field.asInstanceOf[DataField].getIndicator2==_)).map(field => field.asInstanceOf[DataField].getSubfields(subfield)).flatten.map(subfield => subfield.getData.normalize.trim)
  }

  def filterRecord(record: Record)(implicit params: Params): Boolean = {
    implicit val impRec = record
    if (record.getControlNumber == null) {
      logger.warn("Null record!")
      return false
    }
    val yklClasses = getField("084", 'a').map(_.trim)
    val hkljClasses = getField("588", 'a').map(_.trim)
    if (yklClasses.isEmpty && hkljClasses.isEmpty) {
      logger.warn(s"Not anything!: ${record.getControlNumber}")
      return false;
    }
    if (!yklClasses.exists(params.yklTypeMap.contains) && !hkljClasses.exists(params.hkljTypeMap.contains)) {
      logger.debug(s"Class definition (${yklClasses.mkString(", ")}/${hkljClasses.mkString(", ")}) not in the accepted classes of (${params.yklTypeMap.keySet.mkString(", ")}/${params.hkljTypeMap.keySet.mkString(", ")}) ${record.getControlNumber}")
      return false;
    }
    val languages = getField("041", 'a')
    if (!languages.contains("fin") && !languages.contains("swe")) {
      logger.debug(s"Not Finnish or Swedish: ${languages.mkString(", ")}): ${record.getControlNumber}")
      return false;
    }
    val descLang = {
      val tmp = Try(record.getVariableField("008").asInstanceOf[ControlField].getData.normalize.substring(35, 37)).getOrElse("")
      if (tmp.trim.isEmpty || tmp.exists(!_.isLetter)) {
        logger.warn(s"No or bad description language (${record.getVariableField("008")}): ${record.getControlNumber}")
        return false;
      }
      makeLang(params.languageMap.getOrElse(tmp, tmp))
    }
    val keywords = (getField("650", 'a') ++ getField("650", 'x') ++ getField("655", 'a')).map(_ + '_' + descLang)
    if (!keywords.intersect(params.filteredKeywords).isEmpty) {
      logger.debug(s"Filtered based on keywords: (${keywords.intersect(params.filteredKeywords).mkString(", ")}) ${record.getControlNumber}")
      return false;
    }
    if (params.sparqlEndpoint.alreadyExistsAsEdited(URI("http://data.kirjasampo.fi/physicalWork_" + URLEncoder.encode(record.getControlNumber.trim, "UTF-8")), params.datasourceIRI.pointer)) {
      logger.debug(s"http://data.kirjasampo.fi/physicalWork_${record.getControlNumber} already exists and has been processed!")
      return false;
    }
    return true;
  }

  val bs = BookSampoSchema

  val skos = SKOSPrefix

  val foaf = FOAFPrefix[Rdf]

  def convertRecordToGraph(r: Record)(implicit params: Params): Rdf#Graph = {
    implicit val impR = r;

    val isbns = getField("020", 'a').map(value => isbn.findFirstIn(value).getOrElse(value))

    val descriptionFutures = isbns.map(BTJService.getDescription(_))

    val imageURLFutures = isbns.map(BTJService.getImageURL(_))

    val ref = params.sparqlEndpoint
    val descLang = {
      val tmp = r.getVariableField("008").asInstanceOf[ControlField].getData.normalize.substring(35, 37)
      makeLang(params.languageMap.getOrElse(tmp, tmp))
    }
    val ares = URI("http://data.kirjasampo.fi/abstractWork_" + URLEncoder.encode(r.getControlNumber.trim, "UTF-8"))

    val litLangs = getField("041", 'a').map(lang => params.languageMap.getOrElse(lang, lang))
    val physPrefLabels = getField("245", 'a').map(fix).flatMap(label => litLangs.map(lang => LangLiteral(label, lang)))
    val altActorLabelMap: Map[String, String] = if (r.getVariableField("900") != null) {
      r.getVariableFields("900").map(vf => {
        val f = vf.asInstanceOf[DataField]
        Map[String, String](f.getSubfield('a').getData.normalize -> f.getSubfield('y').getData.normalize, f.getSubfield('y').getData.normalize -> f.getSubfield('a').getData.normalize)
      }).foldLeft(Map[String, String]().empty) { (r, c) => r ++ c }
    } else Map.empty
    val creatorLabels = getField("100", 'a').map(fix)

    val bse = params.sparqlEndpoint
    val creators: Seq[PointedGraph[Rdf]] = creatorLabels.flatMap(clabel => {

      val mac = bse.getMatchingActors(clabel, descLang).map(PointedGraph[Rdf])

      if (!mac.isEmpty) mac else
        Some((URI("http://data.kirjasampo.fi/actor_" + URLEncoder.encode(clabel, "UTF-8")).toPG.a(foaf.Person)
          -- bs.datasource ->- params.datasourceIRI
          -- skos.prefLabel ->- clabel
          -- bs.writerAltName ->- altActorLabelMap.get(clabel)))
    })

    val mainActors = getField("600", 'a').map(fix).map(alabel =>
      (URI("http://data.kirjasampo.fi/maincharacter_" + UUID.randomUUID).toPG.a(bs.character_type)
        -- bs.datasource ->- params.datasourceIRI
        -- skos.prefLabel ->- alabel
        -- skos.altLabel ->- altActorLabelMap.get(alabel)))

    val acodes = Seq('a', 'x', 'z', 'y')

    val conglomerateKeyword = r.getVariableFields("650").filter(f => f.asInstanceOf[DataField].getSubfields().size() > 2).map(f => {
      f.asInstanceOf[DataField].getSubfields().filter(sf => acodes.contains(sf.getCode())).foldLeft("") { (rs, sf) => if (rs.isEmpty) sf.getData.normalize else rs + " : " + sf.getData.normalize }
    }).foldLeft("") { (rs, kw) => if (rs.isEmpty) kw else rs + ", " + kw }.lang(descLang)

    val keywordGraph = (getField("650", 'a') ++ getField("650", 'x') ++ getField("655", 'a')).flatMap(klabel => {
      val mac = bse.getMatchingKeywordsAndTypes(klabel, descLang).map(p => Graph(Triple(ares, p._2, p._1)))

      if (!mac.isEmpty) mac else {
        val tmp = (URI("http://data.kirjasampo.fi/localKeyword_" + URLEncoder.encode(klabel + '_' + descLang, "UTF-8")).toPG.a(bs.keyword_type)
          -- bs.datasource ->- params.datasourceIRI
          -- skos.prefLabel ->- klabel.lang(descLang))
        Some(Graph(Triple(ares, bs.localkeyword, tmp.pointer)) union tmp.graph)
      }
    }).foldLeft(emptyGraph) { (rg, cg) => rg union cg }

    val times = getField("650", 'y').flatMap(tlabel => {
      val mac = bse.getMatchingTimes(tlabel, descLang).map(PointedGraph[Rdf])

      if (!mac.isEmpty) mac else
        Some((URI("http://data.kirjasampo.fi/time_" + URLEncoder.encode(tlabel + '_' + descLang, "UTF-8")).toPG.a(bs.time_type)
          -- bs.datasource ->- params.datasourceIRI
          -- skos.prefLabel ->- tlabel.lang(descLang)))
    })

    val places = (getField("650", 'z') ++ getField("651", 'a')).flatMap(plabel => {
      val mac = bse.getMatchingPlaces(plabel, descLang).map(PointedGraph[Rdf])

      if (!mac.isEmpty) mac else
        Some((URI("http://data.kirjasampo.fi/place_" + URLEncoder.encode(plabel + '_' + descLang, "UTF-8")).toPG.a(bs.place_type)
          -- bs.datasource ->- params.datasourceIRI
          -- skos.prefLabel ->- plabel.lang(descLang)))
    })

    val isTranslation = ((!(getField("240", 'a') ++ getField("765", 't')).isEmpty)) || r.getVariableFields("041").exists(f => f.asInstanceOf[DataField].getIndicator1() === 1 || (!getField("041", 'h').isEmpty))

    val physAltLabels = getField("245", 'b').map(fix).flatMap(label => litLangs.map(lang => LangLiteral(label, lang)))

    val pageCounts = getField("300", 'a').flatMap(count => numbers.findFirstIn(count).map(TypedLiteral(_, xsd.integer)))

    val series = if (r.getVariableField("490") != null) {
      r.getVariableFields("490").filter(_.asInstanceOf[DataField].getSubfield('a') != null).flatMap(f => {
        val df = f.asInstanceOf[DataField]
        val seriesName = fix(df.getSubfield('a').getData.normalize)
        val seriesNames = litLangs.map(ll => seriesName.lang(ll))
        val existingSeries = seriesNames.flatMap(sn => {
          bse.getMatchingSeries(sn.lexicalForm, sn.lang)
        }).map(uri => PointedGraph[Rdf](uri))
        val tSeries = if (existingSeries.isEmpty) {
          Seq(URI("http://data.kirjasampo.fi/series_" + URLEncoder.encode(seriesName + '_' + litLangs(0), "UTF-8")).toPG.a(bs.series_type)
            -- bs.datasource ->- params.datasourceIRI
            -- skos.prefLabel ->- ObjectList(seriesNames))
        } else existingSeries
        if (df.getSubfield('v') != null && !numbers.findFirstIn(df.getSubfield('v').getData.normalize).isEmpty) {
          val num = TypedLiteral(numbers.findFirstIn(df.getSubfield('v').getData.normalize).get, xsd.integer)
          Seq(URI("http://data.kirjasampo.fi/seriesPart_" + URLEncoder.encode(seriesName, "UTF-8") + '_' + litLangs(0) + '_' + URLEncoder.encode(num.lexicalForm, "UTF-8")).toPG.a(bs.seriesPart_type)
            -- bs.osanumero ->- num
            -- bs.datasource ->- params.datasourceIRI
            -- skos.prefLabel ->- ObjectList(seriesNames.map(sn => (sn.lexicalForm + ": " + num.lexicalForm).lang(sn.lang)))
            -- bs.partofseries2 ->- ObjectList(tSeries))
        } else tSeries
      })
    } else Seq.empty

    val physLangsR = getField("041", 'a').flatMap(lang => params.languageURIMap.get(lang)).map(PointedGraph[Rdf])

    val publhist = getField("500", 'a').map(f => f.lang(descLang))

    val altPublisherLabelMap: Map[String, String] = if (r.getVariableField("910") != null) {
      r.getVariableFields("910").map(vf => {
        val f = vf.asInstanceOf[DataField]
        Map[String, String](f.getSubfield('a').getData.normalize -> f.getSubfield('y').getData.normalize, f.getSubfield('y').getData.normalize -> f.getSubfield('a').getData.normalize)
      }).foldLeft(Map[String, String]().empty) { (r, c) => r ++ c }
    } else Map.empty

    val publishers = getField("264", 'b', None, Some('1')).map(fix).flatMap(plabel => {
      val mac = bse.getMatchingPublishers(plabel, descLang).map(PointedGraph[Rdf])

      if (!mac.isEmpty) mac else
        Some((URI("http://data.kirjasampo.fi/actor_" + URLEncoder.encode(plabel, "UTF-8")).toPG.a(bs.publisher_type)
          -- bs.datasource ->- params.datasourceIRI
          -- skos.prefLabel ->- plabel
          -- skos.altLabel ->- altPublisherLabelMap.get(plabel)))
    })

    val publTimes = getField("264", 'c').map(fix).flatMap(tlabel => {
      val mac = bse.getMatchingTimes(tlabel, descLang).map(PointedGraph[Rdf])
      if (!mac.isEmpty) mac else
        Some((URI("http://data.kirjasampo.fi/time_" + URLEncoder.encode(tlabel + '_' + descLang, "UTF-8")).toPG.a(bs.time_type)
          -- bs.datasource ->- params.datasourceIRI
          -- skos.prefLabel ->- tlabel.lang(descLang)))
    })

    val origLangs = getField("041", 'h').map(lang => params.languageMap.getOrElse(lang, lang))

    val origLangsR = getField("041", 'h').flatMap(lang => params.languageURIMap.get(lang)).map(PointedGraph[Rdf])

    val origPrefLabelsForTranslation = (getField("240", 'a') ++ getField("765", 't')).map(fix).flatMap(label => {
      val langsToUse = if (origLangs.isEmpty) litLangs else origLangs
      langsToUse.map(lang => LangLiteral(label, lang))
    })

    val absPrefLabels = physPrefLabels ++ origPrefLabelsForTranslation

    val aresTypes = {
      val kwMapTypes = (
        getField("084", 'a').flatMap(c => (getField("650", 'a') ++ getField("655", 'a')).flatMap(kw => params.yklTypeKeywordMap.get(c).flatMap(kwMap => kwMap.get(kw + '_' + descLang)))) ++
        getField("588", 'a').flatMap(c => (getField("650", 'a') ++ getField("655", 'a')).flatMap(kw => params.hkljTypeKeywordMap.get(c).flatMap(kwMap => kwMap.get(kw + '_' + descLang)))))
      if (!kwMapTypes.isEmpty) kwMapTypes else {
        val types = getField("084", 'a').flatMap(c => params.yklTypeMap.get(c)) ++ getField("588", 'a').flatMap(c => params.hkljTypeMap.get(c))
        if (!types.isEmpty) types else Seq(bs.ateos_type)
      }
    }

    val presOrigs = if (!origPrefLabelsForTranslation.isEmpty) {
      val mac = origPrefLabelsForTranslation.flatMap(pl => creatorLabels.flatMap(cname => aresTypes.flatMap(tiri => bse.getMatchingConcreteWorks(pl.lexicalForm, cname, tiri, pl.lang)))).map(PointedGraph[Rdf])
      if (!mac.isEmpty) mac else
        Seq((URI("http://data.kirjasampo.fi/physicalWork_originalOf_" + URLEncoder.encode(r.getControlNumber.trim, "UTF-8")).toPG.a(bs.fteos_type)
          -- bs.datasource ->- params.datasourceIRI
          -- bs.firstversion ->- bs.ktrue
          -- bs.lang ->- ObjectList(origLangsR)
          -- skos.prefLabel ->- ObjectList(origPrefLabelsForTranslation)))
    } else Seq.empty

    val descriptions = (getField("505", 'a') ++ getField("520", 'a') ++ getField("586", 'a')).map(_.lang(descLang))

    val resolvedImages = Await.result(Future.sequence(imageURLFutures), Duration(60, "seconds")).flatMap(_.toSeq.map(
      (URI("http://data.kirjasampo.fi/coverImage_" + URLEncoder.encode(r.getControlNumber.trim, "UTF-8")).toPG.a(bs.kansikuva_type)
        -- skos.prefLabel ->- ObjectList(physPrefLabels)
        -- bs.imageURL2 ->- _)))

    val pres = (URI("http://data.kirjasampo.fi/physicalWork_" + URLEncoder.encode(r.getControlNumber.trim, "UTF-8")).toPG.a(bs.fteos_type)
      -- bs.datasource ->- params.datasourceIRI
      -- skos.prefLabel ->- ObjectList(physPrefLabels)
      -- bs.imageURL ->- ObjectList(resolvedImages)
      -- bs.lang ->- ObjectList(physLangsR)
      -- bs.pagecount ->- ObjectList(pageCounts)
      -- bs.firstversion ->- ObjectList(if (!isTranslation) Some(bs.ktrue) else None)
      -- bs.partofseries1 ->- ObjectList(series)
      -- bs.publhistory ->- ObjectList(publhist)
      -- bs.publisher ->- ObjectList(publishers)
      -- bs.publicationdate ->- ObjectList(publTimes)
      -- bs.alkuteos ->- ObjectList(presOrigs)
      -- bs.subLabel ->- ObjectList(physAltLabels))

    val presContributorGraph = if (r.getVariableField("700") == null) emptyGraph else r.getVariableFields("700").map(_.asInstanceOf[DataField])
      .filter(df => (df.getIndicator1() =/= '1' || df.getIndicator2() =/= '2' && df.getSubfield('t') == null))
      .map(df => (fix(df.getSubfield('a').getData.normalize), if (df.getSubfield('e') != null) Some(df.getSubfield('e').getData.normalize.trim.toLowerCase) else None)).flatMap {
        case (actorName: String, actorRole: Option[String]) =>
          val roleProp = if (actorRole.isEmpty) bs.otherCreator else params.actorRoleMap.getOrElse(actorRole.get, bs.otherCreator)
          val actors = {
            val mac = bse.getMatchingActors(actorName, descLang).map(PointedGraph[Rdf])
            if (!mac.isEmpty) mac else
              Seq((URI("http://data.kirjasampo.fi/actor_" + URLEncoder.encode(actorName, "UTF-8")).toPG.a(foaf.Person)
                -- bs.datasource ->- params.datasourceIRI
                -- skos.prefLabel ->- actorName
                -- bs.writerAltName ->- altActorLabelMap.get(actorName)))
          }
          actors.map(_ -<- roleProp -- pres).map(_.graph)
      }.foldLeft(emptyGraph) { (r, c) => r union c }

    val subWorkMap = if (r.getVariableField("700") != null) r.getVariableFields("700").map(_.asInstanceOf[DataField])
      .filter(df => (df.getIndicator1() === '1' && df.getIndicator2() === '2' && df.getSubfield('t') != null))
      .map(df => (fix(df.getSubfield('t').getData.normalize), (fix(df.getSubfield('a').getData.normalize), if (df.getSubfield('e') != null) Some(df.getSubfield('e').getData.normalize.trim.toLowerCase) else None)))
      .groupBy(_._1).mapValues(_.map(_._2))
    else Map[String, Seq[(String, Option[String])]]().empty

    val abstractParts = subWorkMap.flatMap {
      case (partName: String, actorsAndRoles: Seq[(String, Option[String])]) =>
        val creatorLabels = actorsAndRoles.filter(_._2.isEmpty).map(_._1)
        val existingWorks = creatorLabels.flatMap(cl => aresTypes.flatMap(params.collectionPartTypeMap.get(_).map(partType => litLangs.flatMap(ll => bse.getMatchingAbstractWorks(partName, cl, partType, ll)).map(PointedGraph[Rdf])).getOrElse(Seq.empty)))
        if (!existingWorks.isEmpty) existingWorks else {
          val partCreators = creatorLabels.flatMap { partActorName =>
            val mac2 = litLangs.flatMap(ll => bse.getMatchingActors(partActorName, ll)).map(PointedGraph[Rdf])
            if (!mac2.isEmpty) mac2 else Seq((URI("http://data.kirjasampo.fi/actor_" + URLEncoder.encode(partActorName, "UTF-8")).toPG.a(foaf.Person)
              -- skos.prefLabel ->- partActorName
              -- bs.writerAltName ->- altActorLabelMap.get(partActorName)))
          }
          val partTypes = aresTypes.map(params.collectionPartTypeMap.get(_).getOrElse(bs.ateos_type)).map(PointedGraph[Rdf])
          Seq(URI("http://data.kirjasampo.fi/abstractWorkPart_" + URLEncoder.encode(r.getControlNumber.trim, "UTF-8") + "_" + UUID.randomUUID).toPG
            -- rdf.typ ->- ObjectList(partTypes)
            -- bs.datasource ->- params.datasourceIRI
            -- bs.creator ->- ObjectList(partCreators)
            -- bs.origlang ->- ObjectList(if (!isTranslation) physLangsR else origLangsR)
            -- skos.prefLabel ->- ObjectList(litLangs.map(ll => partName.lang(ll))))
        }
    } ++ {
      if (getField("740", 'a').length > 2) {
        getField("740", 'a').flatMap { partName =>
          val mac = aresTypes.flatMap(params.collectionPartTypeMap.get(_).map(partType => litLangs.flatMap(ll => bse.getMatchingAbstractWorks(partName, partType, ll)).map(PointedGraph[Rdf])).getOrElse(Seq.empty))
          if (!mac.isEmpty) mac else {
            val partTypes = aresTypes.map(params.collectionPartTypeMap.get(_).getOrElse(bs.ateos_type)).map(PointedGraph[Rdf])
            Seq((URI("http://data.kirjasampo.fi/abstractWorkPart_" + URLEncoder.encode(r.getControlNumber.trim, "UTF-8") + "_" + UUID.randomUUID).toPG
              -- rdf.typ ->- ObjectList(partTypes)
              -- bs.datasource ->- params.datasourceIRI
              -- bs.creator ->- ObjectList(creators)
              -- bs.origlang ->- ObjectList(if (!isTranslation) physLangsR else origLangsR)
              -- skos.prefLabel ->- ObjectList(litLangs.map(ll => partName.lang(ll)))))
          }
        }
      } else Seq.empty
    }

    val physPartsGraph = abstractParts.map { ap =>
      val workIRI = URI("http://data.kirjasampo.fi/concreteWorkPart_" + URLEncoder.encode(r.getControlNumber.trim, "UTF-8") + "_" + UUID.randomUUID)
      val workName = (ap / skos.prefLabel).headOption.map(o => o.pointer.asInstanceOf[Rdf#LangLiteral].lexicalForm).getOrElse("")
      val partActorsGraph = if (subWorkMap.get(workName).isEmpty) emptyGraph else subWorkMap.get(workName).get.filter(!_._2.isEmpty).map(t => (t._1, t._2.get)).flatMap {
        case (partActorName: String, partActorRole: String) =>
          val actors = {
            val mac2 = litLangs.flatMap(ll => bse.getMatchingActors(partActorName, ll)).map(PointedGraph[Rdf])
            if (!mac2.isEmpty) mac2 else Seq((URI("http://data.kirjasampo.fi/actor_" + URLEncoder.encode(partActorName, "UTF-8")).toPG.a(foaf.Person)
              -- skos.prefLabel ->- partActorName
              -- bs.writerAltName ->- altActorLabelMap.get(partActorName)))
          }
          val roleProp = params.actorRoleMap.getOrElse(partActorRole, bs.otherCreator)
          actors.map(_ -<- roleProp -- workIRI)
      }.map(_.graph).foldLeft(emptyGraph) { (c, g) => c union g }
      (ap -- bs.partVersion ->-
        (workIRI.toPG.a(bs.fworkPart_type)
          -- bs.datasource ->- params.datasourceIRI
          -- skos.prefLabel ->- ObjectList((ap / skos.prefLabel).toSet)
          -- bs.lang ->- ObjectList(physLangsR)
          -- bs.firstversion ->- ObjectList(if (!isTranslation) Some(bs.ktrue) else None)
          -- bs.partOfCollectiveWorks ->- pres)).graph union partActorsGraph
    }.foldLeft(emptyGraph) { (rg, cg) => rg union cg }

    val otherAbstractWorksGraph = {
      val labelsToFind = origPrefLabelsForTranslation ++ physPrefLabels
      labelsToFind.flatMap(pl =>
        aresTypes.flatMap(tiri =>
          creatorLabels.flatMap(cl => bse.getMatchingAbstractWorks(pl.lexicalForm, cl, tiri, pl.lang).map(uri =>
            (pres -<- bs.version -- uri).graph)))).foldLeft(emptyGraph) { (rg, cg) => rg union cg }
    }

    val resolvedDescriptions = Await.result(Future.sequence(descriptionFutures), Duration(60, "seconds")).flatMap(_.toSeq.flatMap(l => litLangs.map(l.lang(_))))

    (ares.toPG
      -- rdf.typ ->- ObjectList(aresTypes.map(PointedGraph[Rdf]))
      -- bs.datasource ->- params.datasourceIRI
      -- bs.isbn ->- ObjectList(isbns.flatMap(isbn => litLangs.map(isbn.lang(_))))
      -- skos.prefLabel ->- ObjectList(absPrefLabels)
      -- bs.creator ->- ObjectList(creators)
      -- bs.maincharacter ->- ObjectList(mainActors)
      -- bs.concreteplace ->- ObjectList(places)
      -- bs.concretetime ->- ObjectList(times)
      -- bs.description ->- ObjectList(descriptions ++ resolvedDescriptions)
      -- bs.ckeywordl ->- ObjectList(if (!conglomerateKeyword.lexicalForm.isEmpty) Some(conglomerateKeyword) else None)
      -- bs.origlang ->- ObjectList(if (!isTranslation) physLangsR else origLangsR)
      -- bs.version ->- pres
      -- bs.version ->- ObjectList(presOrigs)).graph union keywordGraph union otherAbstractWorksGraph union physPartsGraph union presContributorGraph
  }

  def process(file: File)(implicit params: Params): Rdf#Graph = {
    logger.info(s"Processing: ${file}")
    val reader: MarcReader = if (file.getName.endsWith("xmk")) new MarcXmlReader(new FileInputStream(file)) else new MarcStreamReader(new FileInputStream(file))
    reader.filter(r => filterRecord(r)).map(convertRecordToGraph).foldLeft(ops.emptyGraph) { (cg, graph) => cg union graph }
  }

  def main(args: Array[String]): Unit = {
    val conf: Config = ConfigFactory.load()
    val parser = new OptionParser[Params]("marc2bs") {
      head("marc2bs", "0.1")
      (opt[String]("blacklist")
        optional ()
        unbounded ()
        action { (x, c) => c.copy(blacklist = c.blacklist :+ x) }
        text ("Blacklist of files/directories to not import"))
      (opt[String]("sparql-endpoint")
        optional ()
        action { (x, c) => c.copy(sparqlEndpoint = new BookSampoSPARQLEndpoint(x)) }
        text ("SPARQL endpoint to query for references " + (Try(s"(default: ${conf.getString("sparqlEndpoint")})") getOrElse "")))
      (opt[String]("datasourceIRI")
        optional ()
        action { (x, c) => c.copy(datasourceIRI = URI(x).toPG) }
        text ("Data source IRI to mark generated resources with " + (Try(s"(default: ${conf.getString("datasourceIRI")})") getOrElse "")))
      help("help") text ("prints this usage text")
      (arg[String]("<file>...")
        unbounded ()
        required ()
        action { (x, c) => c.copy(files = c.files :+ x) }
        text ("Files/directories to import (recursively)"))
    }

    val languageMap = conf.getObject("languageMap").map(entry => (entry._1, entry._2.unwrapped().asInstanceOf[String])).toMap

    val languageURIMap = conf.getObject("languageURIMap").map(entry => (entry._1, URI(entry._2.unwrapped().asInstanceOf[String]))).toMap

    val yklTypeMap = conf.getObject("yklTypeMap").map(entry => (entry._1, URI(entry._2.unwrapped().asInstanceOf[String]))).toMap

    val hkljTypeMap = conf.getObject("hkljTypeMap").map(entry => (entry._1, URI(entry._2.unwrapped().asInstanceOf[String]))).toMap

    val yklTypeKeywordMap = conf.getObject("yklTypeKeywordMap").map(entry => (entry._1, entry._2.atKey("foo").getObject("foo").map(entry => (entry._1, URI(entry._2.unwrapped().asInstanceOf[String]))).toMap)).toMap

    val hkljTypeKeywordMap = conf.getObject("yklTypeKeywordMap").map(entry => (entry._1, entry._2.atKey("foo").getObject("foo").map(entry => (entry._1, URI(entry._2.unwrapped().asInstanceOf[String]))).toMap)).toMap

    val collectionPartTypeMap = conf.getObject("collectionPartTypeMap").map(entry => (URI(entry._1), URI(entry._2.unwrapped().asInstanceOf[String]))).toMap
    val actorRoleMap = conf.getObject("actorRoleMap").map(entry => (entry._1, URI(entry._2.unwrapped().asInstanceOf[String]))).toMap

    implicit val params: Params = parser.parse(args,
      Params(
        Try(new BookSampoSPARQLEndpoint(conf.getString("sparqlEndpoint"))) getOrElse null,
        Try(URI(conf.getString("datasourceIRI")).toPG) getOrElse null,
        Try[Seq[String]](conf.getStringList("filteredKeywords")) getOrElse Seq.empty,
        languageMap,
        languageURIMap,
        yklTypeKeywordMap,
        yklTypeMap,
        hkljTypeKeywordMap,
        hkljTypeMap,
        collectionPartTypeMap,
        actorRoleMap)) getOrElse sys.exit(1)

    if (params.sparqlEndpoint == null) {
      Console.err.println("SPARQL endpoint must be defined either at the command line or in the configuration!")
      parser.showUsage
      sys.exit(1)
    }
    if (params.datasourceIRI == null) {
      Console.err.println("Datasource IRI must be defined either at the command line or in the configuration!")
      parser.showUsage
      sys.exit(1)
    }

    val files = params.files.flatMap(file => filesAt(params.blacklist)(new File(file)))

    val combinedGraph = ImmutableJenaGraph(
      files.map(process).foldRight(emptyGraph) { (cg, graph) => cg union graph }.asInstanceOf[ImmutableJenaGraph].triples,
      Map(
        "foaf" -> FOAFPrefix[Rdf].prefixIri,
        "skos" -> SKOSPrefix.prefixIri,
        "bs-data" -> "http://data.kirjasampo.fi/",
        "kauno" -> "http://www.yso.fi/onto/kauno/",
        "yso" -> "http://www.yso.fi/onto/yso/",
        "koko" -> "http://www.yso.fi/onto/koko/",
        "dbpedia" -> "http://dbpedia.org/resource/",
        "suo" -> "http://www.yso.fi/onto/suo/",
        "ks-place" -> "http://www.yso.fi/kulttuuriSampo/paikat#",
        "geonames" -> "http://sws.geonames.org/",
        "saha" -> "http://www.seco.tkk.fi/applications/saha#",
        "saha3" -> "http://seco.tkk.fi/saha3/",
        "olgd" -> "http://linkedgeodata.org/triplify/node/",
        "lexvo" -> "http://lexvo.org/id/iso639-3/",
        "btj" -> "http://www.btj.fi/",
        "bs-schema2" -> "http://seco.tkk.fi/saha3/kirjasampo/",
        "bs-schema" -> "http://www.yso.fi/onto/kaunokki#") ++ PrefixMapping.Standard.getNsPrefixMap())
    logger.info("Writing /tmp/marc2bs-out.nt");
    RDFDataMgr.write(new FileOutputStream("/tmp/marc2bs-out.nt"), combinedGraph, RDFFormat.NT)
    logger.info("/tmp/marc2bs-out.nt written");
    //    val graphAsString = writer.asString(combinedGraph, null) getOrElse sys.error("coudn't serialize the graph")
    //    println(graphAsString)
    System.exit(0);
  }
}

