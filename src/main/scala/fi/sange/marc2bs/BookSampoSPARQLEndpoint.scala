/**
 *
 */
package fi.sange.marc2bs

import com.hp.hpl.jena.query.{ ParameterizedSparqlString, QueryParseException, QuerySolutionMap }
import com.hp.hpl.jena.shared.impl.PrefixMappingImpl
import com.typesafe.scalalogging.slf4j.Logging
import java.net.URL
import org.w3.banana.{ FOAFPrefix, RDFSPrefix }
import org.w3.banana.jena.util.QuerySolution
import scala.collection.JavaConversions.mapAsJavaMap
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{ Failure, Success, Try }
import scalaz.Memo

/**
 * @author jiemakel
 *
 */
class BookSampoSPARQLEndpoint(val endpointURL: String) extends Logging {
  private val ep = sparqlHttp(new URL(endpointURL))

  import ops._

  @annotation.tailrec
  private def retry[T](n: Int)(fn: => T): T = {
    Try { fn } match {
      case Success(x) => x
      case _ if n > 1 =>
        logger.warn(s"$fn failed, ${n - 1} tries left."); retry(n - 1)(fn)
      case Failure(e) => throw e
    }
  }

  val prefixes = new PrefixMappingImpl().setNsPrefixes(Map(
    "bs-schema" -> "http://www.yso.fi/onto/kaunokki#",
    "rdf" -> rdf.prefixIri,
    "rdfs" -> RDFSPrefix[Rdf].prefixIri,
    "skos" -> SKOSPrefix.prefixIri,
    "text" -> "http://jena.apache.org/text#",
    "foaf" -> FOAFPrefix[Rdf].prefixIri))

  val qsol = QuerySolution()

  def toQuery(query: String, bindings: Map[String, Rdf#Node]): Rdf#Query = {
    val psq = new ParameterizedSparqlString(query, if (bindings != null) qsol.getMap(bindings).asInstanceOf[QuerySolutionMap] else null, prefixes)
    try {
      psq.asQuery()
    } catch {
      case e: QueryParseException => logger.error(s"Couldn't parse query $psq", e); throw e
    }
  }

  private val bs = BookSampoSchema

  val alreadyExistsAsEditedQuery =
    s"""
      |ASK {
      |  { ?s ?p ?iri }
      |  UNION
      |  { ?iri ?p ?o }
      |  FILTER NOT EXISTS { ?iri <${bs.datasource}> ?datasourceIRI }
      |}""".stripMargin

  def alreadyExistsAsEdited(iri: Rdf#URI, datasourceIRI: Rdf#Node): Boolean = {
    val bindings = Map("iri" -> iri, "datasourceIRI" -> datasourceIRI)
    retry(10)(Await.result(ep.executeAsk(toQuery(alreadyExistsAsEditedQuery, bindings)), Duration(60, "seconds")))
  }

  def getMatches(query: String, name: String, lang: Rdf#Lang): Iterable[Rdf#URI] = {
    val bindings = Map("name" -> name.toNode, "lname" -> name.lang(lang))
    retry(10)(Await.result(ep.executeSelect(toQuery(query //      .replaceAllLiterally("|LANG|",lang.toString)
    //      .replaceAllLiterally("|NAME|",name.replaceAllLiterally("\"","\\\\\\\""))
    , bindings)), Duration(60, "seconds"))
      .toIterable.map(row => row("iri").get.asInstanceOf[Rdf#URI]))
  }

  private val ls =
    s"""
      |  { ?iri skos:prefLabel|skos:altLabel|<${bs.writerAltName}> ?name }
      |  UNION
      |  { ?iri skos:prefLabel|skos:altLabel|<${bs.writerAltName}> ?lname }
    """.stripMargin
  /*    s"""
      |  { ?iri text:query ("label_subjects_" "\\"|NAME|\\"" ) }
      |  UNION
      |  { ?iri text:query ("label_subjects_|LANG|" "\\"|NAME|\\"" ) }
    """.stripMargin */

  private val qs = "SELECT DISTINCT ?iri {" + ls

  val getMatchingActorsQuery = qs + s"?book <${bs.creator}>|<${bs.translator}>|<${bs.illustrator}>|<${bs.otherCreator}> ?iri }"

  val getMatchingActorsM = Memo.mutableHashMapMemo[(String, Rdf#Lang), Iterable[Rdf#URI]] {
    case (name, lang) => getMatches(getMatchingActorsQuery, name, lang)
  }

  def getMatchingActors(name: String, lang: Rdf#Lang): Iterable[Rdf#URI] = {
    getMatchingActorsM((name, lang))
  }

  val getMatchingPlacesQuery = qs + s"?book <${bs.concreteplace}> ?iri }"

  val getMatchingPlacesM = Memo.mutableHashMapMemo[(String, Rdf#Lang), Iterable[Rdf#URI]] {
    case (name, lang) => getMatches(getMatchingPlacesQuery, name, lang)
  }

  def getMatchingPlaces(name: String, lang: Rdf#Lang): Iterable[Rdf#URI] = {
    getMatchingPlacesM((name, lang))
  }

  val getMatchingTimesQuery = qs + s"?iri a <${bs.time_type}> }"

  val getMatchingTimesM = Memo.mutableHashMapMemo[(String, Rdf#Lang), Iterable[Rdf#URI]] {
    case (name, lang) => getMatches(getMatchingTimesQuery, name, lang)
  }
  def getMatchingTimes(name: String, lang: Rdf#Lang): Iterable[Rdf#URI] = {
    getMatchingTimesM((name, lang))
  }

  val getMatchingPublishersQuery = qs + s"?book <${bs.publisher}> ?iri }"

  val getMatchingPublishersM = Memo.mutableHashMapMemo[(String, Rdf#Lang), Iterable[Rdf#URI]] {
    case (name, lang) => getMatches(getMatchingPublishersQuery, name, lang)
  }
  def getMatchingPublishers(name: String, lang: Rdf#Lang): Iterable[Rdf#URI] = {
    getMatchingPublishersM((name, lang))
  }

  val getMatchingSeriesQuery = qs + s"?book <${bs.partofseries1}>|<${bs.partofseries2}> ?iri }"

  val getMatchingSeriesM = Memo.mutableHashMapMemo[(String, Rdf#Lang), Iterable[Rdf#URI]] {
    case (name, lang) => getMatches(getMatchingSeriesQuery, name, lang)
  }

  def getMatchingSeries(name: String, lang: Rdf#Lang): Iterable[Rdf#URI] = {
    getMatchingSeriesM((name, lang))
  }

  val getMatchingAbstractWorksQueryWithCreatorName = qs + s"""
      | { ?creatorIRI skos:prefLabel|skos:altLabel|<${bs.writerAltName}> ?cname }
      | UNION
      | { ?creatorIRI skos:prefLabel|skos:altLabel|<${bs.writerAltName}> ?lcname }
      | ?iri <${bs.creator}> ?creatorIRI .
      | ?iri rdf:type/rdfs:subClassOf* ?typeIRI .
      | }""".stripMargin
  /*      | ?iri rdf:type/rdfs:subClassOf* ?typeIRI .
      | ?iri <${bs.creator}> ?creatorIRI .
      | { ?creatorIRI text:query ("label_subjects_" "\\"|CNAME|\\"" ) }
      | UNION
      | { ?creatorIRI text:query ("label_subjects_|LANG|" "\\"|CNAME|\\"" ) }
      | }""".stripMargin */

  def getMatchingAbstractWorks(name: String, creatorName: String, typeIRI: Rdf#URI, lang: Rdf#Lang): Iterable[Rdf#URI] = {
    val bindings = Map("typeIRI" -> typeIRI, "name" -> name.toNode, "lname" -> name.lang(lang), "cname" -> creatorName.toNode, "lcname" -> creatorName.lang(lang))
    retry(10)(Await.result(ep.executeSelect(toQuery(getMatchingAbstractWorksQueryWithCreatorName //      .replaceAllLiterally("|LANG|",lang.toString)
    //      .replaceAllLiterally("|NAME|",name.replaceAllLiterally("\"","\\\\\\\""))
    //      .replaceAllLiterally("|CNAME|",creatorName.replaceAllLiterally("\"","\\\\\\\""))
    , bindings)), Duration(60, "seconds"))
      .toIterable.map(row => row("iri").get.asInstanceOf[Rdf#URI]))
  }

  val getMatchingAbstractWorksQueryWithoutCreatorName = qs + "?iri rdf:type/rdfs:subClassOf* ?typeIRI }"

  def getMatchingAbstractWorks(name: String, typeIRI: Rdf#URI, lang: Rdf#Lang): Iterable[Rdf#URI] = {
    val bindings = Map("typeIRI" -> typeIRI, "name" -> name.toNode, "lname" -> name.lang(lang))
    retry(10)(Await.result(ep.executeSelect(toQuery(getMatchingAbstractWorksQueryWithoutCreatorName //      .replaceAllLiterally("|LANG|",lang.toString)
    //      .replaceAllLiterally("|NAME|",name.replaceAllLiterally("\"","\\\\\\\""))
    , bindings)), Duration(60, "seconds"))
      .toIterable.map(row => row("iri").get.asInstanceOf[Rdf#URI]))
  }

  val getMatchingConcreteWorksQuery = qs + s"""
      | { ?creatorIRI skos:prefLabel|skos:altLabel|<${bs.writerAltName}> ?cname }
      | UNION
      | { ?creatorIRI skos:prefLabel|skos:altLabel|<${bs.writerAltName}> ?lcname }
      | ?awork <${bs.version}> ?iri .
      | ?awork <${bs.creator}> ?creatorIRI .
      | ?awork rdf:type/rdfs:subClassOf* ?typeIRI .
      | }""".stripMargin
  /*      | ?awork <${bs.version}> ?iri .
      | ?awork rdf:type/rdfs:subClassOf* ?typeIRI .
      | ?awork <${bs.creator}> ?creatorIRI .
      | { ?creatorIRI text:query ("label_subjects_" "\\"|CNAME|\\"" ) }
      | UNION
      | { ?creatorIRI text:query ("label_subjects_|LANG|" "\\"|CNAME|\\"" ) }
      | }""".stripMargin */

  def getMatchingConcreteWorks(name: String, creatorName: String, typeIRI: Rdf#URI, lang: Rdf#Lang): Iterable[Rdf#URI] = {
    val bindings = Map("typeIRI" -> typeIRI, "name" -> name.toNode, "lname" -> name.lang(lang), "cname" -> creatorName.toNode, "lcname" -> creatorName.lang(lang))
    retry(10)(Await.result(ep.executeSelect(toQuery(getMatchingConcreteWorksQuery //      .replaceAllLiterally("|LANG|",lang.toString)
    //      .replaceAllLiterally("|NAME|",name.replaceAllLiterally("\"","\\\\\\\""))
    //      .replaceAllLiterally("|CNAME|",creatorName.replaceAllLiterally("\"","\\\\\\\""))
    , bindings)), Duration(60, "seconds"))
      .toIterable.map(row => row("iri").get.asInstanceOf[Rdf#URI]))
  }

  val getMatchingKeywordsAndTypesQuery =
    s"""
      |SELECT ?iri ?property WHERE {
      |  {
      |    SELECT ?iri ?property WHERE {
      |      $ls
      |      ?book ?property ?iri .
      |      FILTER(?property=<${bs.concretetime}>||?property=<${bs.concreteplace}>||?property=<${bs.genre}>||?property=<${bs.theme}>||?property=<${bs.actor}>||?property=<${bs.place}>||?property=<${bs.time}>||?property=<${bs.localkeyword}>)
      |    }
      |    GROUP BY ?iri ?property
      |    ORDER BY DESC(COUNT(*))
      |  } UNION {
      |    SELECT ?iri ?property WHERE {
      |      BIND(STRLANG(?name,"fi") AS ?namefi)
      |      ?iri skos:prefLabel|skos:altLabel|<${bs.writerAltName}> ?namefi .
      |      ?book ?property ?iri .
      |      FILTER(?property=<${bs.concretetime}>||?property=<${bs.concreteplace}>||?property=<${bs.genre}>||?property=<${bs.theme}>||?property=<${bs.actor}>||?property=<${bs.place}>||?property=<${bs.time}>||?property=<${bs.localkeyword}>)
      |    }
      |    GROUP BY ?iri ?property
      |    ORDER BY DESC(COUNT(*))
      |  }
      |  FILTER(BOUND(?iri))
      |}
      |LIMIT 1
    """.stripMargin

  val getMatchingKeywordsAndTypesM = Memo.mutableHashMapMemo[(String, Rdf#Lang), Iterable[(Rdf#URI, Rdf#URI)]] {
    case (name, lang) =>
      val bindings = Map("name" -> name.toNode, "lname" -> name.lang(lang))
      retry(10)(Await.result(ep.executeSelect(toQuery(getMatchingKeywordsAndTypesQuery //      .replaceAllLiterally("|LANG|",lang.toString)
      //      .replaceAllLiterally("|NAME|",name.replaceAllLiterally("\"","\\\\\\\""))
      , bindings)), Duration(60, "seconds"))
        .toIterable.filter(_("iri").isSuccess).map(row => (row("iri").get.asInstanceOf[Rdf#URI], row("property").get.asInstanceOf[Rdf#URI])))
  }

  def getMatchingKeywordsAndTypes(name: String, lang: Rdf#Lang): Iterable[(Rdf#URI, Rdf#URI)] = {
    getMatchingKeywordsAndTypesM((name, lang))
  }

}
