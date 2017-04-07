package fi.sange.marc2bs

import com.typesafe.scalalogging.slf4j.Logging
import java.net.{URLEncoder, URL}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import com.hp.hpl.jena.query.{QuerySolutionMap, ParameterizedSparqlString}
import com.hp.hpl.jena.shared.impl.PrefixMappingImpl
import scala.collection.JavaConversions.mapAsJavaMap
import org.w3.banana.{Turtle, RDFWriter, FOAFPrefix, RDFSPrefix}
import org.geonames.{Style, ToponymSearchCriteria, WebService}
import org.w3.banana.diesel._
import org.apache.jena.riot.{Lang => JenaLang, RDFFormat, RDFDataMgr}
import java.io.FileOutputStream
import org.apache.jena.riot.system.RiotLib
import org.w3.banana.jena.ImmutableJenaGraph

/**
 * Created by jiemakel on 20.11.2013.
 */
object MigratePlacesToGeonames extends Logging {

  import ops._

  val ep = sparqlHttp(new URL("http://217.78.0.106/kirjasampo/sparql"))

  val prefixes = new PrefixMappingImpl().setNsPrefixes(Map(
    "bs-schema" -> "http://www.yso.fi/onto/kaunokki#",
    "rdf" -> rdf.prefixIri,
    "rdfs" -> RDFSPrefix[Rdf].prefixIri,
    "skos" -> SKOSPrefix.prefixIri,
    "text" -> "http://jena.apache.org/text#",
    "foaf" -> FOAFPrefix[Rdf].prefixIri))

  val query =
    """
      |SELECT DISTINCT ?label {
      |  ?a bs-schema:worldPlace|bs-schema:hasLivedIn|bs-schema:placeOfBirth|bs-schema:hasPlaceOfEducation|bs-schema:placeOfDeath ?iri .
      |  ?iri skos:prefLabel ?label .
      |}
    """.stripMargin

  def main(args: Array[String]): Unit = {
    val qs = Await.result(ep.executeSelect(new ParameterizedSparqlString(query, prefixes).asQuery), Duration(60, "seconds"))
    val labels = qs.toIterable.map(row => row("label").get.asInstanceOf[Rdf#Literal]).toSeq
    logger.info(s"got ${labels.size} labels")
    WebService.setUserName("jiemakel")

    val graphs = labels.map(label => {
      Thread.sleep(2000)
      val tsc = new ToponymSearchCriteria()
      tsc.setNameEquals(label.getLiteralLexicalForm)
      if (label.getLiteralLanguage!="") tsc.setLanguage(label.getLiteralLanguage)
      //tsc.setCountryBias("FI")
      tsc.setMaxRows(1)
      val tsr = WebService.search(tsc)
      if (!tsr.getToponyms.isEmpty) {
        logger.info(s"Found ${tsr.getTotalResultsCount} results for label ${label}")
        val place = tsr.getToponyms.get(0)
        (URI("http://sws.geonames.org/" + place.getGeoNameId +"/").toPG
          -- SKOSPrefix.prefLabel ->- makeLangLiteral(label.getLiteralLexicalForm,makeLang(label.getLiteralLanguage))
          -- URI("http://www.w3.org/2003/01/geo/wgs84_pos#lat") ->- (""+place.getLatitude)
          -- URI("http://www.w3.org/2003/01/geo/wgs84_pos#long") ->- (""+place.getLongitude)
          ).graph
      } else {
        logger.info(s"Didn't find any results for label ${label}")
        emptyGraph
      }
    })
    val combinedGraph = graphs.foldLeft(emptyGraph) {(cg,res) => res union cg}
    RDFDataMgr.write(new FileOutputStream("/tmp/geonamesResolve.nt"), combinedGraph, RDFFormat.NTRIPLES)
  }
}
