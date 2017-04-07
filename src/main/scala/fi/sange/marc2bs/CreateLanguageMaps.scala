package fi.sange.marc2bs

import org.w3.banana.RDFXML
import org.w3.banana.RDFReader
import scalax.io.Output
import scalax.io.Resource
import scalax.io.Codec
import java.io.FileInputStream
import org.w3.banana.RDFOps
import org.w3.banana.syntax._
import org.w3.banana.diesel._
import org.w3.banana.PointedGraph
import scala.util.Success

object CreateLanguageMaps {

  implicit val ops = RDFOps[Rdf]
  import ops._

  def main(args: Array[String]): Unit = {

    val reader = RDFReader[Rdf, RDFXML]
    val graph: Rdf#Graph = reader.read(new FileInputStream("/home/jiemakel/lexvo.rdf"), null) getOrElse sys.error("Couldn't read LEXVO")

    trait PURICompanionObject extends Function1[String, Rdf#URI] {
      def apply(s: String): Rdf#URI = makeUri(graph.getPrefixMapping().expandPrefix(s))
      def unapply(uri: Rdf#URI): Option[String] = Some(fromUri(uri))
    }
    object PURI extends PURICompanionObject

    val output: Output = Resource.fromFile("src/main/resources/languageMap.conf")
    implicit val codec = Codec.UTF8
    for {
      processor <- output.outputProcessor
      out = processor.asOutput
    } {
      out.write("languageMap {\n")
      find(graph, ANY, PURI("lvont:iso6392BCode"), ANY)
        .map(t => (fromTriple(t)._3.as[String], (PointedGraph[Rdf](fromTriple(t)._1, graph) / PURI("lvont:iso639P1Code")).exactlyOneAs[String]))
        .collect { case (Success(key), Success(value)) => (key, value) }
        .foreach { (tuple) => out.write(s"  ${tuple._1} : ${tuple._2}\n") }
      out.write("}")
    }
    val output2: Output = Resource.fromFile("src/main/resources/languageURIMap.conf")
    for {
      processor <- output2.outputProcessor
      out = processor.asOutput
    } {
      out.write("languageURIMap {\n")
      find(graph, ANY, PURI("lvont:iso6392BCode"), ANY)
        .map(t => (fromTriple(t)._3.getLiteralLexicalForm(), fromTriple(t)._1.getURI()))
        .foreach { (tuple) => out.write(s"  ${tuple._1} : '${tuple._2}'\n") }
      out.write("}")
    }
  }

}