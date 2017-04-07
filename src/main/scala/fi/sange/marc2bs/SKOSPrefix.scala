/**
 *
 */
package fi.sange.marc2bs

import org.w3.banana.PrefixBuilder

/**
 * @author jiemakel
 *
 */
object SKOSPrefix extends PrefixBuilder("skos", "http://www.w3.org/2004/02/skos/core#")(ops) {
  val Concept = apply("Concept")
  val broader = apply("broader")
  val prefLabel = apply("prefLabel")
  val altLabel = apply("altLabel")
  val narrower = apply("narrower")
  val definition = apply("definition")
  val notation = apply("notation")
  val member = apply("member")
  val related = apply("related")
  val note = apply("note")
  val broaderTransitive = apply("broaderTransitive")
  val narrowerTransitive = apply("narrowerTransitive")
  val exactMatch = apply("exactMatch")
  val skosMappingExactMatch = ops.URI("http://www.w3.org/2004/02/skos/mapping#exactMatch")
}