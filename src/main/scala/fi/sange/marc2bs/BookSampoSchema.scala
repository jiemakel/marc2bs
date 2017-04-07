/**
 *
 */
package fi.sange.marc2bs

import org.w3.banana.PrefixBuilder

/**
 * @author jiemakel
 *
 */
object BookSampoSchema extends PrefixBuilder("bs", "http://www.yso.fi/onto/kaunokki#")(ops) {
  val time_type = ops.URI("http://www.yso.fi/onto/time-schema#Time")
  val place_type = apply("Place")
  val conglomeratekeyword_type = apply("asiasanaYhdiste")
  val series_type = apply("sarja")
  val seriesPart_type = apply("sarjaOsa")
  val publisher_type = apply("kustantaja")
  val keyword_type = apply("keyword")
  val character_type = apply("henkiloHahmo")
  val ateos_type = apply("teos")
  val kansikuva_type = apply("kansi")
  val fteos_type = apply("fyysinen_teos")
  val fworkPart_type = apply("fyysisen_teoksen_osa")
  val ktrue = apply("true")

  val lang = apply("kieli")
  val creator = apply("tekija")
  val version = apply("manifests_in")
  val partVersion = apply("manifests_in_part")
  val partOfCollectiveWorks = apply("partOfCollectiveWorks")
  val firstversion = apply("onEnsimmainenVersio")
  val origlang = apply("alkukieli")
  val publisher = apply("hasPublisher")
  val publicationdate = apply("ilmestymisvuosi")
  val pagecount = apply("sivuLkm")
  val ckeyword = apply("ketjutettu_asiasana")
  val ckeywordl = apply("ketjutettu_asiasana_l")
  val publhistory = apply("analyticLabel")
  val datasource = apply("tietolahde")
  val osanumero = apply("osanumero")

  val subLabel = ops.URI("http://seco.tkk.fi/saha3/kirjasampo/alanimeke")

  val writerAltName = ops.URI("http://seco.tkk.fi/saha3/kirjasampo/kirjailijanMuuNimi")

  val isbn = ops.URI("http://schema.org/isbn")

  val maincharacter = apply("paahenkilo")
  val genre = apply("genre")
  val theme = apply("teema")
  val actor = apply("toimija")
  val place = apply("paikka")
  val time = apply("tapahtumaaika")
  val localkeyword = apply("asiasana")
  val concreteplace = apply("worldPlace")
  val concretetime = apply("hasTimeOfStory")
  val partofseries2 = apply("sarjaInstanssi")
  val partofseries1 = apply("sarjassa")
  val alkuteos = apply("alkuteos")
  val translator = apply("kaantaja")
  val illustrator = apply("kuvittaja")
  val otherCreator = apply("toimittaja")
  val imageURL = apply("kansikuva")
  val imageURL2 = ops.URI("http://kulttuurisampo.fi/annotaatio#tiedostoUrl")

  val description = ops.URI("http://purl.org/dc/elements/1.1/description")
}
