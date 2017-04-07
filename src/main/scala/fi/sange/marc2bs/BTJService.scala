package fi.sange.marc2bs

import dispatch._, Defaults._
import com.typesafe.scalalogging.slf4j.Logging
import scala.None

/**
 * Created by jiemakel on 11.11.2013.
 */
object BTJService extends Logging {

  val id = "495d5b089d36f7c2"
  val descriptionURL = url(s"http://armas.btj.fi/request.php?id=${id}&error=1&qtype=b")
  val imageURL = url(s"http://armas.btj.fi/request.php?id=${id}&error=1&qtype=m&ftype=07")

  def getDescription(pid : String) : Future[Option[String]] = {
    Http((descriptionURL <<? Map("pid"->pid))).either.map({
      case Left(error) => {
        logger.warn(s"Couldn't fetch description for $pid",error); 
        None
      }
      case Right(r) => {
        if (r.getStatusCode==404) None
        else if (r.getStatusCode==200) {
          val content = r.getResponseBody()
          val indS = content.lastIndexOf("<p>")
          val indE = content.lastIndexOf("</p>")
          if (indS != -1 && indE != -1)
            Some(content.substring(indS+3,indE).trim)
          else {
            logger.warn(s"Content does not contain description for $pid: $content")
            None
          }
        } else {
          logger.warn(s"Bad status code when getting description for $pid: $r")
          None
        }
      }
    })
  }

  def getImageURL(pid : String) : Future[Option[String]] = {
    val req = imageURL  <<? Map("pid"->pid)
    Http(req.HEAD).either.map({
      case Left(error) => {
        logger.warn(s"Couldn't fetch image URL for $pid",error); 
        None
      }
      case Right(r) => if (r.getStatusCode==404) None
        else if (r.getStatusCode==200) Some(req.url)
        else {
          logger.warn(s"Bad status code when getting image URL for $pid: $r")
          None
        }
    })
  }
}
