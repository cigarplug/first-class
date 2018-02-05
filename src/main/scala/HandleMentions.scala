import twitter4j._
// import twitter4j.conf.ConfigurationBuilder
import scala.util.matching.Regex
import scala.collection.JavaConversions._

val twitter = new TwitterFactory().getInstance()

class Tweeter  (handle: String, paginationCount: Int){

  def this(handle: String) = this(handle, 200)

  val p = new Paging()
  p.setCount(paginationCount)

  val handle_pattern: Regex = """@[A-Za-z0-9]*""".r

  val respApi =  twitter.getUserTimeline(handle, p)

  def handleMentionsFreq = respApi.
    map(_.getText).
    flatMap(s => handle_pattern.findAllMatchIn(s)).
    map(_.toString()).
    groupBy(identity).
    mapValues(_.size)
}

