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

  val hashtag_pattern: Regex = """#[A-Za-z0-9]*""".r

  val respApi =  twitter.getUserTimeline(handle, p)

  private def asIs() = respApi

  def getTweets() = asIs().map(_.getText)

  def hashtagUsageFreq = getTweets().
    flatMap(s => hashtag_pattern.findAllMatchIn(s)).
    map(_.toString.toLowerCase()).
    groupBy(identity).
    mapValues(_.size)

  def handleMentionsFromTweetText = getTweets().
    flatMap(s => handle_pattern.findAllMatchIn(s)).
    map(_.toString()).
    groupBy(identity).
    mapValues(_.size)

  def handleMentionsFromLibraryMethod = respApi.
    flatMap(_.getUserMentionEntities.map(_.getScreenName)).
    groupBy(s => s).
    mapValues(_.size)

  def wordUsageFreq = getTweets().
    flatMap(_.toLowerCase.split("\\s+")).
    groupBy(s => s).
    mapValues(_.size)
}
