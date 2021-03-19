package objsets

object GoogleVsApple {

  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val allTweets: TweetSet = TweetReader.allTweets

  def corpFeeds(allTweets: TweetSet, corpKeywords: List[String]): TweetSet = {
    allTweets.filter { t => corpKeywords.exists(kw => t.text.contains(kw)) }
  }

  val googleTweets: TweetSet = corpFeeds(allTweets, google)
  val appleTweets: TweetSet = corpFeeds(allTweets, apple)

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {

  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
