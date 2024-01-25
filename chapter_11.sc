import $ivy.`org.jsoup:jsoup:1.13.1`, org.jsoup._
import collection.JavaConverters._
import scala.collection.mutable.Buffer

// val doc = Jsoup.connect("http://en.wikipedia.org").get()

// val headlines = doc.select("#mp-itn b a").asScala

// val sel = doc.select("#mp-itn b a")

// val nhi = Jsoup.connect("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2505250/").get()

def downloadMDN (): Buffer[(String, String, String, Buffer[(String, String)])] = {
  val hostname = "https://developer.mozilla.org"
  val moz = Jsoup.connect("https://developer.mozilla.org/en-US/docs/Web/API").get()

  val links = moz.select("h2#interfaces").nextAll.select("div.index a").asScala

  val linkData = links.map(link => (link.attr("href"), link.text))

  def downloadLink(url: String) = {
    val doc = Jsoup.connect(s"$hostname/$url").get()

    val summary = doc.select("article.main-page-content > p").asScala.headOption match {
      case Some(n) => n.text
      case None => ""
    }

    val nameElements = doc.select("article.main-page-content dl dt").asScala
    val nameDescPairs = nameElements.map(element => (element, element.nextElementSibling))
    val textPairs = nameDescPairs.map{
      case (k, v) => (k.text, v.text)
    }
    (summary, textPairs)
  }

  for ((url, name) <- linkData) yield {
    println(s"scraping: $url" )
    val (summary, methodsAndProperties) = downloadLink(url)
    (url, name, summary, methodsAndProperties)
  }
}

// val url = "https://raw.githubusercontent.com/handsonscala/handsonscala/v1/resources/11/Lobsters.html"
// val doc = Jsoup.connect(url).get()

// val comments = doc.select("ol.comments")

// val commentSubtree = comments.select("> li.comments_subtree")


val url = "https://lobste.rs"
val stories = Jsoup.connect(url).get().select("li.story").asScala
  .map {
    story => {
      val title =  story.select("a.u-url").asScala.headOption
      val comments = story.select("span.comments_label").asScala.headOption
      (title, comments)  match {
         case (Some(t), Some(c)) => {
           val commentsLink = c.select("a").asScala.headOption match {
             case Some(link) => link.attr("href")
             case None => None
           }
           (t.text, commentsLink)
         }
         case _ => None
       }
   }
 }

val url2 = "https://lobste.rs/s/3gorvj/why_i_returned_mac_mini_m2_pro"

// def commentsSubtree (a: ) = {
//   val commentText = a.select("> div.comment").asScala.map(_.text)
//   commentText
// }

val doc = Jsoup.connect(url2).get()
// val root = doc.select("div#inside").asScala.head
val root = doc.select("ol.comments.comments1").asScala.head
val trees = root.select("> li.comments_subtree").asScala
  .filter {
    _.select("div.comment_form_container").asScala.headOption match {
      case Some(_) => false
      case None => true
    }
  }

val tree = trees(0)

val details = tree.select("div.comment > div.details")
val commentText = details.select("> div.comment_text")
val nested = tree.select("div.comment")
