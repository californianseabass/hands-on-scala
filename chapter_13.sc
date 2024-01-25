import scala.concurrent._, duration.Duration.Inf, java.util.concurrent.Executors
import $ivy.`at.favre.lib:bcrypt:0.9.0`
import at.favre.lib.crypto.bcrypt.{BCrypt, LongPasswordStrategies}

implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

val hasher = BCrypt.`with`(LongPasswordStrategies.hashSha512(BCrypt.Version.VERSION_2A))

val WIKIPEDIA_API_URL = "https://en.wikipedia.org/w/api.php"

def hash(name: String) = {
  val salt = name.take(16).padTo(16, ' ')
  println(s"Salt: $salt")
  val bytes = hasher.hash(17, salt.getBytes, os.read.bytes(os.pwd / name))
  new String(bytes)
}

val chinatownUrl = "https://github.com/handsonscala/handsonscala/raw/v1/resources/13/Chinatown.jpg"
val kresgeUrl = "https://github.com/handsonscala/handsonscala/raw/v1/resources/13/Kresge.jpg"
val memorialUrl = "https://github.com/handsonscala/handsonscala/raw/v1/resources/13/Memorial.jpg"
val zcenterUrl = "https://github.com/handsonscala/handsonscala/raw/v1/resources/13/ZCenter.jpg"

def downloadJPEG (url: String, outputFileName: String) = {
  val output = os.pwd / outputFileName
  os.remove(output)
  println(s"start download of $url")
  os.write(
    output,
    requests.get.stream(url)
  )
}

def timeAndLog(f: (String, String) => Unit): (String, String) => Unit = {
  return (url, string) => {
    val (_, duration) = time(f(url, string))
    println(s"elapsed: $duration")
  }
}

val toDownload: Array[(String, String)] = Array(
(chinatownUrl, "chinatown.jpg"),
(kresgeUrl, "kresge.jpg"),
(memorialUrl, "memorial.jpg"),
(zcenterUrl, "zcenter.jpg")
)

// val imageDownloads = toDownload.map{
//   case (url, fileOutputName) => Future{ timeAndLog(downloadJPEG)(url, fileOutputName) }
// }

val f1 = Future { "hello" + 123 + "world"}


def hashImages (): Unit = {
  val (hashes, elapsed) = time {
    val futures = for (p <- toDownload.map(_._2)) yield  Future{
      println("Hashing " + p)
      hash(p)
    }
    futures.map(Await.result(_, Inf))
  }
  println(s"Time elapsed, hashing images: $elapsed")
}

def fetchLinks(title: String): Seq[String] = {
  val resp = requests.get(
    WIKIPEDIA_API_URL,
    params = Seq(
      "action" -> "query",
      "titles" -> title,
      "prop" -> "links",
      "format" -> "json"
    )
  )

  for {
    page <- ujson.read(resp)("query")("pages").obj.values.toSeq
    links <- page.obj.get("links").toSeq
    link <- links.arr
  } yield link("title").str
}

def fetchAllLink(startTitle: String, depth: Int): Set[String] = {
  var seen = Set(startTitle)
  var current = Set(startTitle)
  for (i <- Range(0, depth)) {
    val futures = for (title <- current) yield Future { fetchLinks(title) }
    val nextTitleLists = futures.map(Await.result(_, Inf))
    current = nextTitleLists.flatten.filter(!seen.contains(_))
    seen = seen ++ current
  }
  seen
}

def fetchAllLinksRec(startTitle: String, maxDepth: Int, maxConcurrency: Int): Future[Set[String]] = {
  def rec(current: Seq[(String, Int)], seen: Set[String]): Future[Set[String]] = {
    if (current.isEmpty ) Future.successful(seen)
    else {
      val (throttled, remaining) = current.splitAt(maxConcurrency)
      val futures: Seq[Future[(Seq[String], Int)]] = for ((title, depth) <- throttled)
        yield fetchLinksAsync(title).map((_, depth))

      Future.sequence(futures).map { nextTitleLists => {
        val flattened = for {
          (titles, depth) <- nextTitleLists
          title <- titles
          if !seen.contains(title) && depth < maxDepth
        } yield (title, depth + 1)
        rec(remaining ++ flattened, seen ++ flattened.map(_._1))
      }}.flatten

    }
  }

  rec(Seq(startTitle -> 0), Set(startTitle))
}

import $ivy.`org.asynchttpclient:async-http-client:2.5.2`
import scala.concurrent._

val asyncHttpClient = org.asynchttpclient.Dsl.asyncHttpClient()

def fetchLinksAsync(title: String)(implicit ec: ExecutionContext): Future[Seq[String]] = {
  val p = Promise[String]

  val listenableFut = asyncHttpClient.prepareGet(WIKIPEDIA_API_URL)
  .addQueryParam("action", "query").addQueryParam("titles", title)
  .addQueryParam("prop", "links").addQueryParam("format", "json")
  .execute()

  listenableFut.addListener(() => p.success(listenableFut.get().getResponseBody), null)

  val scalaFut: Future[String] = p.future
  scalaFut.map {
    responseBody =>
    for {
      page <- ujson.read(responseBody)("query")("pages").obj.values.toSeq
      links <- page.obj.get("links").toSeq
      link <- links.arr
    } yield link("title").str
  }
}

import $ivy.`org.jsoup:jsoup:1.13.1`, org.jsoup._
import collection.JavaConverters._
import scala.collection.mutable.Buffer

def downloadMDN (): Seq[(String, String, String)] = {
  val hostname = "https://developer.mozilla.org"
  val moz = Jsoup.connect("https://developer.mozilla.org/en-US/docs/Web/API").get()

  val links = moz.select("h2#interfaces").nextAll.select("div.index a").asScala
  println(s"number of links ${links.size}")

  val linkData = links.map(link => (link.attr("href"), link.text))

  def downloadLink(url: String): (String, String, String) = {
    println(s"scraping: $url" )
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
    val (name, description) = textPairs.toArray.headOption match {
      case Some(x) => (x._1, x._2)
      case None => ("", "")
    }
    (summary, name, description)
  }

  val downloads = for ((url, name) <- linkData) yield Future { downloadLink(url) }
  Await.result(Future.sequence(downloads), Inf).toSeq
}

def mergeSort[T: Ordering](items: IndexedSeq[T]): IndexedSeq[T] = {
  if (items.length <= 1) items
  else {
    val (left, right) = items.splitAt(items.length / 2)
    val (sortedLeft, sortedRight) = (mergeSort(left), mergeSort(right))
    merge(sortedLeft, sortedRight)
  }
}

def merge[T: Ordering](left: IndexedSeq[T], right: IndexedSeq[T]) = {
  var (leftIdx, rightIdx) = (0, 0)
  val output = IndexedSeq.newBuilder[T]

  while (leftIdx < left.length || rightIdx < right.length) {
    val takeLeft = (leftIdx < left.length, rightIdx < right.length) match {
      case (true, false) => true
      case (false, true) => false
      case (true, true) => Ordering[T].lt(left(leftIdx), right(rightIdx))
    }
    if (takeLeft) {
      output += left(leftIdx)
      leftIdx += 1
    } else {
      output += right(rightIdx)
      rightIdx += 1
    }
  }

  output.result
}
