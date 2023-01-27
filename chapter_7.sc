import scala.io.StdIn.readLine
import scala.io.StdIn.readChar

val src = os.pwd / "blog"
val dest = os.pwd / "blog-copy"

val helloFile = dest / "hello" / "hello.txt"


def sync(src: os.Path, dest: os.Path) = {
  for (srcSubPath <- os.walk(src)) {
    val subPath = srcSubPath.subRelativeTo(src)
    val destSubPath = dest / subPath
    (os.isDir(srcSubPath), os.isDir(destSubPath)) match {
      case (false, true) if !os.isDir(destSubPath) =>
        println("deleting: ", destSubPath)
        os.remove(destSubPath)
      case (true, false) =>
        println(s"create folder $subPath")
        os.copy.over(srcSubPath, destSubPath, createFolders = true)
      case (false, false) if !os.exists(destSubPath) ||
          !os.read.bytes(srcSubPath).sameElements(os.read.bytes(destSubPath)) =>
        println(s"copy over file: $subPath")
        os.copy.over(srcSubPath, destSubPath, createFolders = true)
      case _ => // od nothing
    }
  }
}

def interactiveWalk (src: os.Path) = {
  var continue = true
  var curr = src
  while (continue) {
    if (os.pwd != curr) {
      println(s"changing directory: $curr")
      os.walk(curr)
    }
    os.walk(curr)

    val directoryContents = os.list(curr)
    val directories = directoryContents.filter(os.isDir(_))

    directories.foreach(println)

    val input = readChar()
    if(input == null) {
      println("exiting")
      continue = false
    } else {
      input match {
        case 'j' => {
          val next = curr / os.up
          curr = next
        }
        case 'k' => {
          println("choose a directory:")
          directories.zip(LazyList.from(1))
            .foreach(x => println(s"${x._2}. ${x._1}"))
          val choice = readLine().toInt
          println(s"choice: $choice")
          if (choice > 0 && choice <= directories.length ) {
            val next =  directories(choice - 1)
            println(s"change directory to $next")
            curr = next
          }
        }
        case _ => {

        }
      }
      println("")
      println("")
    }
  }
}

