def fizzbuzz(limit: Int) {
  for (i <- Range.inclusive(1, limit)) {
    println(
      if (i % 3 == 0 && i % 5 == 0) "FizzBuzz"
      else if (i % 3 == 0) "Fizz"
      else if (i % 5 == 0) "Buzz"
      else i
    )
  }
}

def hello(title: String, firstName: String, lastName: Option[String]) = {
  lastName match {
    case Some(name) => println(s"Hello $title. $name")
    case None => println(s"Hello $firstName")
  }
}

def myLoop(start: Int, end: Int) (callback: Int => Unit) = {
  for (i <- Range(start, end)) {
    callback(i)
  }
}

def flexiblefizzbuzz(s: String => Unit) {
  for (i <- Range.inclusive(1, 100)) {
    s(
      if (i % 3 == 0 && i % 5 == 0) "FizzBuzz"
      else if (i % 3 == 0) "Fizz"
      else if (i % 5 == 0 ) "Buzz"
      else s"$i"
    )
  }
}

class Msg(val id: Int, val parent: Option[Int], val text: String)

def printMessages(messages: Array[Msg]): Unit = {
  def printMessagesInner (parent: Option[Int], indent: String): Unit = {
    for (message <- messages if message.parent == parent) {
      println(s"$indent#${message.id} ${message.text}")
      printMessagesInner(Some(message.id), indent + "    ")
    }
  }

  printMessagesInner(None, "")
}

val messages =  Array(
  new Msg(0, None, "Hello"),
  new Msg(1, Some(0), "World"),
  new Msg(2, None, "I am Cow"),
  new Msg(3, Some(2), "hear me moo!"),
  new Msg(4, Some(2), "Here I stand"),
  new Msg(5, Some(2), "I am Cow"),
  new Msg(6, Some(3), "Here me moo, moo!")
)

def withFileWriter[T](fileName: String)(handler: java.io.BufferedWriter => T) = {
  val output = java.nio.file.Files.newBufferedWriter(java.nio.file.Paths.get(fileName))
  try handler(output)
  finally output.close()
}

def withFileReader[T](fileName: String)(handler: java.io.BufferedReader => T) = {
  val input = java.nio.file.Files.newBufferedReader(java.nio.file.Paths.get(fileName))
  try handler(input)
  finally input.close()
}
