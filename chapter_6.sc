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

class Trie[T]() {
  /*
   * Each node represents a character, and a path of characters represents a "word", "substring."
   */
  class Node(
    var data: Option[T],
    val children: collection.mutable.Map[Char, Node] = collection.mutable.Map()
  )
  /*
   * Just an empty node, we always start here for searching
   */
  val root = new Node(None)
  def add(s: String, data: T) = {
    var current = root
    for (c <- s) current = current.children.getOrElseUpdate(c, new Node(None))
    current.data = Some(data)
  }
  /*
   * We want to travel the tree by checking to see if there is a child attached
   * to the current node that is the next character we're looking for, so we
   * iterate over the original string. At the end we want to check if that node
   * is marked as the end of a valid string.
   */
  def contains(s: String): Boolean = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    current.exists(_.data != None)
  }
  def get(s: String): Option[T] = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    current.get.data
  }
  /*
   * In order to find all valid substrings of a string, we travel through a path,
   * taking note of all the "on" nodes, and gather these indices as something to
   * return at the end of our trip.
   */
  def prefixesMatchingStringIndices(s: String): Set[Int] = {
    var current = Option(root)
    val output = Set.newBuilder[Int]
    for((c, i) <- s.zipWithIndex if current.nonEmpty) {
      if (current.get.data.nonEmpty) {
        output += i
      }
      current = current.get.children.get(c)
    }
    if (current.exists(_.data.nonEmpty)) {
      output += s.length
    }
    output.result()
  }
  def prefixesMatchingString(s: String): Set[String] = {
    prefixesMatchingStringIndices(s).map(s.substring(0, _))
  }
  /*
   * We want to build new strings, by checking all the children for a node, and
   * then adding those to our output. We then recurse in all of the children, to
   * add their valid ancestors.
   */
  def recurse(n: Node = root, p: List[Char] = Nil): Set[String] = {
    val output = Set.newBuilder[String]
    if (n.data.nonEmpty) output += p.reverse.mkString
    for ((c, n) <- n.children) {
      output ++= recurse(n, (c :: p))
    }
    output.result()
  }
  def remove(s: String): Boolean = {
    if (s.length == 0) true
    var current = root.children.get(s(0))
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    if (current.nonEmpty) {
      current.get.data = None
      return true
    }
    return false
  }
  def stringsMatchingPrefix(s: String): Set[String] = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c) // initial walk
    if (current.isEmpty) Set()
    else {
      val output = Set.newBuilder[String]
      def recurse(current: Node, path: List[Char]): Unit = {
        if (current.data.nonEmpty) output += (s + path.reverse.mkString)
        for ((c, n) <- current.children) recurse(n, c :: path)
      }
      recurse(current.get, Nil)
      output.result()
    }
  }
}

val t = new Trie[Int]()
t.add("mango", 1337)
t.add("mandarin", 31337)
t.add("map", 37)
t.add("man", 7)

// t.recurse(t.root, Nil)

def search[T](start: T, graph: Map[T, Seq[T]]): Set[T] = {
  val seen = collection.mutable.Set(start)
  val queue = collection.mutable.ArrayDeque(start)
  while (queue.nonEmpty) {
    val current = queue.removeHead()
    for (next <- graph(current) if !seen.contains(next)) {
      seen.add(next)
      queue.append(next)
    }
  }
  seen.to(Set)
}

def searchPaths[T](start: T, graph: Map[T, Seq[T]]): Map[T, List[T]] = {
  val seen = collection.mutable.Map(start -> List(start))
  val queue = collection.mutable.ArrayDeque(start -> List(start))
  while (queue.nonEmpty) {
    val (current, path) = queue.removeHead()
    for (next <- graph(current) if !seen.contains(next)) {
      val newPath = next :: path
      seen(next) = newPath
      queue.append((next, newPath))
    }
  }
  seen.toMap
}

def shortestPath[T](start: T, dest: T, graph: Map[T, Seq[T]]): Seq[T] = {
  val shortestReversedPaths = searchPaths(start, graph)
  shortestReversedPaths(dest).reverse
}

val graph = Map(
  "a" -> Seq("b", "c"),
  "b" -> Seq("a"),
  "c" -> Seq("b")
)

val DAG = Map(
  "a" -> Seq("b", "c"),
  "b" -> Seq("c", "d"),
  "c" -> Seq("d"),
  "d" -> Seq()
)

def binarySearch[T: Ordering](sorted: IndexedSeq[T], x: T): Option[T] = {
  pprint.log(sorted)
  if (sorted.size == 0) {
    return None
  } else if (sorted.size == 1 && sorted(0) == x) {
    return Some(x)
  } else if (sorted.size == 1 && sorted(0) != x) {
    return None
  } else {
    val middle = sorted.size / 2
    val middleValue = sorted(middle)
    val comparison = Ordering[T].compare(middleValue, x)
    if (comparison > 0) {
      return binarySearch(sorted.slice(0, middle), x)
    } else {
      return binarySearch(sorted.slice(middle, sorted.size), x)
    }
  }
}

// class ImmutableTrie(vocabulary: Seq[String]) {
//   case class Node(hasValue: Boolean, children: Map[Char, Node])
//   val root = Node(false, Map())

//   var currentWordIndex = 0
//   val sortedVocab = vocabulary.sorted
//   while (currentWordIndex < vocabulary.size - 1) {
//     var i = 0
//     // find the longest matching substring between this and the next word
//     val current = vocabulary(currentWordIndex)
//     val next = vocabulary(currentWordIndex + 1)
//     while (current(i) == next(i)) {
//       i += 1
//     }
//     val shared = current.substring(0, i)
//     if (i > 0) {
//       println(shared)
//       var runLength = 0
//       while (currentWordIndex + runLength < vocabulary.size && vocabulary(currentWordIndex + runLength).substring(0, i) == shared) {
//         runLength += 1
//       }
//       println(sortedVocab.slice(currentWordIndex, currentWordIndex + runLength))
//       var current = Node(false, children)
//       // TODO what do I do next
//       for (c <- shared) current = current.children.
//       // graft
//       currentWordIndex += runLength
//     } else {
//       currentWordIndex += 1
//     }
//   }

//   // def graft(tr: Map[Char, Node], base: Node): Map[Char, Node] {
//   // }
// }
//

class ImmutableTrie(inputs: Seq[String]) {
  class Node(index: Int, inputs: Seq[String]) {
    val hasValue = inputs.exists(_.length == index)
    val children = {
      val filteredInputs = inputs.filter(_.length > index)
      for((childChar, childInputs) <- filteredInputs.groupBy(_.charAt(index)))
      // find all of the words that have the
      yield (childChar, new Node(index + 1, childInputs))
    }
  }

  val root = new Node(0, inputs)

def contains(s: String): Boolean = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c)
    current.exists(_.hasValue)
  }
  /*
   * In order to find all valid substrings of a string, we travel through a path,
   * taking note of all the "on" nodes, and gather these indices as something to
   * return at the end of our trip.
   */
  def prefixesMatchingStringIndices(s: String): Set[Int] = {
    var current = Option(root)
    val output = Set.newBuilder[Int]
    for((c, i) <- s.zipWithIndex if current.nonEmpty) {
      if (current.get.hasValue) {
        output += i
      }
      current = current.get.children.get(c)
    }
    if (current.exists(_.hasValue)) {
      output += s.length
    }
    output.result()
  }
  def prefixesMatchingString(s: String): Set[String] = {
    prefixesMatchingStringIndices(s).map(s.substring(0, _))
  }
  /*
   * We want to build new strings, by checking all the children for a node, and
   * then adding those to our output. We then recurse in all of the children, to
   * add their valid ancestors.
   */
  def recurse(n: Node = root, p: List[Char] = Nil): Set[String] = {
    val output = Set.newBuilder[String]
    if (n.hasValue) output += p.reverse.mkString
    for ((c, n) <- n.children) {
      output ++= recurse(n, (c :: p))
    }
    output.result()
  }

  def stringsMatchingPrefix(s: String): Set[String] = {
    var current = Option(root)
    for (c <- s if current.nonEmpty) current = current.get.children.get(c) // initial walk
    if (current.isEmpty) Set()
    else {
      val output = Set.newBuilder[String]
      def recurse(current: Node, path: List[Char]): Unit = {
        if (current.hasValue) output += (s + path.reverse.mkString)
        for ((c, n) <- current.children) recurse(n, c :: path)
      }
      recurse(current.get, Nil)
      output.result()
    }
  }
}

def DFS[T](start: T, graph: Map[T, Seq[T]]): Map[T, List[T]] = {
  // node -> path to get to node
  val seen = collection.mutable.Map(start -> List(start))
  var q = start :: Nil
  while(q.size > 0) {
    val current = q.head
    q = q.tail
    println(s"current: ${current}")
    val path = seen(current)
    val children = graph(current)
    for (child <- children if !seen.contains(child)) {
      println("path: ${path}")
      seen(child) = child :: path
      q = child :: q
    }
    println(s" queue: ${q}")
  }
  seen.toMap
}

val vocab = Array(
  "marauder",
  "marriage",
  "map",
  "mango",
  "manatee",
  "mantaray",
  "armadillo",
  "armada"
)

val tr = new ImmutableTrie(vocab)



def nineBlock(): Array[Array[(Int, Int)]] = {
  Range(0, 9).map {
    i => Range(0, 9).map {
      j => {
        val row = (i / 3) * 3 + j / 3
        val col = (i % 3) * 3 + j % 3
        println(s"($row,$col)")
        (row, col)
      }
    }.toArray
  }.to(Array)
}

val sudokuOne = Array(
  Array(3, 1, 6, 5, 7, 8, 4, 9, 2),
  Array(5, 2, 9, 1, 3, 4, 7, 6, 8),
  Array(4, 8, 7, 6, 2, 9, 5, 3, 1),

  Array(2, 6, 3, 0, 1, 0, 0, 8, 0),
  Array(9, 7, 4, 8, 6, 3, 0, 0, 5),
  Array(8, 5, 1, 0, 9, 0, 6, 0, 0),

  Array(1, 3, 0, 0, 0, 0, 2, 5, 0),
  Array(0, 0, 0, 0, 0, 0, 0, 7, 4),
  Array(0, 0, 5, 2, 0, 6, 3, 0, 0)
)

def renderSudoku(grid: Array[Array[Int]]): Unit = {
  def formatRowOfNumbers (row: Array[Int]): String = {
    val groups = row
      .map(x => if (x == 0) " " else x.toString())
      .grouped(3)
      .toArray
      .map(_.mkString(" ", " ", " "))
    groups.mkString("|", "|", "|")
  }

  val division =  "+" + "-" * 7 + "+" + "-" * 7 + "+" + "-" * 7 + "+"
  val rowsFormatted = Array(division) ++
    grid.slice(0, 3).map(formatRowOfNumbers) ++
    Array(division) ++
    grid.slice(3, 6).map(formatRowOfNumbers) ++
    Array(division) ++
    grid.slice(6, 9).map(formatRowOfNumbers) ++
    Array(division)
  println(rowsFormatted.mkString("\n"))
}

def isValidSudoku(grid: Array[Array[Int]]): Boolean = {
  !Range(0, 9).exists(
    i => {
      val row = Range(0, 9).map { grid(i)(_) }.filter { _ != 0 }
      val col = Range(0, 9).map { grid(_)(i) }.filter { _ != 0 }
      val square = Range(0, 9)
        .map {
          j => grid((i / 3) * 3 + j / 3)((i % 3) * 3 + j % 3)
        }.filter {
          _ != 0
        }
      row.distinct.length != row.length ||
        col.distinct.length != col.length ||
        square.distinct.length != square.length
    }
  )
}


def solveSudoku(grid: Array[Array[Int]]): Option[Array[Array[Int]]] = {
  val isComplete = !grid
    .exists {
      row => row.exists(_ == 0)
    }
  if (isComplete) {
    if (isValidSudoku(grid)) {
      return Some(grid)
    } else {
      return None
    }
  }

  Range(0, 9)
    .foreach (
    i => {
      var candidates: List[Array[Array[Int]]] = List()
      val row: Array[Int] = grid(i)
      val numbersInRow = row.filter(_ != 0)
      val hole = row.indexWhere(_ == 0)

      def updateRow(n: Int): Array[Int] = {
        row.zipWithIndex.map { case (x, i) => if (i == hole) n else x }.toArray
      }

      def updateGrid(input: Array[Array[Int]], r: Int, c: Int, update: Int): Array[Array[Int]] = {
        val output = Array.ofDim[Int](9, 9)
        for {
          i <- 0 until 9
          j <- 0 until 9
        } {
          if (r == i && c == j) {
            output(r)(c) = update
          } else {
            output(i)(j) = input(i)(j)
          }
        }
        output
      }
      for (n <- Range(1, 10) if !numbersInRow.contains(n)) {
        val update = updateGrid(grid, i, hole, n)
        if (isValidSudoku(update)) {
          return solveSudoku(update)
        }
      }
    }
  )
  Some(grid)
}

def floodfill (
  src: String,
  dest: String,
  startX: Int,
  startY: Int,
  compare: (java.awt.Color, java.awt.Color) => Boolean
): Unit = {
  val raw = javax.imageio.ImageIO.read(new java.io.File(src))

  val inBounds = (x: Int, y: Int) => {
    x < raw.getWidth && y < raw.getHeight && x >= 0 && y >= 0
  }

  var q = collection.mutable.ArrayDeque.empty[(Int, Int)]
  raw.setRGB(startX, startY, java.awt.Color.BLACK.getRGB)
  var seen = Set[(Int, Int)]()
  q.append((startX, startY))
  while (q.nonEmpty) {
    val (x, y) = q.removeHead()
    seen += ((x, y))
    for { i <- List(-1, 1); j <- List(-1, 1) } {
      val (x1, y1) =  (x + i, y + j)
      val color =  new java.awt.Color(raw.getRGB(x, y))
      println(s"$x1, $y1")
      if (inBounds(x1, y1) && !seen.contains((x1, y1))) {
        val neighbor = new java.awt.Color(raw.getRGB(x1, y1))
        println(s"${compare(color, neighbor)}")
        if (compare(color, neighbor)) {
          raw.setRGB(x1, y1, java.awt.Color.BLACK.getRGB)
          q.append((x1, y1))
        }
      }
    }
  }
  javax.imageio.ImageIO.write(raw, "jpg", new java.io.File(dest))
}

val compareColors = { (a: java.awt.Color, b: java.awt.Color) =>
  def sqrDiff(f: java.awt.Color => Int) = math.pow(f(a) - f(b), 2)
  math.sqrt(sqrDiff(_.getBlue) + sqrDiff(_.getGreen) + sqrDiff(_.getRed)) < 25
}

val run = () => {
  floodfill(src = "book_cover.jpg", dest = "out.jpg", startY = 90, startX = 180, compare = compareColors)
}
