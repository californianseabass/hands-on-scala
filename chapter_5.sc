sealed trait Expr
case class BinOp(left: Expr, op: String, right: Expr) extends Expr
case class Literal(value: Int) extends Expr
case class Variable(name: String) extends Expr

def stringify(expr: Expr): String = expr match {
  case BinOp(left, op, right) => s"(${stringify(left)} $op ${stringify(right)})"
  case Literal(value) => value.toString
  case Variable(name) => name
}

def evaluate(expr: Expr, values: Map[String, Int]): Int = expr match {
  case BinOp(left, "+", right) => evaluate(left, values) + evaluate(right, values)
  case BinOp(left, "-", right) => evaluate(left, values) - evaluate(left, values)
  case BinOp(left, "*", right) => evaluate(left, values) * evaluate(right, values)
  case Literal(value) => value
  case Variable(name) => values(name)
}

def reduce(expr: Expr): Expr = {
  val res = expr match {
    case BinOp(Literal(0), "+", right) => right
    case BinOp(left, "+", Literal(0)) => left

    case BinOp(Literal(0), "*", right) => Literal(0)
    case BinOp(left, "*", Literal(0)) => Literal(0)

    case BinOp(Literal(1), "*", right) => right
    case BinOp(left, "*", Literal(1)) => left

    case BinOp(Literal(left), "+", Literal(right))  => Literal(left.toInt + right.toInt)
    case BinOp(Literal(left), "-", Literal(right)) => Literal(left.toInt - right.toInt)
    case BinOp(Literal(left), "*", Literal(right)) => Literal(left.toInt * right.toInt)

    case BinOp(Literal(left), op, right) => BinOp(Literal(left), op, reduce(right))
    case BinOp(left, op, Literal(right)) => BinOp(reduce(left), op, Literal(right))

    // case BinOp(Variable(x), op, Variable(y)) => BinOp(Variable(x), op, Variable(y))
    case BinOp(left, op, right) => BinOp(reduce(left), op, reduce(right))

    case Literal(value) => Literal(value)
    case Variable(name) => Variable(name)
  }

  if (res == expr) res
  else reduce(res)
}

def stringify2(expr: Expr): String = expr match {
  case BinOp(Literal(left), "*", Literal(right)) if left == 0 || right == 0 => "0"
  case BinOp(Literal(0), "+", right) => stringify(right)
  case BinOp(left, "+", Literal(0)) => stringify(left)
  case default => stringify(default)
}

def simplify(expr: Expr): String =  {
  stringify(reduce(expr))
}

val al1 = BinOp(Literal(1), "+", Literal(1))
val al2 = BinOp(al1, "*", Variable("x"))
val al3 = BinOp(BinOp(Literal(2), "-", Literal(1)), "*", Variable("x"))
val al4 = BinOp(BinOp(al1, "*", Variable("y")), "+",BinOp(BinOp(Literal(1), "-", Literal(1)), "*", Variable("x")))

def retry[T](max: Int, initial: Int)(f: => T): T = {
  var tries = 0
  var result: Option[T] = None

  while (result == None) {
    try { result = Some(f) }
    catch {
      case e: Throwable => {
        tries +=1
        if (tries > max) {
          throw e
        }
        else {
          val exponentialBackoff = Math.pow(2, tries - 1)
          val sleepTimeMs = (exponentialBackoff * initial).toLong
          println(s"Sleeping for $sleepTimeMs milliseconds")
          Thread.sleep(sleepTimeMs)
        }
      }
    }
  }
  result.get
}

val httpBinUrl = "https://httpbin.org/status/200,400,500"

def stringSplitBrackets(s: String): Seq[String] = {
  assert(s.head == '[')
  assert(s.last == ']')

  val indices = collection.mutable.ArrayDeque.empty[Int]
  var openBrackets = 0

  for (i <- Range(1, s.length - 1)) {
    s(i) match {
      case '[' => openBrackets += 1
      case ']' => openBrackets -= 1
      case ',' => {
        if (openBrackets == 0) {
          indices += i
        }
      }
      case _ => // no-op
    }
  }

  val allIndices = Seq(0) ++ indices ++ Seq(s.length - 1)

  val sectionIndices = allIndices.zip(allIndices.tail)

  // pass the rest of the
  for ((start, finish) <- sectionIndices)
  yield s.substring(start + 1, finish)
}

trait StrParser[T] { def parse(s: String): T }

object StrParser {
  implicit object ParseInt extends StrParser[Int] {
    def parse(s: String): Int = s.toInt
  }
  implicit object ParseBoolean extends StrParser[Boolean] {
    def parse(s: String): Boolean = s.toBoolean
  }
  implicit object ParseDouble extends StrParser[Double] {
    def parse(s: String): Double = s.toDouble
  }

  implicit def ParseList[T](implicit p: StrParser[T]) = new StrParser[List[T]] {
    def parse(s: String) = stringSplitBrackets(s).toList.map(p.parse)
  }

  implicit def ParseTuple[S, T](implicit p: StrParser[S], q: StrParser[T]) =
    new StrParser[(S, T)]{
      def parse(s: String) = {
        val Seq(left, right)= stringSplitBrackets(s)
        (p.parse(left), q.parse(right))
      }
    }
}

def parseFromString[T](s: String)(implicit parser: StrParser[T]) = {
  parser.parse(s)
}

val threeLayersString =  "[[[1],[true]],[[2,3],[false,true]],[[4,5,6],[false,true,false]]]"

val threeLayersOfNesting = parseFromString[List[(List[Int], List[Boolean])]](threeLayersString)

trait StrWriter[T] { def write(t: T): String }

object StrWriter {
  implicit object IntWriter extends StrWriter[Int] {
    def write(i: Int) = i.toString
  }
  implicit object BooleanWriter extends StrWriter[Boolean] {
    def write(b: Boolean) = b.toString
  }
  implicit object Double extends StrWriter[Double] {
    def write(d: Double) = d.toString
  }
  implicit def WriteList[T](implicit w: StrWriter[T]) =
    new StrWriter[List[T]]{
      def write(input: List[T]) = {
        input.map(w.write).mkString("[", ",", "]")
      }
    }
  implicit def WriteTuple[S, T](implicit u: StrWriter[S], v: StrWriter[T]) =
    new StrWriter[(S, T)]{
      def write(input: (S, T)) = {
        val (left, right) = input
        "[" + u.write(left) + "," + v.write(right) + "]"
      }
    }
}

def writeToString[T](t: T)(implicit writer: StrWriter[T]): String = {
  writer.write(t)
}
