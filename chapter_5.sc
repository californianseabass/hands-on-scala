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
