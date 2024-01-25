val numbers = upickle.default.read[Seq[Int]]("[1, 2, 3, 4]")

case class Author(login: String, id: Int, site_admin: Boolean)


implicit val authorRW: upickle.default.ReadWriter[Author] = upickle.default.macroRW

case class Foo(val i: Int, val s: String)

implicit val FooRW = upickle.default.readwriter[ujson.Value].bimap[Foo] (
  f => ujson.Obj(("i", f.i), ("s", f.s)),
  v => Foo(v("i").num.toInt, v("s").str)
)

val foo = upickle.default.read[Foo]("{\"i\": 42, \"s\": \"The answer is:\"}")

val exampleString = """{"login": "lihaoyi", "id": 313373, "site_admin": true}"""

def traverseFilter (v: ujson.Value): Option[ujson.Value] = v match {
  case a: ujson.Arr => Some(ujson.Arr.from(a.arr.flatMap(traverseFilter)))
  case o: ujson.Obj => Some(
    ujson.Obj.from(o.obj.flatMap { (k, v) => traverseFilter(b).map(k -> _) })
  )
  case s: ujson.Str => if (!s.startsWith("https://"))  Some(s) else None
  case default => Some(default)
}

val input = os.read(os.pwd / "ammonite-releases.json")
