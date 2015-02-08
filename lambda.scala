

//Map generated unique names to original names to make toString methods more readable
object NameDirectory {
  var directory = scala.collection.mutable.Map[String, String]()
  def update(unique: String, old: String) = directory += (unique -> old)
  def lookup(name: String) = directory.getOrElse(name, name)
}

//Lambda Expressions
sealed trait Expr {
  def sub(name: Expr, arg: Expr): Expr
  def beta(arg: Expr): Expr
  def simplify(): Expr
  def toString(): String
  def eval(): Expr
}
case class Name(name: String) extends Expr {
  def sub(nm: Expr, arg: Expr): Expr = {
    if (this == nm) arg else this
  }
  def simplify(): Expr = this
  def beta(arg: Expr): Expr = throw new RuntimeException("Cannot beta reduce a name!")
  def eval(): Expr = this
  override def toString(): String = NameDirectory.lookup(name)
}
//Generates unique names to avoid name clashes and
//stores them in the NameDirectory
object UniqueName {
  var index = 0
  def gen(original: Name): Name = {
    index += 1
    val unique = new Name("u" + index)
    NameDirectory.update(unique.name, original.name)
    unique
  }
}
case class Lmbd(param: Name, body: Expr) extends Expr {
  def beta(arg: Expr): Expr = {
    body.sub(param, arg.simplify())
  }
  def sub(name: Expr, arg: Expr): Lmbd = {
      val uniqueName = UniqueName.gen(param)
      Lmbd(uniqueName, body.sub(param, uniqueName).sub(name, arg))
  }
  def simplify(): Expr = Lmbd(param, body.simplify())
  def eval(): Expr = this
  override def toString(): String = "\\" ++ param.toString ++ "." ++ body.toString
}
case class Appl(fn: Expr, arg: Expr) extends Expr {
  def sub(name: Expr, a: Expr): Appl = {
    Appl(fn.sub(name, a), arg.sub(name, a))
  }

  def simplify(): Expr = {
    fn.simplify() match {
      case n@Name(_) => Appl(n, arg.simplify())
      case l@Lmbd(_, _) => l.beta(arg.simplify()).simplify()
      case ap@Appl(_, _) => Appl(ap, arg.simplify())
    }
  }
  def beta(a: Expr): Expr = {
    val simplified = this.simplify()
    simplified match {
      case Name(_) => Appl(simplified, a.simplify())
      case Appl(_, _) => Appl(simplified, a.simplify())
      case Lmbd(_, _) => simplified.beta(a.simplify())
    }
  }
  def eval(): Expr = this.simplify()
  override def toString(): String = "(" ++ fn.toString ++ " " ++ arg.toString ++ ")"
}

//case class LExp(exp: String) {
//  def s: Expr = {
//    def loop(e: List[Char]): Expr = {
//      e match {
//        case h::m::t if h == "\\" => Lmbd(Name(m), loop(t))
//        case h::t if h == "(" => Appl(loop(parseFn(t)), loop(restAppl(t))
//        case n == Name(n)
//      }
//    }
//    loop(exp.toList)
//  }
//
//  def parseFn(t: String) = t.split(" ")(0)
//}



val x = Name("x")
val y = Name("y")
val z = Name("z")
val f = Name("f")
val m = Name("m")
val n = Name("n")
val o = Name("o")
val p = Name("p")
val c = Name("c")
val b1 = Name("b1")
val b2 = Name("b2")

val id = Lmbd(x, x)
val selfApp = Lmbd(x, Appl(x, x))

val tr = Lmbd(x, Lmbd(y, x))
val fs = Lmbd(x, Lmbd(y, y))

val first = Lmbd(x, Lmbd(y, x))
val second = Lmbd(x, Lmbd(y, y))
val makePair = Lmbd(x, Lmbd(y, Lmbd(o, Appl(Appl(o, x), y))))

val cond = Lmbd(b1, Lmbd(b2, Lmbd(c, Appl(Appl(c, b1), b2))))

val zero = Lmbd(f, Lmbd(z, z))
val succ = Lmbd(n, Lmbd(f, Lmbd(z, Appl(f, Appl(Appl(n, f), z)))))
val isZero = Lmbd(n, Appl(Appl(n, Appl(x, fs)), tr))  //If not zero, ignore the number and return false

val zz = Appl(Appl(makePair, zero), zero) // (0 0)
val ns = Lmbd(p, Appl(Appl(makePair, Appl(p, second)), Appl(succ, Appl(p, second)))) //(0 0) -> (0 1), (0 1) -> (1 2), (1 2) -> (2 3),...
val pred = Lmbd(n, Appl(Appl(Appl(n, ns), zz), first)) //Apply ns n times to zz. Choose the first value, which will be the predecessor of n


val add = Lmbd(m, Lmbd(n, Lmbd(f, Lmbd(z, Appl(Appl(m, f), Appl(Appl(n, f), z))))))

val one = Appl(succ, zero)
val two = Appl(succ, one)
val three = Appl(succ, two)