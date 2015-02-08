

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
  //Syntactic sugar for function application
  //e.g. plus \ one \ two  ->  three
  def \(arg: Expr): Expr
}
case class Name(name: String) extends Expr {
  def sub(nm: Expr, arg: Expr): Expr = {
    if (this == nm) arg else this
  }
  def simplify(): Expr = this
  def beta(arg: Expr): Expr = throw new RuntimeException("Cannot beta reduce a name!")
  def eval(): Expr = this
  def \(a: Expr): Expr = this
  override def toString(): String = NameDirectory.lookup(name)
}
//Generates unique names using mutable index to avoid name clashes and
//maps unique name to original in the NameDirectory to improve output readability
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
  def beta(a: Expr): Expr = {
    body.sub(param, a.simplify()).simplify()
  }
  def sub(name: Expr, arg: Expr): Lmbd = {
      val uniqueName = UniqueName.gen(param)
      Lmbd(uniqueName, body.sub(param, uniqueName).sub(name, arg))
  }
  def simplify(): Expr = Lmbd(param, body.simplify())
  def eval(): Expr = this
  def \(a: Expr): Expr = this.beta(a)
  override def toString(): String = "λ" ++ param.toString ++ "." ++ body.toString
}
case class Appl(fn: Expr, arg: Expr) extends Expr {
  def sub(name: Expr, a: Expr): Appl = {
    Appl(fn.sub(name, a), arg.sub(name, a))
  }
  def simplify(): Expr = {
    val sFn = fn.simplify()
    val sArg = arg.simplify()
    sFn match {
      case Name(_) => Appl(sFn, sArg)
      case Lmbd(_, _) => sFn.beta(sArg)
      case Appl(_, _) => Appl(sFn, sArg)
    }
  }
  def beta(a: Expr): Expr = Appl(this, a).eval()
  def eval(): Expr = this.simplify()
  def \(a: Expr): Expr = Appl(this, a).eval()
  override def toString(): String = "(" ++ fn.toString ++ " " ++ arg.toString ++ ")"
}


//Definitions
val x = Name("x")
val y = Name("y")
val z = Name("z")
val f = Name("f")
val g = Name("g")
val m = Name("m")
val n = Name("n")
val o = Name("o")
val p = Name("p")
val c = Name("c")
val b1 = Name("b1")
val b2 = Name("b2")

val id = Lmbd(x, x)
val selfapp = Lmbd(x, Appl(x, x))

val tr = Lmbd(x, Lmbd(y, x))
val fs = Lmbd(x, Lmbd(y, y))

val first = Lmbd(x, Lmbd(y, x))
val second = Lmbd(x, Lmbd(y, y))
val makePair = Lmbd(x, Lmbd(y, Lmbd(o, Appl(Appl(o, x), y))))

val ifelse = Lmbd(c, Lmbd(b1, Lmbd(b2, Appl(Appl(c, b1), b2))))

val zero = Lmbd(f, Lmbd(z, z))
val succ = Lmbd(n, Lmbd(f, Lmbd(z, Appl(f, Appl(Appl(n, f), z)))))
val iszero = Lmbd(n, Appl(Appl(n, Appl(first, fs)), tr))  //If not zero, ignore the number and return false

val zz = Appl(Appl(makePair, zero), zero) // (0 0)
val ns = Lmbd(p, Appl(Appl(makePair, Appl(p, second)), Appl(succ, Appl(p, second)))) //(0 0) -> (0 1), (0 1) -> (1 2), (1 2) -> (2 3),...
val pred = Lmbd(n, Appl(Appl(Appl(n, ns), zz), first)) //Apply ns n times to zz. Choose the first value, which will be the predecessor of n.  pred(zero) = zero

val plus = Lmbd(m, Lmbd(n, Lmbd(f, Lmbd(z, Appl(Appl(m, f), Appl(Appl(n, f), z))))))
val minus = Lmbd(m, Lmbd(n, Appl(Appl(n, pred), m))) //Bottoms out at zero
val mult = Lmbd(n, Lmbd(m, Appl(Appl(n, Appl(plus, m)), zero)))
val pow = Lmbd(n, Lmbd(m, Appl(m, n))) //Returns nonsense if you raise anything to zero

val one = Appl(succ, zero)
val two = Appl(succ, one)
val three = Appl(succ, two)
val four = Appl(succ, three)
val five = Appl(succ, four)
val six = Appl(succ, five)
val seven = Appl(succ, six)
val eight = Appl(succ, seven)
val nine = Appl(succ, eight)
val ten = Appl(succ, nine)
//Constructing new numbers is fun: e.g. plus \ (mult \ three \ ten ) \ six  ->  thirty-six

val fixedPoint = Lmbd(f, Appl(selfapp, Lmbd(g, Appl(f, Appl(g, g)))))

