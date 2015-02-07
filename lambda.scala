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
  override def toString(): String = name
}
case class Lmbd(param: Name, body: Expr) extends Expr {
  def beta(arg: Expr): Expr = {
    body.sub(param, arg.simplify())
  }
  def sub(name: Expr, arg: Expr): Lmbd = {
    if (param == name || param == arg) this else Lmbd(param, body.sub(name, arg))
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


val x = Name("x")
val y = Name("y")
val z = Name("z")
val f = Name("f")
val n = Name("n")

val id = Lmbd(x, x)
val selfApp = Lmbd(x, Appl(x, x))
val tr = Lmbd(x, Lmbd(y, x))
val fs = Lmbd(x, Lmbd(y, y))
val makePair = Lmbd(x, Lmbd(y, Lmbd(f, Appl(Appl(f, x), y))))

val zero = Lmbd(f, Lmbd(x, x))
val succ = Lmbd(n, Lmbd(f, Lmbd(x, Appl(f, Appl(Appl(n, f), x)))))

val one = Appl(succ, zero)
val two = Appl(succ, one)
val three = Appl(succ, two)




