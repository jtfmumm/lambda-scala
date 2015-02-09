# lambda-scala
Implementation of Lambda Calculus in Scala

## Expressions
Expr ::=
	Name(name: String) 
	| Lmbd(param: Expr, body: Expr)
        | Appl(fn: Expr, arg: Expr)

Examples:  
Name("x")  
//-> "x"    
Lmbd(Name("y"), Name("y"))   
//-> "λy.y"      
Lmbd(Name("f"), Lmbd(Name("x"), Name("x")))  
//-> "λf.λx.x"  
Appl(Name("f"), Name("a"))    
//-> "(f a)"  

## Syntactic sugar

### \

The \ operator peforms function application. Using named variables for functions as well as
\ leads to simpler code:

val x = Name("x")  
//-> "x"  
val y = Name("y")  
//-> "y"  
val f = Name("f")  
//-> "f"  
val id = Lmbd(x, x)  
//-> "λx.x"  
val makePair = Lmbd(x, Lmbd(y, Lmbd(o, Appl(Appl(o, x), y))))  
//-> "λx.λy.λo.((o x) y)"  

id \ y      
//-> "y"  

val a = Name("a")  
val b = Name("b")  

makePair \ a \ b  
//-> "λf.((f a) b)"  

### ()

You can also use traditional argument passing:  

id(y)  
//-> "y"  

makePair(a)(b)    
//-> "λf.((f a) b)"    



	
