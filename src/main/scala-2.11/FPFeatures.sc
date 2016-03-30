import scala.annotation.tailrec

/**
  * First class / High order functions
  */

def multiply(x: Int, y: Int): Int = x * y
multiply(10, 20)

val multiplier: (Int, Int) => Int = multiply
multiplier(10, 20)

//partially applied function - /a way to transform a def into a function value
val anotherMultiplier = multiply _
anotherMultiplier(10, 20)

val a = multiply(1, _: Int)
a(30)

val yetAnotherMultiplier = (x: Int, y: Int) => x * y
yetAnotherMultiplier(10, 20)

val lastMultiplier = yetAnotherMultiplier
lastMultiplier(10, 20)

/**
  * Higher order means higher abstraction - you just tell what to do
  * One benefit of higher-order functions is they enable you to create
  * control abstractions that allow you to reduce code duplication.
  * At each call, you decide on the suitable algorithm/functionality.
  */
def operate(f: (Int, Int) => Int, x: Int, y: Int) = f(x, y)

def addMore(x: Int) = (y: Int, z: Int) => x + multiply(y, z)
val addOne = addMore(1)
addOne(1, 2)

/**
  * Recursion
  */
def factorial(n: Int): Int = {
  @tailrec def factorialAcc(acc: Int, n: Int): Int = {
    if (n <= 1) acc
    else factorialAcc(n * acc, n - 1)
  }
  factorialAcc(1, n)
}
factorial(5)

def fib(x: Int): Int = {
  @tailrec def fibAcc(x: Int, prev: Int = 0, next: Int = 1): Int = x match {
    case 0 => prev
    case 1 => next
    case _ => fibAcc(x - 1, next, (next + prev))
  }
  fibAcc(x)
}
fib(3)

/**
  * Call by name / call by name
  */
def time() = {
  println("Getting time in nano seconds")
  System.nanoTime
}

//re-evaluated each time appears - call by name
def delayed(t: => Long) = {
  println("In delayed method")
  println(s"Param: $t")
  t
}
delayed(time())
//streams - lazy tail (call by name - see Cons.scala)
def from(n: Int): Stream[Int] = n #:: from(n + 1)
def sieve(s: Stream[Int]): Stream[Int] =
  s.head #:: sieve(s.tail filter (_ % s.head != 0))
val primes = sieve(from(2))
(primes take 30).toList
/**
  * Curried functions
  */
val all = (x: Int, y: Double, z: String) => x + y + z
val separate = all.curried
val one = separate(1)
val onePlus = separate(1)
val two = one(2.5)
val three = two("three")
val same = separate(1)(2.5)("three")
/**
  * Closures
  * Scala's closures capture variables themselves, not the
  * value to which variables refer.
  */
var something = 1
val addSomething = (_: Int) + something
addSomething(1)
something = 3
addSomething(1)
/**
  * Pattern matching
  */
case class Rational(n: Int, d: Int)

//stable identifier - capital letter or backticks
val Subunitary = Rational(1, 2)
val unity = Rational(1, 1)

def checkNumber(number: Any) = number match {
  case 3 => "three"
  case Subunitary => "subunitary"
  case `unity` => "unity"
  case unity => "unity?"
}
checkNumber(unity)

//seq patterns
def matchSeq[A](seq: Seq[A]): String = seq match {
  case Seq(1, 2, 3) => "1 -> 3"
  case Seq(1, _, _) => "1 is the first element in a sequence of 3 elements"
  case Seq(1, _*) => "1 is the first element in a sequence of at least 1 element"
  case x +: Nil => s"The only element is $x"
  case x :: _ => s"The first element is $x"
  case _ :+ x => s"The last element is $x"
  case Nil => "Empty sequence"
}

//tuple patterns
def whatTimeIsIt(any: Any): String = any match {
  case (x, "12:00") => s"From $x to high noon"
  case (x, _) => s"From $x 'till you get tired"
  case (x, y, z, t) => s"From $x to $t with a break from $y to $z"
  case _ => "Whenever you are free"
}

//typed patterns - forget about isInstanceOf / asInstanceOf
case class Account(balance: Int)
case object NoMoney

def money(input: Any): Int = input match {
  case a: Account => a.balance
  case NoMoney => 0
  case n: Int => n
  case _: String => 0
  case _ => -1
}
//type erasure in pattern matching - compile to see the warnings
//except for arrays - the element type is stored with the array value - Java way
def areRationals(seq: Any): Boolean = seq match {
  case s: List[Rational]        => true
  case s: Array[Rational]       => true
  case s: Map[String, Rational] => true
  case _                        => false
}

//patern binders
case class User(id: Int, name: String)
def broke(user: User) = user match {
  case u@User(_, "gigel") => true
  case _ => false
}
def specialAccount(user: User) = user match {
  case User(_, _) => println(a)
  case _ => "Nothing special about this user's account"
}
/**
 * Patterns everywhere
 */

//try & catch
def toAccount(s: String): Account =
  try {
    Account(s.toInt)
  } catch {
    case _: NumberFormatException => Account(0)
  }

//value definitions
def currentUser(): User = User(1, "name")
def doSmthWithName(name: String) = println(name)

val user = currentUser()
doSmthWithName(user.name) //we don't need the entire obj
val User(_, name) = currentUser()
doSmthWithName(name)

