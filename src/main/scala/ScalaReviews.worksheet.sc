import scala.util.Random
import scala.util.matching.Regex
// Classes

class Point(var z: Int = 0, var a: Int = 0) {
  private var _x = 0
  private var _y = 0

  def x = _x

  // TODO: setter
  def x_= (newValue: Int): Unit = {
    if (newValue > 0) _x = newValue else printWarning
  }

  def y = _y
  def y_=(newValue: Int): Unit = {
    if (newValue > 0) _y = newValue else printWarning
  }

  def printWarning = println("Warning: less than 0")
}


val point = new Point(1,2)
point.x
point.y
point.z
point.a

// syntax "_=()" for setter
point.x_=(31)
point.y_=(42)

// another setter for attribute which is defined after class name
point.z = 3

point.x
point.y
point.z

// For comprehension
val numPairs = List((1,2),(3,4),(5,6))
for ((left, right) <- numPairs) {
  print(s"$left -- $right -- ")
}

// Datatype that supports the operations "withFilter", "map" and "flatMap" can be used in for-comprehension
// Map, Vector ?
val vector = Vector(1,2,3)
vector.withFilter(_ > 2).foreach(print)

// TODO: ---  
abstract class AbsIterator {
  type T
  def hasNext(): Boolean
  def next(): T
}

class StringIterator(s: String) extends AbsIterator {
  type T = Char
  private var i = 0

  def hasNext(): Boolean = i < s.length()
  def next() = {
    var ch = s charAt i
    i += 1
    ch
  }
}

trait RichIterator extends AbsIterator {
  def foreach(f: T => Unit): Unit = while (hasNext()) f(next())
}

class RichStringIter extends StringIterator("ScalaString") with RichIterator {}

val richStringIter = new RichStringIter
richStringIter.foreach(print)

// TODO: --- Higher order function
val salaries = Seq(20, 30, 40)
var doubleSalary = (x: Int) => x * 2
var newSalaries = salaries.map[Int](doubleSalary)


doubleSalary = (x: Int) => x * 3
newSalaries = salaries.map[Int](doubleSalary)
newSalaries

newSalaries = salaries.map(_ * 4)
newSalaries

newSalaries = salaries.map(e => e * 5)
newSalaries

// TODO: Coercing methods into functions
case class WeeklyWeatherForecast(temperatures: Seq[Double]) {

  def convertCtoF(temp: Double): Double = temp * 1.8 + 32

  def forecastInFahrenheit: Seq[Double] = temperatures.map(convertCtoF) // <-- passing the method convertCtoF
}

object SalaryRaiser {

  private def promotion(salaries: List[Double], f: Double => Double) = salaries.map(f)
  
  def smallPromotion(salaries: List[Double]): List[Double] =
    promotion(salaries, _ * 1.1)

  def greatPromotion(salaries: List[Double]): List[Double] =
    // promotion(salaries, _:Double * math.log(_))
    promotion(salaries, s => s * math.log(s))

  def hugePromotion(salaries: List[Double]): List[Double] =
    // promotion(salaries, _ * _)
    promotion(salaries, s => s * s)

}

// default or identity element ?
var defaultValue: Int = _

// TODO: Functions that return functions

def returnFunction(x: Int) = (y: Int) => x + y

def returnFunction2(x: Int) = (y: Int, z: Int) => x * y + z

//  Currying
def returnFunction3(x: Int, y: Int) = (z: Int) => returnFunction2(x)(y,z)

//Currying 2
def returnFunction4(x: Int, y: Int, z: Int) = x * y + z
def returnFunction5(x: Int) = (y,z) => returnFunction4(x,y,z)


var returnF = returnFunction(3)
var returnFvalue = returnF(4)

var returnF2 = returnFunction2(3)
returnFvalue = returnF2(10,4)
returnFvalue

returnFunction3(3, 10)(4)

returnFunction5(3)(10, 4)

// case class - copying

case class Message(sender: String, recipient: String, body: String)

var message1 = Message("senderA", "RecipientA", "BodyA")
var message2 = Message("senderA", "RecipientA", "BodyA")
message1 == message2

message2 = message1.copy("senderB")
message2

message2 = message1.copy(sender = "senderB", body = "bodyB")
message2
message1 == message2

//
// keyword sealed : All subtypes must be declared in the same file. This assures that all subtypes are known.
// Trait or class can be marked "sealed"

sealed abstract class Furniture(a: String)
case class Couch(b: String) extends Furniture("Couch")
case class Chair(c: String) extends Furniture("Chair")

// another view
// sealed abstract class Furniture(a: String)
case class Couch2(a: String, b: String) extends Furniture(a: String)
case class Chair2(a: String, c: String) extends Furniture(a: String)

def findPlace(piece: Furniture) = piece match {

  // optional but match may be exhaustive
  case p: Chair => "Sit on the chair " + p.c
  case p2: Chair2 => " " + p2.a + " ---- " + p2.c

  // Pattern guards
  // case Couch("a: Couch - b: Couch") => "Lie on the couch Couch" // TODO + c.a
  case Couch(b) => "Lie on the couch " + b
  case Couch2(a,b) => a match {
      case "Couch2" => "Couch2 with a = Couch222222"
      case "Couch3" => "Couch2 with a = Couch333333"
    }

}

var couch1 = Couch("a: Couch - b: Couch")
var couch2 = Couch2("Couch2", "b: b-Couch2")
var couch3 = Couch2("Couch3", "b: b-Couch2")

findPlace(couch1)
findPlace(couch2)
findPlace(couch3)

// Singleton 
// lazy val
object Logger {
  def info(message: String) = println(s"INFO: $message")
}

// TODO: Companion Object
case class Circle(radius: Double) {
  
  //important import
  import Circle.calculateArea
  import Circle.methodTest2
  import Circle._

  def area: Double = calculateArea(radius)

  def method2 = methodTest2

  def method3 = methodTest3()

}

object Circle {
  private def calculateArea(radius: Double) = math.Pi * math.pow(radius, 2.0)
  
  private def methodTest2: Unit = print("method test 2")
  private def methodTest3(): Unit = println("method test 3")
}

val circle = Circle(3.2d)

circle.method2
circle.method3
circle.area

// TODO: UNDERSCORE underscore _
// import scala._    // Wild card -- all of Scala is imported
// import scala.{ Predef => _, _ } // Exception, everything except Predef
// def f[M[_]]       // Higher kinded type parameter
// def f(m: M[_])    // Existential type
// _ + _             // Anonymous function placeholder parameter
// m _               // Eta expansion of method into method value
// m(_)              // Partial function application
// _ => 5            // Discarded parameter
// case _ =>         // Wild card pattern -- matches anything
// val (a, _) = (1, 2) // same thing
// for (_ <- 1 to 10)  // same thing
// f(xs: _*)         // Sequence xs is passed as multiple parameters to f(ys: T*)
// case Seq(xs @ _*) // Identifier xs is bound to the whole matched sequence
// var i: Int = _    // Initialization to the default value
// def abc_<>!       // An underscore must separate alphanumerics from symbols on identifiers
// t._2              // Part of a method name, such as tuple getters
// 33_6_46_74         // Numeric literal separator (Scala 2.13+)

println("Intern method returns the canonical presentation \n of the string object.".intern())

val numberPattern: Regex = "[0-9]".r

numberPattern.findFirstMatchIn("awesomepassword") match {
  case Some(_) => println("Password OK")
  case None => println("Password must contain a number")
}


// TODO: EXTRACTOR Objects
// An extractor object is an OBJECT with an UNAPPLY method. 

object CustomerID {
  def apply(name: String) = s"$name--${Random.nextLong()}"

  def unapply(customerId: String) = {
    val stringArray: Array[String] = customerId.split("--")
    if (stringArray.tail.nonEmpty) Some(stringArray.head) else None
  }
}

// Scala executes the apply method
val customerId = CustomerID("customerName AAAA")

customerId match {
  case CustomerID(anotherName) => println(anotherName)
  case _ => println("error of unapply")
}


val customerId2 = 3
customerId match {
  case CustomerID("name") => println("abc")
  case _ => println("error of unapply")
}


// unapply - extract
val CustomerID(nameC) = customerId
println(nameC)
// equivalent to 
val nameC2 = CustomerID.unapply(customerId).get

// TODO: VARIANCES
// +A : covariant class
// -A : contravariant class
//  A : invariant class 


abstract class Animal {
  def name: String
}
case class Cat(name: String) extends Animal
case class Dog(name: String) extends Animal

def printAnimalNames(animals: List[Animal]): Unit =
  animals.foreach {
    animal => print(animal.name)
  }

val cats: List[Cat] = List(Cat("Whiskers"), Cat("Tom"))
val dogs: List[Dog] = List(Dog("Fido"), Dog("Rex"))

// prints: Whiskers, Tom
printAnimalNames(cats)
// prints: Fido, Rex
printAnimalNames(dogs)

// COVARIANT
val catAndDog: List[Animal] = List(Cat("Cat1"), Dog("Dog1"))
printAnimalNames(catAndDog)


// TODO: CONTRAVARIANT - use for Containter or Some Type constructor ???
abstract class Printer[-A] {
  def print(value: A): Unit
}

class AnimalPrinter extends Printer[Animal] {
  def print(animal: Animal): Unit =
    println("The animal's name is: " + animal.name)
}

class CatPrinter extends Printer[Cat] {
  def print(cat: Cat): Unit =
    println("The cat's name is: " + cat.name)
}

def printMyCat(printer: Printer[Cat], cat: Cat): Unit =
  printer.print(cat)

val catPrinter: Printer[Cat] = new CatPrinter
val animalPrinter: Printer[Animal] = new AnimalPrinter

// the animalPrinter can recognize Cat and print Cat with Animal format
// because the definition of Printer is declared with CONTRAVARIANT
// => Printer[Animal] is subtype of Printer[Cat]
// because Printer[Animal] is subtype Printer[Cat], so we can use Printer[Animal] in function print (Printer[Cat])

printMyCat(catPrinter, Cat("Boots"))
printMyCat(animalPrinter, Cat("Boots"))



// TODO: ---------- Upper Type Bounds
// P <: A
// -> P must be a SUB-TYPE of A


// TODO: ---------- Lower Type Bounds
// P >: A
// -> P must be a SUPER-TYPE of A


// TODO: INNER CLASS
// syntax
// OuterClass#InnerClass
// For example: Graph#Node


// TODO: ABSTRACT TYPE MEMBER
trait Buffer {
  // abstract type member
  type T
  val element: T
}

abstract class SeqBuffer extends Buffer {
  type U
  type T <: Seq[U]
  def length = element.length
}

// change position of T
trait Buffer2[T]{
  type S
  val element: T
  def toS: S = ???
}

//dynamic definition in declaration
abstract class SeqBuffer2 extends Buffer2[String] {
  //dynamic definition in implementation
  type S = Int
  def length = element.length
  override def toS: Int = element.toInt
} 

// +T <: Seq[U]
// -> All covariant of T is SUB-TYPE of Seq[U]


// Compound Types
// TODO: Type of an object is a subtype of several other types.
// syntax: obj: A with B
// for example: cloneableResetable: Cloneable with Resetable

trait Cloneable extends java.lang.Cloneable {
  override def clone(): Cloneable = {
    super.clone().asInstanceOf[Cloneable]
  }
}
trait Resetable {
  def reset: Unit
}

// like State: run -> newState -> return new State
def cloneAndReset(obj: Cloneable with Resetable): Cloneable = {
  val obj2 = obj.clone()
  obj.reset
  obj2
}

// TODO: Self-type
trait User {
  // will appear in Tweeter
  def username: String
}

trait Tweeter {
  this: User =>  // REASSIGN THIS
  def tweet(tweetText: String) = println(s"$username: $tweetText")
}

// class VerifiedTweeter extends Tweeter { // compile error: self-type does not conform to Tweeter's selftype with User
class VerifiedTweeter(val usernameParameter : String) extends Tweeter with User { //we mixin User because Tweeter required it in definition 
  // Missing implementation for: def username: String
  // => reassign or implement "username: String"
  def username = s"real $usernameParameter"
}

val realBeyonce = new VerifiedTweeter("Beyoncé") 
realBeyonce.tweet("Just spilled my glass of lemonade")


// TODO: Implicit Parameters
// Scala will look if it can get an implicit value of the correct type
// two place of search:
// 1. implicit definitions and implicit parameters that can be accessed directly (without a prefix)
// 2. members marked implicit in all the companion objects associated with the implicit candidate type.

// "zero and operation"
// or "add and unit"
abstract class Monoid[A] {
  def op(x: A, y: A): A
  def zero: A
}

// implicit val intMonoid = new Monoid[Int] {
//   def op(x: Int, y: Int): Int = x + y
//   def zero: Int = 0
// }

implicit val stringMonoid = new Monoid[String] {
  def op(x: String, y: String): String = x + y
  def zero: String = ""
}

def sum[A](xs: List[A])(implicit m: Monoid[A]): A = {
  if (xs.isEmpty) m.zero
  else m.op(xs.head, sum(xs.tail))
}

// Scala wil find implicit Monoid[Int] in this context and use that implicit Monoid[Int]
//sum(List(1,2,3)) 

// Scala will find implicit Monoid[String] in this context and use that implicit Monoid[String]
sum(List("a", "b", "c"))

// the difficult thing is how we recognize and provide (or implicit) the needed implicit.


// TODO: Implicit Conversions
// slow to find implicit
// Implcit a function to CONVERSE TYPE instead of impliciting a parameter
implicit def conversion(x: Int): String = x.toString
def show(s: String): Unit = println(s)
show(1)


//Implicit class
// TODO: add functions to a class A with syntax:
// implicit class _className(a: A) 
def multiply(s: String)(n: Int) = List.fill(n)(s).mkString
multiply("hello")(5)



// add function ** to class String.
//
implicit class className(s: String) {
  def **(n: Int) = multiply(s)(n)
}

"hello" ** 5



// TODO: Polymorphic Methods
// syntax: [A] after method name.
// def method[A](x: A)


// TODO: Type Inference

case class MyPair[A, B](x: A, y: B)
// MyPair[Int, String]
val p = MyPair(1, "scala")

//Tuple2[Int, String]
val tuple = (1, "string")


// The compiler never infers method parameter types.
// However, in certain cases, it can infer anonymous function parameter types when the function is passed as argument.
// For example: x => x * 2 will be Int => Int
Seq(1, 3, 4).map(x => x * 2)



// TODO: By-name Parameters
// By-name parameters are evaluated every time they are used. 
// syntax              input: => Int
// for example with while loop

// two by-name parameters
def whileLoop(condition: => Boolean)(body: => Unit): Unit =
  if (condition) {
    body
    whileLoop(condition)(body)
  }

// condition and body will be evaluated every iter

var i = 5

whileLoop (i > 0) {
  print(i)
  i -= 1
}


// TODO: operators
// Any method with a SINGLE can be used as an infix operator. 
// this, that for name of parameters
10.+(1)
10 + 1

// for example:

case class MyBool(x: Boolean) {
  def and(that: MyBool): MyBool = if (x) that else this
  def or(that: MyBool): MyBool = if (x) this else that
  def negate: MyBool = MyBool(!x)

  //use it as infix operator
  def not(x: MyBool) = x.negate
  def xor(x: MyBool, y: MyBool) = (x or y) and not(x and y)
}

// TODO: Precedence
// * / %
// + -
// :
// = !
// < >
// &
// ^
// |
// (all letters)

// IMPORTANT: ?^ has the highest precedence because it starts with the character ?



// TODO: Annotations
// like Java Annotations
// IMPORTANT: @tailrec -> Scala will optimize the method associated with @tailrec if the method is tail recursive. If not, Scala will fail.
// with error: “Recursive call not in tail position”.
