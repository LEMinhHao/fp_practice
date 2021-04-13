// Classes

class Point(var z: Int = 0, var a: Int = 0) {
  private var _x = 0
  private var _y = 0

  def x = _x

  //setter
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

// --- Higher order function
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

//Coercing methods into functions
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

// Functions that return functions

def returnFunction(x: Int) = (y: Int) => x + y

def returnFunction2(x: Int) = (y: Int, z: Int) => x * y + z

//Currying
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
  case Couch(b) => "Lie on the couch " + b // TODO + c.a
  case Couch2(a,b) => " " + a + " ---- " + b

}

var couch1 = Couch("a: Couch - b: Couch")
var couch2 = Couch2("a: Couch2", "b: b-Couch2")

findPlace(couch1)
findPlace(couch2)

// Singleton 
// lazy val
object Logger {
  def info(message: String) = println(s"INFO: $message")
}

// Companion Object
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







