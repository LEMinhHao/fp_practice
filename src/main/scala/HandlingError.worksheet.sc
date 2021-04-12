import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.Exception.allCatch



val capitals = Map("France" -> "Paris", "Japan" -> "Tokyo")
capitals.get("France")
capitals.get("N")


capitals.get("Japan") match {
    case None => "?"
    case Some(c) => c 
}

//pure or unit
Option.apply("String")
Option.apply(Some("Str"))

//empty
Option.empty[Int]
Option.empty

//when
val a = 10
val cond = a < 10
Option.when(cond) (a)

//isDefined
val isDefined = Some(("1", "2")).isDefined

// + of String is deprecated. Using s" $a " instead
val size = Option("3").size + "---" + None.size

// Option size ?? 
// Because Option is a collection with zero or one element.
// The foreach, map, flatMap method can be used in many situation 
val optionSize = Option("3").size
val noneSize = None.size
val size2 = s"option's size $optionSize  and None's size $noneSize"

//Option.orElse
Option("3").orElse(Option("2"))
Option(null)

// try, catch
def toInt(x: String) = {
    try {
        Some(Integer.parseInt(x))
    } catch {
        case _ => None
    }
}
//allCatch
def toInt2(x: String) = allCatch.opt(x.toInt)

//toIntOption
def toInt3(x: String) = x.toIntOption
def toInt4(x: String) = Try(x.toInt).toOption

toInt2("2")
toInt3("hi")
toInt3("3")

toInt4("a")
toInt4("4")

// conclusion 1: using functions like object._Option. For example: s.toIntOption may occur NullPointExeception

toInt3("x").getOrElse(3)

//Option is a collection with zero or one element.
toInt3("3").foreach {

    x => println(s"${x} is a integer")
}

toInt3("nono").foreach {
    x => println(s"${x} is a integer")
}

//Using Option with Scala collections
val bag = List("1", "2", "ex", null)

bag.map(toInt2)
//bag.map(toInt3) // NullPointerException
bag.map(toInt4)

//flatten
bag.map(toInt2).flatten
bag.flatMap(toInt2)
bag.map(toInt2).collect{ case Some(value) => value }


bag.map(toInt2).collect( x => { x.toString } )
// bag.map(toInt2).collect( b => b.get) : Exception NonSuchElement "None.get"
bag.map(toInt2).collect( b => b.getOrElse(-1) > 0)

//   allCatch.opt, allCatch.withTry, allCatch.either
//   Try().toOption, Try().toEither


// scala.util.{Try, Success, Failure}
def divideXByY(x: Int, y: Int) = {
    Try(x / y)
}

divideXByY(1,1)
divideXByY(1,0)
divideXByY(1,0).getOrElse(-1)

divideXByY(1,1).foreach(println)
divideXByY(1,0).foreach(println)

divideXByY(1,1) match {
    case Success(value) => println(s"success $value")
    case Failure(exception) => println(exception.getMessage())
}

divideXByY(1,0) match {
    case Success(value) => println(s"success $value")
    case ex => ex.toString
}

var z = for {
    a <- Try("3".toInt)
    b <- Try("a".toInt)
} yield a * b

z.getOrElse(-1)

// cant use with Option
// z = for {
//     a <- toInt4("3")
//     b <- toInt4("2")
// } yield a * b

def readTextFile(fileName: String): Try[List[String]] = {
    Try(Source.fromFile(fileName).getLines().toList)
}

var error = readTextFile("wrongName.txt")
readTextFile("build.sbt")

// recover like map for exception
error.recover( (ex) => List("defaultLine1", "defaultLine2"))

// recoverWith like flatMap for exception
error.recoverWith( (ex) => readTextFile("build.sbt"))

// transform
error.transform(s => Try(s), t => Try(List("def1", "def2")))

// Either, Left, Right

def divideXByY2(x: Int, y: Int): Either[String, Int] = {
    if (y == 0) Left("Cant divide by 0")
    else Right(x / y)
}

divideXByY2(1,1)
divideXByY2(1,0)

divideXByY2(1,1) match {
    case Left(value) => println(value)
    case Right(value) => println(value + 2)
}

divideXByY2(1,1).isLeft
divideXByY2(1,1).isRight



// for comprehension
val l1 = List(1,2,3)
val l2 = List("a", "b")

for {
    e1 <- l1
    e2 <- l2
} yield (s"$e1-$e2")

for {
    i <- 0 until l1.size
    j <- i+1 until l2.size 
    e1 <- Option(l1)
    e2 <- Option(l2)
} yield (s"$i-$j")




