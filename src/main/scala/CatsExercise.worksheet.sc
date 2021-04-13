import cats.implicits._
import cats.Semigroup
// TODO: import cats.syntax._


// TODO: read function of List (most used )
// Review Applicative, do exercise of Cats
// Rework Calculation part => read the source or document.

// cats.simplicits._ already has cats.syntax
// view cats.MonadOps

// --- SEMIGROUP

Semigroup[Int].combine(1, 2)
Semigroup[List[Int]].combine(List(1, 2, 3), List(4, 5, 6)) 
Semigroup[Option[Int]].combine(Option(1), Option(2))
Semigroup[Option[Int]].combine(Option(1), None)

Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) // combine(6 + 1, 6 * 10) = combine(7,60)

Map("a" -> 1).combine(Map("a" -> 2, "b" -> 0))
//different
Map("a" -> 1) ++ Map("a" -> 2, "b" -> 0)


val v1 = Map("a" -> List(1,2)).combine(Map("a" -> List(3), "b" -> List(42)))
v1
//different
val v2 = Map("a" -> List(1,2)) ++ Map("a" -> List(3), "b" -> List(42))
v2


val v3 = Map("a" -> Map("b" -> 5)).combine(Map("a" -> Map("b" -> 6), "baz" -> Map()))
v3
val v4 = Map("a" -> Map("b" -> 5)) ++ (Map("a" -> Map("b" -> 6), "baz" -> Map()))
v4


val one: Option[Int] = Option(1)
val two: Option[Int] = Option(2)
val n: Option[Int] = None

one |+| two
n   |+| two
n   |+| n
two |+| n 

// --------------------------------------------------
// --- Monoid
import cats.Monoid
Monoid[String].empty
Monoid[String].combineAll(List("a", "b", "c"))
Monoid[String].combineAll(List[String](null, "a", "b", "c")) 
Monoid[String].combineAllOption(List[String](null, "a", "b", "c"))

val v5 = Monoid[Map[String, Int]].combineAll(List(Map("a" -> 1, "b" -> 2), Map("a" -> 3)))
v5
Monoid[Map[String, Int]].combineAll(List())

val l = List(1, 2, 3, 4, 5)
l.foldMap[Int](identity)
l.foldMap[String](i => i.toString()) // "12345"


// -------------------------------------------------
// --- Functor -> map
import cats.Functor

// -- map
Option(1).map(_.toString())
List(1,2,3).map(_ + 2)
Vector(1,2,3,1).map(_ * 2)

implicit val optionFunctor: Functor[Option] = new Functor[Option] {
  def map[A,B](fa: Option[A])(f: A => B): Option[B] = fa map f  
}

implicit val listFunctor: Functor[List] = new Functor[List] {
  def map[A,B](fa: List[A])(f: A => B): List[B] = fa map f
}

// However, functors can also be created for types which don't have a map method.
// For example, if we create a Functor for Function1[In, *] we can use andThen to implement map:
// TODO :


// -------------------------------------------------
val addArity2 = (a: Int, b: Int) => a + b
val addArity3 = (a: Int, b: Int, c: Int) => a + b + c

val option2 = (Option(1), Option(2))
val option3 = (option2._1, option2._2, Option.empty[Int])

option2 mapN addArity2
option3 mapN addArity3

option2 apWith Some(addArity2)
option3 apWith Some(addArity3)

option2.tupled
option3.tupled


