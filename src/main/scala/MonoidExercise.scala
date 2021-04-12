

object MonoidExercise extends App {
  println("test234")


  trait Monoid[A] {
    def op(a1: A, a2: A): A

    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    def zero = ""
  }

  val intAdditionMonoid = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val composeAddConcat = new Monoid[(Int, String)] {
    def op(a1: (Int, String), a2: (Int, String)): (Int, String) = (intAdditionMonoid.op(a1._1, a2._1), stringMonoid.op(a1._2, a2._2))

    def zero: (Int, String) = (0, "")
  }
  println(composeAddConcat.op((1, "one"), (2, "two")))


  class MonoidComposition[A, B](ma: Monoid[A], mb: Monoid[B]) extends Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (ma.op(a1._1, a2._1), mb.op(a1._2, a2._2))

    def op2(a1: (A, B), a2: (A, B)): (A, B) = opMany(a1,a2)

    def zero: (A, B) = (ma.zero, mb.zero)

    def opMany(a: (A, B)* ) = {
      a.fold(zero)(op(_,_))
    }

    def foldLeft(a: (A, B)* ) = {
      a.foldLeft(zero)(op(_,_))
    }

    def foldRight(a: (A, B)* ) = {
      a.foldRight(zero)(op(_,_))
    }

    def proveAssociative2(p1: (A,B), p2: (A,B), p3: (A,B)) = {
      val b = foldLeft(p1,p2,p3) //(p1 op p2) op p3
      val c = foldRight(p1,p2,p3) // p1 op (p2 op p3)


      println(b)
      println(c)
      b == c
    }

    def proveAssociative(a: (A,B)*) = {
      val b = foldLeft(a: _*)
      val c = foldRight(a: _*)

      println(b)
      println(c)
      b == c
    }


  }

  val composeAddConcat2 = new MonoidComposition(intAdditionMonoid, stringMonoid)
  println(composeAddConcat2.op((1, "one"), (2, "two")))

  val p1 = (1, "2")
  val p2 = (3, "4")
  val p3 = (5, "6")

  val isTrue = composeAddConcat2.proveAssociative2(p1, p2, p3)
  println(isTrue)

//  val composeMultAddConcat = new Monoid[(Int, Int, String)] {
//    def op(a1: (Int, Int, String), a2: (Int, Int, String)) = (intMultiplication.op(a1._1, a2._1),
//      intAdditionMonoid.op(a1._2, a2._2),
//      stringMonoid.op(a1._3, a2._3))
//
//    def zero = (intMultiplication.zero, intAdditionMonoid.zero, stringMonoid.zero)
//  }
//
//  println(composeMultAddConcat.op((1, 2, "One"), (1, 2, "Two")))



}