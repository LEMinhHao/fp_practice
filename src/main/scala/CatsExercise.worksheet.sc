import cats.implicits._

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
