object Monads extends App {

  //type constructor: List[A], Option[A], ...
  //general class base on 'map'
  trait Functor[F[_]] {

    def map[A,B](fa: F[A])(f: A => B): F[B]

    //or unzip
    def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) = {
      (map(fab)(ab => ab._1), map(fab)(ab => ab._2))
      // (map(fab)(_._1), map(fab)(_._2))
    }

    //
    def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A,B]] = {
      e match {
        case Left(fa) => map(fa)(Left(_))
        case Right(fb) => map(fb)(Right(_))
      }
    }

    //another view from Cats document
    def lift[A,B](f: A => B): F[A] => F[B] =
      fa => map(fa)(f)

  }

  val functorOption = new Functor[Option] {
    def map[A,B](fa: Option[A])(f: A => B) = fa match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  val functorList = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B) = as map f
  }

  trait Monad[F[_]] {

    //or pure
    def unit[A](a: => A): F[A]

    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

    def map[A,B](fa: F[A])(f: A => B): F[B] = {
      flatMap(fa)(a => unit(f(a)))
    }

    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
      flatMap(fa)(a => map(fb)(b => f(a,b)))
    }
    // TODO:
    // def sequence[A](lfa: List[F[A]]) : F[List[A]] = ???
    // def traverse[A,B](la: List[A])(f: A => F[B]) : F[List[B]]
    // def product
    // def replicateM

  }

  val monadList = new Monad[List] {
    override def unit[A](a: => A): List[A] = ???

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = ???
  }

  // TODO: re-implement State like a Monadic Data Type




}
