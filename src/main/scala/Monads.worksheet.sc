
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

trait Monad[F[_]] extends Functor[F]{

    //or pure
    def unit[A](a: => A): F[A]

    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

    def map[A,B](fa: F[A])(f: A => B): F[B] = {
        flatMap(fa)(a => unit(f(a)))
    }

    // (fa, fb)( A,B => C): fc 
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
        flatMap(fa)(a => map(fb)(b => f(a,b)))
    }
    
    // List[F[A]] : F[List[A]]
    def sequence[A](lfa: List[F[A]]) : F[List[A]] =
        lfa.foldRight(unit(List[A]()))((a, fla) => map2(a, fla)(_ :: _))
    
    
    // (la)(A => F[B]) : flb
    def traverse[A,B](la: List[A])(f: A => F[B]) : F[List[B]] = 
        la.foldRight(unit(List[B]()))( (a, flb) => map2(f(a), flb)(_ :: _)) 

    // (List[A])(A => F[B])  : F[List[B]]
    def traverse2[A,B](la: List[A])(f: A => F[B]) : F[List[B]] = 
        la.foldRight(unit(List[B]()))((a, flb) => map2(f(a), flb)(_ :: _))

    def sequence2[A](lfa: List[F[A]]): F[List[A]] = 
        traverse2(lfa)(fa => fa)
    
    // (n, fa) => fla
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
        sequence(List.fill(n)(fa))
    
    // TODO:
    // def product

}






// monad = minimal set(pure, flatMap) + laws(id, asso)
case class Id[A](id: A) { //pure or unit
    
    // flatMap
    def flatMap[B](f: A => Id[B]): Id[B] = f(id)

    // map
    def map[B](f: A => B): Id[B] = Id(f(id)) //unit(map())
}

val idMonad = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)

    //def flatMap[A,B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(a => f(a))
    def flatMap[A,B](fa: Id[A])(f: A => Id[B]) = fa.flatMap(f)
}


for {
    a <- Id("Hello ")
    b <- Id(" Monad!")
} yield a + b


case class State[S, A](run: S => (A, S)) {
    //map - f: A => B
    //this.map = State[S, A] => State[S, B]
    def map[B](f: A => B): State[S, B] = State( s => {
        val (a, s1) = run(s)
        (f(a), s1)
    })

    //
    def flatMap[B](f: A => State[S,B]): State[S,B] =  State( s => {
        val (a, s1) = run(s)
        f(a).run(s1)
    })
}

// can't implement Monad[State]
// => partial apply

// type IntState[A] = State[Int, _]

// object IntStateMonad extends Monad[ ({type IntState[A] = State[Int, A]})#IntState ] {
//   override def unit[A](a: => A): IntState[A] = State(s => (a, s))
//   override def flatMap[A,B](fa: IntState[A])(f: A => IntState[B]): IntState[B] = fa.flatMap(f)
// }

//type lambda:
// Monad[ ({type IntState[A] = State[Int, A]})#IntState ]

def stateMonad[S] = new Monad[ ({type f[x] = State[S,x]}) #f ] {
    // unit
    def unit[A](a: => A): State[S,A] = State( s => (a, s))

    //flatMap
    def flatMap[A, B](fa: State[S,A])(f: A => State[S,B]): State[S,B] = fa.flatMap(f)
}