
object PurelyFunctionalState extends App {


    // case class State[S, A](run: S => (A, S)) {
    //     def map[B](f: A => B): State[S, B] = {
    //         State( s => {
    //             val (a, s1) = run(s)
    //             (f(a),s1)
    //         })
    //     }
    // }
    
    case class State[S, A](run: S => (A, S)) {
        //map - f: A => B
        //this.map = State[S, A] => State[S, B]
        def map[B](f: A => B): State[S, B] = State( s => {
            val (a, s1) = run(s)
            (f(a), s1)
        })

        //
        def flatMap[B](f: A => State[S, B]): State[S, B] =  State( s => {
            val (a, s1) = run(s)
            f(a).run(s1)
        })

        
    }




    
    // trait RNG {
    //     def nextInt: (Int, RNG)
    // }

    // case class SimpleRNG(seed: Long) extends RNG {
    //     def nextInt: (Int, RNG) =  {
    //         val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    //         val nextRNG = SimpleRNG(newSeed)
    //         val n = (newSeed >>> 16).toInt
    //         (n, nextRNG)
    //     }
    // }

    // // type Rand[+A] = RNG => (A, RNG)

    // // val int: Rand[Int] = _.nextInt

    // // def unit[A](a: A): Rand[A] = rng => (a, rng)

    // // def map[A,B](s: Rand[A])(f: A => B): Rand[B] = 
    // //     rng => {
    // //         val (a, rng2) = s(rng)
    // //         (f(a), rng2)
    // //     }

    // // def nonNegativeInt(rng: RNG): (Int, RNG) = {
    // //     val (a, newState) = rng.nextInt
    // //     (if (a < 0) -a
    // //         else a, newState)
    // // }

    // // def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i%2)
    // // val nonRng = SimpleRNG(1)
    // // val nonNeg1 = nonNegativeEven(nonRng)
    // // println(nonNeg1._1)
    // // val nonNeg2 = nonNegativeEven(nonRng)
    // // println(nonNeg2._1)
    // // val nonNeg3 = nonNegativeEven(nonNeg2._2)
    // // println(nonNeg3._1)
    // // def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    // //     rng => {
    // //         val (a, rng1) = f(rng)
    // //         g(a)(rng1)
    // //     }
    // // }
    // // val a = unit[Int](10)(SimpleRNG(1))
    // // val b = unit[Int](10)(SimpleRNG(2))

    // // val c = map[Int, Double](int)(_.toDouble + 3)
    
    // // val nextVal = int(SimpleRNG(3))
    // // val nextVal2 = int(nextVal._2)

    // // val nextVal3 = c(nextVal2._2)
    // // val nextVal4 = c(nextVal3._2)

    // // println(nextVal._1)
    // // println(nextVal2._1)
    // // println(nextVal3._1)
    // // println(nextVal4._1)


    // // println(int)
    // // println(a)
    // // println(a)
    // // println(c)


    // //we can use run = s => (a,s) for methods
    // case class State[S, +A](run: S => (A,S)) {
    //     // input A, output: State[S, A]
    //     def unit[S, A](a: A): State[S, A] = State[S, A](run = s => (a, s))

    //     // input f: A => State[S, B](run = S => (B, S))
    //     // output:  State[S, B](run = S => (B, S))
    //     def flatMap[B](f: A => State[S, B]): State[S, B] = State[S,B]( run = s => {
    //         val (a, newS) = run(s)
    //         f(a).run(newS)
    //     })


    //     def map[B](f: A => B): State[S,B] = flatMap[B](a => unit[S,B](f(a)))


    //     def map2[B,C](s2: State[S,B])(f: (A,B) => C) : State[S,C] = {
    //         //flatMap[C] with f: A => State[S, C]
    //         // flatMap[C](a => "State[S,C]" )

    //         // State[S,C] = run: S => (C,S)
    //         // need C here. f(a,b) => c

    //         // flatMap[C](a => f(a,b))
    //         // need b here.

    //         // flatMap[C](a => s2.map(b => f(a,b)))
    //         flatMap[C](a => s2.map[C](b => f(a, b)))
    //     }

    //     // TODO: 
    //     // Hard: If you can combine two RNG transitions, you should be able to combine a whole
    //     // list of them. Implement sequence for combining a List of transitions into a single
    //     // transition. Use it to reimplement the ints function you wrote before. For the latter,
    //     // you can use the standard library function List.fill(n)(x) to make a list with x
    //     // repeated n times.
    //     // def sequence[A](fs: List[Rand[A]]): Rand[List[A]]
    //     // def sequence(fs: List[State[S,A]]): State[S, List[A] = ???

    // }

    // sealed trait Input
    // case object Coin extends Input
    // case object Turn extends Input

    // case class Machine(locked: Boolean, candies: Int, coins: Int)
    
    // /* The rules of the machine are as follows:
    //     Inserting a coin into a locked machine will cause it to unlock if there’s any
    // candy left.
    //     Turning the knob on an unlocked machine will cause it to dispense candy and
    // become locked.
    //     Turning the knob on a locked machine or inserting a coin into an unlocked
    // machine does nothing.
    //     A machine that’s out of candy ignores all inputs.
    // */ 
    
    
    // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

}