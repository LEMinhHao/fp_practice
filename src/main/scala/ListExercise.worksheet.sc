case class Book(title: String, pages: Int) 
  val books = Seq( Book("Future of Scala developers", 85), 
                  Book("Parallel algorithms", 240), 
                  Book("Object Oriented Programming", 130), 
                  Book("Mobile Development", 495) ) 

// def maxBy[B](f: A => B): A
books.maxBy(book => book.pages)
//use underscore instead
books.minBy(_.pages)

// filter
books.filter(_.pages > 120).foreach(b => print(b.title))

// diff, intersect, union, distinct
// forAll

// partition
val numbers = Seq(3, 7, 2, 9, 6, 5, 1, 4, 2) 
numbers.partition(n => n % 2 == 0)

// range
List.range(0, 10, 2)


// fill, 
List.tabulate(5)(n => n * n)
List.tabulate[Int](3,4)((n1, n2) => n1 * 10 + n2)

// prepend
0 :: List(1,2)

List(0,1) ::: List(1,2)

