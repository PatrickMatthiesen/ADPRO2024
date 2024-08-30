// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.intro

object MyModule:

  def abs(n: Int): Int =
    if n < 0 then -n else n

  // Exercise 1

  def square(n: Int): Int =
    n*n

  private def formatAbs(x: Int): String =
    s"The absolute value of ${x} is ${abs(x)}"

  val magic: Int = 42
  var result: Option[Int] = None

  @main def printAbs: Unit =
    assert(magic - 84 == magic.-(84))
    println(formatAbs(magic - 100))
    println (abs(-54))

end MyModule

// Exercise 2 requires no programming

// Exercise 3

def fib(n: Int): Int =
  def aux (x: Int) (acc1: Int) (acc2: Int): Int = 
    x match
      case 0 => acc1
      case _: Int => aux (x-1) (acc1 + acc2) (acc1)
  aux (n-1) (0) (1)


// Exercise 4

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
  if (as.length < 2) true else
  as.foldLeft (true, as(0)) ((acc, e) => if (acc._1) (ordered (acc._2, e), e) else acc)._1

// Exercise 5

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a:A) => (b:B) => f(a,b)

def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
  curry(isSorted)

// Exercise 6

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a:A,b:B) => f (a) (b)

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
  uncurry(curry(isSorted))

// Exercise 7

def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a:A) => f(g(a))
