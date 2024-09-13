// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

// Exercise 1

trait OrderedPoint 
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  override def compare(that: java.awt.Point): Int = 
    that match
      case p if x < p.x || (x == p.x && y < p.y) => -1
      case p if x == p.x && y == p.y => 0
      case _ => 1

// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro._
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)



// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2

  def size[A](t: Tree[A]): Int =
    t match
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)

  // Exercise 3

  def maximum (t: Tree[Int]): Int = 
    t match
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)

  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))

  // Exercise 5

  def fold[A,B](t: Tree[A])(f: (B, B) => B)(g: A => B): B =
    t match
      case Leaf(value) => g(value)
      case Branch(left, right) => f (fold (left) (f) (g), fold (right) (f) (g))

  def size1[A](t: Tree[A]): Int = 
    fold (t) ((a:Int, b:Int) => 1+a+b) (_ => 1)

  def maximum1(t: Tree[Int]): Int = 
    fold (t) ((a:Int,b:Int) => a max b) (identity[Int])

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = 
    fold (t) ((a:Tree[B],b:Tree[B]) => Branch(a,b)) (l => Leaf(f (l)))




enum Option[+A]:
  case Some(value: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] =
    this match
      case Some(value) => Some(f(value))
      case None => None    

  def getOrElse[B >: A] (default: => B): B = 
    this match
      case Some(value) => value
      case None => default

  def flatMap[B](f: A => Option[B]): Option[B] = 
    this match
      case Some(value) => f(value)
      case None => None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match
      case None => ob
      case some => some
    

  def filter(p: A => Boolean): Option[A] =
    this match
      case Some(value) => if p (value) then Some(value) else None
      case None => None

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(a) => p(a)
    



object Option:

  // Exercise 9

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
    for
      a <- ao
      b <- bo
    yield f(a,b)

  // Exercise 10

  def sequence[A](aos: List[Option[A]]): Option[List[A]] =
    aos.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ :: _))

  // Exercise 11

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((a, b) => map2(f(a), b)(_ :: _))
        
    
end Option

 

// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h:: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String,Int)]): Option[Int] =
  headOption (lst.map((_,i) => i))

def headGrade1(lst: List[(String,Int)]): Option[Int] =
  headOption( 
    for 
      x1 <- lst
    yield x1._2
  )

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.length == 0 then None
  else Some(xs.sum / xs.length)

// Exercise 8

def varianceNormal(xs: Seq[Double]): Option[Double] = // not correct - uses map
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

// now using for-comprehension
def variance(xs: Seq[Double]): Option[Double] =
  for 
    m <- mean(xs)
    v <- mean((for x <- xs yield math.pow(x - m, 2)))
  yield v

// Scroll up, to the Option object for Exercise 9
