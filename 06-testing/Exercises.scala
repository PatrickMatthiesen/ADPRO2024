// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
// import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] = 
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
  yield list2lazyList(la)
  
/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
  * not tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None => empty
  Gen.const(loop)

def infiniteLazyListOfErros[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => Cons(() => { ???; a} , () => loop)
      case None => empty
  Gen.const(loop)

/* The test suite */

object LazyListSpec 
  extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random" 

  // Exercise 2

  property("Ex02.01: headOption does not force the tail of a lazy list") =

    given Arbitrary[LazyList[Int]] = Arbitrary(Cons[Int](() => 1, () => sys.error("tail forced"))) // fails if tail is forced

    forAll { (s: LazyList[Int]) => s.headOption == Some(1) } :| "head only"
    Prop { cons(42, ???).headOption == Some(42) } :| "head only"
    
  // Exercise 3

  property("Ex03.01: take take does not force any heads nor any tails of the lazy list") =

    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyListOfErros[Int])

    forAll { (s: LazyList[Int], n: Int) => s.take(n) ; true } :| "take n"

  // Exercise 4

  property("Ex04.01: take(n) does not force the (n+1)st element") = 
    
    forAll(Gen.choose(0, 100)) { (n: Int) =>
    
      // has the (n+1)st element been accessed
      var accessed = false
      
      // Create a LazyList with side effects on the (n+1)st element
      val lazyList = LazyList.from(0).map { x =>
        if (x == n) accessed = true // Side effect on the (n+1)st element
        x
      }

      // Take the first n elements
      lazyList.take(n).toList
            
      // Assert that the (n+1)st element was not accessed
      !accessed && lazyList.drop(n).headOption.isDefined && accessed
    }

  // Exercise 5

  def zip [A, B] (l1: LazyList[A], l2: LazyList[B]): LazyList[(A, B)] = (l1, l2) match
    case (Cons(h1, t1), Cons(h2, t2)) => cons((h1(), h2()), zip(t1(), t2()))
    case _ => empty

  def equals [A] (l1: LazyList[A], l2: LazyList[A]): Boolean = (l1, l2) match
    case (Cons(h1, t1), Cons(h2, t2)) => h1() == h2() && equals(t1(), t2())
    case (Empty, Empty) => true
    case _ => false

  property("Ex05.01: take(n).take(n) is the same as take(n)") =

    forAll(infiniteLazyList[Int],Gen.choose(0, 10000)) { (s: LazyList[Int], n: Int) => 
      val l1 = s.take(n).take(n)
      val l2 = s.take(n) 
      
      // compare them structurally
      equals(l1, l2)
    } :| "take n"
  
  // Exercise 6
  
  property("Ex06.01: drop(n).drop(m) is the same as drop(n+m)") =

    forAll(infiniteLazyList[Int],Gen.choose(0, 10000), Gen.choose(0, 1000)) { (s: LazyList[Int], n: Int, m: Int) => 
      val l1 = s.drop(n).drop(m)
      val l2 = s.drop(n+m)
      
      // compare them structurally
      l1.headOption == l2.headOption
    } :| "drop n+m"

  // Exercise 7

  property("Ex07.01: drop(n) does not force the first n elements") =

    forAll(infiniteLazyList[Int],Gen.choose(0, 10000)) { (s: LazyList[Int], n: Int) => s.drop(n) ; true } :| "drop n"

  // Exercise 8

  property("Ex08.01: l.map(identity) == l for any lazy list l") =

    forAll(genNonEmptyLazyList[Int]) { (s: LazyList[Int]) => 
      equals(s.map(identity), s)
    } :| "map identity"

  // Exercise 9

  property("Ex09.01: map terminates on infinite lists") =

    forAll(infiniteLazyList[Int]) { (s: LazyList[Int]) => 
      s.map(identity) ; true
    } :| "map identity"
 
  // Exercise 10

  property("Ex10.01: append terminates on infinite lists") =

    forAll(infiniteLazyList[Int],infiniteLazyList[Int]) { (s1: LazyList[Int], s2: LazyList[Int]) => 
      s1.append(s2) ; true
    } :| "append"

  property("Ex10.02: append appends the two lists") =

    forAll(genNonEmptyLazyList[Int],genNonEmptyLazyList[Int]) { (s1: LazyList[Int], s2: LazyList[Int]) => 
      val newlist = s1.append(s2)
      newlist.toList == s1.toList ++ s2.toList
    } :| "append"