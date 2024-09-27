// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*
import scala.annotation.static


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG) 

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 


  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    rng.nextInt match
      case (n, next) if n == Int.MinValue => nonNegativeInt(next)
      case (n, next) => (n.abs, next)

  // Exercise 2

  def double(rng: RNG): (Double, RNG) = 
    rng.nextInt match
      case (n, next) =>
        (n.abs.toDouble / Int.MaxValue.toDouble, next)

  // Exercise 3
  
  // The return type is broken and needs to be fixed
  def intDouble(rng: RNG): ((Int, Double), RNG) = 
    val (i, next) = nonNegativeInt(rng)
    val (d, next2) = double(next)
    ((i,d), next2)

  // The return type is broken and needs to be fixed
  def doubleInt(rng: RNG): ((Double, Int), RNG) = 
    val ((i, d), next) = intDouble(rng)
    ((d, i), next)

  // Exercise 4

  // The return type is broken and needs to be fixed
  def ints(size: Int)(rng: RNG): (List[Int], RNG) = 
    def aux (n: Int, r: RNG, acc: List[Int]): (List[Int], RNG) =
      if n == 0 then (acc, r)
      else
        val (i, next) = r.nextInt
        aux(n-1, next, i :: acc)
    aux(size, rng, List())


  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] = 
    map(int) { i => i.abs.toDouble / Int.MaxValue.toDouble }

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => 
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)

  // Exercise 7

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    rng => 
      ras.foldRight((List.empty[A], rng)) {
        case (ra, (acc, r)) =>
          val (a, next) = ra(r)
          (a :: acc, next)
      }

  def sequence2[A](ras: List[Rand[A]]): Rand[List[A]] =
    rng => 
      ras.foldRight((List.empty[A], rng)) ( (ra, accRng) =>
        val (acc, r) = accRng
        val (a, next) = ra(r)
        (a :: acc, next)
      )

  def ints2(size: Int): Rand[List[Int]] =
    sequence(List.fill(size)(int))

  // Exercise 8

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => 
      val (a, rng2) = f(rng)
      g(a)(rng2)

  def nonNegativeLessThan(bound: Int): Rand[Int] =
    flatMap(nonNegativeInt) (i => unit(i % bound))

end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = 
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }

  def map[B](f: A => B): State[S, B] = 
    State { s => 
      val (a, s1) = run(s)
      (f(a), s1)
    }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    State { s => 
      val (a, s1) = run(s)
      val (b, s2) = sb.run(s1)
      (f(a, b), s2)
    }


object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)
 
  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    State { s =>
      sas.foldRight((List.empty[A], s)) {
        case (ra, (acc, r)) =>
          val (a, next) = ra.run(r)
          (a :: acc, next)
      }
    }

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)
  
  def stateToLazyList[S, A](s: State[S,A])(initial: S): LazyList[A] =
    val (a, s1) = s.run(initial)
    Cons(() => a, () => stateToLazyList(s)(s1))


  // Exercise 11 (lazyInts out of stateToLazyList)
  
  def lazyInts(rng: RNG): LazyList[Int] = 
    val state = State(RNG.int)
    stateToLazyList(state)(rng)

  lazy val tenStrictInts: List[Int] = 
    lazyInts(RNG.SimpleRNG(42)).take(10).toList

end State
