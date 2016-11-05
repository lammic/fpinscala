package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    Cons(h, tail(l))
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    //case Nil => Nil
    //case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Nil")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, acc) => acc + 1)
  }

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]) = {
    foldLeft(l, 0)(_ + _)
  }

  def product3(l: List[Double]) = {
    foldLeft(l, 1.0)(_ * _)
  }

  def length2[A](l: List[A]): Int = {
    foldLeft(l, 0)((acc, _) => acc + 1)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((acc, x) => Cons(x, acc))
  }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil: List[Int])((x, xs) => Cons(x + 1, xs))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil: List[String])((x, xs) => Cons(x.toString, xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = {
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x),xs))
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil: List[A])((x, xs) => if (f(x)) Cons(x, xs) else xs)
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil: List[A])(append)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def addPair(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPair(xs, ys))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }
}

object TestList {
  import List._

  def main(args: Array[String]): Unit = {
    println("x = " + x)
    println("tail of Nil = " + tail(Nil))
    val l = List(1, 2, 3, 4, 5)
    println("tail of l = " + tail(l))
    val unary = List(1)
    println("tail of unary = " + tail(unary))
    println("set head of Nil = " + setHead(Nil, 1))
    println("set head of l = " + setHead(l, 0))
    println("dropping 0 from Nil = " + drop(Nil, 0))
    println("dropping 1 from Nil = " + drop(Nil, 1))
    println("dropping 0 from l = " + drop(l, 0))
    println("dropping 1 from l = " + drop(l, 1))
    println("dropping 2 from l = " + drop(l, 2))
    println("dropping while < 0 from l = " + dropWhile(l, (x: Int) => x < 0))
    println("dropping while < 3 from l = " + dropWhile(l, (x: Int) => x < 3))
    println("dropping while < 23 from l = " + dropWhile(l, (x: Int) => x < 23))
    println("init of l = " + init(l))
    println("length of l = " + length(l))
  }
}