package jt

import scala.annotation.tailrec

object Ch3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  // exercise 3.1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case _ => 101
  }

  // exercise 3.2
  def tail[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  // exercise 3.3
  def setHead[A](a: A, lst: List[A]): List[A] = lst match {
    case Nil => Cons(a, Nil)
    case Cons(_, xs) => Cons(a, xs)
  }

  // exercise 3.4
  @tailrec
  def drop[A](lst: List[A], n: Int): List[A] = (lst, n) match {
    case (Nil, _) => Nil
    case (_, 0) => lst
    case (Cons(_, xs), _) => drop(xs, n - 1)
  }

  def tail1[A](lst: List[A]): List[A] = drop(lst, 1)

  // exercise 3.5
  def dropWhile[A](lst: List[A], f: A => Boolean): List[A] = lst match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => lst
  }

  def append[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match {
    case Nil => lst2
    case Cons(x, xs) => Cons(x, append(xs, lst2))
  }

  // exercise 3.6
  def init[A](lst: List[A]): List[A] = lst match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x1, Cons(_, xs)) => Cons(x1, init(xs))

  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]): Int = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Double]): Double = foldRight(ns, 1.0)(_ * _)

  // exercise 3.7
  def product3(ns: List[Long]): Long = foldRight(ns, 1L) {
    case (0, _) => 0
    case (a, b) => a * b
  }

  // exercise 3.8
  // Cons(1, Cons(2, Cons(3, Nil)))

  // exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, i) => i + 1)

  // exercise 3.10
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
  }

  // exercise 3.11
  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def product4(ns: List[Long]): Long = foldRight(ns, 0L)(_ * _)

  // exercise 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  // exercise 3.13
  def foldRight2[A,B](as: List[A], z: B)(f: (A,B) => B): B = {
    foldLeft(reverse(as), z)((a, b) => f(b, a))
  }

  // exercise 3.14
  def append_v1[A](lst1: List[A], lst2: List[A]): List[A] =
    foldRight(lst1, lst2)((a, b) => Cons(a, b))

  def append_v2[A](lst1: List[A], lst2: List[A]): List[A] =
    foldLeft(reverse(lst1), lst2)((b, a) => Cons(a, b))

  // exercise 3.15
  def concat[A](lsts: List[List[A]]): List[A] =
    foldRight(lsts, Nil: List[A])((a,b) => append(a, b))

  // exercise 3.16
  def listadd1(ints: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(x,xs) => Cons(x+1, listadd1(xs))
  }

  // exercise 3.17
  def doubles2strings(doubles: List[Double]): List[String] = doubles match {
    case Nil => Nil
    case Cons(d, ds) => Cons(d.toString, doubles2strings(ds))
  }



}
