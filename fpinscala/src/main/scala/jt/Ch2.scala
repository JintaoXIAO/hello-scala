package jt

import scala.annotation.{tailrec, unused}
object Ch2 {

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, acc * n)
    go(n, 1)
  }

  // exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def loop(c: Int, a: Int, b: Int): Int =
      if (c == n) b
      else loop(c + 1, b, a + b)
    loop(0, 1, 0)
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n + 1)
    }
    loop(0)
  }

  def findFirst[A](ss: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n + 1)
    }
    loop(0)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(i: Int): Boolean = {
      if (i >= as.length) true
      else
        ordered(as(i-1), as(i)) && loop(i + 1)
    }
    loop(1)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  // exercise 2.3
  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  // exercise 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))


}


