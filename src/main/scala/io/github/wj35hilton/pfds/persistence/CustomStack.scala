package io.github.wj35hilton.pfds.persistence

sealed trait CustomStack[+A]
case object Nil extends CustomStack[Nothing]
case class Cons[+A](head: A, tail: CustomStack[A]) extends CustomStack[A]

// Chapter 2: 2.1-Lists
object CustomStack {
  def cons[A](a: A, s: CustomStack[A]): CustomStack[A] = Cons(a, s)
  def empty[A]: CustomStack[A] = Nil

  def head[A](s: CustomStack[A]): A = s match {
    case Cons(h, _) => h
    case _ => throw new IllegalStateException("Empty")
  }

  def isEmpty[A](s: CustomStack[A]): Boolean = s match {
    case Nil => true
    case _ => false
  }

  def tail[A](s: CustomStack[A]): CustomStack[A] = s match {
    case Cons(_, t) => t
    case _ => throw new IllegalStateException("Empty")
  }

  def append[A](xs: CustomStack[A], ys: CustomStack[A]): CustomStack[A] = xs match {
    case Nil => ys
    case Cons(h, t) => cons(h, append(t, ys))
  }

  def update[A](xs: CustomStack[A], i: Int, v: A): CustomStack[A] = xs match {
    case Nil => throw new IllegalStateException("Subscript")
    case Cons(h, t) if (i == 0)=> cons(v, t)
    case Cons(h, t) => cons(h, update(t, i - 1, v))
  }

  def suffixes[A](xs: CustomStack[A]): CustomStack[CustomStack[A]] = xs match {
    case s @ Cons(_, t) => Cons(s, suffixes(t))
    case Nil => empty
  }
}
