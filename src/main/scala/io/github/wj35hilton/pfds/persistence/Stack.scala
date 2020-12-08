package io.github.wj35hilton.pfds.persistence

// Chapter 2: 2.1-Lists
trait Stack[A] {
  def empty: Stack[A]
  def isEmpty(s: Stack[A]): Boolean

  def cons(a: A, s: Stack[A]): Stack[A]
  def head(s: Stack[A]): A
  def tail(s: Stack[A]): Stack[A]
}
