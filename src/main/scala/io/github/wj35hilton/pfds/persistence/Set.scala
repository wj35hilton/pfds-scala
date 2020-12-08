package io.github.wj35hilton.pfds.persistence

trait Set[S[_], A] {
  def empty: S[A]
  def insert(a: A, s: S[A]): S[A]
  def member(a: A, s: S[A]): Boolean
}

sealed trait UnbalancedSet[+A]
case object E extends UnbalancedSet[Nothing]
case class T[+A](l: UnbalancedSet[A], a: A, r: UnbalancedSet[A]) extends UnbalancedSet[A]

object UnbalancedSet {
  implicit def unbalanced[A](implicit O: Ordering[A]): Set[UnbalancedSet, A] =
    new Set[UnbalancedSet, A] {
      def empty: UnbalancedSet[A] = E

      def insert(a: A, s: UnbalancedSet[A]): UnbalancedSet[A] = s match {
        case T(l, aa, r) if O.lt(a, aa) => T(insert(a, l), aa, r)
        case T(l, aa, r) if O.gt(a, aa) => T(l, aa, insert(a, r))
        case t: T[A] => t
        case E => T(E, a, E)
      }

      def member(a: A, s: UnbalancedSet[A]): Boolean = s match {
        case T(l, aa, r) if O.lt(a, aa) => member(a, l)
        case T(l, aa, r) if O.gt(a, aa) => member(a, r)
        case t: T[A] => true
        case E => false
      }

      // ex2.2
      def member2(x: A, s: UnbalancedSet[A]): Boolean = {
        def _member(z: A, t: UnbalancedSet[A]): Boolean = t match {
          case E => (x == z)
          case T(l, y, r) =>
            if (O.lt(x, y))
              _member(z, l)
            else
              _member(y, r)
        }

        s match {
          case E => false
          case t @ T(_, y, _) => _member(y, t)
        }
      }

      // ex2.3
      def insert2(x: A, s: UnbalancedSet[A]): UnbalancedSet[A] = {
        def insertAux(t: UnbalancedSet[A]): Option[UnbalancedSet[A]] = t match {
          case E => Some(T(E, x, E))
          case T(l, y, r) if O.lt(x, y) => insertAux(l).map(T(_, y, r))
          case T(l, y, r) if O.gt(x, y) => insertAux(r).map(T(l, y, _))
          case t: T[A] => None
        }

        insertAux(s).getOrElse(s)
      }

      // ex2.4
      def insert3(x: A, s: UnbalancedSet[A]): UnbalancedSet[A] = {
        def insertAux(z: A, t: UnbalancedSet[A]): Option[UnbalancedSet[A]] = t match {
          case E => Option.when(x != z)(T(E, x, E))

          case T(l, y, r) =>
            if (O.lt(x, y))
              insertAux(z, l).map(T(_, y, r))
            else
              insertAux(y, r).map(T(l, y, _))
        }

        s match {
          case E => T(E, x, E)
          case t @ T(_, y, _) => insertAux(y, s).getOrElse(s)
        }
      }

      // ex2.5a
      def complete(x: A, d: Int): UnbalancedSet[A] = d match {
        case 0 => E
        case _ => {
          val sub = complete(x, d - 1)

          T(sub, x, sub)
        }
      }

      // ex2.5b
      def completeBal(x: A, s: Int): UnbalancedSet[A] = {
        def complete2(m: Int): (UnbalancedSet[A], UnbalancedSet[A]) = m match {
          case 0 => (E, T(E, x, E))
          case _ => {
            val (sp0, sp1) = complete2((m - 1) / 2)
            if (m % 2 == 0)
              (T(sp0, x, sp1), T(sp1, x, sp1))
            else
              (T(sp0, x, sp0), T(sp0, x, sp1))
          }
        }

        complete2(s)._1
     }

      // ex2.6
      // TODO: finite set
    }
}
