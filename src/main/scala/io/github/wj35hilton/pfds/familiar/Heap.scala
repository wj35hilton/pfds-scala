package io.github.wj35hilton.pfds.familiar

trait Heap[H[_], A] {
  def empty: H[A]
  def isEmpty(h: H[A]): Boolean
  def insert(x: A, h: H[A]): H[A]
  def merge(a: H[A], b: H[A]): H[A]
  def findMin(h: H[A]): Option[A]
  def deleteMin(h: H[A]): H[A]
}

object LeftistHeap {
  sealed trait LeftistHeap[+A]
  case object E extends LeftistHeap[Nothing]
  case class T[+A](rank: Int, a: A, l: LeftistHeap[A], r: LeftistHeap[A]) extends LeftistHeap[A]

  implicit def leftistInstance[A](implicit O: Ordering[A]): Heap[LeftistHeap, A] =
    new Heap[LeftistHeap, A] {
      def empty: LeftistHeap[A] = E
      def isEmpty(h: LeftistHeap[A]): Boolean = h match {
        case E => true
        case _ => false
      }

      def insert(x: A, h: LeftistHeap[A]): LeftistHeap[A] = merge(T(1, x, E, E), h)

      def merge(a: LeftistHeap[A], b: LeftistHeap[A]): LeftistHeap[A] = {
        def rank(h: LeftistHeap[A]): Int = h match {
          case T(r, _, _, _) => r
          case E => 0
        }

        def makeT(x: A, l: LeftistHeap[A], r: LeftistHeap[A]): LeftistHeap[A] =
          if (rank(l) >= rank(r))
            T(rank(r) + 1, x, l, r)
          else
            T(rank(l) + 1, x, r, l)

        (a, b) match {
          case((h, E)) => h
          case((E, h)) => h
          case((t1 @ T(_, x, ax, bx), t2 @ T(_, y, ay, by))) =>
            if (O.lt(x, y))
              makeT(x, ax, merge(bx, t2))
            else
              makeT(y, ay, merge(t1, by))
        }
      }

      def findMin(h: LeftistHeap[A]): Option[A] = h match {
        case T(_, a, _, _) => Some(a)
        case E => None
      }

      def deleteMin(h: LeftistHeap[A]): LeftistHeap[A] = h match {
        case T(_, _, l, r) => merge(l, r)
        case E => h
      }

      // ex 3.1 proof
      // review write up
      // x = log2(n + 1)
      // 2^x = n + 1
      // ex
    }
}
