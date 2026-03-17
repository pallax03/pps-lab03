package it.unibo.pps

import scala.annotation.tailrec
import it.unibo.pps.u03.Optionals.Optional

// Task 1
object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:
    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03
    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    @tailrec
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Nil() => Nil()
      case Cons(h, t) => if (n <= 0) Cons(h, t) else skip(t)(n-1)

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Cons((h1, h2), zip(t1, t2))
      case _ => Nil()

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = (s1, s2) match
      case (Cons(h, t), _) => Cons(h, concat(t, s2))
      case (_, Cons(h, t)) => Cons(h, t)
      case _ => Nil()

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */
    def reverse[A](s: Sequence[A]): Sequence[A] = s match
      case Cons(h, t) => concat(reverse(t), Cons(h, Nil()))
      case _ => Nil()

    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = s match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _ => Nil()

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => 10
     * E.g., [10, 1, 30] => 1
     */
    def min(s: Sequence[Int]): Optional[Int] = s match
      case Cons(h, t) =>
        lazy val minOptional = min(t)
        if Optional.orElse(minOptional, Int.MaxValue) > h then Optional.Just(h) else minOptional
      case Nil() => Optional.Empty()

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] = ???

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    def contains[A](s: Sequence[A])(elem: A): Boolean = ???

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] = ???

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = ???

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) = ???

// Task 2
object SequencePerson:
  import SequencePerson.Person.*
  import Sequences.*
  import Sequences.Sequence.*

  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, courses: Sequence[String])

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    def course(p: Person): Sequence[String] = p match
      case Teacher(_, c) => c
      case _ => throw Exception()

  extension (p: Person)
    def isTeacher: Boolean = p match
      case Person.Teacher(_, _) => true
      case _ => false

  def mapToCourse(s: Sequence[Person]): Sequence[String] =
    flatMap(s)(p => p match
      case Teacher(_, c) => c
      case _ => Nil()
    )

  def foldLeft[A, B](list: Sequence[A])(default: B)(f: (B, A) => B): B = list match
    case Cons(h, t) => f(foldLeft(t)(default)(f), h)
    case Nil() => default

  extension [A](s: Sequence[A])
    def length: Int = s match
      case Cons(h, t) => t.length + 1
      case Nil() => 0

  def totalCoursesByAllTeachers(s: Sequence[Person]): Int =
    foldLeft(
      map(filter(s)(p => p.isTeacher))(p => course(p).length)
    )(0)(_ + _)

// Task 3
object Streams:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def fromList[A](list: List[A]): Stream[A] = list match
      case head :: tail => cons(head, fromList(tail))
      case _ => Empty()

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 3
    def takeWhile[A](stream: Stream[A])(predicate: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if predicate(head()) => cons(head(), takeWhile(tail())(predicate))
      case _ => Empty()

    def fill[A](n: Int)(k: A): Stream[A] =
      if n <= 0 then Empty() else cons(k, fill(n-1)(k))

    def interleave[A](s1: Stream[A])(s2: Stream[A]): Stream[A] = (s1, s2) match
      case (Cons(h1, t1), Cons(h2, t2)) => cons(h1(), cons(h2(), interleave(t1())(t2())))
      case (Cons(h1, t1), _) => cons(h1(), interleave(t1())(Empty()))
      case (_ , Cons(h2, t2)) => cons(h2(), interleave(t2())(Empty()))
      case _ => Empty()

    def cycle[A](lst: Sequence[A]): Stream[A] = lst match
      case Sequence.Nil() => Empty()
      case _ =>
        def loop(current: Sequence[A]): Stream[A] = current match
          case Sequence.Cons(h, t) => cons(h, loop(t))
          case _ => loop(lst)
        loop(lst)

  end Stream

  lazy val fibonacci: Stream[Int] = Stream.map(
      Stream.iterate((0, 1))((prec, curr) => (curr, prec + curr))
    )
    ((c, _) => c)
