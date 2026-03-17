package it.unibo.pps


//TASK 2
import it.unibo.pps
import it.unibo.pps.Person.{Student, Teacher, course}
import it.unibo.pps.u03.Sequences
import u03.Sequences.*
import u03.Sequences.Sequence.*

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
    case Teacher(_, _) => true
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
import u03.Streams.*

def takeWhile[A](s: Int)(n: Int): Stream[A] = ???
