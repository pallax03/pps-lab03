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

val teacherAguzzi = Teacher("Gianluca Aguzzi", Cons("PPS", Nil()))
val teacherViroli = Teacher("Mirko Viroli", Cons("PPS", Cons("OOP", Nil())))
val teacherRicci = Teacher("Alessandro Ricci", Cons("PC", Nil()))
val studentMe = Student("Alex Mazzoni", 2025)
val studentFriend = Student("Nicola Graziotin", 2025)
val sequence: Sequence[Person] = Cons(teacherRicci, Cons(teacherViroli, Cons(teacherAguzzi, Cons(studentMe, Cons(studentFriend, Nil())))))

def mapToCourse(s: Sequence[Person]): Sequence[String] =
  flatMap(s)(p => p match
    case Teacher(_, c) => c
    case _ => Nil()
  )

@main
def testPersonSequence(): Unit =
  println(sequence)
  println(mapToCourse(sequence))


def foldLeft[A, B](list: Sequence[A])(default: B)(f: (B, A) => B): B = list match
  case Cons(h, t) => f(foldLeft(t)(default)(f), h)
  case Nil() => default

@main
def testFoldLeft(): Unit =
  val lstInt = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
  println(foldLeft(lstInt)(0)(_ - _)) // -16
  println(foldLeft(lstInt)(0)(_ + _)) //  16
  val lstBoolean = Cons(true, Cons(true, Cons(false, Cons(true, Nil()))))
  println(foldLeft(lstBoolean)(false)(_ | _)) // true

extension [A](s: Sequence[A])
  def length: Int = s match
    case Cons(h, t) => t.length + 1
    case Nil() => 0


def totalCoursesByAllTeachers(s: Sequence[Person]): Int =
  foldLeft(
    map(filter(s)(p => p.isTeacher))(p => course(p).length)
  )(0)(_ + _)

@main
def testPersonSequenceFoldLeft(): Unit =
  println(totalCoursesByAllTeachers(sequence))


// Task 3
