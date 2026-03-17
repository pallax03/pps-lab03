package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*
import u03.Optionals.Optional.{Empty, Just}

class PersonSequenceTest:

  import it.unibo.pps.Person
  import it.unibo.pps.Person.*
  import it.unibo.pps.u03.Sequences.Sequence.*
  import it.unibo.pps.u03.Sequences.*

  val teacherAguzzi = Teacher("Gianluca Aguzzi", Cons("PPS", Nil()))
  val teacherViroli = Teacher("Mirko Viroli", Cons("PPS", Cons("OOP", Nil())))
  val teacherRicci = Teacher("Alessandro Ricci", Cons("PC", Nil()))
  val studentMe = Student("Alex Mazzoni", 2025)
  val studentFriend = Student("Nicola Graziotin", 2025)
  val sequence: Sequence[Person] = Cons(teacherRicci, Cons(teacherViroli, Cons(teacherAguzzi, Cons(studentMe, Cons(studentFriend, Nil())))))

  @Test def testMapToCourseFromSequenceOfPerson() =
    import it.unibo.pps.mapToCourse
    val courses = Cons("PC", Cons("PPS", Cons("OOP", Cons("PPS", Nil()))))
    assertEquals(courses, mapToCourse(sequence))

  @Test def testFoldLeft() =
    import it.unibo.pps.foldLeft
    val lstInt = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lstInt)(0)(_ - _))
    assertEquals(16, foldLeft(lstInt)(0)(_ + _))

    val lstBoolean = Cons(true, Cons(true, Cons(false, Cons(true, Nil()))))
    assertEquals(true, foldLeft(lstBoolean)(false)(_ | _))

  @Test def testPersonSequenceFoldLeft() = {
    import it.unibo.pps.totalCoursesByAllTeachers
    assertEquals(4, totalCoursesByAllTeachers(sequence))
  }

