package it.unibo.pps

import it.unibo.pps.u03.Optionals.Optional.{Empty, Just}
import org.junit.Assert.assertEquals
import org.junit.Test

// Task 1
class SequenceTest:
  import it.unibo.pps.Sequences.*
  import it.unibo.pps.Sequences.Sequence.*

  val sequence: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(sequence))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(sequence)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(sequence)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(sequence)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(sequence)(_ != 20))

  @Test def testSkip() =
    assertEquals(Cons(30, Nil()), skip(sequence)(2))
    assertEquals(Nil(), skip(sequence)(3))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), skip(sequence)(0))
    assertEquals(Nil(), skip(Nil())(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(sequence, l2))
    assertEquals(Nil(), zip(sequence, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(sequence, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testReverse() =
    assertEquals(Cons(30, Cons(20, Cons(10, Nil()))), reverse(sequence))
    assertEquals(Nil(), reverse(Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(sequence)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(sequence))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testEvenIndices() =
    assertEquals(Cons(10, Cons(30, Nil())), evenIndices(sequence))
    assertEquals(Nil(), evenIndices(Nil()))

  @Test def testContains() =
    assertEquals(true, contains(sequence)(10))
    assertEquals(false, contains(sequence)(15))
    assertEquals(false, contains(Nil())(10))

  @Test def testDistinct() =
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), distinct(sequence))
    assertEquals(Cons(10, Cons(20, Nil())), distinct(Cons(10, Cons(20, Cons(10, Nil())))))
    assertEquals(Nil(), distinct(Nil()))

  @Test def testGroup() =
    val sequence = Cons(10, Cons(10, Cons(20, Cons(30, Cons(20, Nil())))))
    val grouped =
      Cons(Cons(10, Cons(10, Nil())), Cons(Cons(20, Nil()), Cons(Cons(30, Nil()), Cons(Cons(20, Nil()), Nil()))))
    assertEquals(group(sequence), grouped)
    assertEquals(Nil(), group(Nil()))

  @Test def testPartition() =
    val sequence = Cons(11, Cons(20, Cons(31, Nil())))
    val (even, odd) = partition(sequence)(x => x % 2 == 0)
    assertEquals(Cons(20, Nil()), even)
    assertEquals(Cons(11, Cons(31, Nil())), odd)

    val emptySequence = Nil()
    val (evenEmpty, oddEmpty) = partition(emptySequence)(x => true)
    assertEquals(Nil(), evenEmpty)
    assertEquals(Nil(), oddEmpty)

// Task 2
class PersonSequenceTest:

  import it.unibo.pps.SequencePerson.*
  import it.unibo.pps.SequencePerson.Person.*
  import it.unibo.pps.Sequences.Sequence.*
  import it.unibo.pps.Sequences.*

  val teacherAguzzi = Teacher("Gianluca Aguzzi", Cons("PPS", Nil()))
  val teacherViroli = Teacher("Mirko Viroli", Cons("PPS", Cons("OOP", Nil())))
  val teacherRicci = Teacher("Alessandro Ricci", Cons("PC", Nil()))
  val studentMe = Student("Alex Mazzoni", 2025)
  val studentFriend = Student("Nicola Graziotin", 2025)
  val sequence: Sequence[Person] = Cons(teacherRicci, Cons(teacherViroli, Cons(teacherAguzzi, Cons(studentMe, Cons(studentFriend, Nil())))))

  @Test def testMapToCourseFromSequenceOfPerson() =
    val courses = Cons("PC", Cons("PPS", Cons("OOP", Cons("PPS", Nil()))))
    assertEquals(courses, mapToCourse(sequence))

  @Test def testFoldLeft() =
    val lstInt = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lstInt)(0)(_ - _))
    assertEquals(16, foldLeft(lstInt)(0)(_ + _))

    val lstBoolean = Cons(true, Cons(true, Cons(false, Cons(true, Nil()))))
    assertEquals(true, foldLeft(lstBoolean)(false)(_ | _))

  @Test def testPersonSequenceFoldLeft() = {
    assertEquals(4, totalCoursesByAllTeachers(sequence))
  }

// Task 3
class StreamTest:
  import it.unibo.pps.Streams.fibonacci
  import it.unibo.pps.Streams.*
  import it.unibo.pps.Sequences.Sequence.*

  @Test def testTakeWhile() =
    val stream = Stream.iterate(0)(_+1)
    val result = Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil())))))
    assertEquals(result, Stream.toList(Stream.takeWhile(stream)(_ < 5)))

  @Test def testFill() =
    val result = Cons("a", Cons("a", Cons("a", Nil())))
    assertEquals(result, Stream.toList(Stream.fill(3)("a")))

  @Test def testFibonacci() =
    val fibonacciFirstFive = Cons(0, Cons(1, Cons(1, Cons(2, Cons(3, Nil())))))
    val fibonacciSecondFive = Cons(5, Cons(8, Cons(13, Cons(21, Cons(34, Nil())))))

    assertEquals(fibonacciFirstFive, Stream.toList(Stream.take(fibonacci)(5)))
    assertEquals(concat(fibonacciFirstFive, fibonacciSecondFive), Stream.toList(Stream.take(fibonacci)(10)))

  // Optionals
  @Test def testFromList() =
    val result = Stream.toList(Stream.take(Stream.iterate(1)(_+1))(3))
    val list = Stream.toList(Stream.fromList(List(1, 2, 3)))
    assertEquals(result, list)

  @Test def testInterleave() =
    val s1  = Stream.fromList(List(1, 3, 5))
    val s2  = Stream.fromList(List(2, 4, 6, 8, 10))
    val result = Stream.fromList(List(1, 2, 3, 4, 5, 6, 8, 10))
    assertEquals(Stream.toList(result), Stream.toList(Stream.interleave(s1)(s2)))

  @Test def testCycle() =
    val result = Cons('a', Cons('b', Cons('c', Cons('a', Cons('b', Nil())))))
    val repeat = Stream.cycle(Cons('a', Cons('b', Cons('c', Nil()))))
    assertEquals(result, Stream.toList(Stream.take(repeat)(5)))
    assertEquals(Nil(), Stream.toList(Stream.take(Stream.cycle(Nil()))(5)))