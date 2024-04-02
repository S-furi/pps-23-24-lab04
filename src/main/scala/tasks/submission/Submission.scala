package tasks.submission

object Submission:
    // TASK 1
    object Ex1ComplexNumbers:
      trait ComplexADT:
        type Complex
        def complex(re: Double, im: Double): Complex
        extension (complex: Complex)
          def re(): Double
          def im(): Double
          def sum(other: Complex): Complex
          def subtract(other: Complex): Complex
          def asString(): String

      object BasicComplexADT extends ComplexADT:
        case class ComplexImpl(re: Double, im: Double)

        type Complex = ComplexImpl
        def complex(re: Double, im: Double): Complex = ComplexImpl(re, im)
        extension (complex: Complex)
          def re(): Double = complex match
            case ComplexImpl(re, _) => re

          def im(): Double = complex match
            case ComplexImpl(_, im) => im

          def sum(other: Complex): Complex = (complex, other) match
            case (ComplexImpl(re1, im1), ComplexImpl(re2, im2)) => ComplexImpl(re1 + re2, im1 + im2)

          def subtract(other: Complex): Complex = (complex, other) match
            case (ComplexImpl(re1, im1), ComplexImpl(re2, im2)) => ComplexImpl(re1 - re2, im1 - im2)

          def asString(): String = complex match
            case ComplexImpl(re, im) if re == 0.0 && im == 0.0 => "0.0"
            case ComplexImpl(re, im) if re == 0.0 => im + "i"
            case ComplexImpl(re, im) if im == 0.0 => s"$re"
            case ComplexImpl(re, im) if im > 0.0 => s"$re + ${im}i"
            case ComplexImpl(re, im) if im < 0.0 => s"$re - ${Math.abs(im)}i"

    // TASK 2
    object SchoolModel:
      import u03.Sequences.*
      import u03.Sequences.Sequence.*
      import u03.Optionals.*
      import u03.Optionals.Optional.*
      trait SchoolModule:
        type School
        type Teacher
        type Course
        extension (school: School)
          def addTeacher(name: String): School
          def addCourse(name: String): School
          def teacherByName(name: String): Optional[Teacher]
          def courseByName(name: String): Optional[Course]
          def nameOfTeacher(teacher: Teacher): String
          def nameOfCourse(course: Course): String
          def setTeacherToCourse(teacher: Teacher, course: Course): School
          def coursesOfATeacher(teacher: Teacher): Sequence[Course]

      object School extends SchoolModule:
        private case class TeacherImpl(name: String, courses: Sequence[Course])
        private case class CourseImpl(name: String)
        private case class SchoolImpl(teachers: Sequence[Teacher], courses: Sequence[Course])

        opaque type Teacher = TeacherImpl
        opaque type Course = CourseImpl
        opaque type School = SchoolImpl

        def teacher(name: String, courses: Sequence[Course]): Teacher = TeacherImpl(name, courses)
        def course(name: String): Course = CourseImpl(name)
        def school(teachers: Sequence[Teacher], courses: Sequence[Course]): School = SchoolImpl(teachers, courses)

        extension (school: School)
          def teacherByName(name: String): Optional[Teacher] = school match
            case SchoolImpl(teachers, _) => teachers.findFirst(t => t match
              case TeacherImpl(n, _) => n == name
            )

          def nameOfTeacher(teacher: Teacher): String = teacher match
            case TeacherImpl(name, _) => name

          def addCourse(name: String): School = school match
            case SchoolImpl(teachers, courses) => SchoolImpl(teachers, courses.addElement(course(name)))

          def coursesOfATeacher(teacher: Teacher): Sequence[Course] = teacher match
            case TeacherImpl(_, courses) => courses

          def setTeacherToCourse(teacher: Teacher, course: Course): School = school match
            case SchoolImpl(teachers, courses) if teachers.contains(teacher) =>
              SchoolImpl(
                filter(teachers)(school.nameOfTeacher(_) != school.nameOfTeacher(teacher)).addElement(addCourseToTeacher(teacher, course)),
                courses.addIfNotPresent(course),
              )
            case SchoolImpl(teachers, courses) =>
              SchoolImpl(
                teachers.addElement(addCourseToTeacher(teacher, course)),
                courses.addIfNotPresent(course)
              )

          private def addCourseToTeacher(t: Teacher, c: Course): Teacher = t match
            case TeacherImpl(name, courses) => TeacherImpl(name, courses.addIfNotPresent(c))

          def addTeacher(name: String): School = school match
            case SchoolImpl(teachers, courses) => SchoolImpl(teachers.addElement(teacher(name, Nil())), courses)

          def nameOfCourse(course: Course): String = course match
            case CourseImpl(name) => name

          def courseByName(name: String): Optional[Course] = school match
            case SchoolImpl(_, courses) => courses.findFirst(c => c match
              case Course(n) => n == name
            )

        extension[A] (s: Sequence[A])
          /**
            * Finds the first element that satisfies the given predicate.
            *
            * @param p the predicate to test the elements with
            * @return the element wrapped in and Optional, empty if no elements satisfies
            *   the given predicate.
            */
          def findFirst(p: A => Boolean): Optional[A] = filter(s)(p) match
            case Cons(h, _) => Optional.Just(h)
            case _ => Optional.Empty()

          /**
            * Add a new element in the head of the sequence.
            */
          def addElement(a: A): Sequence[A] = s match
            case Cons(h, t) => Cons(a, Cons(h, t))
            case _ => Cons(a, Nil())

          def contains(a: A): Boolean = !Optional.isEmpty(s.findFirst(_ == a))

          def addIfNotPresent(a: A): Sequence[A] = if !s.contains(a) then s.addElement(a) else s
    
    // TASK 3
    object Ex3Stacks:
      import u03.Optionals.*
      import u03.Optionals.Optional.*
      import u03.Sequences.*
      import u03.Sequences.Sequence.*

      trait StackADT:
        type Stack[A]
        def empty[A]: Stack[A] // factory
        extension [A](stack: Stack[A])
          def push(a: A): Stack[A]
          def pop(a: A): Optional[(A, Stack[A])]
          def asSequence(): Sequence[A]

      object StackImpl extends StackADT:
        type Stack[A] = Sequence[A]
        def empty[A]: Stack[A] = Nil()
        extension [A](stack: Stack[A])
          def push(a: A): Stack[A] = stack match
            case Cons(head, tail) => Cons(a, Cons(head, tail))
            case _ => Cons(a, Nil())
          def pop(a: A): Optional[(A, Stack[A])] = stack match
            case Cons(head, tail) => Optional.Just((head, tail))
            case _ => Optional.Empty()

          def asSequence(): Sequence[A] = stack

    // TASK 4
    object Ex4Summables:
      import u03.Sequences.*
      import u03.Sequences.Sequence.*

      def sumAllInt(seq: Sequence[Int]): Int = seq match
        case Cons(h, t) => h + sumAllInt(t)
        case _ => 0

      trait Summable[A]:
        def sum(a1: A, a2: A): A
        def zero: A

      def sumAll[A: Summable](seq: Sequence[A]): A = 
        val summable = summon[Summable[A]]
        seq match
          case Cons(h, t) => summable.sum(h, sumAll(t))
          case _ => summable.zero

      given Summable[Int] with
        def sum(a1: Int, a2: Int): Int = a1 + a2
        def zero: Int = 0

      // write givens for Summable[Double] and Summable[String]
      given Summable[Double] with
        def sum(a1: Double, a2: Double): Double = a1 + a2
        def zero: Double = 0.0

      given Summable[String] with
        def sum(a1: String, a2: String): String = a1 + a2
        def zero: String = ""


    // TASK 5
    object Ex5Traversable:
      import u03.Optionals.*
      import u03.Optionals.Optional.*
      import u03.Sequences.*
      import u03.Sequences.Sequence.*

      def log[A](a: A): Unit = println("The next element is: "+a)

      def logAll[A](seq: Sequence[A]): Unit = seq match
        case Cons(h, t) => log(h); logAll(t)
        case _ => ()

      trait Traversable[T[_]]:
        extension [A](t: T[A]) def apply(c: A => Unit): Unit

      given Traversable[Sequence] with
        extension [A](t: Sequence[A]) def apply(c: A => Unit): Unit = t match
          case Cons(head, tail) => c(head); tail.apply(c)
          case _ =>

      given Traversable[Optional] with
        extension [A](t: Optional[A]) def apply(c: A => Unit): Unit = t match
          case Just(a) => c(a)
          case _ =>

    // TASK 6
    object Ex6TryModel:
      import u04.monads.Monads.Monad.*
      import u04.monads.Monads.*

      private enum TryImpl[A]:
        case Success(value: A)
        case Failure(exception: Throwable)

      opaque type Try[A] = TryImpl[A]

      def success[A](value: A): Try[A] = TryImpl.Success(value)
      def failure[A](exception: Throwable): Try[A] = TryImpl.Failure(exception)
      def exec[A](expression: => A): Try[A] = try success(expression) catch failure(_)

      extension [A](m: Try[A]) 
        def getOrElse[B >: A](other: B): B = m match
          case TryImpl.Success(value) => value
          case TryImpl.Failure(e) => println(e); other

      given Monad[Try] with
        override def unit[A](value: A): Try[A] = success(value)
        extension [A](m: Try[A]) 

          override def flatMap[B](f: A => Try[B]): Try[B] = m match
            case TryImpl.Success(value) => f(value)
            case TryImpl.Failure(exception) => failure(exception)