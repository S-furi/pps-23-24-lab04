package tasks.adts
import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Optionals.*
import u03.Optionals.Optional.*
import u02.AlgebraicDataTypes.Person
import u03.Sequences.Sequence.map

/*  Exercise 2: 
 *  Implement the below trait, and write a meaningful test.
 *  Suggestion: 
 *  - reuse Sequences and Optionals as imported above
 *  - Course is a simple case classes with just the name
 *  - Teacher is a case class with name and sequence of courses
 *  - School is a case class with (sequences of) teachers and courses
 *  - add/set methods below create the new school 
 */

object SchoolModel:

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
      

      def setTeacherToCourse(teacher: Teacher, course: Course): School = ???

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
      
      
