package tasks.adts

import org.junit.*
import org.junit.Assert.*

import u03.Sequences.*
import u03.Sequences.Sequence.*
import u03.Optionals.*

import tasks.adts.SchoolModel
import tasks.adts.SchoolModel.*
import tasks.adts.SchoolModel.School.*

class SchoolModelTest:
    val courses: Sequence[Course] = map(Cons("Math", Cons("OOP", Cons("PPS", Nil()))))(name => course(name))
    val teachers: Sequence[Teacher] = Cons(teacher("A", Cons(course("Math"), Nil())), Cons(teacher("B", Cons(course("OOP"), Cons(course("PPS"), Nil()))), Nil()))
    val s: School  = school(teachers, courses)

    @Test def testSchoolCreation(): Unit =
        assertNotNull(school)

    @Test def testGetTeacherByName(): Unit =
        val expectedTeacher: Optional[Teacher] = Optional.Just(teacher("A", Cons(course("Math"), Nil())))
        assertEquals(expectedTeacher, s.teacherByName("A"))
        assertEquals(Optional.Empty(), s.teacherByName("idonotexist"))

    @Test def testGetCourseByName(): Unit =
        val expectedCourse: Optional[Course] = Optional.Just(course("OOP"))
        assertEquals(expectedCourse, s.courseByName("OOP"))
        assertEquals(Optional.Empty(), s.courseByName("MDP"))

    @Test def testTeacherAdd(): Unit =
        val expectedTeacher: Optional[Teacher] = Optional.Just(teacher("C", Nil()))
        val s2 = s.addTeacher("C")
        assertEquals(expectedTeacher, s2.teacherByName("C"))

    @Test def testCourseAdd(): Unit =
        val expectedCourse: Optional[Course] = Optional.Just(course("MDP"))
        val s2 = s.addCourse("MDP")
        assertEquals(expectedCourse, s2.courseByName("MDP"))
    