package tasks.submission


object SubmissionTest:
    class ComplexTest:

      // Choice of implementation to test
      val complexADT: ComplexADT = BasicComplexADT
      import complexADT.*

      // From now, everything is independent of specific implementation of Complex

      @Test def testReal() =
        assertEquals(10, complex(10, 20).re(), 0)

      @Test def testImaginary() =
        assertEquals(20, complex(10, 20).im(), 0)

      @Test def testSum() =
        assertEquals(complex(11, 22), complex(10, 20) sum complex(1, 2))

      @Test def testSubtract() =
        assertEquals(complex(9, 18), complex(10, 20) subtract complex(1, 2))

      @Test def testAsString() =
        assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())

      @Test def optionalTestAdvancedAsString() =
        assertEquals("0.0", complex(0, 0).asString())
        assertEquals("10.0", complex(10.0, 0).asString())
        assertEquals("10.0 + 5.0i", complex(10.0, 5.0).asString())
        assertEquals("10.0 - 5.0i", complex(10.0, -5.0).asString())
        assertEquals("5.0i", complex(0, 5.0).asString())
        assertEquals("-5.0i", complex(0, -5.0).asString())

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

        @Test def testNameOfTeacher(): Unit =
            val t: Teacher = teacher("C", Cons(course("PCD"), Nil()))
            assertEquals("C", s.nameOfTeacher(t))

        @Test def testNameOfCourse(): Unit =
            val c: Course = course("PCD")
            assertEquals("PCD", s.nameOfCourse(c))

        @Test def testCoursesOfATeacher(): Unit =
            val expectedCourses = Cons(course("OOP"), Cons(course("PPS"), Nil()))
            val t: Teacher = Optional.orElse(s.teacherByName("B"), teacher("B", Nil()))
            assertEquals(expectedCourses, s.coursesOfATeacher(t))

        @Test def testSetTeacherToCourse(): Unit =
            val expectedCourses =  map(Cons("MDP", Cons("OOP", Cons("PPS", Nil()))))(name => course(name))
            val t = Optional.orElse(s.teacherByName("B"), teacher("B", Nil()))
            val s2 = s.setTeacherToCourse(t, course("MDP"))
            assertEquals(expectedCourses, s2.coursesOfATeacher(Optional.orElse(s2.teacherByName("B"), teacher("B", Nil()))))
            assertFalse(Optional.isEmpty(s2.courseByName("MDP")))

    class Stacktest:
      val stack = StackImpl
      import stack.*

      @Test def testEmpty() =
        assertEquals(Sequence.Nil(), empty[Int].asSequence())

      @Test def testPush() =
        assertEquals(Sequence.Cons(10, Sequence.Nil()), empty[Int].push(10).asSequence())

      @Test def testPopOnEmpty() =
        assertEquals(Optional.Empty(), empty[Int].pop(10))

      @Test def testPopOnNotEmpty() =
        assertEquals(Optional.Just((10, Sequence.Nil())), empty[Int].push(10).pop(10))