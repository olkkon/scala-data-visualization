package library.test

/* This file contains all correct test results. They were moved here in order to save space in test class */
object CorrectResults {
  
  /* Comment cleaned test files, in order to do some comparing */
  val data: Map[String, String] = Map(
      "Test1" -> ("""
        |#Datafile DVLS
        |
        |
        |#Info
        |name : Datafile test 1
        |desc : This is a test dataset, which was read from file datafile_test1.df
        |
        |#Grid
        |size   : 10
        |status : 0
        |
        |#Data_line
        |2010 : 0.5
        |2011 : 0.9
        |2012 : 0.7
        |2013 : 0.7
        |2014 : 0.1
        |
        |#EOF
        |""".stripMargin drop 1),
        "Test_entityUpdate_1_before" -> ("""
        |#Datafile DVLS
        |
        |#Misc_line
        |desc :
        |x_unit :// If something is left blank, it can be set later during the program. 
        |x_name : Akseli // The same goes if these are missing from this file
        |y_unit :
        |y_name :
        |
        |#Data_line
        |2010 : 0.5 // ANOTHER
        |2011 : 0.9 //and one more
        |2012 : 0.7
        |2013 : 0.7
        |2014 : 0.1
        |
        |#EOF
        |""".stripMargin drop 1),
        "Test_entityUpdate_1" -> ("""
        |#Datafile DVLS
        |
        |#Misc_line
        |desc : This graphs description was not found in the file, so it was generated automatically.
        |x_unit : [x unit]// If something is left blank, it can be set later during the program. 
        |x_name : Akseli // The same goes if these are missing from this file
        |y_unit : [y unit]
        |y_name : [y name]
        |
        |#Data_line
        |2010 : 0.5 // ANOTHER
        |2011 : 0.9 //and one more
        |2012 : 0.7
        |2013 : 0.7
        |2014 : 0.1
        |
        |#EOF
        |""".stripMargin drop 1),
        "Test_entityUpdate_2_before" -> ("""
        |#Datafile DVLS
        |
        |#Misc_line
        |desc :
        |x_unit : // If something is left blank, it can be set later during the program. 
        |x_name : // The same goes if these are missing from this file
        |y_unit :
        |y_name :
        |data_names: Test1__Test2
        |
        |#Data_line
        |2010 : 0.5__1
        |2011 : 0.9__2
        |2012 : 0.7__3
        |2013 : 0.7__4
        |2014 : 0.1__5
        |
        |#EOF
        |""".stripMargin drop 1),
        "Test_entityUpdate_2" -> ("""
        |#Datafile DVLS
        |
        |#Misc_line
        |desc : Beautiful line diagram
        |x_unit : Flowers/Meter // If something is left blank, it can be set later during the program. 
        |x_name : Flowers in our town // The same goes if these are missing from this file
        |y_unit : Some..Thing!
        |y_name : lol
        |data_names : Test1__X
        |
        |#Data_line
        |2010 : 2__10
        |2011 : 3__11
        |2012 : 4__3
        |2013 : 0.7__4
        |2014 : 0.1__5
        |
        |#EOF
        |""".stripMargin drop 1),
        "Test_allGraphs_before" -> ("""
        |#Datafile DVLS
        |
        |#Info
        |name : Complete dataset
        |desc : This dataset represents the structure that .df files obey.
        |
        |#Grid
        |size   : 10
        |status : 0
        |
        |#Misc_line
        |desc : Line diagram
        |x_unit : a
        |x_name : Year
        |y_unit : Grade
        |y_name : Her average grade
        |
        |#Data_line
        |2005 : 0.2
        |2006 : 0.6
        |2007 : 0.5
        |2008 : 0.8
        |2009 : 0.6
        |2010 : 0.5
        |2011 : 0.9
        |2012 : 0.7
        |2013 : 0.7
        |2014 : 0.1
        |
        |#Misc_histo
        |desc : Histogram.
        |color : #008000
        |x_name : Grade
        |y_name : Number of students
        |
        |#Data_histo
        |1 : 50
        |2 : 40
        |3 : 120
        |4 : 45
        |5 : 10
        |
        |#Misc_pie
        |desc : The distribution of grades between the students in a some course.
        |show_percentage_in_graph : 1
        |show_explanation_panel : 1
        |
        |#Data_pie
        |1 : 50 : #337ab7
        |2 : 40 : #99ff33
        |3 : 120 : #980000
        |4 : 45 : #f4e9c1
        |5 : 10 : #56ff00
        |
        |#EOF
        |""".stripMargin drop 1),
        "Test_allGraphs" -> ("""
        |#Datafile DVLS
        |
        |#Info
        |name : Complete dataset
        |desc : This dataset represents the structure that .df files obey.
        |
        |#Grid
        |size   : 10
        |status : 0
        |
        |#Misc_line
        |desc : Line diagram
        |x_unit : a
        |x_name : Year
        |y_unit : Grade
        |y_name : Her average grade
        |
        |#Data_line
        |2005 : 0.2
        |2006 : 0.6
        |2007 : 0.5
        |2008 : 0.8
        |2009 : 0.6
        |2010 : 0.5
        |2011 : 0.9
        |2012 : 0.7
        |2013 : 0.7
        |2014 : 0.1
        |
        |#Misc_histo
        |desc : Histogramming historically memorable things
        |color : #008000
        |x_name : Grade
        |y_name : Number of students
        |
        |#Data_histo
        |1 : 50
        |2 : 40
        |3 : 120
        |4 : 45
        |5 : 10
        |
        |#Misc_pie
        |desc : The distribution of grades between the students in a some course.
        |show_percentage_in_graph : 0
        |show_explanation_panel : 1
        |
        |#Data_pie
        |1 : 30 : #f4e9c1
        |2 : 100 : #99ff33
        |3 : 10 : #56ff00
        |4 : 150 : #f4e9c1
        |5 : 10 : #56ff00
        |
        |#EOF
        |""".stripMargin drop 1)
    )
}
