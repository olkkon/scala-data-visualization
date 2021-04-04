package library.test

import library._
import java.io._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/* This class is solely for all unit tests, which has something to do with data input-output. It's all about 
 * parsing and handling the data coming from files. */
class IOTest extends FlatSpec with Matchers {
  
  /* Location for datafiles (.df) */
  val datafileLocation = "Data/"
  
  /* General method to check, that dataset has correct values assigned. Used later inside this file. */
  private def datasetHasCorrectGraphInfo(dataset: Dataset, oneLiners: Seq[Seq[String]], twoLiners: Seq[Seq[(String, String)]],
                                        threeLiners: Seq[Seq[(String, String, String)]] ) ={
    withClue("problems with gridData") {
      dataset.getGridData.toList.length should equal (2)
      dataset.getGridData.values.toList should equal (oneLiners(0).map(x => x.toInt))
    }
    withClue("problems with general information header") {
      dataset.getInfoData.toList.length should equal (2)
      dataset.getInfoData.values.toList should equal (List(oneLiners(1)(0), oneLiners(1)(1)))
    }
    withClue("problems with lineData") {
      if (!twoLiners(1).isEmpty){
        dataset.getLineData should not equal (None)
      }
      if (dataset.getLineData != None){
        dataset.getLineData.get._2("Line 1") should equal (twoLiners(1).toMap)
      }
      if (twoLiners.length > 2){
        dataset.getLineData.get._2("Line 2") should equal (twoLiners(2).toMap)
      }
    }
    withClue("problems with histoData") {
      if (!twoLiners(0).isEmpty){
        dataset.getHistoData should not equal (None)
      }
      if (dataset.getHistoData != None){
        dataset.getHistoData.get._2 should equal (twoLiners(0).toMap)
      }
    }
    withClue("problems with pieData") {
      if (!threeLiners(0).isEmpty){
        dataset.getPieData should not equal (None)
      }
      if (dataset.getPieData != None){
        dataset.getPieData.get._2 should equal ((threeLiners(0).map(x => x._1) zip threeLiners(0).map(x => x._2)).toMap)
      }
    }
  }
  
  /* Helper method to change file contents */
  private def changeFileContents(f: String, c: String){
      val write = new StringWriter()
      val read = new BufferedReader( new StringReader(c))
      var curLin = read.readLine()
      
      while (curLin != null){
        write.write(curLin + "\n")
        curLin = read.readLine()
      }
      val newf = new File(f)
      newf.createNewFile()
      val fileWriter = new FileWriter(f)
      fileWriter.write(write.toString)
      
      fileWriter.close()
      write.close()
      read.close()
  }
    
  "DatafileIO.loadData" should "be able to load the data and form a correct Dataset" in {
    
      val dataset1 = DatafileIO.loadData(datafileLocation + "datafile_test1.df")
      val oneLiners1: Seq[Seq[String]] = Seq(Seq("10","0"),Seq("Datafile test 1","This is a test dataset, which was read from file datafile_test1.df"))
      val twoLiners1: Seq[Seq[(String, String)]] = Seq(Seq(),Seq(("2010","0.5"),("2011","0.9"),("2012","0.7"),("2013","0.7"),("2014","0.1")))
      
      datasetHasCorrectGraphInfo(dataset1, oneLiners1, twoLiners1, Seq(Seq()))
      
      val dataset2 = DatafileIO.loadData(datafileLocation + "datafile_test2.df")
      val oneLiners2: Seq[Seq[String]] = Seq(Seq("10","0"),Seq("Datafile test 2",
          "This is a test dataset, which was read from file datafile_test2.df"),
          Seq("This graphs description was not found in the file, so it was generated automatically.","#022000"))
      val twoLiners2: Seq[Seq[(String, String)]] = Seq(Seq(),Seq(("2010","0.5"),("2011","0.9"),("2012","0.7"),("2013","0.7"),("2014","0.1")),
          Seq(("2010","1"),("2011","2"),("2012","3"),("2013","4"),("2014","5")))
      val threeLiners2: Seq[Seq[(String, String, String)]] = Seq(Seq(("1","50","#337ab7"),("2","40","#99ff33"),("3","120","#980000"),
          ("4","45","#f4e9c1"),("5","10","#56ff00")))
       
     datasetHasCorrectGraphInfo(dataset2, oneLiners2, twoLiners2, threeLiners2)
     
     val dataset3 = DatafileIO.loadData(datafileLocation + "datafile_test3.df")
     val oneLiners3: Seq[Seq[String]] = Seq(Seq("10","0"),Seq("Dataset",
         "This dataset's description was not found in the file, so it was generated automatically"),Seq(
          "This graphs description was not found in the file, so it was generated automatically.","#000000"))
     val twoLiners3: Seq[Seq[(String, String)]] = Seq(Seq(("1","50"),("2","40"),("3","120"),("4","45"),("5","10")), Seq())
          
     datasetHasCorrectGraphInfo(dataset3, oneLiners3, twoLiners3,Seq(Seq()))
      
    } 
  
    it should "throw CorruptedDatafileException when the file doesn't have starting/ending/data" in {
      intercept[CorruptedDatafileException]{
        DatafileIO.loadData(datafileLocation + "datafile_test_broken_nodata.df")
      }
      intercept[CorruptedDatafileException]{
        DatafileIO.loadData(datafileLocation + "datafile_test_broken_noending.df")
      }
      intercept[CorruptedDatafileException]{
        DatafileIO.loadData(datafileLocation + "datafile_test_broken_nostarting.df")
      }
    }
    
    
    
    "DatafileIO.updateData" should "be able to change entities in the target file correctly" in {
      
      /********************** ENTITY UPDATE 1 *******************************/
      val dataset = DatafileIO.loadData(datafileLocation + "datafile_test_entityUpdate1.df")
      val linediag = new Line(dataset)
      
      /* Testing this part is done as follows: First we make dataset according to datafile entityUpdate1 (which includes data only
       * for line diagram). Because the datafile has empty entities, they're going to be fulfilled automatically. Now, when
       * we update the data, these automatically added entities should be added to the file, also. Commenting should be
       * as it was in the original file. */
      linediag.updateEntities()
      
      /* Reading target file */
      val buf = new BufferedReader( new FileReader(datafileLocation + "datafile_test_entityUpdate1.df") )
      var contents = ""
      var newLine = buf.readLine()
      
      while (newLine != null){
        contents += newLine + "\n"
        newLine = buf.readLine()
      }
      buf.close()
      
      withClue("Data wasn't updated correctly!") {
        contents should equal (CorrectResults.data("Test_entityUpdate_1"))
      }
      
      /* Reverting file back to it's original form */
      changeFileContents(datafileLocation + "datafile_test_entityUpdate1.df", CorrectResults.data("Test_entityUpdate_1_before"))
      
      
      
      /********************** ENTITY UPDATE 2 *******************************/
      
      val dataset2 = DatafileIO.loadData(datafileLocation + "datafile_test_entityUpdate2.df")
      val linediag2 = new Line(dataset2)
      
      /* Changing entities */
      linediag2.updateEntity("desc", "Beautiful line diagram")
      linediag2.updateEntity("x_unit", "Flowers/Meter")
      linediag2.updateEntity("x_name", "Flowers in our town")
      linediag2.updateEntity("y_unit", "Some..Thing!")
      linediag2.updateEntity("y_name", "lol")
      
      linediag2.updateEntity("data_names", "X", "Test2")
      
      linediag2.updateEntity("2010", "2", "Test1")
      linediag2.updateEntity("2011", "3", "Test1")
      linediag2.updateEntity("2012", "4", "Test1")
      linediag2.updateEntity("2010", "10","X")
      linediag2.updateEntity("2011", "11","X")
      
      linediag2.updateEntities()
      
      /* Reading target file */
      val buf2 = new BufferedReader( new FileReader(datafileLocation + "datafile_test_entityUpdate2.df") )
      var contents2 = ""
      var newLine2 = buf2.readLine()
      
      while (newLine2 != null){
        contents2 += newLine2 + "\n"
        newLine2 = buf2.readLine()
      }
      buf2.close()
      withClue("Data wasn't updated correctly!") {
        contents2 should equal (CorrectResults.data("Test_entityUpdate_2"))
      }
      
      /* Reverting file back to it's original form */
      changeFileContents(datafileLocation + "datafile_test_entityUpdate2.df", CorrectResults.data("Test_entityUpdate_2_before"))
      
      
      /********************** ENTITY UPDATE 3 *******************************/
      
      val dataset3 = DatafileIO.loadData(datafileLocation + "datafile_test_allGraphs.df")
      
      val histogram = new Histogram(dataset3)
      val pie       = new Pie(dataset3)
      
      /* Changing entities */
      histogram.updateEntity("desc", "Histogramming historically memorable things")
      pie.updateEntity("show_percentage_in_graph","0")
      
      pie.updateEntity("1", "30")
      pie.updateEntity("1", "#f4e9c1")
      pie.updateEntity("2", "100")
      pie.updateEntity("3", "10")
      pie.updateEntity("4", "150")
      pie.updateEntity("3", "#56ff00")
      
      histogram.updateEntities()
      pie.updateEntities()
      
      /* Reading target file */
      val buf3 = new BufferedReader( new FileReader(datafileLocation + "datafile_test_allGraphs.df") )
      var contents3 = ""
      var newLine3 = buf3.readLine()
      
      while (newLine3 != null){
        contents3 += newLine3 + "\n"
        newLine3 = buf3.readLine()
      }
      buf3.close()
      withClue("Data wasn't updated correctly!") {
        contents3 should equal (CorrectResults.data("Test_allGraphs"))
      }
      
      /* Reverting file back to it's original form */
      changeFileContents(datafileLocation + "datafile_test_allGraphs.df", CorrectResults.data("Test_allGraphs_before"))
    }
    
    "DatafileIO.commentParser" should "be able to parse comments from given file" in {
      val test_actual = DatafileIO.commentParser( new FileReader(datafileLocation + "datafile_test1.df") )
      withClue("Comments weren't parsed correctly.") {
        test_actual should equal (CorrectResults.data("Test1"))
      }
    }
  
    "DatafileIO.currentDirHeaders" should "be able to list all datafiles correctly" in {
      val f1 = new File("test1.df"); f1.createNewFile()
      val f2 = new File("test2.df"); f2.createNewFile()
      val f3 = new File("test3.txt"); f3.createNewFile()
      val f4 = new File("actualfile3.df"); f4.createNewFile()
      withClue("One or more datafiles were missing when listing files.") {
        DatafileIO.currentDirHeaders(excludeTest = true) should contain only ("actualfile3")
        DatafileIO.currentDirHeaders() should contain only ("test1","test2","actualfile3")
        f1.delete
        DatafileIO.currentDirHeaders() should contain only ("test2","actualfile3")
      }
      f2.delete
      f4.delete
      withClue("Files were found, even though shouldn't have been.") {
        DatafileIO.currentDirHeaders() should equal (List())
      }
      f3.delete
    }
    
    "Dataset.update" should "be able to change values of entities inside the class" in {
      val dataset = DatafileIO.loadData(datafileLocation + "datafile_test2.df")
      val line = new Line(dataset)
      
      val c = line.updateEntity("y_name", "Kalle","Line 1")
      val d = line.updateEntity("desc", "For testing purposes","Line 1")
      
      withClue("Entity changing method returned false boolean: "){
        c should equal (true)
        d should equal (true)
      }
      withClue("Entity wasn't changed correctly into database") {
        line.data._1("y_name") should equal ("Kalle")
        line.data._1("desc") should equal ("For testing purposes")
      }
      
      val a = line.updateEntity("2014", "1.0", "Line 1")
      val b = line.updateEntity("2010", "AASI", "Line 1")
      val t = line.updateEntity("0101", "TEST", "Line 1")
      
      withClue("Entity changing method returned false boolean: "){
        a should equal (true)
        b should equal (true)
        t should equal (false)
      }
      withClue("Entity wasn't changed correctly into database") {
        line.data._2("Line 1")("2014") should equal ("1.0")
        line.data._2("Line 1")("2010") should equal ("AASI")
      }
      
      dataset.update("name", "info", "test program", true)
      dataset.update("desc", "info", "For testing purposes", true)
      
      withClue("Entity wasn't changed correctly into database") {
        dataset.getInfoData("name") should equal ("test program")
        dataset.getInfoData("desc") should equal ("For testing purposes")
      } 
    }
}







