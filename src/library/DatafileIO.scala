package library

import java.io._
import scala.collection.mutable.Buffer

/* Object DatafileIO is an object to handle and process the data from custom made datafiles.
 * 
 * These files obeys a premade format, example can be found at file datafile_concept.txt.
 * The format isnt that strict, so it requires a lot of parsing inside the DatafileIO to achieve
 * that freedom. 
 * 
 * Methods:
 * loadData(input: String) : Dataset             Creates a dataset from given file, and returns it.
 * updateData(data: Graph, file: String) : Unit  Updates all data associated to given graph into given file.
 * currentDirHeaders(): List[String]             Lists all the datafiles (.df) in the same directory than the program.
 */
object DatafileIO {
  def loadData(input: String): Dataset ={
    var lineReader = new BufferedReader(new FileReader(input))
    var ending = false
    
    /* All information to these buffers is going to be read straight from the datafile */
    var info                   = Buffer[String]()
    var info_line, data_line   = Buffer[String]()
    var info_histo, data_histo = Buffer[String]()
    var info_pie, data_pie     = Buffer[String]()
    var grid                   = Buffer[String]()
    
    /* All information to these buffers is going to be converted from previous data */
    var data_line_converted  = Seq[Buffer[(String,String)]]()
    var data_histo_converted = Buffer[(String, String)]()
    var data_pie_converted   = Buffer[(String, String, String)]()
    
    try {
       /* Trimming additional whitespace in the start of the file */
       var currentLine = lineReader.readLine().trim.toLowerCase
       while (currentLine == ""){ currentLine = lineReader.readLine().trim.toLowerCase }
       
       /* Checking that the datafile obeys the format */
       if (!((currentLine startsWith "#datafile") && (currentLine endsWith "dvls"))) 
               throw new CorruptedDatafileException("Unknown formatting in datafile: "+input)
       
       /* Cleaning all comments from the datafile. */
       val cleanedFile = new StringReader( commentParser(lineReader) )
       lineReader = new BufferedReader( cleanedFile )
       
       /* Reads whole block from the source file, starting with #header and ending to another #header
        * @RETURNS Buffer[String], which contains block's all elements, unparsed but trimmed */
       def readWholeBlock(): Buffer[String]={
         val temp = Buffer[String]()
           if (currentLine startsWith "#" ){
             temp += currentLine.trim
             currentLine = lineReader.readLine()
             while (!(currentLine startsWith "#")){
               if (!currentLine.isEmpty) temp += currentLine.trim
               currentLine = lineReader.readLine()
               if (currentLine == null) return temp
             }
           }
           temp
        }

       /* Reads whole file, until the end is reached ("null"). If a header is met, let's reaWholeBlock() read it. */
        while (currentLine != null){
          if (!currentLine.isEmpty){
            if (currentLine startsWith "#"){
              if (currentLine.take(4).toLowerCase == "#eof"){ // End met, let's stop the reading
                ending = true
                currentLine = null
              } else {
                val blockData = readWholeBlock() // Read whole block, and parse data accordingly
                blockData(0).toLowerCase match {
                  case "#info"       => info       = blockData.tail
                  case "#misc_line"  => info_line  = blockData.tail
                  case "#data_line"  => data_line  = blockData.tail
                  case "#misc_histo" => info_histo = blockData.tail
                  case "#data_histo" => data_histo = blockData.tail
                  case "#misc_pie"   => info_pie   = blockData.tail 
                  case "#data_pie"   => data_pie   = blockData.tail
                  case "#grid"       => grid       = blockData.tail
                  case _               => // No actions needed, unknown block type
                }
              }
            } else {
              currentLine = lineReader.readLine()
            }
          } else {
            currentLine = lineReader.readLine()
          }
        }
        lineReader.close()
        
        /* Essential blocks must be found inside the datafile. These consists of ending and at least one graph's data */
        if ( !ending || (data_line.isEmpty && data_histo.isEmpty && data_pie.isEmpty) ) 
          throw new CorruptedDatafileException("Not enough information to create a dataset.")
        
        /********************************************************************************/
        /* All information is now gathered at the buffers. Let's parse the information! */
        /********************************************************************************/
        
        /* assignDataToGivenSeq assigns data based on given keys. Only data, which are associated to those
         * keys, are going to be added into sequence. If the original sequence is empty, default values are
         * going to be assigned.
         * @RETURNS manipulated Buffer[String], whose elements have been changed according to rules */
        def assignDataToGivenSeq(info: Seq[String], keys: List[String], dft: List[String],
                                          dataSeq: Option[Seq[String]] = None): Buffer[String] ={
          val info_r = Buffer[String]()
          
          /* Expanding the buffer to match the size of default values - list */
          for (i <- dft.indices){ info_r += "" }
          
          /* If no initial values were found and original sequence datatype contains only information,
           * we'll assign default values to it. If the original sequence has also separate data sequence,
           * we have to check whether that sequence is also empty before assigning default values. */
          if (info.isEmpty){
            /* If this is a graph and has some numerical data, assign default values. If this isn't graph, assign also then */
            if (dataSeq == None){ return (dft.toBuffer ++ info_r.drop(dft.length))
            } else if (!dataSeq.get.isEmpty){ return (dft.toBuffer ++ info_r.drop(dft.length)) }
          } 
          
          val parsedInfo = info.map( parseDouble(_) )
          var trashCounter = 0
          
          /* Reading all headers and keeping only those which belong to class */
          for (i <- parsedInfo.indices){
            if (keys contains parsedInfo(i)._1.toLowerCase){
              val ind = keys.indexOf(parsedInfo(i)._1.toLowerCase)
              if (parsedInfo(i)._2.isEmpty){
                if (i >= dft.length){
                  // This variable have no default value, so just skip it
                } else {
                  info_r(ind) = dft(i)
                }
              } else {
                info_r(ind) = parsedInfo(i)._2
              }
            } else {
              trashCounter += 1
            }
          }
          
          /* If there's more default values than data entities, assign default values to the end of the sequence */
          if (dft.length > parsedInfo.length-trashCounter){
            for (k <- 0 until dft.length) if (info_r(k).isEmpty) info_r(k) = dft(k)
          }
          return info_r
        }
        
        /* Parsing dataset's info */
        info = assignDataToGivenSeq(info, List("name","desc"),List("Dataset","Description not found"))
               
        /* Parsing grid information */
        grid = assignDataToGivenSeq(grid, List("size","status"),List("10","0"))
               
        /* Parsing line diagram information */
        info_line = assignDataToGivenSeq(info_line, List("desc","x_unit","x_name","y_unit","y_name","show_explanation_panel","data_names"),
                    List("Description not found.","","","","","1",""))
           
        if (!data_line.isEmpty){
          val lines_bymisc = parseMultipleLines(info_line(6))
          val lines_bydata = parseMultipleLines( parseDouble(data_line(0))._2 )
          val lines_amount = math.max(lines_bymisc.length, lines_bydata.length)
          
          val raw = data_line.map( parseDouble(_) )
          val parsedValues = raw.map( x => parseMultipleLines(x._2))
          
          /* Parsing each line's values to it's own array */
          for (i <- 0 until lines_amount){
            val name = if (i > lines_bymisc.length-1) "Line "+(i+1) else if (lines_bymisc(i).isEmpty) "Line "+(i+1) else lines_bymisc(i)
            var convert = (raw.map( x => x._1 ) zip parsedValues.map( _(i) ).toSeq).toSeq
            convert = (name,"") +: convert
            data_line_converted = data_line_converted :+ convert.toBuffer 
          }
        }
        
        /* Parsing histogram information */
        info_histo = assignDataToGivenSeq(info_histo, List("desc","color","x_name","y_name","show_frequencies"),List("Description not found","#FFFFFF","","","1"))
        if (!info_histo.isEmpty || !data_histo.isEmpty) data_histo_converted = data_histo.map( parseDouble(_) )
        
        /* Parsing pie diagram information */
        info_pie = assignDataToGivenSeq(info_pie, List("desc","show_percentage_in_graph","show_data_values","show_explanation_panel",
                    "data_key_name","data_value_name"), List("Description not found.","1","1","1","",""))
        if (!info_pie.isEmpty || !data_pie.isEmpty) data_pie_converted = data_pie.map( parseTriple(_) ) 
                
        /* Converting all graph data into suitable format, so it can be transferred to dataset - class. */
        var singles = Seq[Seq[String]]()
        var doubles = Seq[Seq[(String,String)]]()
        var tripleys= Seq[Seq[(String,String,String)]]()
        val rawData = Seq(data_histo, data_line, data_pie)
        val rawInfo = Seq(info_histo, info_line, info_pie)
        val convertedData = Seq(data_line_converted, data_histo_converted, data_pie_converted)
        
        singles = singles :+ grid
        
        /* Assigning all values to correct slots */
        for (i <- rawData.indices){
          if (!rawData(i).isEmpty) {
            singles = singles :+ rawInfo(i) 
          } else {
            singles = singles :+ Seq()
          }
          
          /* Graph data handling differs a bit, mainly due to fact that pie diagrams have three data slots */
          i match {
            case 0  => doubles = fulfillEntities(rawData(i), data_histo_converted, doubles)
            case 1  => { // Saving all line diagrams
              for (j <- 0 until data_line_converted.length){
                doubles = fulfillEntities(rawData(i), data_line_converted(j), doubles)
              }
            }
            case 2  => tripleys = fulfillEntities(rawData(i), data_pie_converted, tripleys)
            case _  => // Unknown data type, skip 
          }
        }
        
        /* This helper method fulfills given sequence with fixed values, according to given rules */
        def fulfillEntities[B](raw: Seq[String], conv: Seq[B], dest: Seq[Seq[B]]): Seq[Seq[B]] ={
          var temp = dest
          if (!raw.isEmpty){
            temp = temp :+ conv
          } else {
            temp = temp :+ Seq()
          }
          return temp
        }
        				
        /* Creating a dataset with the information we have just parsed */
        return new Dataset(info(0), info(1), input, (singles, doubles, tripleys))
    } catch { // Some exception occurred, handle it a bit so it's origin can be solved
      case a: IOException =>
        val exc = new CorruptedDatafileException("Datafile reading failed.")
        exc.initCause(a)
        throw exc
    }   
  }
  
  /* These helper methods parses a double or triple from given string.
   * @RETURNS a double (String, String) or a triple (String, String, String) */
  private def parseDouble(source: String): (String, String) ={
    val temp=source.split(":")
    if (temp.length == 2)(temp(0).trim, temp(1).trim) else (temp(0).trim,"") }
  private def parseTriple(source: String): (String, String, String) ={
    val temp=source.split(":")
    if (temp.length == 3)(temp(0).trim, temp(1).trim, temp(2).trim) else (temp(0).trim,"","") }
  private def parseMultipleLines(data: String): Array[String] = data.split("__")
  
  /* Method commentParser takes some kind of reader as an input, parses the comments from it and returns it as a StringReader.
   * Comments start always with //, but they can be either in the start of the row or after some code. That code is naturally going to
   * be executed in it's time.
   * @RETURNS parsed code as a String, which then can be read using StringReader  */
  def commentParser(input: Reader): String ={
    /* Creating needed readers and writers to process the data */
    val lineReader = new BufferedReader(input)
    val lineWriter = new StringWriter()
    var currentLine = lineReader.readLine()
    
    /* Reading whole file line by line, and deciding on each line, whether to keep the line or not */
    while (currentLine != null){
      if (currentLine startsWith "//"){ // Case 1: whole line comments
        // Just skip
      } else if (currentLine contains "//") { // Case 2: line possibly contains a comment somewhere
        lineWriter.write( trimCommentLine(currentLine) )
      } else {
        lineWriter.write(currentLine+"\n")
      }
      currentLine = lineReader.readLine()
    }
    lineWriter.toString
  }
  
  /* A method to trim a single line containing a comment */
  private def trimCommentLine(input: String): String = if(!(input contains "//")) input else {
    input.take(input.lastIndexOfSlice("//")).trim + "\n" }
  
  /* updateData updates all changed information related to graph type inside the given source file.
   * Method compares data found in the file and data contained in the graph, and changes only required
   * entities. If data was not found in the file, or changing the information did not succeed, throws
   * [CorruptedDatafileException]. 
   * @PARAM editHeader: if set true, only the header (#Info) will be edited. False as default. 
   * @RETURNS unit  */
  def updateData(data: Graph, file: String, editHeader: Boolean = false): Unit ={
    try {
       var lineReader = new BufferedReader(new FileReader(file))
       var spaces = 0
       
       /* Trimming additional whitespace in the start of the file */
       var currentLine = lineReader.readLine()
       while (currentLine == ""){ currentLine = lineReader.readLine(); spaces += 1 }
       
       /* Checking that the datafile obeys the format */       
       if (!((currentLine.trim.toLowerCase startsWith "#datafile") && (currentLine.trim.toLowerCase endsWith "dvls"))) 
               throw new CorruptedDatafileException("Unknown formatting in datafile: "+file)
       
       /* Reading the source file and creating the new file will be done simultaneously */
       val lineWriter = new StringWriter()
       
       for (i <- 0 until spaces){
         lineWriter.write("\n")
       }
       lineWriter.write(currentLine + "\n")
       
       /* Determining datatype that is going to be handled */
       var datatype = ""
       if (editHeader) datatype = "info" else datatype = data.dataType
       
       /* Reads whole block from the source file, starting with #header and ending to another #header.
        * Changes the entities according to the updated data, and keeps commenting untouched while doing the process.
        * @RETURNS Buffer[String], which contains all changed rows */
       def changeEntities(d: Graph): Buffer[String]={
         val temp = Buffer[String]()
         if ( (currentLine startsWith "#") && (currentLine.toLowerCase contains datatype) ){
           
           /* Determining structure type. 1 = misc(information), 0 = data(measurements)  */
           val structureType = (currentLine.toLowerCase contains "misc")
           
           temp += currentLine
           currentLine = lineReader.readLine()
           while (!(currentLine startsWith "#")){
             if (!currentLine.isEmpty){
               val uncommented = trimCommentLine(currentLine)
               var line = ""
               line = formNewLine(datatype, structureType)     
               
               /* A helper method to form a new line with new values, coming from the dataset. Leaves commenting untouched in the original file */
               def formNewLine(dataType: String, structure: Boolean) ={
                 var newline = ""
                 if (!structure && dataType == "pie"){ // Only case when triple parsing is needed is with pie's measurements
                   val pieces = parseTriple(uncommented) 
                   newline = pieces._1 +" : "
                   newline += d.setData.getPieData.get._2(pieces._1) + " : "
                   newline += d.setData.getPieData.get._3(pieces._1)
                 } else {
                   val pieces = parseDouble(uncommented)
                   newline = pieces._1 +" :"
                   
                   if(structure){     // Handling information
                     dataType match {
                       case "line" if (pieces._1 == "data_names") => {
                         val t = lineNames.mkString("__"); if (t != "") newline += " " + t
                       }
                       case "line"  => val t = d.setData.getLineData.get._1(pieces._1);                if (t != "") newline += " " + t
                       case "histo" => val t = d.setData.getHistoData.get._1(pieces._1);               if (t != "") newline += " " + t
                       case "pie"   => val t = d.setData.getPieData.get._1(pieces._1);                 if (t != "") newline += " " + t
                       case "grid"  => val t = d.setData.getGridData(pieces._1);                                    newline += " " + t
                       case "info"  => val t = d.setData.getInfoData(pieces._1);                       if (t != "") newline += " " + t
                       case _       => throw new CorruptedDatafileException("Unknown dataType when updating graph info!")
                     }
                   } else {            // Handling numerical data
                     dataType match {
                       case "line"  => newline += " " + valuesWithSameKey(pieces._1).mkString("__")
                       case "histo" => newline += " " + d.setData.getHistoData.get._2(pieces._1)
                       case _       => throw new CorruptedDatafileException("Unknown dataType when updating graph info!")
                     }
                   }
                   /* Handy helper methods to convert data between different formats. Must only be used with line diagram */
                   def lineNames: List[String] = data.asInstanceOf[Line].data._2.keys.toList
                   def valuesWithSameKey(key: String): List[String] = data.asInstanceOf[Line].data._2.toList.map( x => x._2(key) )
                   
                   /* Adding possible comment back */
                   newline += currentLine.takeRight( currentLine.length - uncommented.trim.length )
                 }
                 newline
               }
               
               /* Adding newly formed line to list of new lines */
               temp += line
             } else {
               temp += "" // Empty line
             }
             currentLine = lineReader.readLine()
             if (currentLine == null) return temp
           }
         }
         temp
        }
       currentLine = lineReader.readLine()
       
       /* Reads whole file, until the end is reached ("null"). If suitable header is met, changes it's values in the helper method */
       while (currentLine != null){
         if (!currentLine.isEmpty){
           if (currentLine startsWith "#"){
             if (currentLine.take(4).toLowerCase == "#eof"){ // End met, let's stop the reading
               lineWriter.write(currentLine + "\n")
               currentLine = null
             } else {
               if (currentLine.toLowerCase contains datatype){ // Interesting block
                 val block = changeEntities(data)
                 for (i <- block){ // Writing changes to file
                   lineWriter.write(i + "\n")
                 }
               } else { // Some block whose information we don't want to change
                 lineWriter.write(currentLine + "\n")
                 currentLine = lineReader.readLine()
               }
             }
           } else { // Some other block's data
             lineWriter.write(currentLine + "\n")
             currentLine = lineReader.readLine()
           }
         } else { // Empty lines
           lineWriter.write(currentLine + "\n")
           currentLine = lineReader.readLine()
         }
       }
       lineReader.close()
       
       /* Updating changed information to the file */
       val newf = new File(file)
       newf.createNewFile()
       
       val fileWriter = new FileWriter(file)
       fileWriter.write(lineWriter.toString)
       
       /* Closing writers */
       lineWriter.close()
       fileWriter.close()
       
    } catch { // Some exception occurred, handle it a bit so it's origin can be solved
       case a: IOException =>
        val exc = new CorruptedDatafileException("Datafile reading/writing failed.")
        exc.initCause(a)
        throw exc
    } 
  }
  
  /* currentDirHeaders() scans for datafiles (.df) in the given directory.
   * Current directory is scanned as a default. Method returns an empty list, if nothing was found.
   * @RETURNS a List[String] containing all datafiles in the current directory. */
  def currentDirHeaders(dir: String = System.getProperty("user.dir"), excludeTest: Boolean = false): List[String] ={
    val d = new File(dir)
    var files: List[File] = List()
    if (d.exists && d.isDirectory) files = d.listFiles.filter(_.isFile).toList.filter { file => file.getName.endsWith(".df") }
         else return List()
    if (excludeTest) return files.filterNot { file => file.getName.contains("test") }.map( _.getName.dropRight(3) )
                else return files.map( _.getName.dropRight(3) )
  }
}