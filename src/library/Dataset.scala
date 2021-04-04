package library

import scala.collection.mutable.Buffer

/* Class dataset is to get together all information related to one dataset. All graph data
 * is somewhat parsed, and final parsing will be done inside this class. For updating data,
 * this class calls DatafileIO. All graph data is easily accessible in Map - structures.     */
class Dataset(private var name: String, private var desc: String, source: String, 
     private var graphData: (Seq[Seq[String]], Seq[Seq[(String, String)]], Seq[Seq[(String, String, String)]]) ){
  
  /* graphData has following structure:
   * 
   * Outermost structure is a triple, which consists of three kind of sequencies:
   * a) Seq[String]: All information, which can be expressed with one lines.
   * 		0) gridData
   * 		1) histo info
   * 		2) line  info
   * 		3) pie   info
   * b) Seq[(String, String)]: All information, which has to be expressed as doubles.
   * 		0) histo data
   * 		1) line data
   * 		.  each entity will
   * 		.  consist of one line
   * 		n) diagrams data
   * c) Seq[(String, String, String)]: All information, which has to be expressed as triples.
   * 		0) pie   data
   * 
   * If a graph exist in a datafile, both info and data entities are present.
   * gridData will always exist. If it's not set in the file, it's set during the parsing.
   * */
  
  /* STRUCTURE OF DATAFILE ENTITIES AND THEIR INFORMATION
   * 
   * INFO (key,value)
   * name, desc
   * 
   * GRID (key, value)
   * size, status
   * 
   * LINE
   * 1) Misc (key, value)
   * 		desc,x_unit,x_name,y_unit,y_name, show_explanation_panel, data_names
   * 2) Amount of line diagrams * Data (key, value)
   * 		NOTE: graph name will be the first entity of it's data sequence
   *    (and more precisely the first entity of tuple of first entity of sequence)
   * 
   * HISTO
   * 1) Misc (key, value)
   * 		desc,color,x_name,y_name, show_frequencies
   * 2) Data (key, value)
   * 
   * PIE
   * 1) Misc (key, value)
   * 		desc,show_percentage_in_graph,show_data_values,show_explanation_panel,data_key_name,data_value_name
   * 2) Data (key, value, color)
   */
  
  private var infoData:  Map[String, String] = Map()
  private var gridData:  Map[String, Int]    = Map()
  private var lineData:  Option[(Map[String,String],Map[String, Map[String, String]])]         = None
  private var histoData: Option[(Map[String,String],Map[String, String])]                      = None
  private var pieData:   Option[(Map[String,String],Map[String, String], Map[String, String])] = None
  
  /* This map is used to help track entity places inside the graphData. */
  private var indexData = Map("info" -> Map("name" -> 0, "desc" -> 1),
                             "grid" -> Map("size" -> 0, "status" -> 1),
                             "line" -> Map("desc" -> 0, "x_unit" -> 1, "x_name" -> 2, "y_unit" -> 3, "y_name" -> 4, "show_explanation_panel" -> 5),
                             "histo" -> Map("desc" -> 0, "color" -> 1, "x_name" -> 2, "y_name" -> 3, "show_frequencies" -> 4),
                             "pie" -> Map("desc" -> 0,"show_percentage_in_graph" -> 1, "show_data_values" -> 2,
                             "show_explanation_panel" -> 3,"data_key_name" -> 4,"data_value_name" -> 5))
  
  private def updateGraphMaps() {                                                      
    
    /* Parsing infoData straight from the input values */
    infoData = Map("name" -> name, "desc" -> desc)
  
    /* gridData is just setting names and their respective values, in integers */
    gridData = Map("size" -> graphData._1(0)(0).toInt, "status" -> graphData._1(0)(1).toInt)
  
    /* lineData consists of three maps. First is for settings and their values, second is to access different line diagrams
     * and third consist data of these line diagrams */
    if (graphData._2.length > 1){
      
      val info = Map("desc" -> graphData._1(2)(0), "x_unit" -> graphData._1(2)(1), "x_name" -> graphData._1(2)(2),
                     "y_unit" -> graphData._1(2)(3), "y_name" -> graphData._1(2)(4), "show_explanation_panel" -> graphData._1(2)(5))
      var lines: Seq[Map[String,String]] = Seq()
      var names: Seq[String]             = Seq()
      for (i <- 1 to graphData._2.length-1){
        names = names :+ graphData._2(i)(0)._1
        lines = lines :+ graphData._2(i).drop(1).toMap 
      }
      lineData = Some(info, (names zip lines).toMap)
    }
    
    /* histoData consists of two maps. First is for settings and their values, second is purely for values and their keys */
    if (!graphData._2(0).isEmpty){
      histoData = Some( (Map("desc" -> graphData._1(1)(0), "color" -> graphData._1(1)(1), "x_name" -> graphData._1(1)(2), 
                        "y_name" -> graphData._1(1)(3),"show_frequencies" -> graphData._1(1)(4)), (graphData._2(0).toMap) ) )
    }
    /* pieData consists of three maps. First is for settings and their values, second is for values and keys, and 
  	 * third for colors and keys */
    if (!graphData._3.isEmpty){ // Before entering any indexes, we have to be sure that whole list exist
      if (!graphData._3(0).isEmpty){
        val triple = graphData._3(0).unzip3
        pieData = Some( (Map("desc" -> graphData._1(3)(0), "show_percentage_in_graph" -> graphData._1(3)(1),"show_data_values" -> graphData._1(3)(2),
                    "show_explanation_panel" -> graphData._1(3)(3), "data_key_name" -> graphData._1(3)(4),
                    "data_value_name" -> graphData._1(3)(5)), (triple._1 zip triple._2).toMap, (triple._1 zip triple._3).toMap ) )
      }
    }
  }
  
  /* Initial values of searching structures */
  updateGraphMaps()
  
  /* Accessing methods */
  def sourceFile = source
  def getInfoData  = infoData
  def getGridData  = gridData
  def getLineData  = lineData
  def getHistoData = histoData
  def getPieData   = pieData
  
  /* Delivering update request from graph forward to IO */
  def update(graph: Graph) = DatafileIO.updateData(graph, source)
  
  /* Updating an entity on one of the searching structures
   * @RETURNS true if the update was successful, otherwise false */
  def update(entity: String, dataType: String, newValue: String, block: Boolean, lineName: String = "Line 1"): Boolean ={
    
    /* Handy helper method to list all line diagram names */
    def lineNames: List[String] ={
      var names = List[String]()
      for (i <- graphData._2){
        if (!i.isEmpty){
          names = names :+ i(0)._1
        } else {
          names = names :+ ""
        }
      }
      names
    }
    
    if (block){ // Handling information blocks
      var graphData1 = graphData._1.toBuffer
      var index = 0
      
      /* Assigning new values to dataset, according to dataType */
      dataType match {
        case "line"  => index = 2
        case "histo" => index = 1
        case "pie"   => index = 3
        case "grid"  => index = 0
        case "info"  => index = -1
        case _       => // Unknown datatype
      }
      
      if (index == -1){ // Index is the only entity which doesn't follow the seq procedure
        indexData(dataType)(entity) match {
            case 0 => name = newValue
            case 1 => desc = newValue
        }
      } else if (index == 2 && entity == "data_names"){ // Another special case: assigning new name for line 
        val index = lineNames.indexOf(lineName)
        if (index == -1) return false
        
        /* Changing first entity with new name and replacing it in the original array */
        var upd = graphData._2.toBuffer
        var nev = upd(index).drop(1)
        nev = (newValue,"") +: nev
        upd(index) = nev
        
        /* Calling updaters here and exiting because we're inside the info segment */
        graphData = (graphData._1,upd.toSeq,graphData._3)
        updateGraphMaps()
        return true
        
      } else {
        var lineData = graphData1(index).toBuffer
        lineData( indexData(dataType)(entity) ) = newValue
        graphData1(index) = lineData.toSeq
      }
      
      if (dataType != "info") graphData = (graphData1.toSeq,graphData._2,graphData._3)
    } else {  // Handling measurements block
      var graphData1 = Buffer[Any]()
      if (dataType == "pie"){ graphData1 = graphData._3.toBuffer } else {
        graphData1 = graphData._2.toBuffer }
      
      /* Assigning new values to dataset, according to dataType */
      dataType match {
        case "line"  =>{
          /* Determining which entity must be changed and changing it */
          val index = lineNames.indexOf(lineName)
          if (index == -1) return false
          var lineData = graphData1(index).asInstanceOf[Seq[(String, String)]].toBuffer
          lineData( lineData.map( x => x._1).indexOf(entity) ) = (entity, newValue)
          graphData1(index) = lineData.toSeq  }
        case "histo" =>{
          var lineData = graphData1(0).asInstanceOf[Seq[(String, String)]].toBuffer
          lineData( lineData.map( x => x._1).indexOf(entity) ) = (entity, newValue)
          graphData1(1) = lineData.toSeq  }
        case "pie"   =>{
          var lineData = graphData1(0).asInstanceOf[Seq[(String, String, String)]].toBuffer
          
          /* Determining which values is going to be changed (number or color) */
          val index = lineData.map( x => x._1).indexOf(entity)
          if (newValue startsWith "#"){
            lineData( index ) = (entity, lineData(index)._2, newValue)
          } else {
            lineData( index ) = (entity, newValue , lineData(index)._3)
          }
          graphData1(0) = lineData.toSeq  }
      }
      
      /* Updating values to main variable */
      if (dataType == "pie") graphData = (graphData._1, graphData._2, graphData1.asInstanceOf[Seq[Seq[(String, String, String)]]].toSeq)
        else graphData = (graphData._1,graphData1.asInstanceOf[Seq[Seq[(String, String)]]].toSeq,graphData._3) 
    }
    /* Updating information to searching structures */
    updateGraphMaps()
    return true
  }
  
  override def toString = "Dataset ("+name+","+desc+")"
}