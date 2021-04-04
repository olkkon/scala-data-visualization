package library

import java.awt.{Graphics2D, Dimension}

/* Sealed trait Graph consists of all information related to different graphs. All graphs are subclasses of this
 * trait. Only graph types, which are declared inside this file, are going to be used in the program. If we want
 * later declare new graph types, they have to be coded to this file.  */
sealed trait Graph {
  val setData: Dataset
  val dataType: String
  def desc: String
  
  def draw(g: Graphics2D, dim: Dimension, companion: Graph = this, clear: Boolean = true): Unit = Drawer.drawGraph(g, this, dim)(companion,clear)
  def updateEntities() : Unit = setData.update(this)
  override def toString: String
}

/* Class for Line diagram */
class Line(val setData: Dataset) extends Graph {
  val dataType = "line"
  def data : (Map[String,String],Map[String, Map[String, String]]) = setData.getLineData.get
  
  /* Accessing methods */
  def desc                   = data._1("desc")
  def x_unit                 = data._1("x_unit")
  def x_name                 = data._1("x_name")
  def y_unit                 = data._1("y_unit")
  def y_name                 = data._1("y_name")
  def show_explanation_panel = data._1("show_explanation_panel").toInt
  def data_names             = data._2.keys.toList
  
  /* Updates entity to Dataset
   * @RETURNS false, if the update wasn't successful, and true otherwise */
  def updateEntity(s: String, newv: String, line: String = "Line 1"): Boolean ={    
    if ((data._1.keys exists (_ ==s)) || (s == "data_names")){
      setData.update(s, dataType, newv, true, line)
      true
    } else if (data._2.map(_._2).toList(0).map(_._1).toList exists (_ == s)){
      setData.update(s, dataType, newv, false, line)
      true
    } else { false }
  }
  override def toString = "Line diagram which is part of " + setData
}

/* Class for Histogram */
class Histogram(val setData: Dataset) extends Graph {
  val dataType = "histo"
  def data : (Map[String,String],Map[String, String]) = setData.getHistoData.get
  
  /* Accessing methods */
  def desc             = data._1("desc")
  def color            = data._1("color")
  def x_name           = data._1("x_name")
  def y_name           = data._1("y_name")
  def show_frequencies = data._1("show_frequencies").toInt
  
  /* Updates entity to Dataset
   * @RETURNS false, if the update wasn't successful, and true otherwise */
  def updateEntity(s: String, newv: String): Boolean ={
    if (data._1.keys exists (_ ==s)){
      setData.update(s, dataType, newv, true)
      true
    } else if (data._2.keys exists (_ == s)){
      setData.update(s, dataType, newv, false)
      true
    } else { false }
  }
  override def toString = "Histogram which is part of " + setData
}

/* Class for Pie diagram */
class Pie(val setData: Dataset) extends Graph {
  val dataType = "pie"
  def data : (Map[String,String],Map[String, String], Map[String, String]) = setData.getPieData.get
  
  /* Accessing methods */
  def desc                     = data._1("desc")
  def show_percentage_in_graph = data._1("show_percentage_in_graph").toInt
  def show_data_values         = data._1("show_data_values").toInt
  def show_explanation_panel   = data._1("show_explanation_panel").toInt
  def data_key_name            = data._1("data_key_name")
  def data_value_name          = data._1("data_value_name")
  
  /* Updates entity to Dataset
   * @RETURNS false, if the update wasn't successful, and true otherwise */
  def updateEntity(s: String, newv: String): Boolean ={
    if (data._1.keys exists (_ ==s)){
      setData.update(s, dataType, newv, true)
      true
    } else if (data._2.keys exists (_ == s)){
      setData.update(s, dataType, newv, false)
      true
    } else if (data._3.keys exists (_ == s)){
      setData.update(s, dataType, newv, false)
      true
    } else { false }
  }
  override def toString = "Pie diagram which is part of " + setData
}

/* Class for Grid */
class Grid(val setData: Dataset) extends Graph {
  val dataType = "grid"
  def data : Map[String, Int] = setData.getGridData
  
  /* Accessing methods */
  def desc   = toString
  def size   = data("size")
  def status = data("status")
  
  /* Updates entity to Dataset
   * @RETURNS false, if the update wasn't successful, and true otherwise */
  def updateEntity(s: String, newv: String): Boolean ={
    if (data.keys exists (_ == s)){
      setData.update(s, dataType, newv, true)
      true
    } else { false }
  }
  def toggle(c: Boolean) = updateEntity("status",if(c) "1" else "0")
  override def toString = "Grid which is part of " + setData
}


