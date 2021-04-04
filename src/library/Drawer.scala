package library

import java.awt._
import math._

/* Object Drawer is used to draw different kind of graphs into given [Graphics2D] buffers. Object can be called from
 * graph's own methods, or directly. All methods are private except the general one (drawGraph), which will assign the 
 * tasks to private methods according to graph types. To draw a graph, generally three parameters are needed: drawing board
 * of type java.awt.Graphics2D, graph object of type [Graph], and dimensions of graphing board in type java.awt.Dimension.
 * @RETURNS nothing, will draw directly to given buffer                                                                   */
object Drawer {
  
  /* Shuffling color array so that we get different set of colors every time. Used to draw line diagrams */
  val colors = Array(Color.red,Color.green,Color.blue,Color.yellow,Color.black,Color.orange,Color.gray,Color.cyan,Color.magenta,Color.pink)
  java.util.Collections.shuffle(java.util.Arrays.asList(colors:_*))
  
  /* Draws shape of given graph to current canvas. Currently the method supports only
   * 4 shapes(line,histo,pie,grid), but this method can be extended to support almost anything
   * that can be drawn in two dimensions                                   */
  def drawGraph(g: Graphics2D, obj: Graph, dim: Dimension)(companion: Graph = obj, clear: Boolean = true): Unit ={
    
    if (clear) g.clearRect(0, 0, dim.width, dim.height)
    
    /* Determine graph type and assign task to submethod */
    obj match {
      case i: Line      => drawLine(g, obj.asInstanceOf[Line], dim)
      case j: Histogram => drawHisto(g, obj.asInstanceOf[Histogram], dim)
      case k: Pie       => drawPie(g, obj.asInstanceOf[Pie], dim)
      case l: Grid      => drawGrid(g, obj.asInstanceOf[Grid], companion, dim)
      case _            =>  // Unknown graph type
    }
  }
  
  /* Private method to draw line diagram */
  private def drawLine(g: Graphics2D, data: Line, dim: Dimension) ={
    val origin = (200, dim.height-50)
    
    val names = lineNames(data)
    val data_ = data.data._2.toList.map( x => x._2.toList )
    val mappedData = data_.map( x => x.map( y => (y._1.toDouble, y._2.toDouble) ) )
    
    /* Calculating smallest and biggest value of all line diagrams in order to build axis */
    var small = (0.0,0.0, true) // x.min, y.min
    var big  = (0.0,0.0, true) // x.max, y.max
    for (k <- mappedData){
      val x = k.map(_._1)
      val y = k.map(_._2)
      if (small._3 == true || big._3 == true){ // First time iterating
        small = (x.min,y.min, false)
        big   = (x.max,y.max, false)
      } else {                                  // Multiple line diagrams
        if (small._1 > x.min) small = (x.min,small._2,false)
        if (small._2 > y.min) small = (small._1,y.min,false)
        if (big._1 < x.max)   big   = (x.max, big._2, false)
        if (big._2 < y.max)   big   = (big._1,y.max,  false)
      }
    }
    
    /* Y axis & it's styling */
    g.setFont( new Font("Ariel", Font.PLAIN, 15) )
    g.drawLine(origin._1,origin._2,origin._1,100)
    g.fillPolygon(Array(origin._1-5,origin._1+5,origin._1), Array(100,100,90), 3)
    var metrics = g.getFontMetrics
    
    /* Size between two guidelines */
    var deltaPixels_y = (origin._2-100)/12
    var deltaValue_y  = (big._2-small._2)/9
    val ratio = deltaPixels_y / deltaValue_y
    
    /* Making sure that scale is pretty & readable */
    val roundConst = roundPrecision(deltaValue_y)
    deltaValue_y = roundAt(roundConst)(deltaValue_y)
    deltaPixels_y = (deltaValue_y * ratio).toInt
    
    /* Near the origin bottom will be set to 0 */
    var smallest_y = small._2
    smallest_y -= deltaValue_y
    if ((smallest_y - deltaValue_y) < 0){
      smallest_y = 0
    } else {
      smallest_y = roundAt(roundConst)(smallest_y)
    }
    
    /* Determine whether to print decimals or integers */
    var roundMode = false  // true: Double, false: Int
    if (deltaValue_y.isValidInt){ roundMode = false } else { roundMode = true }
    
    /* Draw guidelines and their values. Making sure to draw enough guidelines so that all values are covered */
    var curText: Double = 0
    var i = 0
    while (curText < big._2+deltaValue_y){
      val curY = origin._2 -i*deltaPixels_y
      curText = smallest_y+i*deltaValue_y
      val text = if(roundMode) roundAt(2)(curText).toString else curText.toInt.toString
      g.drawLine(origin._1,curY,origin._1-8,curY)
      g.drawString(text,origin._1-metrics.charsWidth(text.toArray,0,text.length)-15,curY+metrics.getHeight/3)
      i += 1
    }
    
    /* Y axis unit and it's name */
    g.setFont( new Font("Ariel", Font.BOLD, 15) )
    metrics = g.getFontMetrics
    g.drawString(data.y_unit,origin._1-metrics.charsWidth(data.y_unit.toArray, 0, data.y_unit.length)-15,100)
    g.drawString(data.y_name,origin._1-metrics.charsWidth(data.y_name.toArray, 0, data.y_name.length)/2,60)
    
    /* X axis & it's styling */
    val xAxisEnd = dim.width-dim.width/4
    g.setFont( new Font("Ariel", Font.PLAIN, 15) )
    g.drawLine(origin._1,origin._2,xAxisEnd,origin._2)
    g.fillPolygon(Array(xAxisEnd,xAxisEnd,xAxisEnd+10), Array(origin._2-5,origin._2+5,origin._2), 3)
    metrics = g.getFontMetrics
    
    /* Determine the amount of guidelines */
    var guidelines = 0
    if (data_(0).map(_._1).length < 21){ guidelines = data_(0).map(_._1).length+1 } else { guidelines = 21 }
    
    /* Size between two guidelines */
    var deltaPixels_x = (xAxisEnd-origin._1)/guidelines
    var deltaValue_x  = (big._1-small._1)/(guidelines-2)
    val ratio_ = deltaPixels_x / deltaValue_x
    
    /* Making sure that scale is pretty & readable */
    val roundConst_ = roundPrecision(deltaValue_x)
    deltaValue_x = roundAt(roundConst_)(deltaValue_x)
    deltaPixels_x = (deltaValue_x * ratio_).toInt
    
    var smallest_x = small._1
    smallest_x -= deltaValue_x
    smallest_x = roundAt(roundConst_)(smallest_x)
    
    /* Determine whether to print decimals or integers */
    var roundMode_ = false  // true: Double, false: Int
    if (deltaValue_x.isValidInt){ roundMode_ = false } else { roundMode_ = true }
    
    /* Draw guidelines and their values. Making sure to draw enough guidelines so that all values are covered */
    var curText_ : Double = 0
    var j = 0
    while (curText_ < big._1){
      val curX = origin._1 +j*deltaPixels_x
      curText_ = smallest_x+j*deltaValue_x
      val text = if(roundMode_) roundAt(2)(curText_).toString else curText_.toInt.toString
      g.drawLine(curX,origin._2,curX,origin._2+8)
      g.drawString(text,curX-metrics.charsWidth(text.toArray, 0, text.length)/2,origin._2+(metrics.getHeight*1.5).toInt)
      j += 1
    }
    
    /* X axis unit and it's name */
    g.setFont( new Font("Ariel", Font.BOLD, 15) )
    metrics = g.getFontMetrics
    g.drawString(data.x_unit,xAxisEnd-metrics.charsWidth(data.x_unit.toArray, 0, data.x_unit.length)/2, origin._2+(metrics.getHeight*1.5).toInt)
    g.drawString(data.x_name,xAxisEnd+50,origin._2+metrics.getHeight/3)
    
    /* Plotting every line at the same time */
    var plotData: Seq[Array[(Double,Double)]] = Seq()
    for (i <- names.indices){
      plotData = plotData :+ data_(i).toArray.map(x =>(x._1.toDouble,x._2.toDouble)).sortBy( x => x._1 )
    }    
    val plotOrigin = (smallest_x, smallest_y)
    
    /* Conversion rates, pixels/value */
    val pixelsPerValueUnitX = deltaPixels_x / deltaValue_x 
    val pixelsPerValueUnitY = deltaPixels_y / deltaValue_y 
    
    /* Calculating all points to an array */
    var plots: Seq[Array[(Int,Int)]] = Seq()
    var temp: Array[(Int,Int)] = Array()
    for (j <- plotData){
      temp = Array()
      for (k <- j){
        temp = temp :+ (origin._1+((k._1-plotOrigin._1)*pixelsPerValueUnitX).toInt,origin._2-((k._2-plotOrigin._2)*pixelsPerValueUnitY).toInt)
      }
      plots = plots :+ temp
    }
    
    /* Plotting points and connecting them with lines */
    for (v <- plots.indices){
      for (b <- plots(v).indices){
        g.setColor(Color.black)
        g.fillOval(plots(v)(b)._1-3, plots(v)(b)._2-3, 6, 6)
        g.setColor( colors(v) )
        g.setStroke( new BasicStroke(2) )
        if (b != 0 && b != plots(v).length){
          g.drawLine(plots(v)(b-1)._1, plots(v)(b-1)._2, plots(v)(b)._1, plots(v)(b)._2) 
        }
      }
    }
    
    /* Draw explanation panel, if selected so */
    if (data.show_explanation_panel == 1){
        val headerOrigin = (dim.width-(dim.width/4)+10,dim.height/4)
        val boxSizes = (30,30)
        var y = headerOrigin._2
        
        /* Box & text for each entity */
        for (i <- names.indices){
          g.setColor( colors(i) )
          g.fillRect(headerOrigin._1,y,boxSizes._1,boxSizes._2)
          g.setColor(Color.black)
          g.drawString(names(i),headerOrigin._1+(boxSizes._1*1.5).toInt,(y+boxSizes._2*0.7).toInt)
          
          y += (boxSizes._2*1.5).toInt
        }
    }
  }
  
  /* Private method to draw histogram */
  private def drawHisto(g: Graphics2D, data: Histogram, dim: Dimension) ={
    val origin = (200, dim.height-50)
    
    /* Y axis & it's styling */
    g.setFont( new Font("Ariel", Font.PLAIN, 15) )
    g.drawLine(origin._1,origin._2,origin._1,100)
    g.fillPolygon(Array(origin._1-5,origin._1+5,origin._1), Array(100,100,90), 3)
    var metrics = g.getFontMetrics
    
    val data_ = data.data._2.toArray.sortBy( x => x._1 )
    val mapped_y = data_.map( x => x._2.toDouble )
    var smallest_y = mapped_y.min
    val biggest_y  = mapped_y.max
    val difference_y = biggest_y-smallest_y
    
    /* Size between two guidelines */
    var deltaPixels_y: Double = (origin._2-100)/12
    var deltaValue_y : Double  = difference_y/9
    val ratio = deltaPixels_y / deltaValue_y
    
    /* Making sure that scale is pretty & readable */
    val roundConst = roundPrecision(deltaValue_y)
    deltaValue_y = roundAt(roundConst)(deltaValue_y)
    deltaPixels_y = (deltaValue_y * ratio)
    
    /* Near the origin bottom will be set to 0 */
    smallest_y -= deltaValue_y
    if ((smallest_y - deltaValue_y) < 0){
      smallest_y = 0
    } else {
      smallest_y = roundAt(roundConst)(smallest_y).toInt
    }
    
    /* Draw guidelines and their values */
    for (i <- 0 to 11){
      val curY = (origin._2 -i*deltaPixels_y).toInt
      val curText = roundAt(2)(smallest_y+i*deltaValue_y).toInt.toString
      g.drawLine(origin._1,curY,origin._1-8,curY)
      g.drawString(curText,origin._1-metrics.charsWidth(curText.toArray,0,curText.length)-15,curY+metrics.getHeight/3)
    }
    
    /* Y axis name */
    var yName = data.y_name
    g.setFont( new Font("Ariel", Font.BOLD, 15) )
    metrics = g.getFontMetrics
    g.drawString(yName,origin._1-metrics.charsWidth(yName.toArray, 0, yName.length)/2,60)
    
    /* X axis & it's styling */
    val xAxisEnd = dim.width-dim.width/4
    g.setFont( new Font("Ariel", Font.PLAIN, 15) )
    g.drawLine(origin._1,origin._2,xAxisEnd,origin._2)
    g.fillPolygon(Array(xAxisEnd,xAxisEnd,xAxisEnd+10), Array(origin._2-5,origin._2+5,origin._2), 3)
    metrics = g.getFontMetrics
    
    var guidelines, smallest_x, biggest_x, difference_x = 0
    var stringMode = false
    if (isNumber(data_.map(x => x._1).toList(0))) stringMode = false else stringMode = true
    
    if (stringMode == false){
      val mapped_x = data_.map( x => x._1.toInt ).toList
      smallest_x = mapped_x.min
      biggest_x  = mapped_x.max
      difference_x = biggest_x - smallest_x
    }
    
    /* Determine the amount of guidelines */
    if (data_.length < 21){ guidelines = data_.length+1 } else { guidelines = 21 }
    
    /* Size between two guidelines */
    val deltaPixels_x = (xAxisEnd-origin._1)/guidelines
    val deltaValue_x  = if (stringMode) deltaPixels_x else difference_x /(guidelines-2)
    
    /* Calculating rectangle lower corners */
    var corners: Array[Int] = Array()
    for (i <- 0 to (guidelines-1)){ corners = corners :+ (origin._1 +i*deltaPixels_x) }
    
    /* Draw guidelines and their values */
    for (i <- 1 to (guidelines-1)){
      val curX = (origin._1 +i*deltaPixels_x) - deltaPixels_x/2
      var curText = ""
      if (stringMode){
        curText = data_(i-1)._1
      } else {
        curText = (smallest_x+(i-1)*deltaValue_x).toInt.toString
      }
      g.drawLine(curX,origin._2,curX,origin._2+8)
      g.drawString(curText,curX-metrics.charsWidth(curText.toArray, 0, curText.length)/2,origin._2+(metrics.getHeight*1.5).toInt)
    }
    
    /* X axis unit and it's name */
    var xName = data.x_name
    g.setFont( new Font("Ariel", Font.BOLD, 15) )
    metrics = g.getFontMetrics
    g.drawString(xName,xAxisEnd+50,origin._2+metrics.getHeight/3)
    
    /* Starting to draw rectangles */
    val plotData = data_.toArray.sortBy( x => x._1 )
    val plotOrigin = (smallest_x, smallest_y)
    
    /* Conversion rates, pixels/value */
    val pixelsPerValueUnitX = deltaPixels_x / deltaValue_x 
    val pixelsPerValueUnitY = deltaPixels_y / deltaValue_y 
    
    /* Drawing each individual rectangle */
    for (j <- plotData.indices){
      val width = deltaPixels_x
      val height = ((plotData(j)._2.toInt-smallest_y)*pixelsPerValueUnitY).toInt 
      
      /* Filled rectangle */
      g.setColor(Color.decode( data.color ))
      g.fillRect(corners(j),origin._2-height,width,height)
      
      /* Border to separate different rectangles */
      g.setColor(Color.black)
      g.drawRect(corners(j),origin._2-height,width,height)
      
      /* Value in the center of rectangle, if wanted */
      if (data.show_frequencies == 1){
        val value = plotData(j)._2
        if (metrics.getHeight < height){
          g.drawString(value,corners(j)+width/2-metrics.charsWidth(value.toArray, 0, value.length)/2,origin._2-height/2+metrics.getHeight/3)
        }
      }
    }
  }
  
  /* Private method to draw pie diagram */
  private def drawPie(g: Graphics2D, data: Pie, dim: Dimension){
    
    /* General settings for pie diagram */
    val origin = (dim.width/2-200,dim.height/2+20)
    val entities = data.data._2.toList.sortBy(x=>x._1)
    val degreesPerUnit = 360.0 / data.data._2.values.map(_.toInt).sum
    
    var curAng = 0
    val radius = 200
    g.setFont( new Font("Ariel", Font.BOLD, 20) )
    val metrics = g.getFontMetrics
    
    /* Draw each pie separately. First calculate change in angle, then get the color for piece and draw it */
    for (i <- entities.indices){
      
      /* Calculating needed angle. Making sure that whole circle is filled */
      var deltaAng = (entities(i)._2.toInt * degreesPerUnit).round.toInt
      if (i == entities.length-1 && curAng+deltaAng != 360) deltaAng = 360-curAng
      
      /* Draw the area of piece and color it */
      g.setColor( Color.decode( data.data._3(entities(i)._1) ) )
      g.fillArc(origin._1-radius,origin._2-radius,2*radius,2*radius,curAng,deltaAng)
      
      /* Draw percents, if selected so */
      if (data.show_percentage_in_graph == 1){
        /* Playing with font metrics for perfect alignment */
        val prc = ((deltaAng/360.0)*100).round
        if (prc >= 3){ // Drawing only when there's space for text
          val prc_text = prc.toString +"%"
          val width = metrics.charsWidth(prc_text.toArray, 0, prc_text.length)
          g.setColor(Color.black)
          g.drawString(prc_text,origin._1+(radius*0.85*cos(toRadians(curAng+deltaAng/2))-width/2).toInt,
                    origin._2-(radius*0.85*sin(toRadians(curAng+deltaAng/2))-metrics.getHeight/3).toInt)
        }
      }
      curAng += deltaAng
    }
    
    /* Draw explanation panel, if selected so */
    if (data.show_explanation_panel == 1){
        val headerOrigin = (dim.width-(dim.width/3)-(dim.width/20),dim.height/3)
        val boxSizes = (30,30)
        var y = headerOrigin._2
        
        val key =   data.data_key_name
        val value = data.data_value_name
        val key_x = headerOrigin._1+(boxSizes._2*2).toInt
        val value_x = key_x + metrics.charsWidth(key.toArray, 0, key.length)/2 + 20 + metrics.charsWidth(value.toArray, 0, value.length)/2
        
        /* Box & text for each entity */
        for (i <- entities.indices){
          g.setColor(Color.decode( data.data._3(entities(i)._1) ))
          g.fillRect(headerOrigin._1,y,boxSizes._1,boxSizes._2)
          g.setColor(Color.black)
          g.drawString(entities(i)._1,key_x,(y+boxSizes._2*0.8).toInt)
          
          /* Drawing data values after keys, if selected so */
          if (data.show_data_values == 1){
            g.drawString(entities(i)._2,value_x,(y+boxSizes._2*0.8).toInt)
          }
          y += (boxSizes._2*1.5).toInt
        }
        
        /* Data key & value names to the top of the explanation panel */
        g.drawString(key, key_x+5 - metrics.charsWidth(key.toArray, 0, key.length)/2, headerOrigin._2-20)
        if (data.show_data_values == 1) g.drawString(value,value_x+5 - metrics.charsWidth(value.toArray, 0, value.length)/2,headerOrigin._2-20)
    }
  }
  
  /* Private method to draw grid for line diagram and histogram */
  private def drawGrid(g: Graphics2D, data: Grid, companion: Graph, dim: Dimension) ={
    
    /* Same settings with line&histogram axes */
    val origin = (200, dim.height-50)
    val xAxisEnd = dim.width-dim.width/4
    g.setColor( Color.decode("#a19d94") )
    
    /* Drawing dashed lines */
    val dashed = new BasicStroke(1, BasicStroke.CAP_BUTT, BasicStroke.JOIN_BEVEL, 0, Array[Float]{1}, 0);
    g.setStroke(dashed)
    
    /* Determine for which graph the grid will be drawn */
    companion match {
      case i: Line        => drawForLine()
      case j: Histogram   => drawForHistogram()
      case _              => // No grid for this graph type
    }
    
    /* Draws grid for line diagram */
    def drawForLine(){
      var yCounter = origin._2 - data.size
      while (yCounter > 100){
        g.drawLine(origin._1,yCounter,xAxisEnd,yCounter)
        yCounter -= data.size
      }
      var xCounter = origin._1 + data.size
      while (xCounter < xAxisEnd){
        g.drawLine(xCounter,origin._2,xCounter,100)
        xCounter += data.size
      }
      /* Top and right */
      g.drawLine(xAxisEnd,origin._2,xAxisEnd,100)
      g.drawLine(origin._1,100,xAxisEnd,100)
    }
    
    /* Draws grid for histogram */
    def drawForHistogram(){
      var yCounter = origin._2 - data.size
      while (yCounter > 100){
        g.drawLine(origin._1,yCounter,xAxisEnd,yCounter)
        yCounter -= data.size
      }
      /* Top and right */
      g.drawLine(xAxisEnd,origin._2,xAxisEnd,100)
      g.drawLine(origin._1,100,xAxisEnd,100)
      
      /* Graph repainting */
      g.setColor( Color.black )
      drawGraph(g, companion, dim)(companion, false)
    }
  }
  
  /* Draws default setup for the canvas */
  def drawDefault(text: String, g: Graphics2D, dim: Dimension) ={
    
    /* Background */
    g.setColor(Color.white)
    g.fillRect(0,0,dim.getWidth.toInt,dim.getHeight.toInt)
    g.setColor(Color.black)
    
    /* Text with center alignment */
    g.setFont( new Font("Ariel", Font.BOLD, 50) )
    val width = g.getFontMetrics.charsWidth(text.toArray,0,text.length)
    g.drawString(text,(dim.width-width)/2,dim.height/2)
    
  }
  
  /* A helper method to check if a string is a number or not */
  private def isNumber(s: String): Boolean ={
    try { s.toInt; s.toDouble
      return true
    } catch {
      case a: java.lang.NumberFormatException =>
        try {
          s.toDouble
          return true
        } catch {
          case b: java.lang.NumberFormatException => {
            return false
          }
        }
    }
  }
  
  /* A helper method to get name of all line diagrams in an array */
  private def lineNames(i: Line): Array[String] = i.data._2.keys.toArray
  
  /* A helper method to convert decimals to given accuracy */
  private def roundAt(p: Int)(n: Double): Double = { val s = math pow (10, p); (math round n * s) / s }
  
  /* A helper method used to calculate the needed accuracy for roundAt - method */
  private def roundPrecision(n: Double): Int ={
      if        (n >= 1) { return 1-n.toInt.toString.length    // Values abs(n) > 1 are relatively easy to calculate
      } else if (n <= -1){ return 1-(n.toInt.toString.length-1)
      } else { // Decimal accuracy
        var temp = n.toString
        if (temp contains "E"){ // Smaller than 10^-4
          val ind = temp.indexOf("E")
          return temp(ind+2).toInt - 48 // ASCII Conversion
        } else { // Bigger than 10^-4
          val dot = temp.indexOf(".")
          val ind = temp.indexWhere(_ != '0', dot+1)
          return ind-dot
        }
      }
    }
}