package library

import scala.swing._
import scala.swing.event._
import javax.swing.{ImageIcon}
import java.awt.{Color, Font}
import scala.swing.BorderPanel.Position._
import scala.collection.mutable.Buffer

/* Main program to run the data visualization */
object Window extends SimpleSwingApplication {
  
  var currentDataset: Option[Dataset] = None
  var currentGraph:   Option[Graph]   = None
  var grid:           Option[Grid]    = None
  
  val initWidth = 1800
  val initHeight = 800
  val borderWidth = 5
  val size = new Dimension(initWidth-borderWidth, initHeight)
  
  val dataFolder = "data"
  
  /* Layout management */
  val left = new BoxPanel(Orientation.Vertical){
      contents += FileChoose
      contents += GraphChoose
      contents += GridChoose
  }
  val right = new BoxPanel(Orientation.Vertical){
      contents += GraphTitles
      contents += GraphVisual
      contents += GraphHeader
  }
  val center = new Label(){ maximumSize = new Dimension(borderWidth,initHeight); background = Color.black }
  
  val mainWindow = new MainFrame {
    title = "Data visualizer"
    resizable = false
    minimumSize   = new Dimension(initWidth,initHeight)
    maximumSize   = new Dimension(initWidth*2,initHeight*2)
    
    val components = new BoxPanel(Orientation.Horizontal){
      contents += left
      contents += center
      contents += right
    }
    contents = components
    
    /* Event listening and reacting */
    FileChoose.group.buttons.foreach ( listenTo(_) )
    GraphChoose.group.buttons.foreach( listenTo(_) )
    listenTo(GridChoose.check)
    listenTo(GridChoose.bigger.mouse.clicks)
    listenTo(GridChoose.smaller.mouse.clicks)
    reactions += {
      case ButtonClicked(button) if (button.name == "line" || button.name == "histo" || button.name == "pie") =>{
        button.name match { // Graph choose
          case "line" => currentGraph = Some(new Line(currentDataset.get));      grid = Some(new Grid(currentDataset.get))
          case "histo"=> currentGraph = Some(new Histogram(currentDataset.get)); grid = Some(new Grid(currentDataset.get))
          case "pie"  => currentGraph = Some(new Pie(currentDataset.get));       grid = None
          case _      => // Unknown graph type, some typo perhaps
        }
        GraphTitles.updateSegment()
        GraphHeader.updateSegment()
        GridChoose.updateSegment()
        GraphVisual.updateGraph()
      }
      case ButtonClicked(button) if (button.name == "check") =>{
        button.selected match { // Grid toggle
          case true  => grid.get.toggle(true)
          case false => grid.get.toggle(false)
        }
        GridChoose.updateSegment()
        GraphVisual.updateGraph()
      }
      case ButtonClicked(button) => { // File choose
        val source = dataFolder+"/"+button.name+".df"
        if (currentDataset != None){
          if (currentDataset.get.sourceFile != source) loadData()
        } else {      // Making sure that datafile isn't already in the memory
          loadData()
        }
        def loadData(){
          grid = None
          currentGraph = None
          currentDataset = Some(DatafileIO.loadData(dataFolder+"/"+button.name+".df"))
          GraphTitles.updateSegment()
          GraphChoose.updateSegment()
          GraphHeader.updateSegment()
          GridChoose.updateSegment()
          GraphVisual.updateGraph()
        }
      }
      case MouseClicked(source,_,_,_,_) if (source.name == "avail") => {
        source match { // Grid size
          case GridChoose.bigger  =>                         grid.get.updateEntity("size", (grid.get.size + 1).toString)
          case GridChoose.smaller => if (grid.get.size >= 8) grid.get.updateEntity("size", (grid.get.size - 1).toString)
        }
        GridChoose.updateSegment()
        GraphVisual.updateGraph()
      }
      case _ => // Not interested in other events
    }
  }
  
  def top = mainWindow
}



/* Panel object for file choose segment */
object FileChoose extends BorderPanel {
  
  /* Some styling */
  background = Color.white
  val size_ = new Dimension((Window.size.getWidth*0.25).toInt,(Window.size.getHeight*(1.0/3.0)).toInt)
  maximumSize = size_
  
  val files = DatafileIO.currentDirHeaders(Window.dataFolder,true)
  val header = new Label("No datafiles were found!"){ font = new Font("Ariel", Font.BOLD, 30);
                                                      preferredSize = new Dimension(size_.getWidth.toInt,(size_.getHeight/4).toInt)}
  
  /* These are here purely for that they can be accessed from the event listeners */
  val radio = new BorderPanel{ background = Color.white }
  val group = new ButtonGroup()
  
  layout( new Label(){ preferredSize = new Dimension((size_.getWidth/4).toInt,(size_.getHeight-size_.getHeight/4).toInt) } ) = West
  
  /* Two views depending whether there's datafiles available or not */
  if (!files.isEmpty){       
    var radiobuttons = Buffer[RadioButton]()
    var datasets = 0
    var buttonSize = 30
    
    /* Adjusting size according to amount of datafiles available */
    if (files.length > 4){
      if (files.length > 8){
        val first  = new Label("Showing only   "){ font = new Font("Ariel", Font.BOLD, 15) }
        val second = new Label("8 first datasets   "){ font = new Font("Ariel", Font.BOLD, 15) }
        val warning = new GridPanel(6,1){ contents ++= Array(new Label,new Label,first, second); background = Color.white }
        
        radio.layout( warning ) = East
        datasets = 7
        buttonSize = 11
      } else {
        datasets = files.length-1
        files.length match {
          case 5 => buttonSize = 20
          case 6 => buttonSize = 15
          case 7 => buttonSize = 14
          case 8 => buttonSize = 11
        }
      }
    } else {
      datasets = files.length-1
    }
    
    /* Creating individual button for each dataset */
    for (i <- 0 to datasets){ 
      radiobuttons += new RadioButton(files(i)){ name = files(i)  
                                                 font = new Font("Ariel", Font.PLAIN, buttonSize)
                                                 background = Color.white
                                                 iconTextGap = 10
    }}
    /* Creating button group */
    group.buttons ++= radiobuttons
    
    // Layout adjusting */
    header.text = "Choose dataset"
    radio.layout( new BoxPanel(Orientation.Vertical){ contents ++= radiobuttons; background = Color.white } ) = Center
    
    layout(header) = North
    layout(radio)  = Center
  } else {
    layout(header) = Center
  }
}



/* Panel object for graph choose segment */
object GraphChoose extends BorderPanel {
  
  /* Some styling */
  background = Color.white
  val size_ = new Dimension((Window.size.getWidth*0.25).toInt,(Window.size.getHeight*(1.0/3.0)).toInt)
  maximumSize = size_
  
  val header = new Label("Choose graph"){ font = new Font("Ariel", Font.BOLD, 30);
                                          preferredSize = new Dimension(size_.getWidth.toInt,(size_.getHeight/3).toInt)}
  layout(header) = North
  
  val infotext = new Label("No datafile loaded"){ font = new Font("Ariel",Font.BOLD, 25)}
  layout(infotext) = Center
  
  /* Used to keep sizes of segments identical */
  val westDump_gap   = new Label(){ preferredSize = new Dimension((size_.getWidth/4).toInt,(size_.getHeight-size_.getHeight/3).toInt) }
  val westDump_nogap = new Label(){ preferredSize = new Dimension(1,(size_.getHeight-size_.getHeight/3).toInt) }
  layout( westDump_nogap ) = West
  
  /* These are here purely for that they can be accessed from the event listeners */
  val radio = new BorderPanel
  val group = new ButtonGroup
  
  /* Creating individual button for each graph type */
  var radiobuttons = Buffer[RadioButton]()
  radiobuttons += new RadioButton("Line diagram"){ name = "line" }
  radiobuttons += new RadioButton("Histogram")   { name = "histo" }
  radiobuttons += new RadioButton("Pie diagram") { name = "pie" }
  for (i <- radiobuttons){
    i.font = new Font("Ariel", Font.PLAIN, 30)
    i.background = Color.white
    i.enabled = false
    i.iconTextGap = 10
  }
  
  radiobuttons += new RadioButton(""){ visible = false }
  group.buttons ++= radiobuttons
  
  radio.layout( new BoxPanel(Orientation.Vertical){ contents ++= radiobuttons; background = Color.white } ) = Center
  
  /* Two changeable views depending whether there's datafiles available or not */
  def updateSegment(){
    Window.currentDataset match {
      case Some(i) =>{
        /* Show buttons according to available graphs */
        if (i.getLineData != None)  radiobuttons(0).enabled = true else radiobuttons(0).enabled = false
        if (i.getHistoData != None) radiobuttons(1).enabled = true else radiobuttons(1).enabled = false
        if (i.getPieData != None)   radiobuttons(2).enabled = true else radiobuttons(2).enabled = false
        
        radiobuttons(3).selected = true
        layout(radio) = Center
        layout(westDump_gap) = West
      }
      case None    => {
        radiobuttons.foreach( _.enabled = false )
        infotext.text = "No datafile found"
        layout(infotext) = Center
        layout(westDump_nogap) = West
      }
    }    
  }
}

/* Panel object for grid choose segment */
object GridChoose extends BorderPanel {
  
  /* Some styling */
  background = Color.white
  maximumSize = new Dimension((Window.size.getWidth*0.25).toInt,(Window.size.getHeight*(1.0/3.0)).toInt)
  
  /* Loading icons for bigger & smaller buttons */
  val add_avail = new ImageIcon(Window.dataFolder+"/img/add_avail.jpg")
  val add_unav  = new ImageIcon(Window.dataFolder+"/img/add_unav.jpg")
  val red_avail = new ImageIcon(Window.dataFolder+"/img/red_avail.jpg")
  val red_unav  = new ImageIcon(Window.dataFolder+"/img/red_unav.jpg")
  
  /* Creating layout objects */
  val header =  new Label("Grid"){ font = new Font("Ariel", Font.BOLD, 30) }
  val check =   new CheckBox("ON"){ name = "check"; font = new Font("Ariel", Font.PLAIN, 20); 
                    background = Color.white; horizontalAlignment = Alignment.Center; enabled = false }
  val bigger  = new Label(){ name = "unav"; icon = add_unav; }
  val smaller = new Label(){ name = "unav"; icon = red_unav; }
  
  /* Own panel for all radio elements */
  val radio = new BorderPanel{
    background = Color.white
    layout(check) = North
    layout( new FlowPanel{ contents ++= Array(smaller,bigger); background = Color.white } ) = Center
  }
  
  layout(header) = North
  layout(radio)  = Center
  
  /* Hides grid checkbox when pie diagram is chosen or no graph is available */
  def updateSegment(){
    Window.currentGraph match {
      case None                             => check.enabled = false; check.selected = false
      case Some(i) if (i.isInstanceOf[Pie]) => check.enabled = false; check.selected = false
      case Some(i)                          => check.enabled = true
    }
    
    /* Hide grid if it should'nt be visible */
    if (Window.grid != None){
      if (Window.grid.get.status == 1 && !check.selected) Window.grid.get.toggle(false)
    }
    
    /* Show grid size buttons only if the grid is visible */
    if (check.selected){
      bigger.icon = add_avail
      smaller.icon = red_avail
      bigger.name = "avail"
      smaller.name = "avail"
    } else {
      bigger.icon = add_unav
      smaller.icon = red_unav
      bigger.name = "unav"
      smaller.name = "unav"
    }
  }
}



/* Panel object for graph title segment */
object GraphTitles extends BorderPanel {
  
  /* Some styling */
  background = Color.white
  maximumSize = new Dimension((Window.size.getWidth*0.75).toInt,(Window.size.getHeight*0.2).toInt)
  
  var titleText = "No datafile loaded"
  val title = new Label(titleText){ font = new Font("Ariel", Font.BOLD, 30) }
  var desc = new Label(""){ font = new Font("Ariel",Font.ITALIC,20); visible = false }
  val grid = new GridPanel(4,1){ contents ++= Array(new Label,title,desc); background = Color.white }
  
  layout( grid ) = Center
  
  /* Updates title and desc whenever called */
  def updateSegment(){    
    Window.currentDataset match {
      case Some(i) => {
        titleText = i.getInfoData("name")
        desc.text = i.getInfoData("desc")
        desc.visible = true
      }
      case None    => {
        titleText = "No datafile loaded"
        desc.visible = false
      }
    }
    title.text = titleText
  }
}


/* Panel object for visual graph */
object GraphVisual extends BorderPanel {
  
  /* Some styling & event handling */
  maximumSize = new Dimension((Window.size.getWidth*0.75).toInt,(Window.size.getHeight*0.6).toInt)
  
  /* All painting to canvas is done inside this object. Repainting is done always when the appropriate method is called */
  override def paintComponent(g: Graphics2D){
    /* Paint graph, if selected */
    Window.currentGraph match {
      case Some(j) => j.draw(g, new Dimension(size.getWidth.toInt,size.getHeight.toInt))
      case None    => Drawer.drawDefault("No graph selected", g, new Dimension(size.getWidth.toInt,size.getHeight.toInt))
    }
    /* Paint grid if the toggle is set on */
    if (Window.grid != None){
      val grid = Window.grid.get
      if (grid.status == 1) grid.draw(g, new Dimension(size.getWidth.toInt,size.getHeight.toInt), Window.currentGraph.get, clear = false)
    }
  }
  
  /* Delivers canvas update request forward */
  def updateGraph(){
    repaint()
  }  
}



/* Panel object for graph header segment */
object GraphHeader extends BorderPanel {
  
  /* Some styling */
  background = Color.white
  maximumSize = new Dimension((Window.size.getWidth*0.75).toInt,(Window.size.getHeight*0.2).toInt)
  
  var titleText = "No graph selected"
  val title = new Label(titleText){ font = new Font("Ariel", Font.BOLD, 30) }
  layout(title) = Center
  
  /* Updates title whenever called */
  def updateSegment(){
    Window.currentGraph match {
      case Some(i) => titleText = i.desc
      case None    => titleText = "No graph selected"
    }
    title.text = titleText
  }
}
