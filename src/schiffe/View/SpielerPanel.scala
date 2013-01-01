package schiffe.View
import scala.swing._
import scala.swing.event._
import schiffe.Controller.Controller
import schiffe.Model.Zelle
import schiffe.Controller.CellChanged
import scala.swing.Button
import schiffe.View.SchiffPanel
import schiffe.View.SchiffPanel

class SpielerPanel(controller: Controller, size: Int, schiffPanel: SchiffPanel) extends GridPanel(size, size) {
  //  listenTo(controller)
  listenTo(schiffPanel)
  var schiffGesetzt = false
  var spielSize = size
  var schiffLaenge = 0
  var startButtonGesetzt = false
  var richtungButtonGesetzt = false
  var startReihe = 0
  var startSpalte = 0
  var richtung = 0
  
  val InitialfarbeSpieler = new Color(200, 200, 255)
  val Schiffgesetzt = new Color(192, 255, 192)
  val Schiffgetroffen = new Color(190, 245, 170)
  val SchiffNichtgetroffen = new Color(150, 160, 162)
  //  var alleButtons = setAlleButtonSize(size)
  var alleButtons = Array.ofDim[Button](size, size)
  reactions += {
    case e: SetSchiff => {
      schiffLaenge = e.laenge
      println("HGHGHGHGGH")
    }
  }

  def getZelle(reihe: Int, spalte: Int): Zelle = {
    //    println("reihe: "+ reihe)
    //    println("spalte: "+ spalte)
    return controller.cell(reihe, spalte)
  }
  var reihe = 0
  var spalte = 0

  def button(a: Int, b: Int) = new Button {
    reihe = a
    spalte = b
    preferredSize = new Dimension(60, 60)
    //    println("I-VOR: "+ a)
    //        println("J-VOR: " + b)
    background = java.awt.Color.GRAY

    reactions += {
      case CellChanged =>
        //        redraw
        //        repaint
        //        println("I: "+ a)
        //        println("J: " + b)
        //        println("Reihe: "+ reihe)
        //        println("Spalte: " + spalte)
        //        background = if(getZelle(a,b).getGesetzt == true) java.awt.Color.GREEN else java.awt.Color.RED
        ////        contents.clear()
        //        createButtons

        setBackground

      case ButtonClicked(buttons) =>

        //        println("A: " + a)
        //        println("B: " + b)
        //        println("Schifflänge: " + schiffPanel.aktuelleLaenge)
        //        println("SpielSize: " + spielSize)
        if (startButtonSetzen(a, b, schiffPanel.aktuelleLaenge, spielSize) == true) {
          if (schiffGesetzt == true) {
            background = java.awt.Color.GREEN
            schiffPanel.aktuellerButton.visible_=(false)
            schiffGesetzt = false
            setBackground
          } else {
            background = java.awt.Color.RED
            preferredSize_=(new Dimension(60, 60))
            //          schiffLaenge = 0
          }
        } else {
          setBackground
        }

    }
    listenTo(controller)

  }

  def createButtons {
    //    alleButtons = setAlleButtonSize(size)
    contents.clear()
    background = java.awt.Color.BLACK

    for (m <- 0 to (alleButtons.length - 1)) {
      for (n <- 0 to (alleButtons.length - 1)) {

        //		  println("M: " + m)
        //		  println("N: " + n)
        var buttons = button(m, n)
        //
        //            if (((controller.feld.zellen(i)(j).getGesetzt == true))) {
        //              println("GESETZT")
        //              background = Schiffgesetzt
        //              preferredSize_=(new Dimension(60, 60))
        //            } else {
        //              println(" NICHT GESETZT")
        //              background = (java.awt.Color.GREEN)
        //              preferredSize_=(new Dimension(60, 60))
        //            }
        //                    reactions += {
        //                      case ButtonClicked(buttons) =>
        //                        if (startButtonSetzen(m, n, 2, spielSize) == true) {
        //                          background = java.awt.Color.RED
        //                          preferredSize_=(new Dimension(60, 60))
        //                        }
        //                    }
        //
        //          }
        alleButtons(m)(n) = buttons
        contents += buttons

      }

    }

  }
  createButtons
  def setSize(newSize: Int) = {
    alleButtons = Array.ofDim[Button](newSize, newSize)
    spielSize = newSize
    //    alleButtons = Array.ofDim[Button](newSize-1, newSize-1)
    //    size = newSize
    //    spielfeld(newSize)
    spielSize = newSize
    //    spielfeld(newSize).repaint()

  }
  def redraw = {

    contents.clear()
    alleButtons = Array.ofDim[Button](spielSize, spielSize)
    createButtons

    //    spielSize = newSize
    //    spielfeld(newSize).repaint()

    //    for (i <- 0 to (spielSize - 1)) {
    //      for (j <- 0 to (spielSize - 1)) {
    //        createButtons
    //        var buttons = button(i,j)
    //        setBackground(button(i,j), i, j)
    //        contents += buttons
    //        
    //        createButtons
    //      }
    //    }
    //        if (controller.feld.zellen(i)(j).getGesetzt == true) {
    //      println("GESETZT")
    //      buttons(i)(j).background_=(java.awt.Color.RED)
    //      buttons(i)(j).preferredSize_=(new Dimension(60, 60))
    //
    //    } else {
    //      println(" NICHT GESETZT")
    //      buttons(i)(j).background_=(java.awt.Color.GREEN)
    //      buttons(i)(j).preferredSize_=(new Dimension(60, 60))
    //    }
    //      }
    //    }
    //    
    repaint

    //    spielfeld(newSize).redraw(newSize)

    //    this.spielfeld(newSize).repaint()

  }
  def setAlleButtonSize(anzahl: Int) = {

    alleButtons = Array.ofDim[Button](anzahl, anzahl)
    createButtons

  }
  def setBackground = {
    while (alleButtons.length != controller.feld.zellen.length) {
      setAlleButtonSize(controller.feld.zellen.length)
    }
    //    println("BUTTONS: " + alleButtons.length)
    for (k <- 0 to (alleButtons.length - 1)) {
      for (l <- 0 to (alleButtons.length - 1)) {
        //        println("K: " + k)
        //        println("L: "+ l)
        if (getZelle(k, l).getGesetzt == true) {
          if (getZelle(k, l).getGetroffen == true) {
            alleButtons(k)(l).background = java.awt.Color.RED
          } else {
            alleButtons(k)(l).background = java.awt.Color.GREEN
          }
        } else {
          if (getZelle(k, l).getGetroffen == true) {
            alleButtons(k)(l).background = java.awt.Color.BLACK
          } else {
            alleButtons(k)(l).background = java.awt.Color.GRAY
          }

        }
      }
    }

  }
  def startButtonSetzen(reihe: Int, spalte: Int, laenge: Int, groesse: Int): Boolean = {

    if (startButtonGesetzt == true) {
      richtungButtonGesetzt = true

      if (reihe < startReihe - 1 || reihe > startReihe + 1) {
        richtungButtonGesetzt = false
        return false
      } else {
        if (spalte < startSpalte - 1 || spalte > startSpalte + 1) {
            richtungButtonGesetzt = false
            return false
          } else {
        if (spalte == startSpalte) {

          if (reihe == startReihe) {
            return false
          } else {
            if (reihe > startReihe) { //schiff geht nach unten
              richtung = 1
            } else { //schiff geht nach oben
              richtung = 0
            }
          }

        } else {
          
            if (reihe == startReihe) {

              if (spalte > startSpalte) { //schiff geht nach rechts
                richtung = 2
              } else { //schiff geht nach links
                richtung = 3
              }
            } else {
              if (spalte > startSpalte) { //schiff geht nach rechts
                richtung = 2
              } else { //schiff geht nach links
                richtung = 3
              }
            }
          }
        }
      }
      println("REIHE: " + startReihe)
      println("Spalte: "+ startSpalte)
      println("Richtung: " + richtung)
      println("Länge: " + laenge)
      println("Größe: " + groesse)
      if ((schiffeSetzen(startReihe+1, startSpalte+1, richtung, laenge, groesse)) == true) {
        return true
      } else {
        return false
      }

    } else {

      startReihe = reihe
      startSpalte = spalte
      startButtonGesetzt = true
      return true
    }

  }

  def schiffeSetzen(reihe: Int, spalte: Int, richtung: Int, laenge: Int, groesse: Int): Boolean = {
  
    if (controller.set(laenge, reihe, spalte, richtung, (groesse - 1)) == true) {

      setBackground
      richtungButtonGesetzt = false
      startButtonGesetzt = false
      schiffGesetzt = true
      return true
    } else {
      return false
    }

  }
}


//var buttons = Array.ofDim[Button](size, size)
//def myCell = controller.cell(row, column)
//
//val cellButton = new Button {
//  preferredSize_=(new Dimension(60,60))
//  opaque=true
//  background = if (myCell.getGesetzt & myCell.getGetroffen) Schiffgetroffen
//  else {
//    if(!myCell.getGesetzt & myCell.getGetroffen)
//    SchiffNichtgetroffen
//    else {
//      if(myCell.getGesetzt & !myCell.getGetroffen)
//        Schiffgesetzt
//        else InitialfarbeSpieler
//    }
//  }
//  
//  }
//  contents += cellButton
//  listenTo(cellButton)
// def redraw = {
//  
//
//    repaint
//  }
// def setBackground(p: Panel) = p.background = if (myCell.getGesetzt & myCell.getGetroffen) Schiffgetroffen
//  else if(!myCell.getGesetzt & myCell.getGetroffen)
//    SchiffNichtgetroffen
//    else if(myCell.getGesetzt & !myCell.getGetroffen)
//        Schiffgesetzt
//        else InitialfarbeSpieler
//    
//  }