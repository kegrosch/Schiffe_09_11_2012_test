package schiffe.View
import schiffe.Controller._
import schiffe.Model.Feld
import schiffe.util._
import schiffe.Model.Schiff
import scala.swing.Reactor
import schiffe.Controller.CellChanged
import schiffe.Controller.CellChanged

class Tui(var controller: Controller, pccontroller: Controller) extends Reactor {
  listenTo(controller)
  listenTo(pccontroller)
  printTui
  reactions += {
    case e: FeldResize => printTui
    case CellChanged => printTui
  }
  def update = printTui
  def printTui = {
    println(controller.feld.toString())

  }
 
  var size = controller.feld.zellen.length
  
 println("Sie haben folgende Auswahlmöglichkeiten: Grösse dies Spielfelds verändern (2,5 oder 10 eingeben),set- Schiffe setzen, q- Spiel verlassen")
  def readInput(eingabe: String) = {
    
    var continue = true
    eingabe match {
      case "q" => continue = false
      case "s" => { println(pccontroller.feld.toString()) 
      continue = false  
      }
      case "r" => {controller.reset
      pccontroller.reset
      
      println("Die Spielfelder wurden zurückgesetzt")
      }
      case "set" =>
        size = controller.feld.zellen.length
        size match {
          case 2 =>

            var gesetzt = false
            while (gesetzt == false) {
              println("Geben Sie die das 1.te Feld Ihres Zerstörers und deren Richtung ein (Reihe,Spalte,Richtung(z.B. 1,3,1))")
              println("Richtungen: 0-Oben; 1-Unten; 2-Rechts; 3-Links")
              var pos = readLine()
              pos.toList.filter(c => c != ' ').map(c => c.toString) match {
                case row :: "," :: column :: "," :: richtung :: Nil => {
                   if(row.toInt>controller.feld.zellen.length | column.toInt >controller.feld.zellen.length | row.toInt<1| column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                  if (controller.set(2, row.toInt, column.toInt, richtung.toInt, (size - 1))) {
                    println("Zerstörer wurde bei " + row + " / " + column + " gesetzt")
                    printTui
                    gesetzt = true
                  } else {
                    println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                    printTui
                    gesetzt = false
                  }
                }
                }

                case _ => println("Falsche Eingabe - Geben Sie ZeileSpalte (bsp. 5,5,1) ein")
              }
            }
            
            //Schiffe für Computer setzen
            

            pccontroller.setcomputerschiff2
           

            println("Die Schiffe des Computers wurden gesetzt")
            println("IHR EIGENES SPIELFELD:")
            printTui
            println("DAS SPIELFELD DES COMPUTERS:")
            println(pccontroller.feld.pcToString)
           //controller.setFeldGesetzt(true)
           println("Sie haben folgende Auswahlmöglichkeiten: q- Spiel verlassen, s -Spielfeld des Computers anzeigen und Spiel verlassen, hit- schiessen, r -Feld neu setzen")

          case 5 =>
            var gesetzt = false
            while (gesetzt == false) {
              println("Geben Sie die das 1.te Feld Ihres Kreuzers und deren Richtung ein (Reihe,Spalte,Richtung(z.B. 1,3,1))")
              println("Richtungen: 0-Oben; 1-Unten; 2-Rechts; 3-Links")
              var pos = readLine()
              pos.toList.filter(c => c != ' ').map(c => c.toString) match {
                case row :: "," :: column :: "," :: richtung :: Nil => {
               if(row.toInt>controller.feld.zellen.length | column.toInt >controller.feld.zellen.length | row.toInt<1| column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                  if (controller.set(4, row.toInt, column.toInt, richtung.toInt, (size - 1))) {
                    println("Kreuzer wurde bei " + row + " / " + column + " gesetzt")
                    printTui
                    gesetzt = true
                  } else {
                    println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                    printTui
                    gesetzt = false
                  }
                   }
                }

                case _ => println("Falsche Eingabe - Geben Sie Zeile Spalte und Richtung  (bsp. 5,5,1) ein")
              }
            }
            gesetzt = false
            while (gesetzt == false) {
              println("Geben Sie die das 1.te Feld Ihres U-Boots und deren Richtung ein (Reihe/Spalte/Richtung(z.B. 1,3,1))")
              println("Richtungen: 0-Oben; 1-Unten; 2-Rechts; 3-Links")
              var pos = readLine()
              pos.toList.filter(c => c != ' ').map(c => c.toString) match {
                case row :: "," :: column :: "," :: richtung :: Nil => {
              if(row.toInt>controller.feld.zellen.length | column.toInt >controller.feld.zellen.length | row.toInt<1| column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                  if (controller.set(3, row.toInt, column.toInt, richtung.toInt, (size - 1))) {
                    println("U-Boot wurde bei " + row + " / " + column + " gesetzt")
                    printTui
                    gesetzt = true
                  } else {
                    println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                    printTui
                    gesetzt = false
                  }
                }
                }

                case _ => println("Falsche Eingabe - Geben Sie ZeileSpalte (bsp. 5,5,1) ein")
              }
            }

            
              gesetzt = false
              while (gesetzt == false) {
                println("Geben Sie die das 1.te Feld Ihres Zerstörers und deren Richtung ein (Reihe,Spalte,Richtung(z.B. 1,3,1))")
                println("Richtungen: 0-Oben; 1-Unten; 2-Rechts; 3-Links")
                var pos = readLine()

                pos.toList.filter(c => c != ' ').map(c => c.toString) match {
                  case row :: "," :: column :: "," :: richtung :: Nil => {
                 if(row.toInt>controller.feld.zellen.length | column.toInt >controller.feld.zellen.length | row.toInt<1| column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(2, row.toInt, column.toInt, richtung.toInt, (size - 1))) {
                      println("Zerstörer wurde bei " + row + " / " + column + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                  }
                  }

                  case _ => println("Falsche Eingabe - Geben Sie ZeileSpalte (bsp. 5,5,1) ein")
                }
              }
            
          pccontroller.setcomputerschiff5
          
            //controller.feldGesetzt = true
            println("Die Schiffe des Computers wurden gesetzt")
            println("IHR EIGENES SPIELFELD:")
            printTui
            println("DAS SPIELFELD DES COMPUTERS:")
            println(pccontroller.feld.pcToString)
            println("Sie haben folgende Auswahlmöglichkeiten: q- Spiel verlassen, s -Spielfeld des Computers anzeigen und Spiel verlassen, hit- schiessen, r -Feld neu setzen")
            
          case 10 =>
            
           var gesetzt = false
            while (gesetzt == false) {
              println("Geben Sie die das 1.te Feld Ihres Schlachtschiffes und deren Richtung ein (Reihe,Spalte,Richtung(z.B. 1,3,1))")
              println("Richtungen: 0-Oben; 1-Unten; 2-Rechts; 3-Links")
              var pos = readLine()
              pos.toList.filter(c => c != ' ').map(c => c.toString) match {
                case "1" :: "0" :: "," :: column :: "," :: richtung :: Nil => {
    if( column.toInt >controller.feld.zellen.length | column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                  if (controller.set(5, 10, column.toInt, richtung.toInt, (size - 1))) {
                    println("Schlachtschiff wurde bei " + 10 + " / " + column + " gesetzt")
                    printTui
                    gesetzt = true
                  } else {
                    println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                    printTui
                    gesetzt = false
                  }
                }
                }
                case row :: "," :: "1" :: "0" :: "," :: richtung :: Nil => {
 if(row.toInt>controller.feld.zellen.length |  row.toInt<1|  richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                  if (controller.set(5, row.toInt, 10, richtung.toInt, (size - 1))) {
                    println("Schlachtschiff wurde bei " + row + " / " + 10 + " gesetzt")
                    printTui
                    gesetzt = true
                  } else {
                    println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                    printTui
                    gesetzt = false
                  }
                }
                }
                case "1" :: "0" :: "," :: "1" :: "0" :: "," :: richtung :: Nil => {
              if( richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                  if (controller.set(5, 10, 10, richtung.toInt, (size - 1))) {
                    println("Schlachtschiff wurde bei " + 10 + " / " + 10 + " gesetzt")
                    printTui
                    gesetzt = true
                  } else {
                    println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                    printTui
                    gesetzt = false
                  }
                }
                }
                case row :: "," :: column :: "," :: richtung :: Nil => {
            if(row.toInt>controller.feld.zellen.length | column.toInt >controller.feld.zellen.length | row.toInt<1| column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                  if (controller.set(5, row.toInt, column.toInt, richtung.toInt, (size - 1))) {
                    println("Schlachtschiff wurde bei " + row + " / " + column + " gesetzt")
                    printTui
                    gesetzt = true
                  } else {
                    println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                    printTui
                    gesetzt = false
                  }
                }
                }

                case _ => println("Falsche Eingabe - Geben Sie ZeileSpalte (bsp. 5,5,1) ein")
              }
            }
            for (i <- 1 to 2) {
              var gesetzt = false
              while (gesetzt == false) {
                println("Geben Sie die das 1.te Feld Ihres " + i + ".ten Kreuzers und deren Richtung ein (Reihe,Spalte,Richtung(z.B. 1,3,1))")
                println("Richtungen: 0-Oben; 1-Unten; 2-Rechts; 3-Links")
                var pos = readLine()
                pos.toList.filter(c => c != ' ').map(c => c.toString) match {
                  case "1" :: "0" :: "," :: column :: "," :: richtung :: Nil => {
                   if(column.toInt >controller.feld.zellen.length | column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(4, 10, column.toInt, richtung.toInt, (size - 1))) {
                      println(i + ".ter Kreuzer wurde bei " + 10 + " / " + column + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                   }
                  }
                  case row :: "," :: "1" :: "0" :: "," :: richtung :: Nil => {
              if(row.toInt>controller.feld.zellen.length |row.toInt<1|  richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(4, row.toInt, 10, richtung.toInt, (size - 1))) {
                      println(i + ".ter Kreuzer wurde bei " + row + " / " + 10 + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                   }
                  }
                  case "1" :: "0" :: "," :: "1" :: "0" :: "," :: richtung :: Nil => {
                  if(richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(4, 10, 10, richtung.toInt, (size - 1))) {
                      println(i + ".ter Kreuzer wurde bei " + 10 + " / " + 10 + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                   }
                  }
                  case row :: "," :: column :: "," :: richtung :: Nil => {
                 if(row.toInt>controller.feld.zellen.length | column.toInt >controller.feld.zellen.length | row.toInt<1| column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(4, row.toInt, column.toInt, richtung.toInt, (size - 1))) {
                      println(i + ".ter Kreuzer wurde bei " + row + " / " + column + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                   }
                  }

                  case _ => println("Falsche Eingabe - Geben Sie ZeileSpalte (bsp. 5,5,1) ein")
                }
              }
            }
            for (i <- 1 to 3) {
              var gesetzt = false
              while (gesetzt == false) {
                println("Geben Sie die das 1.te Feld Ihres " + i + ".ten U-Boots und deren Richtung ein (Reihe,Spalte,Richtung(z.B. 1,3,1))")
                println("Richtungen: 0-Oben; 1-Unten; 2-Rechts; 3-Links")
                var pos = readLine()
                pos.toList.filter(c => c != ' ').map(c => c.toString) match {
                  case "1" :: "0" :: "," :: column :: "," :: richtung :: Nil => {
                    if(column.toInt >controller.feld.zellen.length |  column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(3, 10, column.toInt, richtung.toInt, (size - 1))) {
                      println(i + ".tes U-Boot wurde bei " + 10 + " / " + column + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                  }
                  }
                  case row :: "," :: "1" :: "0" :: "," :: richtung :: Nil => {
                 if(row.toInt>controller.feld.zellen.length | row.toInt<1|  richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(3, row.toInt, 10, richtung.toInt, (size - 1))) {
                      println(i + ".tes U-Boot wurde bei " + row + " / " + 10 + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                  }
                  }
                  case "1" :: "0" :: "," :: "1" :: "0" :: "," :: richtung :: Nil => {
                    if( richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(3, 10, 10, richtung.toInt, (size - 1))) {
                      println(i + ".tes U-Boot wurde bei " + 10 + " / " + 10 + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                   }
                  }
                  case row :: "," :: column :: "," :: richtung :: Nil => {
                  if(row.toInt>controller.feld.zellen.length | column.toInt >controller.feld.zellen.length | row.toInt<1| column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(3, row.toInt, column.toInt, richtung.toInt, (size - 1))) {
                      println(i + ".tes U-Boot wurde bei " + row + " / " + column + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                  }
                  }

                  case _ => println("Falsche Eingabe - Geben Sie ZeileSpalte (bsp. 5,5,1) ein")
                }
              }
            }
            for (i <- 1 to 4) {
              var gesetzt = false
              while (gesetzt == false) {
                println("Geben Sie die das 1.te Feld Ihres " + i + ".ten Zerstörers und deren Richtung ein (Reihe,Spalte,Richtung(z.B. 1,3,1))")
                println("Richtungen: 0-Oben; 1-Unten; 2-Rechts; 3-Links")
                var pos = readLine()

                pos.toList.filter(c => c != ' ').map(c => c.toString) match {
                  case "1" :: "0" :: "," :: column :: "," :: richtung :: Nil => {
                  if( column.toInt >controller.feld.zellen.length |  column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(2, 10, column.toInt, richtung.toInt, (size - 1))) {
                      println(i + ".ter Zerstörer wurde bei " + 10 + " / " + column + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                   }
                  }
                  case row :: "," :: "1" :: "0" :: "," :: richtung :: Nil => {
                   if(row.toInt>controller.feld.zellen.length | row.toInt<1|  richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(2, row.toInt, 10, richtung.toInt, (size - 1))) {
                      println(i + ".ter Zerstörer wurde bei " + row.toInt + " / " + 10 + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                   }
                  }
                  case "1" :: "0" :: "," :: "1" :: "0" :: "," :: richtung :: Nil => {
                    if(richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(2, 10, 10, richtung.toInt, (size - 1))) {
                      println(i + ".ter Zerstörer wurde bei " + 10 + " / " + 10 + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                   }
                  }
                  case row :: "," :: column :: "," :: richtung :: Nil => {
  if(row.toInt>controller.feld.zellen.length | column.toInt >controller.feld.zellen.length | row.toInt<1| column.toInt<1 | richtung.toInt<0 |richtung.toInt>3){
                    println("falsche Eingabe. Bitte Koordinate im gültigen Bereich (Reihen/Spalten 1 bis Spielfeldgrösse und Richtung 0-3)")
                  }
                   else{
                    if (controller.set(2, row.toInt, column.toInt, richtung.toInt, (size - 1))) {
                      println(i + ".ter Zerstörer wurde bei " + row + " / " + column + " gesetzt")
                      printTui
                      gesetzt = true
                    } else {
                      println("Falsche eingabe der Position (es darf im umfeld von bereits gesetzten Schiffen kein weiteres gesetzt werden")
                      printTui
                      gesetzt = false
                    }
                   }
                  }

                  case _ => println("Falsche Eingabe - Geben Sie ZeileSpalte (bsp. 5,5,1) ein")
                }
              }
            }

           
            //Schiffe für Computer setzen
         
            pccontroller.setcomputerschiff10
            println("Die Schiffe des Computers wurden gesetzt")
            println("IHR EIGENES SPIELFELD:")
            printTui
            println("DAS SPIELFELD DES COMPUTERS:")
            println(pccontroller.feld.pcToString)
            //controller.setFeldGesetzt(true)
            println("Sie haben folgende Auswahlmöglichkeiten: q- Spiel verlassen, s -Spielfeld des Computers anzeigen und Spiel verlassen, hit- schiessen, r -Feld neu setzen") 
        }

      case "10" => if(controller.getFeldGesetzt()==false){controller.setSize(10); pccontroller.setSize(10)} else println("Sie haben die Zellen schon gesetzt. Sie dürfen das Spielfeld nicht ändern")
      case "5" => if(controller.getFeldGesetzt()==false) {controller.setSize(5); pccontroller.setSize(5)} else println("Sie haben die Zellen schon gesetzt. Sie dürfen das Spielfeld nicht ändern")
      case "2" =>if(controller.getFeldGesetzt()==false) {controller.setSize(2); pccontroller.setSize(2)} else println("Sie haben die Zellen schon gesetzt. Sie dürfen das Spielfeld nicht ändern")
      case "hit" => {
        var quit = false
        if(controller.getFeldGesetzt()==false){
          println("Sie müssen zunächst Ihre Schiffe setzen um das Spiel starten zu können (set)")
          
        } else{
        while(controller.spielfertig == false & pccontroller.spielfertig == false & quit == false){
          
       
          println("Geben Sie die Zelle ein (Reihe, Spalte) um Zelle zu setzen, s um das Spielfeld des Computers zu lösen und q um das Spiel zu beenden")
          var pos = readLine()
              pos.toList.filter(c => c != ' ').map(c => c.toString) match {
                case reihe :: "," :: spalte :: Nil => {
                  if(reihe.toInt>controller.feld.zellen.length | spalte.toInt >controller.feld.zellen.length | spalte.toInt<1| reihe.toInt<1 ){
                    println("falsche Eingabe. Bitte Koordinate der Zelle im gültigen Bereich (1 bis Spielfeldgrösse eingeben")
                  }
                  else {
                  pccontroller.hit(reihe.toInt, spalte.toInt)
                  if(pccontroller.spielfertig == false){
                  var pcHit = false
                  while (pcHit == false){
                    
                  var startReihe = scala.util.Random.nextInt(controller.feld.zellen.length) + 1
                  var startSpalte = scala.util.Random.nextInt(controller.feld.zellen.length) + 1
                  if(controller.feld.zellen(startReihe-1)(startSpalte-1).getGetroffen == false){
                  controller.hit(startReihe,startSpalte)
                    pcHit = true
                  
                  }else{
                    pcHit = false
                  }
                   
                  
                  }
                  }
                  println("DAS SPIELFELD DES COMPUTERS:")
            println(pccontroller.feld.pcToString)
             println("IHR EIGENES SPIELFELD:")
            printTui
                  }
                }
                
               case "s" ::Nil =>{
                println(pccontroller.feld.toString())
                quit = true
                  }
               case "q" ::Nil => quit = true
               case _ => println("Sie haben eine Falsche Eingabe gemacht")
          }
        }
    }
       
        if(controller.spielfertig ==true){
           println("SPIEL BEENDET!")
          println("Der Computer hat gewonnen!")
          
        }else{
          if (quit==true){
             println("SPIEL BEENDET!")
            println("Sie haben das Spiel abgebrochen")
          }
          else
            if (pccontroller.spielfertig==true){
          println("SPIEL BEENDET!")
          println("SIE haben gewonnen!") 
            }
        }
      }
      case _ => //Controller.falseInput
    }
    continue
  }

}