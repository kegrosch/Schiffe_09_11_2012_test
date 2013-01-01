package schiffe.Model
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class FeldSpec extends SpecificationWithJUnit{
  "A new Feld" should{
    val feld = new Feld(2)
    
    "Have 2 cells" in{
      var anzahl = feld.zellen.length
      anzahl must be_==(2)
    }
    "return a new Feld with 2 Cells when reset" in{
      var feldNeu = feld.reset
      var anzahlNeu = feldNeu.zellen.length
      anzahlNeu must be_==(2)
    }
    "all Fields must be hit when solved" in{
      var solvedFeld = feld.solve
      for(i <- 0 to 1){
      var hitCell = solvedFeld.zellen(i)(i).getGetroffen
      hitCell must be_==(true)
      }
    }
    
    
    "could not be hit if already hit" in{
      var newFeld = feld.reset
      newFeld.hit(0,0)
      var couldHit = newFeld.hit(0,0)
      couldHit must be_==(false)
    }
    
    "could be hit if not hit before" in{
      var newFeld = feld.reset
      newFeld.reset
     var couldHit = newFeld.hit(0,0)
     couldHit must be_==(true)
    }
    
    "could be hit if not hit before and a ship is set" in{
      var newFeld = feld.reset
      newFeld.reset
      newFeld.zellen(0)(0).setzen(true)
      var couldHit = newFeld.hit(0,0)
      couldHit must be_==(true)
    }
    
    "the variable hit must be set when hit" in{
      var newFeld = feld.reset
      newFeld.reset
      newFeld.hit(0,0)
      var isHit = newFeld.zellen(0)(0).getGetroffen
      isHit must be_==(true)
    }
    "must return true if Spielfertig (2)" in{
      feld.zaehlerhit=2
      var isFertig = feld.spielFertig
      isFertig must be_==(true)
    }
    "must return true if Spielfertig (5)" in{
      var middleFeld = new Feld(5)
      middleFeld.zaehlerhit=9
      var isFertig = middleFeld.spielFertig
      isFertig must be_==(true)
    }
    "must return true if Spielfertig (10)" in{
      var bigFeld = new Feld(10)
      bigFeld.zaehlerhit=30
      var isFertig = bigFeld.spielFertig
      isFertig must be_==(true)
    }
    "must return false if != Spielfertig (10)" in{
      var bigFeld = new Feld(10)
      bigFeld.zaehlerhit=29
      var isFertig = bigFeld.spielFertig
      isFertig must be_==(false)
    }
    "must return box from toString-method" in{
      var newFeld = feld.reset
      var hbox = "\n+---+---+\n|   |   |\n+---+---+\n|   |   |\n+---+---+\n"
      var feldBox = newFeld.toString()
      feldBox must beEqualTo(hbox)
    }
    "must return box with a ship from toString-method when a ship is set" in{
      var newFeld = feld.reset
      newFeld.zellen(0)(0).setzen(true)
      newFeld.zellen(1)(0).setzen(true)
      newFeld.zellen(0)(0).hit
      var hbox = "\n+---+---+\n| # |   |\n+---+---+\n| X |   |\n+---+---+\n"
      var feldBox = newFeld.toString()
      feldBox must beEqualTo(hbox)
    }
    "must return box with a ship from toString-method when a ship is set and an o when hit" in{
      var newFeld = feld.reset
      newFeld.zellen(0)(0).setzen(true)
      newFeld.zellen(1)(0).setzen(true)
      newFeld.zellen(0)(0).hit
      newFeld.zellen(0)(1).hit
      var hbox = "\n+---+---+\n| # | O |\n+---+---+\n| X |   |\n+---+---+\n"
      var feldBox = newFeld.toString()
      feldBox must beEqualTo(hbox)
    }
    "must return a empty box when a ship is set (method pcToString)" in{
      var newFeld = feld.reset
      newFeld.zellen(0)(0).setzen(true)
      newFeld.zellen(1)(0).setzen(true)
      
      var hbox = "\n+---+---+\n|   |   |\n+---+---+\n|   |   |\n+---+---+\n"
      var feldBox = newFeld.pcToString
      feldBox must beEqualTo(hbox)
    }
    "must return a box with O when hit and theres no ship (method pcToString)" in{
      var newFeld = feld.reset
      newFeld.zellen(0)(0).setzen(true)
      newFeld.zellen(1)(0).setzen(true)
      newFeld.zellen(0)(1).hit
      
      var hbox = "\n+---+---+\n|   | O |\n+---+---+\n|   |   |\n+---+---+\n"
      var feldBox = newFeld.pcToString
      feldBox must beEqualTo(hbox)
    }
    "must return a box with # when hit and theres a ship (method pcToString)" in{
      var newFeld = feld.reset
      newFeld.zellen(0)(0).setzen(true)
      newFeld.zellen(1)(0).setzen(true)
      newFeld.zellen(0)(0).hit
      
      var hbox = "\n+---+---+\n| # |   |\n+---+---+\n|   |   |\n+---+---+\n"
      var feldBox = newFeld.pcToString
      feldBox must beEqualTo(hbox)
    }
   }

}




//  "A new Ausgabe" should {
//    val ausgabe = new Ausgabe(2)
//
//    "Have value 2" in {
//      var result = ausgabe.cells.length
//      result must be_==(2)
//
//    }
//
//    "give the rigth string" in {
//      var erg = ausgabe.toString().length();
//      erg must be_==(9)
//    }
//    "the value of the cell shou be setable" in {
//      for (index <- 0 until 2) {
//        var cell = ausgabe.cells(index)
//        cell.assign("S")
//        var erg3 = cell.value
//        erg3 must be_==("S")
//
//      }
//    }
//    "the cell should make the correct string" in {
//      for (index <- 0 until 2) {
//        var cell = ausgabe.cells(index)
//        var erg2 = cell.mkString
//        var value = cell.value
//        erg2 must be_==("(" + index + ", 0) = " + value)
//
//      }
//    }
//    "the cell should be open" in {
//      for (index <- 0 until 2) {
//        var cell = ausgabe.cells(index)
//        cell.assign(" ")
//        var open = cell.isOpen
//        open must be_==(false)
//
//      }
//    }
//
//    "the cell should be closed" in {
//      for (index <- 0 until 2) {
//        var cell = ausgabe.cells(index)
//        cell.assign("S")
//        var erg3 = cell.isOpen
//        erg3 must be_==(true)
//
//      }
//    }
//    "the cell colum should be as the set column" in {
//      val cell2 = new schiffe.Cell(3, 5)
//
//      var cellY = cell2.y
//
//      cellY must be_==(5)
//
//    }
//    "the cell row should be as the set row" in {
//      val cell2 = new schiffe.Cell(3, 5)
//
//      var cellY = cell2.x
//
//      cellY must be_==(3)
//
//    }
//
//  }
//
//}