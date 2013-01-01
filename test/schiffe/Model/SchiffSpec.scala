package schiffe.Model
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class SchiffSpec extends SpecificationWithJUnit{
  "A new Schiff" should{
    var feld = new Feld(5)
    var zellen = feld.zellen
    var startZelle = zellen(0)(0)
    var schiff = new Schiff(1, startZelle, zellen)
    
    "Cell Should be set when set (Richtung: oben)" in{
      schiff.setzen(0,4)
      var isSet = zellen(0)(0).getGesetzt
      isSet must be_==(true)
      
      
    }
    
   
    "Cell should be set when set (Richtung: unten)" in{
      schiff.setzen(1,4)
      var isSet = zellen(0)(0).getGesetzt
      isSet must be_==(true)
    }
    "Cell should be set when set (Richtung: rechts)" in{
      feld.reset
      schiff.setzen(2,4)
      var isSet = zellen(0)(0).getGesetzt
      isSet must be_==(true)
    }
    "Cell should be set when set (Richtung: links)" in{
      
      feld.reset
      schiff.setzen(3,4)
      var isSet = zellen(0)(0).getGesetzt 
      isSet must be_==(true)
    }
    "Cant set ship when already set" in{
      feld.reset
      schiff.setzen(0,4)
      var isAlreadySet = schiff.setzen(0,4)
      isAlreadySet must be_==(false)
    }
    "Ship is set when set" in{
      var feldNeu = feld.reset
      
      var langSchiff = new Schiff(2, startZelle, feldNeu.zellen)
      var canSet = langSchiff.setzen(0,4)
      canSet must be_==(true)
      
    }
    "Ship is set when set (nach oben)" in{
      var startZelleUnten = zellen(4)(1)
      var untenSchiff = new Schiff(2, startZelleUnten, zellen)
      var canSet = untenSchiff.setzen(0,4)
      canSet must be_==(true)
    }
    "cant set if a ship is set nearby (nach oben)" in{
      var startZelle1 = zellen(4)(1)
      var startZelle2 = zellen(4)(2)
      var schiff1 = new Schiff(2, startZelle1, zellen)
      var schiff2 = new Schiff(2, startZelle2, zellen)
      schiff1.setzen(0,4)
      var canSet = schiff2.setzen(0,4)
      canSet must be_==(false)
    }
    "cant set if a ship is set in the cells already" in{
      var startZelle1 = zellen(4)(1)
      var startZelle2 = zellen(3)(1)
      var schiff1 = new Schiff(2, startZelle1, zellen)
      var schiff2 = new Schiff(2, startZelle2, zellen)
      schiff2.setzen(0,4)
      var canSet = schiff1.setzen(0,4)
      canSet must be_==(false)
    }
    
    //unten
    "if ship is short and cell is already set(direction:down)" in{
      var feldShort = new Feld(5)
      var startZelleShort1 = zellen(0)(0)
      var startZelleShort2 = zellen(0)(0)
      var schiff1 = new Schiff(1, startZelleShort1, feldShort.zellen)
      var schiff2 = new Schiff(1, startZelleShort2, feldShort.zellen)
      schiff1.setzen(1,4)
      var canSet = schiff2.setzen(1,4)
      canSet must be_==(false)
    }
    
    "if ship is not short and is not longer than size of feld(direction: down)" in{
      var feldNeu = new Feld(5)
      feld.reset
    var startZelleNeu = zellen(1)(1)
    var schiffNeu = new Schiff(2, startZelleNeu, feldNeu.zellen)
    var canSet = schiffNeu.setzen(1,4)
    canSet must be_==(true)
    }
    
    "if ship is not short and is longer than size of feld(direction: down)"in{}
    
    "if the cells of the ship are not already set(direction: down)"in{}
    
    "if the cells of the ship are already set(direction: down)"in{}
    
    "if there is no other ship nearby(direction: down)"in{}
    
    "if there is an other ship nearby(direction: down)"in{
      var feldNeu = new Feld(5)
      var startZelle1 = zellen(1)(1)
      var startZelle2 = zellen(1)(2)
      var schiff1 = new Schiff(2, startZelle1, feldNeu.zellen)
      var schiff2 = new Schiff(2, startZelle2, feldNeu.zellen)
      schiff1.setzen(1,4)
      var canSet = schiff2.setzen(1,4)
      canSet must be_==(false)
    }
    
    //rechts
    "if ship is short and cell is already set(direction:right)"in{}
    
    "if ship is not short and is not longer than size of feld(direction: right)"in{}
    
    "if ship is not short and is longer than size of feld(direction: right)" in{}
    
    "if the cells of the ship are not already set(direction: right)" in{}
    
    "if the cells of the ship are already set(direction: right)" in{}
    
    "if there is no other ship nearby(direction: right)" in{}
    
    "if there is an other ship nearby(direction: right)" in{}
    
    //left
    "if ship is short and cell is already set(direction:left)" in{}
    
    "if ship is not short and is not longer than size of feld(direction: left)" in{}
    
    "if ship is not short and is longer than size of feld(direction: left)" in{}
    
    "if the cells of the ship are not already set(direction: left)" in{}
    
    "if the cells of the ship are already set(direction: left)" in{}
    
    "if there is no other ship nearby(direction: left)" in{}
    
    "if there is an other ship nearby(direction: left)" in{}
    
    "if the random direction is wrong" in{}

    "if the ship is not setable" in{}
}
}