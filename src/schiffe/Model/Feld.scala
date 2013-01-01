package schiffe.Model

import scala.math.sqrt
import scala.util.Random

class Feld(anzahl: Int) {
  require(List(2, 5, 10).contains(anzahl))
  var zellen = Array.ofDim[Zelle](anzahl, anzahl)
  
  var anzahlZellen = anzahl*anzahl-1
  var spielfertig = false
  var zaehlerhit =0 

 //Zellen generieren
 
  for(i <- 0 to anzahl-1){
    for(j <- 0 to anzahl-1){
      zellen(i)(j) = new Zelle(i,j)
      
    }
  }

def reset: Feld = new Feld(anzahl); 
def solve: Feld = {
for(i <- 0 to anzahl-1){
    for(j <- 0 to anzahl-1){
      zellen(i)(j).hit
      
    }
  }
return this
}
val size = zellen.size.toInt


def hit(reihe: Int, spalte: Int):Boolean = {

  if(zellen(reihe)(spalte).getGetroffen==false){
    zellen(reihe)(spalte).hit
    if(zellen(reihe)(spalte).gesetzt==true )
      zaehlerhit = zaehlerhit +1 

    return true
  }
  else
    return false
}

def spielFertig: Boolean = {
  if(anzahl==2){
    if(zaehlerhit==2){
      spielfertig= true
    return spielfertig
    }
  } 
    
  
  if(anzahl==5){
    if(zaehlerhit==9){
    spielfertig= true
    return spielfertig  
  }
    
  }
  if(anzahl==10){ 
    if(zaehlerhit==30){
     spielfertig= true
    return spielfertig 
  }
    
  }
    return false
}
def cell (r:Int, c:Int)=zellen(r)(c)
  override def toString = {
    val lineseparator = ("+-" + ("--" * (anzahl/2))) *anzahl + "+\n"
    val line = ("|" + (" " * (anzahl/2))+("x"+(" " * (anzahl/2) ))) * anzahl + "|\n"
    var box = "\n" + (lineseparator + (line)) * anzahl + lineseparator
    for (reihe <- 0 to anzahl-1) {
      for(spalte <- 0 to anzahl-1){
         if(zellen(reihe)(spalte).getGetroffen == true){
          if(zellen(reihe)(spalte).getGesetzt == true){
            (box = box.replaceFirst("x", "getroffen"))

          }else{
            (box = box.replaceFirst("x", "keinTreffer"))

          }
        }else{

      (box = box.replaceFirst("x", zellen(reihe)(spalte).gesetzt.toString()))
      (box = box.replaceFirst("false", " "))
      (box = box.replaceFirst("true", "X"))
      

        }
    }
    }
    (box = box.replaceAll("getroffen", "#"))
      (box = box.replaceAll("keinTreffer", "O"))
    box
  }
  def pcToString = {
    val lineseparator = ("+-" + ("--" * (anzahl/2))) *anzahl + "+\n"
    val line = ("|" + (" " * (anzahl/2))+("x"+(" " * (anzahl/2) ))) * anzahl + "|\n"
    var box = "\n" + (lineseparator + (line)) * anzahl + lineseparator
    for (reihe <- 0 to anzahl-1) {
      for(spalte <- 0 to anzahl-1){

        if(zellen(reihe)(spalte).getGetroffen == true){
          if(zellen(reihe)(spalte).getGesetzt == true){
            (box = box.replaceFirst("x", zellen(reihe)(spalte).gesetzt.toString()))
      (box = box.replaceFirst("false", " "))
      (box = box.replaceFirst("true", "#"))
          }else{
            (box = box.replaceFirst("x", zellen(reihe)(spalte).gesetzt.toString()))
      (box = box.replaceFirst("false", "O"))
      (box = box.replaceFirst("true", "O"))
          }
        }else{
      (box = box.replaceFirst("x", zellen(reihe)(spalte).gesetzt.toString()))
      (box = box.replaceFirst("false", " "))
      (box = box.replaceFirst("true", " "))
        }
    }
    }
    box
  }
}