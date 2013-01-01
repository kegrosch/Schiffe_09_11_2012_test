package schiffe.Model

class Zelle (row: Int, col: Int){
  
  var gesetzt= false
  var reihe = row
  var spalte = col
  var getroffen = false
  
 def setzen (inhalt: Boolean){
  gesetzt = inhalt
  
  }
def hit = {getroffen = true
}
 
  def getReihe: Int = reihe
  def getSpalte: Int = spalte
  def getGesetzt: Boolean = gesetzt
  def getGetroffen: Boolean = getroffen
  

}