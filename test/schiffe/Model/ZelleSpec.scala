package schiffe.Model
import org.specs2.mutable._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner



@RunWith(classOf[JUnitRunner])
class ZelleSpec extends SpecificationWithJUnit{
  "A new Zelle" should{
    val zelle = new Zelle(0,0)
    
    "not be set" in{
      var gesetzt = zelle.getGesetzt
      gesetzt must be_==(false)
    }
    "not be hit" in{
      var getroffen = zelle.getGetroffen
      getroffen must be_==(false)
    }
    
    
    
    "set when set" in{
      zelle.setzen(true)
      var gesetzt = zelle.getGesetzt
      gesetzt must be_==(true)
    }
    "hit when hit" in{
      zelle.hit
      var getroffen = zelle.getGetroffen
      getroffen must be_==(true)
    }
    "reihe should be 0" in{
      var reihe = zelle.getReihe
      reihe must be_==(0)
    }
    "spalte should be 0" in{
      var spalte = zelle.getSpalte
     spalte must be_==(0)
    }
}
}