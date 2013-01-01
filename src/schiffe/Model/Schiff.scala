package schiffe.Model

class Schiff(var laenge: Int, var startZelle: Zelle, var zellen: Array[Array[Zelle]]) {

  def setzen(richtung: Int, groesse: Int): Boolean = {
    var direction = richtung
    var gesetzt = false
    var zaehler = 0;
    while (gesetzt == false) {

      direction match {
        //nach oben
        case 0 =>
          if (laenge == 1) {
            if (schiffLaengeKurz == true) {
              return true
            } else {
              return false
            }

          } else {
            if ((((startZelle.reihe) - laenge) + 1) >= 0) {

              if (schiffKontrolleOben == true) {
                if (umfeldKontrolle(groesse, direction) == true) {
                  for (i <- 0 to laenge - 1) {
                    zellen((startZelle.reihe - i))(startZelle.spalte).setzen(true)

                  }
                  return true
                } else {
                  gesetzt = false
                  zaehler = zaehler + 1
                }
              } else {
                gesetzt = false
                zaehler = zaehler + 1
              }

            } else {
              gesetzt = false
              zaehler = zaehler + 1
            }
          }
        //nach unten  
        case 1 =>
          if (laenge == 1) {
            if (schiffLaengeKurz == true) {
              return true
            } else {
              return false
            }
          } else {
            if ((((startZelle.reihe) + laenge) - 1) <= groesse) {

              if (schiffKontrolleUnten == true) {

                if (umfeldKontrolle(groesse, direction) == true) {

                  for (i <- 0 to (laenge - 1)) {
                    zellen((startZelle.reihe + i))(startZelle.spalte).setzen(true)

                  }
                  return true
                } else {
                  gesetzt = false
                  zaehler = zaehler + 1
                }
              } else {
                gesetzt = false
                zaehler = zaehler + 1
              }

            } else {
              gesetzt = false
              zaehler = zaehler + 1
            }
          }

        //nach rechts
        case 2 =>
          if (laenge == 1) {
            if (schiffLaengeKurz == true) {
              return true
            } else {
              return false
            }
          } else {
            if ((((startZelle.spalte) + laenge) - 1) <= groesse) {

              if (schiffKontrolleRechts == true) {
                if (umfeldKontrolle(groesse, direction) == true) {

                  for (i <- 0 to laenge - 1) {
                    zellen((startZelle.reihe))(startZelle.spalte + i).setzen(true)

                  }
                  return true
                } else {
                  gesetzt = false
                  zaehler = zaehler + 1
                }
              } else {
                gesetzt = false
                zaehler = zaehler + 1
              }

            } else {
              gesetzt = false
              zaehler = zaehler + 1
            }
          }

        //nach links
        case 3 =>
          if (laenge == 1) {
            if (schiffLaengeKurz == true) {
              return true
            } else {
              return false
            }
          } else {
            if ((((startZelle.spalte) - laenge) + 1) >= 0) {
              if (schiffKontrolleLinks == true) {
                if (umfeldKontrolle(groesse, direction) == true) {
                  for (i <- 0 to laenge - 1) {
                    zellen((startZelle.reihe))(startZelle.spalte - i).setzen(true)

                  }
                  return true
                } else {
                  gesetzt = false
                  zaehler = zaehler + 1
                }
              } else {
                gesetzt = false
                zaehler = zaehler + 1
              }

            } else {
              gesetzt = false
              zaehler = zaehler + 1
            }
          }

        //Falsche Zufallszahl  
        case _ =>
          direction = scala.util.Random.nextInt(3 - 0) + 0
          gesetzt = false
      }
      if (gesetzt == false) {
        if (zaehler >= 50) {
          return false
        }
        direction = scala.util.Random.nextInt(3 - 0) + 0
      }

    }
    return true
  }

  def schiffLaengeKurz: Boolean = {
    if (zellen(startZelle.reihe)(startZelle.spalte).getGesetzt == true) {
      return false
    } else {
      zellen(startZelle.reihe)(startZelle.spalte).setzen(true)
      return true
    }

  }

  def schiffKontrolleOben: Boolean = {
    var frei = true
    for (i <- 0 to laenge - 1) {

      if (zellen((startZelle.reihe - i))(startZelle.spalte).getGesetzt == true) {
        frei = false
        return frei
      } else {

        frei = true

      }

    }
    return frei

  }
  def schiffKontrolleUnten: Boolean = {
    var frei = true
    for (i <- 0 to (laenge - 1)) {
      if (zellen((startZelle.reihe + i))(startZelle.spalte).getGesetzt == true) {
        frei = false
        return frei
      } else {

        frei = true
      }

    }
    return frei
  }
  def schiffKontrolleRechts: Boolean = {
    var frei = true
    for (i <- 0 to laenge - 1) {

      if (zellen((startZelle.reihe))(startZelle.spalte + i).getGesetzt == true) {
        frei = false
        return frei
      } else {

        frei = true
      }

    }
    return frei
  }
  def schiffKontrolleLinks: Boolean = {
    var frei = true
    for (i <- 0 to laenge - 1) {

      if (zellen((startZelle.reihe))(startZelle.spalte - i).getGesetzt == true) {
        frei = false
        return frei
      } else {

        frei = true
      }

    }
    return frei
  }
  def umfeldKontrolle(groesse: Int, richtung: Int): Boolean = {
    var okay = true

    startZelle.getReihe match {
      case 0 => //Reihe = erste Reihe
        startZelle.getSpalte match {
          case 0 => //Spalte = erste Spalte & Reihe = erste Reihe
            richtung match {
              case 1 => //Schiff geht nach unten
                if(groesse == (startZelle.getReihe+laenge-1)){
                  for (i <- 0 to laenge-1) {
                  if ((zellen(startZelle.getReihe + i)(startZelle.getSpalte + 1).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }
                }
                if (((zellen(startZelle.getReihe + laenge-1)(startZelle.getSpalte).getGesetzt) == true)) {
                  return false
                } else {
                  if ((zellen(startZelle.getReihe + laenge-1)(startZelle.getSpalte + 1).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }
                }
                }else{
                for (i <- 0 to laenge) { //30.12.2012 - geändert: "-1" Hinzugefügt
                  if ((zellen(startZelle.getReihe + i)(startZelle.getSpalte + 1).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }
                }
                if (((zellen(startZelle.getReihe + laenge)(startZelle.getSpalte).getGesetzt) == true)) {
                  return false
                } else {
                  if ((zellen(startZelle.getReihe + laenge)(startZelle.getSpalte + 1).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }
                }
                }
              case 2 => //Schiff geht nach rechts
                if(groesse == (startZelle.getSpalte+laenge-1)){
                  for (i <- 0 to laenge-1) {
                  if ((zellen(startZelle.getReihe + 1)(startZelle.getSpalte + i).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }

                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte + laenge-1).getGesetzt) == true)) {
                  return false
                } else {
                  if ((zellen(startZelle.getReihe + 1)(startZelle.getSpalte + laenge-1).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }
                }
                }else{
                for (i <- 0 to laenge) {
                  if ((zellen(startZelle.getReihe + 1)(startZelle.getSpalte + i).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }

                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte + laenge).getGesetzt) == true)) {
                  return false
                } else {
                  if ((zellen(startZelle.getReihe + 1)(startZelle.getSpalte + laenge).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }
                }
                }
            }
          case `groesse` => //Spalte = letzte Spalte & Reihe = erste Reihe
            richtung match {
              case 1 => //Schiff geht nach unten
                if(groesse == (startZelle.getReihe+laenge-1)){
                  for (i <- 0 to laenge-1) {
                  if ((zellen(startZelle.getReihe + i)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }
                }
                if (((zellen(startZelle.getReihe + laenge-1)(startZelle.getSpalte).getGesetzt) == true)) {
                  return false
                } else {
                  if ((zellen(startZelle.getReihe + laenge-1)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }
                }
                }else{
                for (i <- 0 to laenge) {
                  if ((zellen(startZelle.getReihe + i)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }
                }
                if (((zellen(startZelle.getReihe + laenge)(startZelle.getSpalte).getGesetzt) == true)) {
                  return false
                } else {
                  if ((zellen(startZelle.getReihe + laenge)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }
                }
                }
              case 3 => //Schiff geht nach links
                if(0 == (startZelle.getSpalte-laenge+1)){
                  for (i <- (groesse - laenge+1) to groesse) {
                  if ((zellen(startZelle.getReihe + 1)(i).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }

                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte - laenge+1).getGesetzt) == true)) {
                  return false
                } else {
                  if ((zellen(startZelle.getReihe + 1)(startZelle.getSpalte - laenge+1).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }
                }
                }else{
                for (i <- (groesse - laenge) to groesse) {
                  if ((zellen(startZelle.getReihe + 1)(i).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }

                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte - laenge).getGesetzt) == true)) {
                  return false
                } else {
                  if ((zellen(startZelle.getReihe + 1)(startZelle.getSpalte - laenge).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }
                }
                }
            }
          case _ => //Spalte != erste Spalte & Spalte != letzte Spalte & Reihe = erste Reihe
            richtung match {
              case 1 => // Schiff geht nach unten
                for (i <- 0 to laenge) {
                  if ((zellen(startZelle.getReihe + i)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    if ((zellen(startZelle.getReihe + i)(startZelle.getSpalte + 1).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }
                    okay = true
                  }
                }
                if ((zellen(startZelle.getReihe + laenge)(startZelle.getSpalte - 1).getGesetzt) == true) {
                  return false
                } else {
                  if ((zellen(startZelle.getReihe + laenge)(startZelle.getSpalte + 1).getGesetzt) == true) {
                    return false
                  } else {
                    if ((zellen(startZelle.getReihe + laenge)(startZelle.getSpalte).getGesetzt) == true) {
                      return false
                    } else {
                      return okay
                    }
                  }
                }
              case 2 => // Schiff geht nach rechts

                if (groesse == (startZelle.getSpalte + laenge - 1)) { //überprüfen ob rechts anstößt
                  //Schiff stößt rechts an
                  for (i <- (startZelle.getSpalte - 1) to groesse) {
                    if ((zellen(startZelle.getReihe + 1)(i).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }
                  }
                  if ((zellen(startZelle.getReihe)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }

                } else { //Schiff stößt nicht rechts an
                  for (i <- (startZelle.getSpalte - 1) to (startZelle.getSpalte + laenge)) {
                    if ((zellen(startZelle.getReihe + 1)(i).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }
                  }
                  if ((zellen(startZelle.getReihe)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    if ((zellen(startZelle.getReihe)(startZelle.getSpalte + 1).getGesetzt) == true) {
                      return okay
                    }
                  }
                }
              case 3 => // Schiff geht links
                if (0 == (startZelle.getSpalte - laenge + 1)) { //überprüfen ob links anstößt
                  //Schiff stößt links an
                  for (i <- (startZelle.getSpalte - 1) to groesse) {
                    if ((zellen(startZelle.getReihe + 1)(i).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }
                  }
                  if ((zellen(startZelle.getReihe)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    return okay
                  }

                } else { //Schiff stößt nicht links an
                  for (i <- (startZelle.getSpalte - 1) to (startZelle.getSpalte + laenge)) {
                    if ((zellen(startZelle.getReihe + 1)(i).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }
                  }
                  if ((zellen(startZelle.getReihe)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    if ((zellen(startZelle.getReihe)(startZelle.getSpalte + 1).getGesetzt) == true) {
                      return okay
                    }
                  }
                }

            }
        }
      case `groesse` => // Reihe = letzte Reihe
        startZelle.getSpalte match {
          case 0 => //Spalte = erste Spalte & Reihe = letzte Reihe
            richtung match {
              case 0 => //Schiff geht nach oben
                if(startZelle.getReihe - laenge < 0){
                  for (i <- (startZelle.getReihe - (laenge-1)) to groesse) {
                  if ((zellen(i)(startZelle.getSpalte + 1).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }
                }
                if (((zellen(startZelle.getReihe - (laenge-1))(startZelle.getSpalte).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
                }else{
                for (i <- (startZelle.getReihe - laenge) to groesse) {
                  if ((zellen(i)(startZelle.getSpalte + 1).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }
                }
                if (((zellen(startZelle.getReihe - laenge)(startZelle.getSpalte).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
                }
              case 2 => //Schiff geht nach rechts
                if(startZelle.getReihe + laenge > groesse){
                  for (i <- 0 to laenge-1) {
                  if ((zellen(startZelle.getReihe - 1)(startZelle.getSpalte + i).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }

                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte + laenge-1).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
                }else{
                for (i <- 0 to laenge) {
                  if ((zellen(startZelle.getReihe - 1)(startZelle.getSpalte + i).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }

                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte + laenge).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
                }
            }
          case `groesse` => //Spalte = letzte Spalte & Reihe = letzte Reihe
            richtung match {
              case 0 => //Schiff geht nach oben
                
                          
                
                if(startZelle.getReihe - laenge < 0 ){
                  return okay
                }else{
                for (i <- (startZelle.getReihe - laenge) to groesse) {
                  if ((zellen(i)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }
                }
                if (((zellen(startZelle.getReihe - laenge)(startZelle.getSpalte).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
                }
              case 3 => //Schiff geht nach links
                 if(startZelle.getSpalte - laenge < 0){
                   for (i <- (groesse - laenge+1) to groesse) {
                  if ((zellen(startZelle.getReihe - 1)(startZelle.getSpalte - i).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }

                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte - laenge+1).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
                 }else{
                for (i <- (groesse - laenge) to groesse) {
                  if ((zellen(startZelle.getReihe - 1)(startZelle.getSpalte - i).getGesetzt) == true) {
                    return false
                  } else {
                    okay = true
                  }

                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte - laenge).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
                 }
            }
          case _ => //Spalte != erste Spalte & Spalte != letzte Spalte & Reihe = letzte Reihe
            richtung match {
              case 0 => // Schiff geht nach oben
                for (i <- (groesse - laenge) to groesse) {
                  if ((zellen(i)(startZelle.getSpalte - 1).getGesetzt) == true) {
                    return false
                  } else {
                    if ((zellen(i)(startZelle.getSpalte + 1).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }

                  }
                }
                if (((zellen(startZelle.getReihe - laenge)(startZelle.getSpalte).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
              case 2 => // Schiff geht nach rechts

                if (groesse == (startZelle.getSpalte + laenge - 1)) { //überprüfen ob rechts anstößt
                  for (i <- (startZelle.getSpalte - 1) to groesse) {
                    if ((zellen(startZelle.getReihe - 1)(i).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }
                  }
                  if (zellen(startZelle.getReihe)(startZelle.getSpalte - 1).getGesetzt == true) {
                    return false
                  } else {
                    okay = true
                    return okay
                  }
                } else { //Schiff stößt nich rechts an

                  for (i <- (startZelle.getSpalte - 1) to ((startZelle.getSpalte + laenge))) {
                    if ((zellen(startZelle.getReihe - 1)(i).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }
                  }
                  if (zellen(startZelle.getReihe)(startZelle.getSpalte - 1).getGesetzt == true) {
                    return false
                  } else {
                    if (zellen(startZelle.getReihe)(startZelle.getSpalte + 1).getGesetzt == true) {
                      return false
                    } else {
                      okay = true
                      return okay
                    }

                  }
                }

              case 3 => // Schiff geht links
              //überprüfen ob links anstößt
              if (0 == (startZelle.getSpalte - laenge + 1)) { //überprüfen ob links anstößt
                for(i <- 0 to (startZelle.getSpalte+1)){
                 
                  if((zellen(startZelle.getReihe-1)(i).getGesetzt)==true){
                    return false
                  }else{
                    okay = true
                  }
                }
                if((zellen(startZelle.getReihe)(startZelle.getSpalte+1).getGesetzt)==true){
                  return false
                }else{
                  return okay
                }
              }else{//stößt nicht links an
                for(i <- (startZelle.getSpalte-laenge) to (startZelle.getSpalte+1)){
                  if((zellen(startZelle.getReihe-1)(i).getGesetzt)==true){
                    return false
                  }else{
                    okay = true
                  }
                }
                if((zellen(startZelle.getReihe)(startZelle.getSpalte-1).getGesetzt)==true){
                  return false
                }else{
                  if((zellen(startZelle.getReihe)(startZelle.getSpalte+1).getGesetzt)==true){
                    return false
                  }else{
                    return okay
                  }
                }
                
              }
            }
        }
      case _ => // Reihe != erste Reihe & Reihe != letzte Reihe
        startZelle.getSpalte match {
          case 0 => //Spalte = erste Spalte & Reihe != erste Reihe & Reihe != letzte Reihe
            richtung match {
              case 0 => //Schiff geht nach oben
                //überprüfen ob oben anstößt
                if(0 == (startZelle.getReihe-laenge+1)){
                  for(i <- 0 to (startZelle.getReihe+1)){
                    if((zellen(i)(startZelle.getSpalte+1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                    }
                  }
                  if((zellen(startZelle.getReihe+1)(startZelle.getSpalte).getGesetzt) == true){
                    return false
                  }else{
                    return okay
                  }
                }else{//stößt nicht oben an
                  for(i <- (startZelle.getReihe-laenge) to (startZelle.getReihe+1)){
                    if((zellen(i)(startZelle.getSpalte+1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                    }
                  }
                  if((zellen(startZelle.getReihe+1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    if((zellen(startZelle.getReihe-1)(startZelle.getSpalte).getGesetzt)==true){
                      return false
                      
                    }else{
                      return okay
                    }
                  }
                  
                }

              case 1 => // Schiff geht nach unten
              //überprüfen ob unten anstößt
                if( groesse == (startZelle.getReihe+laenge-1)){
                  for(i <- (startZelle.getReihe-1) to groesse){
                    if((zellen(i)(startZelle.getSpalte+1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                    }
                  }
                  if((zellen(startZelle.getReihe-1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    return okay
                  }
                }else{//stößt nicht unten an
                  for(i <- (startZelle.getReihe-1) to (startZelle.getReihe+laenge)){
                    if((zellen(i)(startZelle.getSpalte+1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                    }
                  }
                  if((zellen(startZelle.getReihe-1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    if((zellen(startZelle.getReihe+1)(startZelle.getSpalte).getGesetzt)==true){
                      return false
                    }else{
                      return okay
                    }
                  }
                }

              case 2 => //Schiff geht nach rechts
                 for (i <- 0 to (startZelle.getSpalte + laenge)) {
                  if ((zellen(startZelle.getReihe-1)(i).getGesetzt) == true) {
                    return false
                  } else {
                    if ((zellen(startZelle.getReihe+1)(i).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }

                  }
                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte+laenge).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
                
            }
          case `groesse` => //Spalte = letzte Spalte & Reihe != erste Reihe & Reihe != letzte Reihe
            richtung match {
              case 0 => //Schiff geht nach oben
              // überprüfen ob oben anstößt
                if(0 == (startZelle.getReihe-laenge+1)){
                  for(i <- 0 to (startZelle.getReihe+1)){
                    if((zellen(i)(startZelle.getSpalte-1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                    }
                  }
                  if((zellen(startZelle.getReihe+1)(startZelle.getSpalte).getGesetzt) == true){
                    return false
                  }else{
                    return okay
                  }
                }else{//stößt nicht oben an
                  for(i <- (startZelle.getReihe-laenge) to (startZelle.getReihe+1)){
                    if((zellen(i)(startZelle.getSpalte-1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                    }
                  }
                  if((zellen(startZelle.getReihe+1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    if((zellen(startZelle.getReihe-1)(startZelle.getSpalte).getGesetzt)==true){
                      return false
                      
                    }else{
                      return okay
                    }
                  }
                  
                }

              case 1 => // Schiff geht nach unten
              //überprüfen ob unten anstößt
                 if( groesse == (startZelle.getReihe+laenge-1)){
                  for(i <- (startZelle.getReihe-1) to groesse){
                    if((zellen(i)(startZelle.getSpalte-1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                    }
                  }
                  if((zellen(startZelle.getReihe-1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    return okay
                  }
                }else{//stößt nicht unten an
                  for(i <- (startZelle.getReihe-1) to (startZelle.getReihe+laenge)){
                    if((zellen(i)(startZelle.getSpalte-1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                    }
                  }
                  if((zellen(startZelle.getReihe-1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    if((zellen(startZelle.getReihe+1)(startZelle.getSpalte).getGesetzt)==true){
                      return false
                    }else{
                      return okay
                    }
                  }
                }

              case 3 => //Schiff geht nach links
                for (i <- (groesse - laenge) to groesse) {
                  if ((zellen(startZelle.getReihe-1)(i).getGesetzt) == true) {
                    return false
                  } else {
                    if ((zellen(startZelle.getReihe+1)(i).getGesetzt) == true) {
                      return false
                    } else {
                      okay = true
                    }

                  }
                }
                if (((zellen(startZelle.getReihe)(startZelle.getSpalte-laenge).getGesetzt) == true)) {
                  return false
                } else {

                  return okay

                }
                
            }
          case _ => //Spalte != erste Spalte & Spalte != letzte Spalte & Reihe != erste Reihe & Reihe != letzte Reihe
            richtung match {
              case 0 => // Schiff geht nach oben
              //überprüfen ob oben anstößt
                if(0 == (startZelle.getReihe-laenge+1)){
                  for(i <- 0 to (startZelle.getReihe+1)){
                    if((zellen(i)(startZelle.getSpalte-1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                      if((zellen(i)(startZelle.getSpalte+1).getGesetzt)==true){
                        return false
                      }else{
                        okay = true
                      }
                    }
                  }
                  if((zellen(startZelle.getReihe+1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    return okay
                  }
                  
                }else{//stößt nicht oben an
                  for(i <- (startZelle.getReihe-laenge) to (startZelle.getReihe+1)){
                    if((zellen(i)(startZelle.getSpalte-1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                      if((zellen(i)(startZelle.getSpalte+1).getGesetzt)==true){
                        return false
                      }else{
                        okay = true
                      }
                    }
                  }
                  if((zellen(startZelle.getReihe-1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    if((zellen(startZelle.getReihe+1)(startZelle.getSpalte).getGesetzt)==true){
                      return false
                    }else{
                      return okay
                    
                    }
                  }
                }

              case 1 => // SChiff geht nach unten
              //überprüfen ob unten anstößt
                if(groesse == (startZelle.getReihe+laenge-1)){
                  for(i <- (startZelle.getReihe-1) to groesse){
                    if((zellen(i)(startZelle.getSpalte-1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                      if((zellen(i)(startZelle.getSpalte+1).getGesetzt)==true){
                        return false
                      }else{
                        okay = true
                      }
                    }
                  }
                  
                  if((zellen(startZelle.getReihe-1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    return okay
                  }
                  
                }else{//stößt nicht unten an
                  for(i <- (startZelle.getReihe-1) to (startZelle.getReihe+laenge)){
                    if((zellen(i)(startZelle.getSpalte-1).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                      if((zellen(i)(startZelle.getSpalte+1).getGesetzt)==true){
                        return false
                      }else{
                        okay = true
                      }
                    }
                  }
                  if((zellen(startZelle.getReihe-1)(startZelle.getSpalte).getGesetzt)==true){
                    return false
                  }else{
                    if((zellen(startZelle.getReihe+1)(startZelle.getSpalte).getGesetzt)==true){
                      return false
                    }else{
                      return okay
                    }
                  }
                }

              case 2 => // Schiff geht nach rechts
              //überprüfen ob rechts anstößt
                if(groesse ==(startZelle.getSpalte+laenge-1)){
                  for(i <- (startZelle.getSpalte-1) to groesse){
                    if((zellen(startZelle.getReihe-1)(i).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                      if((zellen(startZelle.getReihe+1)(i).getGesetzt)==true){
                        return false
                      }else{
                        okay = true
                      }
                    }
                  }
                  if((zellen(startZelle.getReihe)(startZelle.getSpalte-1).getGesetzt)==true){
                    return false
                  }else{
                    return okay
                    
                  }
                }else{//stößt nicht rechts an
                  for(i <- (startZelle.getSpalte-1) to (startZelle.getSpalte+laenge)){
                    if((zellen(startZelle.getReihe-1)(i).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                      if((zellen(startZelle.getReihe+1)(i).getGesetzt)==true){
                        return false
                      }else{
                        okay = true
                      }
                    }
                  }
                  if((zellen(startZelle.getReihe)(startZelle.getSpalte-1).getGesetzt)==true){
                    return false
                  }else{
                    okay = true
                    if((zellen(startZelle.getReihe)(startZelle.getSpalte+laenge).getGesetzt)==true){
                      return false
                    }else{
                      return okay
                    }
                    
                  }
                }

              case 3 => // Schiff geht links
              //überprüfen ob links anstößt
                if(0 ==(startZelle.getSpalte-laenge+1)){
                  for(i <- 0 to (startZelle.getSpalte+1)){
                    if((zellen(startZelle.getReihe-1)(i).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                      if((zellen(startZelle.getReihe+1)(i).getGesetzt)==true){
                        return false
                      }else{
                        okay = true
                      }
                    }
                  }
                  if((zellen(startZelle.getReihe)(startZelle.getSpalte+1).getGesetzt)==true){
                    return false
                  }else{
                    return okay
                    
                  }
                }else{//stößt nicht links an
                  for(i <- (startZelle.getSpalte-laenge) to (startZelle.getSpalte+1)){
                    if((zellen(startZelle.getReihe-1)(i).getGesetzt)==true){
                      return false
                    }else{
                      okay = true
                      if((zellen(startZelle.getReihe+1)(i).getGesetzt)==true){
                        return false
                      }else{
                        okay = true
                      }
                    }
                  }
                  if((zellen(startZelle.getReihe)(startZelle.getSpalte+1).getGesetzt)==true){
                    return false
                  }else{
                    okay = true
                    if((zellen(startZelle.getReihe)(startZelle.getSpalte-laenge).getGesetzt)==true){
                      return false
                    }else{
                      return okay
                    }
                    
                  }
                }
            }
        }
    }

    return false
  }


}