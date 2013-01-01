package schiffe.View
import javax.swing.ImageIcon
import scala.swing.GridPanel
import scala.swing.Button
import java.awt.Dimension
import scala.swing.Action
import scala.swing.event.ButtonClicked
import scala.swing._
import scala.swing.event.Event

case class SetSchiff(laenge: Int) extends Event
class SchiffPanel(size: Int) extends Publisher{
  var aktuelleLaenge = 0
  var aktuellerButton = new Button
  var schlachtschiff_icon = new ImageIcon("images/Schlachtschiff.png")
  var kreuzer_icon = new ImageIcon("images/Kreuzer.png")
  var uboot_icon = new ImageIcon("images/UBoot.png")
  var zerstoerer_icon = new ImageIcon("images/Zerstoerer.png")
  var z_einzeln = false
  var z = false
        var u = false
        var k = false
         var z1_groﬂ = false
        var z2_groﬂ = false
        var z3_groﬂ = false
        var z4_groﬂ = false
        var u1_groﬂ = false
        var u2_groﬂ = false
        var u3_groﬂ = false
        var k1_groﬂ = false
        var k2_groﬂ = false
        var s_groﬂ = false

  def schiffleiste: GridPanel = {
    size match {
      case 2 =>
        
        var schiffleiste = new GridPanel(1, 1) {
          background = java.awt.Color.WHITE
          var zerstoerer_einzeln = new Button("Zerstˆrer") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))

            reactions += {
              case ButtonClicked(zerstoerer_einzeln) =>
                if (z_einzeln == false) {
                  zerstoerer_einzeln.background_=(java.awt.Color.RED)
                  zerstoerer_einzeln.preferredSize_=(new Dimension(300, 60))
                  publish(new SetSchiff(2))
                  aktuelleLaenge = 2
                  aktuellerButton = this
                  z_einzeln = true
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  z_einzeln = false
                }
            }
          }

          zerstoerer_einzeln.icon = zerstoerer_icon

          contents += zerstoerer_einzeln
          listenTo(zerstoerer_einzeln)
        }
        schiffleiste
      case 5 =>
        var zerstoerer = new Button
        var uboot = new Button
        var kreuzer = new Button
        var schiffleiste = new GridPanel(3, 1) {
          background = java.awt.Color.WHITE
           zerstoerer = new Button("Zerstˆrer"){
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
          
            reactions += {
              case ButtonClicked(zerstoerer) =>
                if (z == false) {
                  zerstoerer.background_=(java.awt.Color.RED)
                  zerstoerer.preferredSize_=(new Dimension(350, 60))
                  publish(new SetSchiff(2))
                  aktuelleLaenge = 2
                  reset(kreuzer)
                  reset(uboot)
                  aktuellerButton = this
                  k = false
                  u = false
                  z = true
                } else {
                  zerstoerer.background = java.awt.Color.WHITE
                  zerstoerer.preferredSize_=(new Dimension(350, 60))
                  z = false
                }
            }
          icon = zerstoerer_icon
          }


          contents += zerstoerer
          listenTo(zerstoerer)
          uboot = new Button("U-Boot") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
          
            reactions += {
              case ButtonClicked(uboot) =>
                if (u == false) {
                	
                  reset(kreuzer)
                  reset(zerstoerer)
                 uboot. background_=(java.awt.Color.RED)
                  uboot.preferredSize_=(new Dimension(300, 60))
                 publish(new SetSchiff(3))
                 aktuellerButton = this
                 aktuelleLaenge = 3
                  z = false
                  k = false
                  u = true
                } else {
                  uboot.background = java.awt.Color.WHITE
                  uboot.preferredSize_=(new Dimension(350, 60))
                  u = false
                }
                
            }
          icon = uboot_icon
        }
          contents += uboot
          listenTo(uboot)
          kreuzer = new Button("Kreuzer") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
          
            reactions += {
              case ButtonClicked(kreuzer) =>
                if (k == false) {
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  reset(uboot)
                reset(zerstoerer)
                aktuellerButton = this
                  aktuelleLaenge = 4
                  z = false
                  u = false
                  k = true
                } else {
                  background_=(java.awt.Color.WHITE)
                  preferredSize_=(new Dimension(350, 60))
                  k = false
                }
            }
            icon = kreuzer_icon
          }          
          

          contents += kreuzer
          listenTo(kreuzer)
        }
        schiffleiste
      case 10 =>
        var schlachtschiff_groﬂ = new Button
           var kreuzer_2_groﬂ = new Button
          var kreuzer_1_groﬂ = new Button
          var uboot_3_groﬂ = new Button
          var uboot_2_groﬂ = new Button
           var uboot_1_groﬂ = new Button
          var zerstoerer_4_groﬂ = new Button
           var zerstoerer_3_groﬂ = new Button
          var zerstoerer_2_groﬂ = new Button
          var zerstoerer_1_groﬂ = new Button
          
        var schiffleiste = new GridPanel(10, 1) {
          background = java.awt.Color.WHITE
          zerstoerer_1_groﬂ = new Button("1.ter Zerstˆrer") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(zerstoerer_1_groﬂ) =>
                if (z1_groﬂ == false) {
                  aktuelleLaenge = 2
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  reset(zerstoerer_2_groﬂ)
                  reset(zerstoerer_3_groﬂ)
                  reset(zerstoerer_4_groﬂ)
                  reset(uboot_1_groﬂ)
                  reset(uboot_2_groﬂ)
                  reset(uboot_3_groﬂ)
                  reset(kreuzer_1_groﬂ)
                  reset(kreuzer_2_groﬂ)
                  reset(schlachtschiff_groﬂ)
                  z1_groﬂ = true
                  z2_groﬂ = false
                  z3_groﬂ = false
                  z4_groﬂ = false
                  u1_groﬂ = false
                  u2_groﬂ = false
                  u3_groﬂ = false
                  k1_groﬂ = false
                  k2_groﬂ = false
                  s_groﬂ = false
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  z1_groﬂ = false
                }
            }
            icon = zerstoerer_icon
          }
          
          contents += zerstoerer_1_groﬂ
          listenTo(zerstoerer_1_groﬂ)
          
          
           
          zerstoerer_2_groﬂ = new Button("2.ter Zerstˆrer") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(zerstoerer_2_groﬂ) =>
                if (z2_groﬂ == false) {
                  aktuelleLaenge = 2
                  reset(zerstoerer_1_groﬂ)
                  reset(zerstoerer_3_groﬂ)
                  reset(zerstoerer_4_groﬂ)
                  reset(uboot_1_groﬂ)
                  reset(uboot_2_groﬂ)
                  reset(uboot_3_groﬂ)
                  reset(kreuzer_1_groﬂ)
                  reset(kreuzer_2_groﬂ)
                  reset(schlachtschiff_groﬂ)
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  z1_groﬂ = false
                  z2_groﬂ = true
                  z3_groﬂ = false
                  z4_groﬂ = false
                  u1_groﬂ = false
                  u2_groﬂ = false
                  u3_groﬂ = false
                  k1_groﬂ = false
                  k2_groﬂ = false
                  s_groﬂ = false
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  z2_groﬂ = false
                }
            }
            icon = zerstoerer_icon
          }
          
          contents += zerstoerer_2_groﬂ
          listenTo(zerstoerer_2_groﬂ)
          
          
          
          zerstoerer_3_groﬂ = new Button("3.ter Zerstˆrer") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(zerstoerer_3_groﬂ) =>
                if (z3_groﬂ == false) {
                  aktuelleLaenge = 2
                  reset(zerstoerer_1_groﬂ)
                  reset(zerstoerer_2_groﬂ)
                  reset(zerstoerer_4_groﬂ)
                  reset(uboot_1_groﬂ)
                  reset(uboot_2_groﬂ)
                  reset(uboot_3_groﬂ)
                  reset(kreuzer_1_groﬂ)
                  reset(kreuzer_2_groﬂ)
                  reset(schlachtschiff_groﬂ)
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  z1_groﬂ = false
                  z2_groﬂ = false
                  z3_groﬂ = true
                  z4_groﬂ = false
                  u1_groﬂ = false
                  u2_groﬂ = false
                  u3_groﬂ = false
                  k1_groﬂ = false
                  k2_groﬂ = false
                  s_groﬂ = false
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  z3_groﬂ = false
                }
            }
            icon = zerstoerer_icon
          }
          
          contents += zerstoerer_3_groﬂ
          listenTo(zerstoerer_3_groﬂ)
          
         
          
          zerstoerer_4_groﬂ = new Button("4.ter Zerstˆrer") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(zerstoerer_4_groﬂ) =>
                if (z4_groﬂ == false) {
                  aktuelleLaenge = 2
                  reset(zerstoerer_1_groﬂ)
                  reset(zerstoerer_2_groﬂ)
                  reset(zerstoerer_3_groﬂ)
                  reset(uboot_1_groﬂ)
                  reset(uboot_2_groﬂ)
                  reset(uboot_3_groﬂ)
                  reset(kreuzer_1_groﬂ)
                  reset(kreuzer_2_groﬂ)
                  reset(schlachtschiff_groﬂ)
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  z1_groﬂ = false
                  z2_groﬂ = false
                  z3_groﬂ = false
                  z4_groﬂ = true
                  u1_groﬂ = false
                  u2_groﬂ = false
                  u3_groﬂ = false
                  k1_groﬂ = false
                  k2_groﬂ = false
                  s_groﬂ = false
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  z4_groﬂ = false
                }
            }
            icon = zerstoerer_icon
          }
          
          contents += zerstoerer_4_groﬂ
          listenTo(zerstoerer_4_groﬂ)
          
          
          
           
          uboot_1_groﬂ = new Button("1.tes U-Boot") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(uboot_1_groﬂ) =>
                if (u1_groﬂ == false) {
                  aktuelleLaenge = 3
                  reset(zerstoerer_1_groﬂ)
                  reset(zerstoerer_2_groﬂ)
                  reset(zerstoerer_3_groﬂ)
                  reset(zerstoerer_4_groﬂ)
                  
                  reset(uboot_2_groﬂ)
                  reset(uboot_3_groﬂ)
                  reset(kreuzer_1_groﬂ)
                  reset(kreuzer_2_groﬂ)
                  reset(schlachtschiff_groﬂ)
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  z1_groﬂ = false
                  z2_groﬂ = false
                  z3_groﬂ = false
                  z4_groﬂ = false
                  u1_groﬂ = true
                  u2_groﬂ = false
                  u3_groﬂ = false
                  k1_groﬂ = false
                  k2_groﬂ = false
                  s_groﬂ = false
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  u1_groﬂ = false
                }
            }
            icon = uboot_icon
          }
          

           
          
          contents += uboot_1_groﬂ
          listenTo(uboot_1_groﬂ)
          uboot_2_groﬂ = new Button("2.tes U-Boot") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(uboot_2_groﬂ) =>
                if (u2_groﬂ == false) {
                  aktuelleLaenge = 3
                  reset(zerstoerer_1_groﬂ)
                  reset(zerstoerer_2_groﬂ)
                  reset(zerstoerer_3_groﬂ)
                  reset(zerstoerer_4_groﬂ)
                  
                  reset(uboot_1_groﬂ)
                  reset(uboot_3_groﬂ)
                  reset(kreuzer_1_groﬂ)
                  reset(kreuzer_2_groﬂ)
                  reset(schlachtschiff_groﬂ)
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  z1_groﬂ = false
                  z2_groﬂ = false
                  z3_groﬂ = false
                  z4_groﬂ = false
                  u1_groﬂ = false
                  u2_groﬂ = true
                  u3_groﬂ = false
                  k1_groﬂ = false
                  k2_groﬂ = false
                  s_groﬂ = false
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  u2_groﬂ = false
                }
            }
            icon = uboot_icon
          }
          

          
         
          
          contents += uboot_2_groﬂ
          listenTo(uboot_2_groﬂ)
          uboot_3_groﬂ = new Button("3.tes U-Boot") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(uboot_3_groﬂ) =>
                if (u3_groﬂ == false) {
                  aktuelleLaenge = 3
                  reset(zerstoerer_1_groﬂ)
                  reset(zerstoerer_2_groﬂ)
                  reset(zerstoerer_3_groﬂ)
                  reset(zerstoerer_4_groﬂ)
                  
                  reset(uboot_1_groﬂ)
                  reset(uboot_2_groﬂ)
                  reset(kreuzer_1_groﬂ)
                  reset(kreuzer_2_groﬂ)
                  reset(schlachtschiff_groﬂ)
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  z1_groﬂ = false
                  z2_groﬂ = false
                  z3_groﬂ = false
                  z4_groﬂ = false
                  u1_groﬂ = false
                  u2_groﬂ = false
                  u3_groﬂ = true
                  k1_groﬂ = false
                  k2_groﬂ = false
                  s_groﬂ = false
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  u3_groﬂ = false
                }
            }
            icon = uboot_icon
          }
          

          
          
          contents += uboot_3_groﬂ
          listenTo(uboot_3_groﬂ)
          kreuzer_1_groﬂ = new Button("1.ter Kreuzer") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(kreuzer_1_groﬂ) =>
                if (k1_groﬂ == false) {
                  aktuelleLaenge = 4
                  reset(zerstoerer_1_groﬂ)
                  reset(zerstoerer_2_groﬂ)
                  reset(zerstoerer_3_groﬂ)
                  reset(zerstoerer_4_groﬂ)
                  reset(uboot_1_groﬂ)
                  reset(uboot_2_groﬂ)
                  reset(uboot_3_groﬂ)
                  
                  reset(kreuzer_2_groﬂ)
                  reset(schlachtschiff_groﬂ)
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  z1_groﬂ = false
                  z2_groﬂ = false
                  z3_groﬂ = false
                  z4_groﬂ = false
                  u1_groﬂ = false
                  u2_groﬂ = false
                  u3_groﬂ = false
                  k1_groﬂ = true
                  k2_groﬂ = false
                  s_groﬂ = false
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  k1_groﬂ = false
                }
            }
            icon = kreuzer_icon
          }
          

          
           
          contents += kreuzer_1_groﬂ
          listenTo(kreuzer_1_groﬂ)
          kreuzer_2_groﬂ = new Button("2.ter Kreuzer") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(kreuzer_2_groﬂ) =>
                if (k2_groﬂ == false) {
                  aktuelleLaenge = 4
                  reset(zerstoerer_1_groﬂ)
                  reset(zerstoerer_2_groﬂ)
                  reset(zerstoerer_3_groﬂ)
                  reset(zerstoerer_4_groﬂ)
                  reset(uboot_1_groﬂ)
                  reset(uboot_2_groﬂ)
                  reset(uboot_3_groﬂ)
                  
                  reset(kreuzer_1_groﬂ)
                  reset(schlachtschiff_groﬂ)
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  z1_groﬂ = false
                  z2_groﬂ = false
                  z3_groﬂ = false
                  z4_groﬂ = false
                  u1_groﬂ = false
                  u2_groﬂ = false
                  u3_groﬂ = false
                  k1_groﬂ = false
                  k2_groﬂ = true
                  s_groﬂ = false
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  k2_groﬂ = false
                }
            }
            icon = kreuzer_icon
          }
          

          contents += kreuzer_2_groﬂ
          listenTo(kreuzer_2_groﬂ)
          schlachtschiff_groﬂ = new Button("Schlachtschiff") {
            background = java.awt.Color.WHITE
            preferredSize_=(new Dimension(350, 60))
            reactions += {
              case ButtonClicked(schlachtschiff_groﬂ) =>
                if (s_groﬂ == false) {
                  aktuelleLaenge = 5
                  reset(zerstoerer_1_groﬂ)
                  reset(zerstoerer_2_groﬂ)
                  reset(zerstoerer_3_groﬂ)
                  reset(zerstoerer_4_groﬂ)
                  reset(uboot_1_groﬂ)
                  reset(uboot_2_groﬂ)
                  reset(uboot_3_groﬂ)
                  reset(kreuzer_1_groﬂ)
                  reset(kreuzer_2_groﬂ)
                  
                  background_=(java.awt.Color.RED)
                  preferredSize_=(new Dimension(300, 60))
                  z1_groﬂ = false
                  z2_groﬂ = false
                  z3_groﬂ = false
                  z4_groﬂ = false
                  u1_groﬂ = false
                  u2_groﬂ = false
                  u3_groﬂ = false
                  k1_groﬂ = false
                  k2_groﬂ = false
                  s_groﬂ = true
                } else {
                  background = java.awt.Color.WHITE
                  preferredSize_=(new Dimension(350, 60))
                  s_groﬂ = false
                }
            }
            icon = schlachtschiff_icon
          }
          
          contents += schlachtschiff_groﬂ
          listenTo(schlachtschiff_groﬂ)
        }

        schiffleiste
    }
  }
 def reset(button: AbstractButton): Unit = {
   button.background_=(java.awt.Color.WHITE)
   button.preferredSize_=(new Dimension(350, 60))
 }
}