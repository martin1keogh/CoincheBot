package IRCBot

import UI.Printer
import GameLogic.{Partie, Card, Joueur, Enchere}
import scala.collection.immutable.SortedMap

class IrcPrinter(val chan:String) extends Printer{

  override def toString() = "irc"

  def sendMessage(s:String):Unit = CoincheBot.bot.sendMessage(chan,s)
  def sendMessage(j:Joueur,s:String):Unit = CoincheBot.bot.sendMessage(j.nom,s)

  /**
   * Print the player's cards during the bidding phase
   */
  def printCardsToAll() {
    def printFamille(j:Joueur,cards:List[Card]):Unit = {
      val couleur = cards.head.familleToString
      val valeurs = cards.map(_.valeurToString).mkString(", ")
      sendMessage(j,couleur+" : "+valeurs)
    }
    def aux(j:Joueur):Unit = {
      val listCartesParFamille = j.main.groupBy(_.famille)
      listCartesParFamille.foreach({famille =>
        printFamille(j,famille._2)
      })
    }
    Partie.listJoueur.foreach(aux(_))

  }

  def printErreurCouleur() {
    sendMessage("Mauvaise Couleur")
  }

  def printErreurEnchere() {
    sendMessage("Mauvaise Enchere")
  }

  def printListEnchere() {
    sendMessage("Liste des encheres precedentes :")
    Enchere.listEnchere.reverse.foreach({enchere => sendMessage(enchere.toString)})
  }

  def printScores() {
    sendMessage("score Nord/Sud :"+Partie.scoreTotalNS)
    sendMessage("score Est/Ouest :"+Partie.scoreTotalEO)
  }


  def printCartes(jouables:List[Card],autres:List[Card]) {
    def println(s:String) = sendMessage(Partie.currentPlayer,s)
    println("Jouables : ")
    //TRES SALE
    SortedMap(jouables.zipWithIndex.groupBy(_._1.famille).toSeq:_*).foreach(
    {case (cle,l) =>
      val sb = new StringBuilder
      if (l.head._1.famille == Partie.enchere.couleur) sb.append("(Atout) ") else sb.append("        ")
      l.foreach({case (card:Card,index:Int) => sb.append(index+"/"+card+"; ")});
      println(sb.toString())
    })
    if (!autres.isEmpty){
      println("Non Jouables : ")
      SortedMap(autres.groupBy(_.famille).toSeq:_*).foreach(
      {case (cle,l) =>
        val sb = new StringBuilder
        if (l.head.famille == Partie.enchere.couleur) sb.append("(Atout) ") else sb.append("        ")
        l.foreach({case card:Card => print(card+"; ")});
        println(sb.toString())
      })
    }
    println("----------------------------------")
  }

  def tourJoueurEnchere(joueur: Joueur) {
    sendMessage("A "+joueur.nom+" de parler.")
  }

  def tourJoueur(j: Joueur) {
    sendMessage("A "+j.nom+" de jouer.")
  }

  def joueurAJoue(c: Card) {
    sendMessage(Partie.currentPlayer+" a joué "+c)
  }

  def remporte(joueur: Joueur, plis: List[(Joueur, Card)]) {
    sendMessage(joueur.nom+" remporte le plis")
  }

  def printFin(NS: Int, EO: Int) {
    sendMessage("Partie fini, score final :")
    sendMessage("N/S :"+NS.toString+"; E/O :"+EO.toString)

  }

  def pasDePrise() {sendMessage("Pas de prise")}

  def enchereFinie(e: Enchere) {
    sendMessage("Fin des encheres")
    sendMessage(e.toString())
  }

  def printTeams() {
  }

  def printCartes() {}
}
