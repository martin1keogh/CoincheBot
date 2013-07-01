package IRCBot

import UI.Printer
import GameLogic.{Partie, Card, Joueur, Enchere}
import scala.collection.immutable.SortedMap
import com.sun.org.apache.xpath.internal.functions.WrongNumberArgsException

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

  def printHelp() : Unit = {
    sendMessage("Command list : !quit; !stop; !join")
    sendMessage("!quit : Disconnects the bot (op only)")
    sendMessage("!stop : stops the current game")
    sendMessage("!join : join the current table (or starts one if the first to join)")
    sendMessage("-------------------")
    sendMessage("Usage : ")
    sendMessage("bidding phase : bid <value> <color> || passe")
    sendMessage("example : bid 80 Co (or bid 80 coeur)")
    sendMessage("valid colors : Coeur(co);Carreau(ca);Pique(P);Trefle(T);TA;SA")
    sendMessage("playing phase : pl <card_number>")
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
    val (a::b::Nil,c::d::Nil) = Partie.listJoueur.partition(_.id%2 == 0)
    sendMessage("score "+a+"/"+b+" :"+Partie.scoreTotalNS)
    sendMessage("score "+c+"/"+d+" :"
      +Partie.scoreTotalEO)
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
        l.foreach({case card:Card => sb.append(card+"; ")});
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
    sendMessage(Partie.currentPlayer+" joue "+c)
  }

  def remporte(joueur: Joueur, plis: List[(Joueur, Card)]) {
    sendMessage(joueur.nom+" remporte le plis")
    sendMessage("----------------------------------")
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
    sendMessage("Equipes : ")
    Partie.listJoueur.groupBy(_.Equipe).foreach({
      case (e,j1::j2::Nil) => sendMessage(e.toString()+" : "+j1+" "+j2)
      case (e,_) => throw new IllegalArgumentException("wrong number of player in team :"+e)
    })
  }

  def printCartes() {}

  def printScoreMain(scoreNS: Int, enchere: Enchere) {
    sendMessage("Contrat : "+enchere.toString())
    val prisParNS = (enchere.id % 2 == 0)
    if (prisParNS) {
      if (scoreNS >= enchere.contrat) {sendMessage("Passe de "+(scoreNS - enchere.contrat))}
      else {sendMessage("Chute de "+(enchere.contrat - scoreNS))}
    } else {
      val scoreEO = 162 - scoreNS
      if (scoreEO >= enchere.contrat) {sendMessage("Passe de "+(scoreEO - enchere.contrat))}
      else {sendMessage("Chute de "+(enchere.contrat - scoreEO))}
    }
  }
}
