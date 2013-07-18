package IRCBot

import UI.Printer
import GameLogic.{Partie, Card, Joueur, Enchere}
import org.jibble.pircbot.Colors
import scala.language.implicitConversions

abstract class IrcPrinter(val chan:String) extends Printer{

  val spade = "♠"
  val diamond = "♦"
  val clubs = "♣"
  val heart = "♥"

  val colorSpade = Colors.GREEN
  val colorDiamond = Colors.YELLOW
  val colorClubs = Colors.NORMAL
  val colorHeart = Colors.RED

  def valeurToString(c:Card):String = c.valeur match {
    case 0 => "7"
    case 1 => "8"
    case 2 => "9"
    case 3 => "J"
    case 4 => "Q"
    case 5 => "K"
    case 6 => "10"
    case 7 => "A"
  }

  implicit def cardToString(c:Card):String = c.famille match {
    case 0 => colorSpade + valeurToString(c) + spade
    case 1 => colorDiamond + valeurToString(c) + diamond
    case 2 => colorClubs + valeurToString(c) + clubs
    case 3 => colorHeart + valeurToString(c) + heart
  }

  def sendMessage(s:String):Unit
  def sendMessage(j:Joueur,s:String):Unit
  def sendMessage(chan:String,s:String):Unit

  /**
   * Print current players
   */
  def printCurrent(listPlayers:List[String]):Unit = {
    if (listPlayers.isEmpty) sendMessage("Aucun joueur a la table.")
    else {
      val sb = new StringBuilder
      listPlayers.foreach({ e => sb.append(e.toString+";")})
      sendMessage("Joueur deja a la table : "+sb.toString())
    }
  }

  def printHelp(chan:String) : Unit = {
    sendMessage(chan,"Command list : !quit, !stop, !join, !current, !encheres," +
      " !cards, !leave, !score, !votekick, !voteban")
    sendMessage(chan,"While playing : bid, pl, !coinche")
    sendMessage(chan,"!help <cmd> for more information on <cmd>")
  }

  def printHelp(chan:String, cmd:String) : Unit = {
    def sendMessage(s:String): Unit = this.sendMessage(chan,s)
    cmd match{
      case "!quit" => sendMessage("!quit : Disconnects the bot (op only)")
      case "!stop" => sendMessage("!stop : stops the current game")
      case "!join" => sendMessage("!join : join the current table (or starts one if the first to join)")
      case "!encheres" => sendMessage("!list : list current bids (if any)")
      case "!current" => sendMessage("!current : show players currently at the table")
      case "!leave" => sendMessage("!leave : leaves the table, if the game hasn't started yet.")
      case "!cards" => sendMessage("!cards : shows the player his cards (query)")
      case "!coinche" => sendMessage("!coinche : coinche the current bid (!sur to surcoinche)")
      case "!score" => sendMessage("!score : print score")
      case "!votekick" => sendMessage("!votekick <nick> : starts a vote among players to kick <nick>. Only works when playing a game!")
      case "!voteban" => sendMessage("!voteban <nick> : starts a vote among players to ban <nick>. Only works when playing a game!")
      case "bid" => {
        sendMessage("During bidding phase : bid <value> <color> || passe")
        sendMessage("example : bid 80 Co (or bid 80 coeur)")
        sendMessage("value  : 80 -...- 160, 250 and 400")
        sendMessage("colors : Coeur(co);Carreau(ca);Pique(P);Trefle(T);TA;SA")
      }
      case "pl" => {
        sendMessage("During playing phase : pl <value> [<color>]")
        sendMessage("valid values : Sept(7) -...- Dix(10) - V/D/R/AS - J/Q/K/AS - Valet/Dame/Roi/As")
        sendMessage("examples : pl 8 Co  || pl as T")
        sendMessage("""/!\ : if <color> is not specified and multiple <value> cards are playables, the color is picked at random.""")
      }
      case _ => sendMessage(cmd+" non trouvée.")
    }
  }

  def printListEnchere(listEnchere:List[Enchere]) {
    if (listEnchere.isEmpty) { sendMessage("Aucune enchere pour le moment")}
    else {
      sendMessage("Liste des encheres precedentes :")
      listEnchere.reverse.foreach({enchere => sendMessage(enchere.toString())})
    }
  }

  def printScores(NS:Int,EO:Int)(implicit listJoueur:List[Joueur]) {
    val (a::b::Nil,c::d::Nil) = listJoueur.partition(_.id%2 == 0)
    sendMessage("score "+a+"/"+b+" :"+NS)
    sendMessage("score "+c+"/"+d+" :"+EO)
  }


  // Unused in CoincheBot (sending every player all their cards
  // every turn spams the irc server)
  def printCards(jouables:List[Card],autres:List[Card])(implicit joueur:Joueur) {}

  def tourJoueurEnchere(implicit joueur: Joueur) {
    sendMessage("A "+joueur.nom+" de parler.")
  }

  def tourJoueur(implicit j: Joueur) {
    sendMessage("A "+j.nom+" de jouer.")
  }

  def joueurAJoue(c: Card)(implicit joueur:Joueur) {
    sendMessage(Colors.BOLD + joueur +" joue "+cardToString(c))
  }

  def remporte(joueur: Joueur, plis: List[(Joueur, Card)]) {
    sendMessage(Colors.BOLD+">>>> "+joueur.nom+" remporte le pli <<<<")
  }

  def printFin(NS: Int, EO: Int)(implicit listJoueur:List[Joueur]) {
    val NS = listJoueur.filter(_.id%2 == 0)
    val EO = listJoueur.filter(_.id%2 == 1)
    sendMessage("Partie finie, score final :")
    sendMessage(NS(0).nom+"/"+NS(1).nom+" :"+NS+";"
      +EO(0).nom+"/"+EO(1).nom + ":"+EO)

  }

  def printRestart(Partie:Partie) : Unit = {
    if (Partie.state == Partie.State.running) tourJoueur(Partie.currentPlayer)
    if (Partie.state == Partie.State.bidding) tourJoueurEnchere(Partie.currentPlayer)
  }

  def pasDePrise() {sendMessage("Pas de prise")}

  def enchereFinie(e: Enchere) {
    sendMessage("Fin des encheres.")
    sendMessage(Colors.BOLD + e.toString())
  }

  def printTeams(listJoueur:List[Joueur]) {
    sendMessage("Equipes : ")
    listJoueur.groupBy(j => j.Equipe).foreach({
      case (e,j1::j2::Nil) => sendMessage(e.toString()+" : "+j1+" "+j2)
      case (e,_) => throw new IllegalArgumentException("wrong number of player in team :"+e)
    })
  }

  def printScoreMain(scoreNS: Int, enchere: Enchere, capotChute:Boolean, generaleChute:Boolean) {
    sendMessage("Contrat : "+enchere.toString())
    if (enchere.contrat == 400) {
      if (generaleChute) sendMessage("Chute !")
      else sendMessage("Passe !")
    }
    else if (enchere.contrat == 250) {
      if (capotChute) sendMessage("Chute !")
      else sendMessage("Passe !")
    } else {
      val prisParNS = enchere.id % 2 == 0
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

  def printCards(implicit j: Joueur):Unit = {
    val stringBuilder = new StringBuilder
    val sbList = j.main.groupBy(_.famille).mapValues(famille => {
      val sb = new StringBuilder
      famille.map(cardToString(_)).addString(sb," ")
    }).values.toList
    sbList.addString(stringBuilder," - ")
    sendMessage(j,stringBuilder.toString())
  }

  def printCards(couleurAtout:Int)(implicit j:Joueur):Unit = {
    val stringBuilder = new StringBuilder
    val sbList = j.main.groupBy(_.famille).mapValues(famille => {
      val sb = new StringBuilder
      if (famille.head.famille == couleurAtout) {
        sb.append("(Atout) ")
        (1,famille.map(cardToString(_)).addString(sb," "))
      } else (0,famille.map(cardToString(_)).addString(sb," "))
    }).values.toList
    // On met l'atout en premiere couleur
    sbList.sortBy(-_._1).unzip._2.addString(stringBuilder,Colors.NORMAL+" - ")
    sendMessage(j,stringBuilder.toString())
  }

  def printCardsToAll(couleurAtout: Int)(implicit listJoueur:List[Joueur]) {
    listJoueur.foreach(j => printCards(couleurAtout)(j))
  }

  /**
   * Print the player's cards during the bidding phase
   * Cards are sorted by OrdreToutAtout
   */
  def printCardsToAll(implicit listJoueur:List[Joueur]) {
    def aux(j:Joueur) = {
      val stringBuilder = new StringBuilder
      val sbList = j.main.groupBy(_.famille).map({case (id,famille) =>
        val sb = new StringBuilder
        famille.sortBy(-_.ordreAtout).map(cardToString(_)).addString(sb," ")
      })
      sbList.addString(stringBuilder,Colors.NORMAL+" - ")
      sendMessage(j,stringBuilder.toString())
    }
    listJoueur.foreach(j => aux(j))
  }


  def printCoinche() {
    sendMessage("Coinché !! 5 secondes pour surcoincher (commande : !sur)")
  }

  def annonceBelote(first: Boolean)(implicit currentPlayer:Joueur) {
    if (first) sendMessage(currentPlayer.nom+" annonce belote.")
    else sendMessage(currentPlayer.nom+ " annonce rebelote.")
  }

  // unused in CoincheBot
  def printCards(jouables: List[Card], autres: List[Card])(implicit joueur: Joueur, couleurAtout: Int) {}
}
