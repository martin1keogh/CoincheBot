package IRCBot

import UI.Printer
import GameLogic.{Partie, Card, Joueur, Enchere}
import scala.collection.immutable.SortedMap
import org.jibble.pircbot.Colors

class IrcPrinter(val chan:String) extends Printer{

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

  def sendMessage(s:String):Unit = CoincheBot.bot.sendMessage(chan,s)
  def sendMessage(j:Joueur,s:String):Unit = CoincheBot.bot.sendMessage(j.nom,s)

  /**
   * Print current players
   */
  def printCurrent():Unit = {
    if (CoincheBot.bot.listPlayers.isEmpty) sendMessage("Aucun joueur a la table.")
    else {
      val sb = new StringBuilder
      CoincheBot.bot.listPlayers.foreach({ e => sb.append(e.toString+";")})
      sendMessage("Joueur deja a la table : "+sb.toString())
    }
  }

  def printHelp(chan:String) : Unit = {
    CoincheBot.bot.sendMessage(chan,"Command list : !quit, !stop, !join, !current, !encheres," +
      " !cards, !leave, !score, !votekick, !voteban")
    CoincheBot.bot.sendMessage(chan,"While playing : bid, pl, !coinche")
    CoincheBot.bot.sendMessage(chan,"!help <cmd> for more information on <cmd>")
  }

  def printHelp(chan:String, cmd:String) : Unit = {
    def sendMessage(s:String): Unit = CoincheBot.bot.sendMessage(chan,s)
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

  def printListEnchere() {
    if (Enchere.listEnchere.isEmpty) { sendMessage("Aucune enchere pour le moment")}
    else {
      sendMessage("Liste des encheres precedentes :")
      Enchere.listEnchere.reverse.foreach({enchere => sendMessage(enchere.toString())})
    }
  }

  def printScores() {
    val (a::b::Nil,c::d::Nil) = Partie.listJoueur.partition(_.id%2 == 0)
    sendMessage("score "+a+"/"+b+" :"+Partie.scoreTotalNS)
    sendMessage("score "+c+"/"+d+" :"+Partie.scoreTotalEO)
  }


  // Unused in CoincheBot (sending every player all their cards
  // every turn spams the irc server)
  def printCartes(jouables:List[Card],autres:List[Card]) {
    def println(s:String) = sendMessage(Partie.currentPlayer,s)
    println("Jouables : ")
    //TRES SALE
    SortedMap(jouables.zipWithIndex.groupBy(_._1.famille).toSeq:_*).foreach(
    {case (cle,l) =>
      val sb = new StringBuilder
      if (l.head._1.famille == Partie.enchere.couleur) sb.append("(Atout) ") else sb.append("        ")
      l.foreach({case (card:Card,index:Int) => sb.append(index+"/"+card+"; ")})
      println(sb.toString())
    })
    if (!autres.isEmpty){
      println("Non Jouables : ")
      SortedMap(autres.groupBy(_.famille).toSeq:_*).foreach(
      {case (cle,l) =>
        val sb = new StringBuilder
        if (l.head.famille == Partie.enchere.couleur) sb.append("(Atout) ") else sb.append("        ")
        l.foreach({case card:Card => sb.append(card+"; ")})
        println(sb.toString())
      })
    }
    println("----------------------------------")
  }

  def tourJoueurEnchere(joueur: Joueur) {
    sendMessage("A "+joueur.nom+" de parler.")
  }

  def printEnchere() {
    if (Enchere.current.isDefined) sendMessage(Colors.BOLD + Enchere.current.get.toString())
  }

  def tourJoueur(j: Joueur) {
    sendMessage("A "+j.nom+" de jouer.")
  }

  def joueurAJoue(c: Card) {
    sendMessage(Colors.BOLD + Partie.currentPlayer+" joue "+cardToString(c))
  }

  def remporte(joueur: Joueur, plis: List[(Joueur, Card)]) {
    sendMessage(Colors.BOLD+">>>> "+joueur.nom+" remporte le pli <<<<")
  }

  def printFin(NS: Int, EO: Int) {
    val NS = Partie.listJoueur.filter(_.id%2 == 0)
    val EO = Partie.listJoueur.filter(_.id%2 == 1)
    sendMessage("Partie finie, score final :")
    sendMessage(NS(0).nom+"/"+NS(1).nom+" :"+Partie.scoreTotalNS+";"
      +EO(0).nom+"/"+EO(1).nom + ":"+Partie.scoreTotalEO)

  }

  def printRestart() : Unit = {
    if (Partie.state == Partie.State.running) tourJoueur(Partie.currentPlayer)
    if (Partie.state == Partie.State.bidding) tourJoueurEnchere(Partie.currentPlayer)
  }

  def pasDePrise() {sendMessage("Pas de prise")}

  def enchereFinie(e: Enchere) {
    sendMessage("Fin des encheres.")
    sendMessage(Colors.BOLD + e.toString())
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
    if (enchere.contrat == 400) {
      if (Partie.generalChute) sendMessage("Chute !")
      else sendMessage("Passe !")
    }
    else if (enchere.contrat == 250) {
      if (Partie.capotChute) sendMessage("Chute !")
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

  def printCartes(j:Joueur):Unit = {
    val stringBuilder = new StringBuilder
    val sbList = j.main.groupBy(_.famille).mapValues(famille => {
      val sb = new StringBuilder
      if (famille.head.famille == Partie.enchere.couleur) {
        sb.append("(Atout) ")
        (1,famille.map(cardToString(_)).addString(sb," "))
      } else (0,famille.map(cardToString(_)).addString(sb," "))
    }).values.toList
    // On met l'atout en premiere couleur
    sbList.sortBy(-_._1).unzip._2.addString(stringBuilder,Colors.NORMAL+" - ")
    sendMessage(j,stringBuilder.toString())
  }

  def printCartes(s:String):Unit = {
    printCartes(Partie.listJoueur.find(_.nom == s).get)
  }


  def printCardsToAll(couleurAtout: Int) {
    Partie.listJoueur.foreach(j => printCartes(j))
  }

  /**
   * Print the player's cards during the bidding phase
   * Cards are sorted by OrdreToutAtout
   */
  def printCardsToAll() {
    def aux(j:Joueur) = {
      val stringBuilder = new StringBuilder
      val sbList = j.main.groupBy(_.famille).map({case (id,famille) =>
        val sb = new StringBuilder
        famille.sortBy(-_.ordreAtout).map(cardToString(_)).addString(sb," ")
      })
      sbList.addString(stringBuilder,Colors.NORMAL+" - ")
      sendMessage(j,stringBuilder.toString())
    }
    Partie.listJoueur.foreach(j => aux(j))
  }


  def printCoinche() {
    sendMessage("Coinché !! 5 secondes pour surcoinché (commande : !sur)")
  }

  def annonceBelote(first: Boolean) {
    if (first) sendMessage(Partie.currentPlayer.nom+" annonce belote.")
    else sendMessage(Partie.currentPlayer.nom+ " annonce rebelote.")
  }
}
