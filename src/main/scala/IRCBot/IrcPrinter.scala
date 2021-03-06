package IRCBot

import UI.Printer
import GameLogic.{Partie, Card, Joueur, Enchere}
import org.jibble.pircbot.Colors
import scala.language.implicitConversions
import GameLogic.Bot.BotTrait
import GameLogic.Joueur.{Nord, Sud, NordSud}
import GameLogic.Enchere._
import GameLogic.Card._

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
    case Sept => "7"
    case Huit => "8"
    case Neuf => "9"
    case Valet => "J"
    case Dame => "Q"
    case Roi => "K"
    case Dix => "10"
    case As => "A"
  }

  implicit def cardToString(c:Card):String = c.couleur match {
    case Pique => colorSpade + valeurToString(c) + spade
    case Carreau => colorDiamond + valeurToString(c) + diamond
    case Trefle => colorClubs + valeurToString(c) + clubs
    case Coeur => colorHeart + valeurToString(c) + heart
    case _ => {
        println("Error in IrcPrinter.cardToString : called with c.couleur = SansAtout/ToutAtout/Undef"); 
        ""}
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

  def printEnchere(e:Option[Enchere]):Unit = {
    if (e.isDefined) sendMessage(chan,e.get.toString())
  }

  def printHelp(chan:String) : Unit = {
    sendMessage(chan,"Command list : !quit, !stop, !join, !current, !encheres," +
      " !cards, !leave, !scores, !votekick, !voteban, !create, !addbot, !removebot")
    sendMessage(chan,"While playing : bid, pl, !coinche")
    sendMessage(chan,"!help <cmd> for more information on <cmd>")
  }

  def printHelp(chan:String, cmd:String) : Unit = {
    def sendMessage(s:String): Unit = this.sendMessage(chan,s)
    cmd match{
      case "!quit" => sendMessage("!quit : Disconnects the bot (op only)")
      case "!create" => sendMessage("!create #chan : Creates a new CoincheBot on channel #chan (op only)")
      case "!stop" => sendMessage("!stop : stops the current game")
      case "!join" => sendMessage("!join : join the current table (or starts one if the first to join)")
      case "!encheres" => sendMessage("!encheres : list current bids (if any)")
      case "!current" => sendMessage("!current : show players currently at the table")
      case "!leave" => sendMessage("!leave : leaves the table, if the game hasn't started yet.")
      case "!cards" => sendMessage("!cards : shows the player his cards (query)")
      case "!coinche" => sendMessage("!coinche : coinche the current bid (!sur to surcoinche)")
      case "!scores" => sendMessage("!score : print scores")
      case "!addbot" => {
        sendMessage("!addbot <botType> <botName>: add <botType> as a player with name <botName>.")
        sendMessage("botTypes : DumBot")
      }
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
    val (a::b::Nil,c::d::Nil) = listJoueur.partition(_.equipe == NordSud)
    sendMessage(s"score $a/$b : $NS --- score $c/$d : $EO")
  }

  def printEtoiles(m:collection.mutable.Map[Joueur,Int]) {
    if (!m.isEmpty) sendMessage("Etoiles : "+m.mkString(", "))
    else sendMessage("Aucune étoile !")
  }

  def cardUnplayable = sendMessage("Carte injouable")

  def annonceImpossible = sendMessage("Annonce impossible")

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

  def printFin(scoreNS: Int, scoreEO: Int, m:collection.mutable.Map[Joueur,Int])(implicit listJoueur:List[Joueur]) {
    val (a::b::Nil,c::d::Nil) = listJoueur.partition(_.equipe == NordSud)
    sendMessage("Partie finie, score final :")
    sendMessage(s"score $a/$b : $scoreNS --- score $c/$d : $scoreEO")
    printEtoiles(m)

  }

  def printRestart(Partie:Partie) : Unit = {
    if (Partie.state == Partie.State.running) tourJoueur(Partie.currentPlayer)
    if (Partie.state == Partie.State.bidding) tourJoueurEnchere(Partie.currentPlayer)
    println(Partie.state)
  }

  def pasDePrise() {sendMessage("Pas de prise")}

  def enchereFinie(e: Enchere) {
    sendMessage("Fin des encheres.")
    sendMessage(Colors.BOLD + e.toString())
  }

  def printTeams(listJoueur:List[Joueur]) {
    sendMessage("Equipes : ")
    listJoueur.groupBy(j => j.equipe).foreach({
      case (e,j1::j2::Nil) => sendMessage(e.toString+" : "+j1+" "+j2)
      case (e,_) => throw new IllegalArgumentException("wrong number of player in team :"+e)
    })
  }

  def printScoreMain(scoreNS: Int, enchere: Enchere, capotChute:Boolean, generaleChute:Boolean) {
    val result = 
      if (enchere.contrat == 400) {
        if (generaleChute) "Chute !" else "Passe !"
      }
      else if (enchere.contrat == 250) {
        if (capotChute) "Chute !" else "Passe !"
      } else {
        val prisParNS = enchere.id == Nord || enchere.id == Sud
        if (prisParNS) {
          if (scoreNS >= enchere.contrat) "Passe de "+(scoreNS - enchere.contrat)
          else "Chute de "+(enchere.contrat - scoreNS)
        } else {
          val scoreEO = 162 - scoreNS
          if (scoreEO >= enchere.contrat) "Passe de "+(scoreEO - enchere.contrat)
          else "Chute de "+(enchere.contrat - scoreEO)
          }
        }
    sendMessage("Contrat : "+enchere.toString()+" --- "+result)
    }

  // printCards during bids
  def printCards(implicit j: Joueur):Unit = {
    val stringBuilder = new StringBuilder
    val sbList = j.main.sortBy(-_.ordreAtout).groupBy(_.couleur).mapValues(famille => {
      val sb = new StringBuilder
      famille.map(cardToString).addString(sb," ")
    }).values.toList
    sbList.addString(stringBuilder," - ")
    sendMessage(j,stringBuilder.toString())
  }

  def printCards(couleurAtout:Couleur)(implicit j:Joueur):Unit = {
    val stringBuilder = new StringBuilder
    val sbList = j.main.groupBy(_.couleur).mapValues(famille => {
      val sb = new StringBuilder
      if (couleurAtout == ToutAtout || couleurAtout == Undef)  { //tout atout ou durant les encheres
        (1,famille.sortBy(-_.ordreAtout).map(cardToString).addString(sb," "))
      }
      else if (famille.head.couleur == couleurAtout) {
        sb.append("(Atout) ")
        (1,famille.sortBy(-_.ordreAtout).map(cardToString).addString(sb," "))
      } else (0,famille.sortBy(-_.ordreClassique).map(cardToString).addString(sb," "))
    }).values.toList
    // On met l'atout en premiere couleur
    sbList.sortBy(-_._1).unzip._2.addString(stringBuilder,Colors.NORMAL+" - ")
    sendMessage(j,stringBuilder.toString())
  }

  def printCardsToAll(couleurAtout: Couleur)(implicit listJoueur:List[Joueur]) {
    listJoueur.foreach(j => j match {case b:BotTrait=>();case _ => printCards(couleurAtout)(j)})
  }

  /**
   * Print the player's cards during the bidding phase
   * Cards are sorted by OrdreToutAtout
   */
  def printCardsToAll(implicit listJoueur:List[Joueur]) {
    def aux(j:Joueur) = {
      val stringBuilder = new StringBuilder
      val sbList = j.main.groupBy(_.couleur).map({case (id,famille) =>
        val sb = new StringBuilder
        famille.sortBy(-_.ordreAtout).map(cardToString).addString(sb," ")
      })
      sbList.addString(stringBuilder,Colors.NORMAL+" - ")
      sendMessage(j,stringBuilder.toString())
    }
    listJoueur.foreach(j => j match {case _:BotTrait => ();case _ =>aux(j)})
  }

  def printCardUnplayable():Unit = {
    sendMessage("Carte non jouable.")
  }


  def printCoinche() {
    sendMessage("Coinché !! 10 secondes pour surcoincher (commande : !sur)")
  }

  def annonceBelote(first: Boolean)(implicit currentPlayer:Joueur) {
    if (first) sendMessage(currentPlayer.nom+" annonce belote.")
    else sendMessage(currentPlayer.nom+ " annonce rebelote.")
  }

  // unused in CoincheBot
  def printCards(jouables: List[Card], autres: List[Card])(implicit joueur: Joueur, couleurAtout: Couleur) {}
}
