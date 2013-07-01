package IRCBot

import UI.Reader
import GameLogic.{Partie, Card}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class IrcReader extends Reader {

  /*
  Really need to use actors here
   */

  class State(var modified:Boolean, var couleur:String, var contrat:Int)

  val enchere = new State(false,"",0)
  def getCouleur: Int = {
    // Re-initialisation
    enchere.couleur = ""
    enchere.contrat = 0
    enchere.modified = false
    while (!enchere.modified) {Thread.sleep(1000)}
    enchere.couleur.toUpperCase match {
      case "PIQUE" | "P" => 1
      case "CARREAU" | "CA" => 2
      case "TREFLE" | "T"=> 3
      case "COEUR" | "CO"=> 4
      case "TA" => 5
      case "SA" => 6
      case "PASSE" => 0
      case _ => getCouleur
    }
  }

  // 'contrat' is set during getCouleur
  def getContrat: Int = {
    enchere.contrat
  }

  // Are we waiting for someone to play a card ?
  // which card is it ?
  var card = ""
  def getCard(jouables: List[Card], autres: List[Card]): Card = {
    card = ""
    while (card.isEmpty) Thread.sleep(1000)
    try {jouables(card.toInt)}
    catch {
      case e:NumberFormatException => getCard(jouables,autres)
      case e:IndexOutOfBoundsException => getCard(jouables,autres)
    }
  }

}
