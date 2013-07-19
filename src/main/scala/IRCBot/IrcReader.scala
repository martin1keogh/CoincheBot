package IRCBot

import UI.Reader
import GameLogic.{Partie, Enchere, Card}

class IrcReader(printer:IrcPrinter) extends Reader {

  /*
  Really need to use actors here
   */

  class State(var modified:Boolean, var couleur:String, var contrat:Int)

  // These must be set when creating Reader
  // Have tp find another way, reader should not
  // depend on Partie...
  var checkStop:(() => Boolean) = () => false
  var stopped:Exception = new Exception

  val enchere = new State(false,"",0)
  var coinche = false
  var surcoinche = false
  def getCouleur: Int = {
    // Re-initialisation
    enchere.couleur = ""
    enchere.contrat = 0
    enchere.modified = false
    coinche = false
    surcoinche = false
    while (!enchere.modified) {
      Thread.sleep(100)
      if (checkStop()) throw stopped
      if (surcoinche) return 8
      if (coinche) return 7
    }
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
  var (famille,valeur) = ("","")
  def getCard(jouables: List[Card], autres: List[Card]): Card = {
    // play the last card automatically
    if (CoincheBot.automaticPlay && jouables.length == 1 && autres.length == 0) jouables(0)
    else
    try {
      famille= ""
      valeur = ""
      while (valeur.isEmpty) {
        Thread.sleep(100)
        if (checkStop()) throw stopped
      }
      // if player only supplied a card value, we check if a (playable) color corresponds
      if (famille.isEmpty) {
        val cardOption = jouables.find(_.valeur == Card.stringToValeur(valeur))
        cardOption.getOrElse({
          printer.sendMessage(printer.chan,"Aucune carte de cette valeur jouable.")
          getCard(jouables,autres)
        })
      }
      // if one of the info is wrong, ask again
      else if (Card.stringToFamille(famille) == -1 || Card.stringToValeur(valeur) == -1) getCard(jouables,autres)
      // else, if the card is playable, play it
      else {
        val card = new Card(Card.stringToFamille(famille)*8 + Card.stringToValeur(valeur))
        val cardOption = jouables.find(_.equals(card))
        cardOption.getOrElse({
          printer.sendMessage(printer.chan,"Cette carte n'est pas jouable.")
          getCard(jouables,autres)
        })
      }
    }
    catch {
      case e:NumberFormatException => {println(e);getCard(jouables,autres)}
      case e:IndexOutOfBoundsException => {println(e);getCard(jouables,autres)}
    }
  }

}
