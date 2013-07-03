package IRCBot

import UI.Reader
import GameLogic.Card

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
    while (!enchere.modified) {Thread.sleep(100)}
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
    famille= ""
    valeur = ""
    while (famille.isEmpty || valeur.isEmpty) {Thread.sleep(100)}
    if (Card.stringToFamille(famille) == -1 || Card.stringToValeur(valeur) == -1) getCard(jouables,autres)
    val card = new Card(Card.stringToFamille(famille)*8 + Card.stringToValeur(valeur))
    try jouables.find(_.equals(card)).get
    catch {
      case e:NumberFormatException => {println(e);getCard(jouables,autres)}
      case e:IndexOutOfBoundsException => {println(e);getCard(jouables,autres)}
      case e:NoSuchElementException => {CoincheBot.bot.sendMessage(CoincheBot.bot.chan,"Cette carte n'est pas jouable.")
                                        getCard(jouables,autres)}
    }
  }

}
