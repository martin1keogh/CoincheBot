package IRCBot

import UI.Reader
import GameLogic.{Enchere, Joueur, Card}
import UI.Reader.{Bid, Passe, Coinche, Message}

class IrcReader(printer:IrcPrinter) extends Reader {

  var interrupt = false

  var sender:Joueur = null
  var contrat = 0
  var couleur = ""
  var coinche = false
  var modified = false
  def getMessage: (Joueur, Message) = {
    contrat = 0
    couleur = ""
    coinche = false
    modified = false

    while (!modified){
      if (interrupt) throw new InterruptedException
      Thread.sleep(1000)
    }
    if (coinche) return (sender,Coinche())
    if (couleur == "passe") return (sender,Passe())
    val c = couleur.toUpperCase match {
      case "PIQUE" | "P" => 1
      case "CARREAU" | "CA" => 2
      case "TREFLE" | "T"=> 3
      case "COEUR" | "CO"=> 4
      case "TA" => 5
      case "SA" => 6
    }
    (sender,Bid(Enchere.intToCouleur(c),contrat))
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
        if (interrupt) throw new InterruptedException
        Thread.sleep(100)
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

  var listSurCoincheur:List[Joueur] = List()
  def getSurCoinche: List[Joueur] = {
    listSurCoincheur = List()
    Thread.sleep(5000)
    listSurCoincheur
  }
}
