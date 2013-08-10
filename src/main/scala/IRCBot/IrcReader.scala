package IRCBot

import UI.Reader
import GameLogic.Card
import UI.Reader._
import GameLogic.Joueur
import UI.Reader.Bid
import UI.Reader.Coinche

class IrcReader() extends Reader{

  type Input = String

  def inputToCard(input: Input):List[Card]  = {
    try {
      val cardInfo = input.toString.split(' ')
      // if player only supplied a card value, we check if a (playable) color corresponds
      if (cardInfo.length == 2) {
        val valeur = Card.stringToValeur(cardInfo(1))
        if (valeur == -1) List.empty[Card]
        else List.tabulate(4)(i => new Card(valeur + i * 8))
      } else {
        val famille = Card.stringToFamille(cardInfo(2))
        val valeur = Card.stringToValeur(cardInfo(1))
        if (famille == -1 || valeur == -1) List.empty[Card]
        else List(new Card(famille*8 + valeur))
      }
    } catch {
      case e:Throwable => List.empty[Card]
    }
  }

  implicit def inputToBiddingMessageOption(joueur: Joueur, input: Input): Option[BiddingMessage] = {
    try {
      val string = input toUpperCase()
      if (string == "PASSE") Some(Passe(joueur))
      else if (string == "!COINCHE") Some(Coinche(joueur))
      else if (string == "!SUR") Some(SurCoinche(joueur))
      else if (string.split(' ')(0).toUpperCase() == "BID"){
        val couleur = string.toString.split(' ')(2) match {
          case "PIQUE" | "P" => 0
          case "CARREAU" | "CA" => 1
          case "TREFLE" | "T"=> 2
          case "COEUR" | "CO"=> 3
          case "TA" => 4
          case "SA" => 5
        }
        val contrat = string.toString.split(' ')(1).toInt
        Some(Bid(joueur,couleur,contrat))
      } else None
    } catch {
      case e:Throwable => None
    }
  }

  override def sendMessage(j:Joueur,m:Input) = {
    if (CoincheBot.debug) {
      println("received "+m+" from player "+j)
      val translated = inputToBiddingMessageOption(j,m) orElse inputToPlayingMessageOption(j,m)
      if (translated.isDefined) println("sending "+translated.get)
      else println("did not recognize the message")
    }
    super.sendMessage(j,m)
  }
}
