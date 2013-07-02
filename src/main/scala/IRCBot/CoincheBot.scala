package IRCBot

import org.jibble.pircbot._
import com.typesafe.config._
import GameLogic.{Enchere, Joueur, Partie}
import scala.concurrent.Future

class CoincheBot(val chan:String) extends PircBot{

  import scala.concurrent.ExecutionContext.Implicits.global
  import Partie.State._


  val printer = new IrcPrinter(chan)
  val reader = new IrcReader()
  var listPlayers = List[String]()

  var f = Future{}

  setName("CoincheBot")
  setMessageDelay(100)

  def isOp(sender:String):Boolean = getUsers(chan).find(_.getNick == sender).get.isOp

  def stopGame(sender:String) : Unit = {
    if (isOp(sender)){
      Partie.stopGame()
      listPlayers = List[String]()
      sendMessage(chan,"Game was stopped")
    } else {
      sendMessage(chan,"Only Op can stop games ATM (todo : have players vote to stop the game)")
    }
  }

  def startGame() : Unit = {
    // rename the players
    Partie.listJoueur.zip(listPlayers).foreach({case (p:Joueur,name:String) => p.rename(name)})

    Partie.Printer = printer;Enchere.Printer = printer
    Partie.Reader = reader;Enchere.Reader = reader

    printer.printTeams()

    // start Partie on another thread
    f = Future{
      Partie.start()
    }

  }

  /**
   * Disconnects from the server, if the caller is an operator.
   *
   * @param sender Person who called '!quit'
   */
  def quit(sender:String):Unit = {
    if (isOp(sender)) {disconnect();sys.exit()}
    else {sendMessage(chan,sender+" : you are not op.")}
  }

  def playerJoins(sender:String):Unit = {
    if (listPlayers.length == 4) sendMessage(chan,"La table de coinche est deja pleine!")
    else if (listPlayers.contains(sender)) sendMessage(chan,sender+" : Deja a la table.")
    else {
      listPlayers = sender :: listPlayers
      sendMessage(chan,sender+" rejoint la table.")
      if (listPlayers.length == 4) startGame()
    }
  }

  override def onMessage(channel:String,
                         sender :String,
                         login  :String,
                         hostname:String,
                         message:String):Unit = {

    val cmd = message.split(' ')(0)

    cmd match {
      case "!join" => playerJoins(sender)
      case "!quit" => quit(sender)
      case "!stop" => stopGame(sender)
      case "!list" => printer.printListEnchere()
      case "!help" => printer.printHelp()
      case "bid" => {
        // We're in the bidding phase
        if (Partie.state == bidding && sender == Partie.currentPlayer.nom) {
          val array = message.split(' ')
          if (Enchere.annonceLegal(array(1).toInt)) {
            try {
              reader.enchere.couleur = array(2)
              reader.enchere.contrat = array(1).toInt
              reader.enchere.modified = true}
            catch {
              case e : NumberFormatException => sendMessage(chan,"Format d'une annonce : 'bid <contrat> <couleur>")
              case e : IndexOutOfBoundsException => sendMessage(chan,"Format d'une annonce : 'bid <contrat> <couleur>")
            }
          }
          else {
          sendMessage(chan,sender+" : annonce illegale.")
          }
        }
      }
      case "passe" => {
        if (Partie.state == bidding && sender == Partie.currentPlayer.nom) {
          reader.enchere.couleur = "passe"
          reader.enchere.modified = true
        }
      }
      case "pl" => {
        if (Partie.state == playing && sender == Partie.currentPlayer.nom) {
          val array = message.split(' ')
          reader.famille = array(1)
          reader.valeur = array(2)
        }
      }
    }


  }

  def start():Unit = {
    joinChannel(chan)
  }

}

object CoincheBot extends App {

  val config = ConfigFactory.load()

  val bot = new CoincheBot("#coinchebot")

  //debug
  bot.setVerbose(true)

  try {
    val address = config.getString("server.address")
    val port = config.getInt("server.port")
    bot.connect(address,port)
  } catch {
    case e : Exception => { println("Error while trying to connect to the IRC server :");
                            println(e.toString);
                            sys.exit(255)}
  }

  bot.start()


}
