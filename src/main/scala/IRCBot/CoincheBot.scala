package IRCBot

import org.jibble.pircbot._
import com.typesafe.config._
import GameLogic.{Enchere, Joueur, Partie}
import scala.concurrent.Future

class CoincheBot(val chan:String) extends PircBot{

  val printer = new IrcPrinter(chan)
  val reader = new IrcReader()
  var listPlayers = List[String]()

  setName("CoincheBot")
  setMessageDelay(100)

  def playerJoins(sender:String):Unit = {
    if (listPlayers.length == 4) sendMessage(chan,"La table de coinche est deja pleine!")
    else if (listPlayers.contains(sender)) sendMessage(chan,sender+" : Deja a la table.")
    else {
      listPlayers = sender :: listPlayers
      sendMessage(chan,sender+" rejoint la table.")
    }
    if (listPlayers.length == 4) {
      // rename the players
      Partie.listJoueur.zip(listPlayers).foreach({case (p:Joueur,name:String) => p.rename(name)})

      Partie.Printer = printer
      Enchere.Printer = printer
      Partie.Reader = reader
      Enchere.Reader = reader

      printer.printTeams()

      import scala.concurrent.ExecutionContext.Implicits.global

      Future{
        Partie.start()
      }
    }
  }

  /**
   * Disconnects from the server, if the caller is an operator.
   *
   * @param sender Person who called '!quit'
   */
  def quit(sender:String):Unit = {
    if (getUsers(chan).filter(_.getNick == sender)(0).isOp) {disconnect();sys.exit()}
    else {sendMessage(chan,sender+" : you are not op.")}
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
      case "bid" => {
        // We're in the bidding phase
        if (reader.enchere.modifiable && sender == Partie.currentPlayer.nom) {
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
        if (reader.enchere.modifiable && sender == Partie.currentPlayer.nom) {
          reader.enchere.couleur = "passe"
          reader.enchere.modified = true
        }
      }
      case "pl" => {
        if (reader.modifiable && sender == Partie.currentPlayer.nom) {
          reader.card = message.split(' ')(1)
        } else {println(sender+" : no good")
          println(reader.modifiable)
          println(Partie.currentPlayer.nom)
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

  val bot = new CoincheBot("#coinche")

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
