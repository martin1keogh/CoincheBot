package IRCBot

import org.jibble.pircbot._
import com.typesafe.config._
import GameLogic.{Enchere, Joueur, Partie}
import scala.concurrent.{Await,Future}
import scala.concurrent.duration.Duration

class CoincheBot(val chan:String) extends PircBot{

  import scala.concurrent.ExecutionContext.Implicits.global
  import Partie.State._


  val printer = new IrcPrinter(chan)
  val reader = new IrcReader()
  var listPlayers = List[String]()

  setName("CoincheBot")
  setMessageDelay(100)

  def isOp(sender:String):Boolean = getUsers(chan).find(_.getNick == sender).get.isOp

  def stopGame(sender:String) : Unit = {
    if (isOp(sender) || listPlayers.contains(sender)){ // not sure about the second condition, probably going to make it a vote
      Partie.stopGame()
      listPlayers = List[String]()
      sendMessage(chan,"Game was stopped")
    } else if (Partie.state == stopped) sendMessage(chan,"Not game running atm")
    else {
      sendMessage(chan,"Only Op can stop games ATM (todo : have players vote to stop the game)")
    }
  }

  def startGame() : Unit = {
    // rename the players
    Partie.listJoueur.zip(listPlayers).foreach({case (p:Joueur,name:String) => p.rename(name)})

    Partie.Printer = printer;Enchere.Printer = printer
    Partie.Reader = reader;Enchere.Reader = reader
    Partie.printOnlyOnce = true

    printer.printTeams()

    // start Partie on another thread
    Future{Partie.start();listPlayers=List()}
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

  def leave(sender:String):Unit = {
    // can't leave if the game's already started
    if (Partie.state == stopped) listPlayers = listPlayers diff List(sender)
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
      case "!encheres" => if (Partie.State != stopped) printer.printListEnchere()
      case "!help" => if (message.trim() == "!help") printer.printHelp(channel)
                      else {println(channel);printer.printHelp(channel,message.split(' ')(1))}
      case "!current" => printer.printCurrent()
      case "!leave" => leave(sender)
      case "!cards" => if (listPlayers.contains(sender)) printer.printCartes(sender)
      case "!score" => printer.printScores()
      case "bid" => {
        // We're in the bidding phase
        if (Partie.state == bidding && sender == Partie.currentPlayer.nom) {
          // "bid 80 Co".split(' ')
          val array = message.split(' ')
          if (Enchere.annonceLegal(array(1).toInt)) {
            try {
              // "Co"
              reader.enchere.couleur = array(2)
              // "80".toInt
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
          if (array.length == 2) reader.valeur = array(1)
          else try {
            reader.famille = array(2)
            reader.valeur = array(1)
          } catch {
            case e:IndexOutOfBoundsException => print(e);()
            case e:Exception => println(e);()
          }
        }
        else println(Partie.state+" : "+Partie.currentPlayer.nom)
      }
      case "!coinche" => {
        if (Enchere.current.isDefined && Enchere.current.get.contrat > 80){
          val idEnchere = Enchere.current.get.id
          val coincheur = Partie.listJoueur.find(_.nom == sender)
          if (coincheur.isDefined && (coincheur.get.id % 2) != (idEnchere % 2))
          {
            reader.coinche = true
          }
        }
      }
      case "!sur" => {
        if (Enchere.current.isDefined && Enchere.current.get.coinche == 2){
          val idEnchere = Enchere.current.get.id
          val coincheur = Partie.listJoueur.find(_.nom == sender)
          if (coincheur.isDefined && (coincheur.get.id % 2) == (idEnchere % 2))
          {
            Enchere.current.get.coinche = 4
          }
        }
      }
      case _ => ()
    }
  }

  override def onPrivateMessage(sender:String, login:String, hostname:String, msg:String):Unit = {

    // command not allowed in queries
    val notOnQuery = List[String]("!join","!quit","!stop","!leave")
    val cmd = msg.split(' ')(0)

    if (!notOnQuery.contains(cmd)) onMessage(sender,sender,login,hostname,msg)
  }

  override def onNickChange(oldNick:String,login:String,hostname:String,newNick:String):Unit = {
    println(oldNick+" to "+newNick)
    listPlayers = listPlayers.map(s => if (s == oldNick) newNick else oldNick)
    Partie.listJoueur.foreach(j => if (j.nom == oldNick) j.rename(newNick))
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
