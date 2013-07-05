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
  setMessageDelay(CoincheBot.throttle)

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

    Partie.Printer = printer;Enchere.Printer = printer
    Partie.Reader = reader;Enchere.Reader = reader
    Partie.printOnlyOnce = true

    printer.printTeams()

    // start Partie on another thread
    Future{Partie.start();Partie.init()}
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

  def changeOneElement(list : List[String],pred : String => Boolean, newValue : String) : List[String] = list match{
    case Nil => Nil
    case x::xs => if (pred(x)) newValue :: xs else x::changeOneElement(xs,pred,newValue)
  }

  def playerJoins(sender:String):Unit = {
    if (listPlayers.length == 4 && listPlayers.find(_ == "None").isEmpty) sendMessage(chan,"La table de coinche est deja pleine!")
    else if (listPlayers.contains(sender)) sendMessage(chan,sender+" : Deja a la table.")
    else {
      // Someone left the table, let's replace him/her
      if (listPlayers.exists(_ == "None")) {
        try {
          listPlayers = changeOneElement(listPlayers,s => s == "None",sender)
          Partie.listJoueur.find(j => j.nom == "None").get.rename(sender)
          sendMessage(chan,sender+" rejoint la table.")
        } catch {
          case e:NoSuchElementException => println("Error in playerJoins :"+e);()
        }
      }
      else {
        listPlayers = sender :: listPlayers
        Partie.listJoueur(listPlayers.length - 1).rename(sender)
        sendMessage(chan,sender+" rejoint la table.")
        if (listPlayers.length == 4 && Partie.state == stopped) startGame()
      }
    }
  }

  def leave(sender:String):Unit = {
    try {
    listPlayers = listPlayers.map(s => if (s == sender) "None" else s)
    Partie.listJoueur.find(_.nom == sender).get.rename("None")
    sendMessage(chan,sender+" left.")
    } catch {
      case e: NoSuchElementException => println(e)
    }
  }

  def enoughPlayers():Boolean = {
    if (listPlayers.length == 4 && listPlayers.find(_ == "None").isEmpty) true
    else {sendMessage(chan,"Missing "+(4 - listPlayers.length + listPlayers.count(_ == "None"))+" player(s).");false}
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
      case "!leave" => if (listPlayers.contains(sender)) leave(sender)
      case "!encheres" => if (Partie.State != stopped) printer.printListEnchere()
      case "!help" => if (message.trim() == "!help") printer.printHelp(channel)
                      else printer.printHelp(channel,message.split(' ')(1))
      case "!current" => printer.printCurrent()
      case "!cards" => if (listPlayers.contains(sender)) printer.printCartes(sender)
      case "!score" => printer.printScores()
      case "bid" => {
        if (!enoughPlayers()) ()
        else {
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
      }
      case "passe" => {
        if (!enoughPlayers()) ()
        else {
          if (Partie.state == bidding && sender == Partie.currentPlayer.nom) {
            reader.enchere.couleur = "passe"
            reader.enchere.modified = true
          }
        }
      }
      case "pl" => {
        if (!enoughPlayers()) ()
        else {
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
    listPlayers = listPlayers.map(s => if (s == oldNick) newNick else s )
    Partie.listJoueur.foreach(j => if (j.nom == oldNick) j.rename(newNick))
  }

  def start():Unit = {
    joinChannel(chan)
  }

}

object CoincheBot extends App {

  val config = ConfigFactory.load()

  val bot = new CoincheBot("#coinchebot")

  // milliseconds
  val throttle = 500

  // automatically play for you, if you don't have a choice
  val automaticPlay = true

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
