package IRCBot

import org.jibble.pircbot._
import com.typesafe.config._
import GameLogic.{Joueur, Partie}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class CoincheBot(val chan:String) extends PircBot{


  val printer = new IrcPrinter(chan) {
    def sendMessage(j:Joueur,s:String) = CoincheBot.this.sendMessage(j.nom,s)
    def sendMessage(s:String) = CoincheBot.this.sendMessage(chan,s)
    def sendMessage(chan:String,s:String) = CoincheBot.this.sendMessage(chan,s)
  }
  val reader = new IrcReader(printer)
  val Partie = new Partie(printer,reader)
  val Enchere = Partie.enchereController

  import Partie.State._

  var listPlayers = List[String]()
  var kickCounter:List[String] = List[String]()
  var voteInProgress:Boolean = false

  setName("CoincheBot")

  /**
   * Simple Spam control system
   * Maps mask -> number of cmd sent to the bot
   * Every 'interval' seconds, those number are decremented
   * If a mask exceeds 'limit', he is kicked or banned (if he was kicked once before)
   */
  object Spam {
    // For each mask, maps his hostMask and nick to the number of command sent
    // Used to kick/ban spammers
    var spamMap = Map[(String,String),Int]()
    var kickedOnceList = List[String]()

    // decrement values every ? milliseconds
    val interval = 1000
    val limit = 4

    def setBan(mask:String):Unit = kickedOnceList = mask :: kickedOnceList

    /**
     * Decrement every mask's value.
     * decrement should be called every X seconds
     */
    def decrement():Unit = try {
      spamMap = spamMap.mapValues(value => if (value > 0) value-1 else 0)
    } catch {
      case e:Exception => println(e)
    }

    def increment(mask:String,nick:String):Unit = try {
      spamMap = spamMap + ((mask,nick) -> (spamMap.getOrElse((mask,nick),0) + 2))
    } catch {
      case e:Exception => println(e)
    }

    def run() : Unit = {
      try {
        while (true) {
          Thread.sleep(interval)
          spamMap.filter({case (_,count) => count > limit}).keySet.foreach(
          {case (mask:String,nick:String) =>
            if (kickedOnceList.contains(mask)) {
              ban(chan,"*!"+mask)
              if (listPlayers.contains(nick)) leave(nick)
            }
            else {kick(chan,nick,"Command spam detected, you'll be banned next time.")
              setBan(mask)
              spamMap = spamMap + ((mask,nick) -> 0)
              if (listPlayers.contains(nick)) leave(nick)
            }
          case _ => println("Unexpected value in CoincheBot.Spam.run()")
          })
          decrement()
        }
      } catch {
        case e : Exception => println(e)
      }
    }
  }

  def isOp(sender:String):Boolean = getUsers(chan).find(_.getNick == sender).get.isOp

  def stopGame(sender:String) : Unit = {
    if (isOp(sender) || listPlayers.contains(sender)){ // not sure about the second condition, probably going to make it a vote
      Partie.stopGame()
      listPlayers = List[String]()
      sendMessage(chan,"Game was stopped")
    } else if (Partie.state == stopped) sendMessage(chan,"No game running atm")
    else {
      // unused right now, need to implement vote
      sendMessage(chan,"Only Op can stop games ATM (todo : have players vote to stop the game)")
    }
  }

  def stopGame() : Unit = {
    Partie.stopGame()
    listPlayers = List[String]()
    sendMessage(chan,"No one left at the table, the game was stopped")
  }

  def voteKick(caller:String,nick:String) : Unit = Future {
    if (!voteInProgress) {
      if (!getUsers(chan).exists(user => user.getNick.toLowerCase == nick.toLowerCase)) sendMessage(chan,"No such person on this channel.")
      else if (isOp(nick)) {kick(chan,caller,"Nice try.")}
      else {
        kickCounter = List[String]()
        voteInProgress = true
        sendMessage(chan,"Vote to kick "+nick+" requested! Players have 1 minute to vote (cmd: !yes). 3 votes needed.")
        var loopCounter = 0
        while (loopCounter < 30) {
          Thread.sleep(2000)
          if (kickCounter.length > 2) {kick(chan,nick);loopCounter = 30} else loopCounter+=1
        }
        voteInProgress = false
      }
    }
  }

  def voteBan(caller:String,nick:String,mask:String) : Unit = Future {
    if (!voteInProgress) {
      if (!getUsers(chan).contains(nick)) sendMessage(chan,"No such person on this channel.")
      else if (isOp(nick)) {kick(chan,caller,"Nice try.")}
      else {
        kickCounter = List[String]()
        voteInProgress = true
        sendMessage(chan,"Vote to ban "+nick+" requested! Players have 1 minute to vote (cmd: !yes). 4 votes needed.")
        var loopCounter = 0
        while (loopCounter < 30) {
          Thread.sleep(2000)
          if (kickCounter.length > 3) {ban(chan,"*!"+mask);leave(nick);loopCounter = 30} else loopCounter+=1
        }
        voteInProgress = false
      }
    }
  }


  def startGame() : Unit = {

    Partie.printOnlyOnce = true

    printer.printTeams(Partie.listJoueur)

    // start Partie on another thread
    Future{Partie.start();listPlayers = List[String]()}
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

  private def changeOneElement(list : List[String],pred : String => Boolean, newValue : String) : List[String] = list match{
    case Nil => Nil
    case x::xs => if (pred(x)) newValue :: xs else x::changeOneElement(xs,pred,newValue)
  }

  /**
   * Add the person to 'listPlayers' if :
   *  - he isn't in it already
   *  - the table isn't full (listPlayers.length < 4 || listPlayers.exists(player == "None"))
   * @param sender Person who wants to join the game
   */
  def playerJoins(sender:String):Unit = {
    if (listPlayers.length == 4 && !listPlayers.exists(_ == "None")) sendMessage(chan,"La table de coinche est deja pleine!")
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
        // if table is full again
        if (listPlayers.length == 4) printer.printRestart(Partie)
      }
      else {
        listPlayers = sender :: listPlayers
        Partie.listJoueur(listPlayers.length - 1).rename(sender)
        sendMessage(chan,sender+" rejoint la table.")
        if (listPlayers.length == 4 && Partie.state == stopped) startGame()
      }
    }
  }

  /**
   * Replaces 'sender' by 'None' in 'listPlayers' (i.e removes him/her from the game).
   * Ends the game if 'sender' was the last person at the table.
   * @param sender the person who wants to quit the game
   */
  def leave(sender:String):Unit = {
    try {
      listPlayers = listPlayers.map(s => if (s == sender) "None" else s)
      Partie.listJoueur.find(_.nom == sender).get.rename("None")
      sendMessage(chan,sender+" left.")
      // nobody left at the table
      if (listPlayers.forall(_ == "None")) stopGame()
    } catch {
      case e: NoSuchElementException => println(e)
    }
  }

  /**
   *
   * @return true if there are 4 players at the table, false otherwise
   */
  def enoughPlayers():Boolean = {
    if (listPlayers.length == 4 && !listPlayers.exists(_ == "None")) true
    else {sendMessage(chan,"Missing "+(4 - listPlayers.length + listPlayers.count(_ == "None"))+" player(s).");false}
  }

  // Main entry point for message
  override def onMessage(channel:String,
                         sender :String,
                         login  :String,
                         hostname:String,
                         message:String):Unit = {

    val cmd = message.split(' ')(0)
    // was the msg a bot command ?
    // set to false in : cmd match {case _ => isCmd = false}
    var isCmd = true

    cmd toLowerCase() match {
      case "!join" => playerJoins(sender)
      case "!quit" => quit(sender)
      case "!stop" => stopGame(sender)
      case "!leave" => if (listPlayers.contains(sender)) leave(sender)
      case "!encheres" => if (Partie.State != stopped) printer.printListEnchere(Partie.enchereController.listEnchere)
      case "!help" => if (message.trim() == "!help") printer.printHelp(channel)
      else printer.printHelp(channel,message.split(' ')(1))
      case "!current" => printer.printCurrent(listPlayers)
      case "!cards" => if (listPlayers.contains(sender)) printer.printCards(Partie.listJoueur.find(_.nom == sender).get)
      case "!score" => printer.printScores(Partie.scoreTotalNS,Partie.scoreTotalEO)(Partie.listJoueur)
      case "!votekick" => if (message.split(' ').length == 2 && listPlayers.contains(sender)) voteKick(sender,message.split(' ')(1))
      case "!voteban" => if (message.split(' ').length == 2 && listPlayers.contains(sender)) voteBan(sender,message.split(' ')(1),login+"@"+hostname)
      case "!yes" => if (listPlayers.contains(sender) && !kickCounter.contains(sender)) kickCounter=sender::kickCounter
      case "!create" =>
        if (isOp(sender)) {
          try {
            val newChan = message.split(' ')(1)
            Future{sendMessage(chan,"Creation d'une nouvelle table sur "+newChan);CoincheBot.routine(newChan,"coinchebot2","")}
          } catch {
            case e:IndexOutOfBoundsException => sendMessage(chan,"usage : !create #CHAN")
          }
        } else sendMessage(chan,"Only Ops can create new Channels!")
      case "bid" => {
        if (!enoughPlayers()) ()
        else {
          try {
            // We're in the bidding phase
            if (Partie.state == bidding && sender == Partie.currentPlayer.nom) {
              // "bid 80 Co".split(' ')
              val array = message.split(' ')
              if (Enchere.annonceLegal(array(1).toInt)) {
                // "Co"
                reader.enchere.couleur = array(2)
                // "80".toInt
                reader.enchere.contrat = array(1).toInt
                reader.enchere.modified = true
              }
              else {
                sendMessage(chan,sender+" : annonce illegale.")
              }
            }
          }
          catch {
            case e : NumberFormatException => sendMessage(chan,"Format d'une annonce : 'bid <contrat> <couleur>")
            case e : IndexOutOfBoundsException => sendMessage(chan,"Format d'une annonce : 'bid <contrat> <couleur>")
          }
        }
      }
      case "passe" if (message.trim == "passe")=> { // "do nothing if msg = 'passe de 20; ...'
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
            else if (array.length == 3) {
              reader.famille = array(2)
              reader.valeur = array(1)
            }
          }
          else println(Partie.state+" : "+Partie.currentPlayer.nom)
        }
      }
      case "!coinche" => reader.coinche = true
      case "!sur" => reader.surcoinche = true
      case _ => isCmd = false
    }
    if (isCmd) Spam.increment(login+"@"+hostname,sender)
  }

  override def onPrivateMessage(sender:String, login:String, hostname:String, msg:String):Unit = {

    // command not allowed in queries
    val notOnQuery = List[String]("!join","!quit","!stop","!leave","!coinche","!votekick","!voteban","bid")
    val cmd = msg.split(' ')(0)

    // if command is allowed and mask was never kick from main channel
    if (!notOnQuery.contains(cmd) && !Spam.kickedOnceList.contains(login+"@"+hostname)) onMessage(sender,sender,login,hostname,msg)
    else Spam.increment(login+"@"+hostname,sender)
  }

  override def onNickChange(oldNick:String,login:String,hostname:String,newNick:String):Unit = {
    listPlayers = listPlayers.map(s => if (s == oldNick) newNick else s )
    Partie.listJoueur.foreach(j => if (j.nom == oldNick) j.rename(newNick))
  }

  override def onPart(channel:String,sender:String,login:String,hostname:String):Unit = {
    if (listPlayers.contains(sender)) leave(sender)
  }

  override def onKick(channel:String,kickerNick:String,kickerLogin:String,kickerHostName:String,kickedNick:String,reason:String) : Unit ={
    if (listPlayers.contains(kickedNick)) leave(kickedNick)
  }

  def start():Unit = {
    joinChannel(chan)
  }

}

object CoincheBot extends App {

  val config = ConfigFactory.load()

  // automatically play for you, if you don't have a choice
  val automaticPlay = config.getBoolean("config.automaticPlay")

  def routine(chan:String,name:String,pass:String) = {
    // bot creation
    val bot = new CoincheBot(chan)
    // IllegalAccessError
    //  bot.setName(config.getString("config.nick"))

    bot.setVerbose(config.getBoolean("config.debug"))
    bot.setMessageDelay(config.getInt("config.throttle"))

    bot.setAutoNickChange(true)

    try {
      // Connection
      val address = config.getString("server.address")
      val port = config.getInt("server.port")
      bot.connect(address,port)

      // identification
      //bot.setName(name)
      if (!pass.isEmpty) bot.identify(pass)


      // AntiSpam
      Future{bot.Spam.run()}

      Future{bot.start()}
    } catch {
      case e : Exception => { println("Error while trying to connect to the IRC server :")
        println(e.toString)
        sys.exit(255)}
    }
  }

  routine(config.getString("server.chan"),config.getString("config.nick"),config.getString("config.pass"))

}
