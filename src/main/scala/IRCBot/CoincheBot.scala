package IRCBot

import org.jibble.pircbot._
import com.typesafe.config._

/**
 * Created with IntelliJ IDEA.
 * User: martin
 * Date: 29/06/13
 * Time: 16:34
 * To change this template use File | Settings | File Templates.
 */
class CoincheBot(val chan:String) extends PircBot{

  var listPlayers = List[String]()

  setName("CoincheBot")

  def playerJoins(sender:String):Unit = {
    if (listPlayers.length == 4) sendMessage(chan,"La table de coinche est deja pleine!")
    else {
      listPlayers = sender :: listPlayers
      sendMessage(chan,sender+" rejoint la table.")
    }
  }

  /**
   * Makes the bot disconnect from the server, if the caller is an operator.
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
