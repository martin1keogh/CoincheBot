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
class CoincheBot extends PircBot{

  setVerbose(true)
  setName("CoincheBot")

}

object CoincheBot extends App {

  val config = ConfigFactory.load()

  val bot = new CoincheBot()

  try {
    val address = config.getString("server.address")
    val port = config.getInt("server.port")
    bot.connect(address,port)
  } catch {
    case e : Exception => { println("Error while trying to connect to the IRC server :");
                            println(e.toString);
                            sys.exit(255)}
  }

  bot.joinChannel("#coinche")


}
