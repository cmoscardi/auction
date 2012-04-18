import java.net.{InetAddress, ServerSocket, Socket, SocketException}
import scala.io.BufferedSource
import java.io.PrintStream

object Client {
  def sendKey(client: Client) {
    println("Sending public key to server")
    val s = new Socket(InetAddress.getByName("localhost"), 8888)
    lazy val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())

    out.println(client.keypair._2)
    out.flush()
    s.close()
  }

  def getStates(s: Socket, in: Iterator[String]): List[State] = {
    var bidStrings = List[String]()
    var bidString = in.next()
    while (bidString != "EOM") {
      bidStrings = bidString :: bidStrings
      bidString = in.next()
    }
    bidStrings.map(State.fromString)
  }

  def sendStates(states: Array[State], s: Socket, out: PrintStream) {
    //out.println(client.keypair._2)
    states.foreach(out.println)
    out.println("EOM")
    out.flush()
  }

  def main(args: Array[String]) {
    val client = new Client(args(0).toInt)
    val bids = args(1).toInt
    println("Bidder index: " + client.index)
    println("Bidder keypair: " + client.keypair)
    sendKey(client)

    println("Hit enter to retrieve state values")
    readLine
    val s = new Socket(InetAddress.getByName("localhost"), 8888)
    lazy val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())

    val states = getStates(s, in)
    println("What's your bid? 0-" + bids)
    sendStates(client.pickStates(states.toArray, readInt, bids), s, out)
    s.close()
  }
}

class Client(val index: Int) {
  val keypair = (1, 1) // genkeys

  def pickStates(oldStates: Array[State], bid: Int, bids: Int): Array[State] = {
    val newStates = State.genStates(index, bids)
    for (i <- 0 until newStates.length) {
      val newState = newStates(i) 
      val oldState = oldStates.find(_ == newState.afterBid(index, bid)).get
      newState.winner_enc = oldState.winner_enc
      newState.price_enc = oldState.price_enc
      newState.reencryptState(keypair._2)
      newStates(i) = newState
    }
    newStates
  }
}
