import java.net.{InetAddress, ServerSocket, Socket, SocketException}
import scala.io.BufferedSource
import java.io.PrintStream
import scala.util.Random
import scala.compat.Platform

object Client {
  def sendKey(client: Client) {
    val s = new Socket(InetAddress.getByName("localhost"), 8888)
    val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())  
    println("Sending public key to server")
    val generator = client.pub_key(2)
    
    val key = generator.modPow(client.priv_key, client.pub_key(0))
    //println(key)
    out.println(key)
    out.flush()
    s.close()
  }

  def getStates(): (BigInt,List[State]) = {
    val s = new Socket(InetAddress.getByName("localhost"), 8888)
    val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())  
    var bidStrings = List[String]()
    
    val current_pub_key = BigInt(in.next())
    var bidString = in.next()
    
    while (bidString != "EOM") {
      bidStrings = bidString :: bidStrings
      bidString = in.next()
    }
    s.close()
    (current_pub_key, bidStrings.map(State.fromString))
  }

  def sendStates(current_pub_key:BigInt, states: Array[State]) {
    val s = new Socket(InetAddress.getByName("localhost"), 8888)
    val in = new BufferedSource(s.getInputStream())
    val out = new PrintStream(s.getOutputStream())  
    //out.println(client.keypair._2)
    out.println(current_pub_key)
    states.foreach(out.println)
    out.println("EOM")
    out.flush()
    s.close()
  }

  def initialize(): (Client, Int) =  {
    val s = new Socket(InetAddress.getByName("localhost"), 8888)
    val in = new BufferedSource(s.getInputStream())
    val out = new PrintStream(s.getOutputStream())    
    
    val receive = in.getLines()
    val index = receive.next
    val bids = receive.next
    val pub_key = for(i<-0 until 3) yield {
      BigInt(receive.next)
    }      
    //println("Pub key:")
    //pub_key.foreach(println) 
    // (p,q,g)
    val rnd = new Random()
    val priv_key = Cryptography.priv_key(pub_key(1),rnd)
    val client = new Client(index.toInt, priv_key, pub_key.toList)
    s.close()
    (client, bids.toInt)
  }

  def main(args: Array[String]) {
    val init = initialize()
    val bids = init._2
    val client = init._1

    println("Bidder index: " + client.index)
    println("Bids: " + bids)
    println("Bidder key: " + client.priv_key)
    println("Hit enter to send key info")
    readLine
    sendKey(client)

    println("Hit enter to retrieve state values")
    readLine

    
    val info = getStates()
    val current_pub_key = info._1
    val states = info._2
    println("What's your bid? 1-" + bids)
    val chosen = client.pickStates(current_pub_key,states.toArray, readInt, bids)
    sendStates(chosen._1,chosen._2)
  }
}

class Client(val index: Int, 
      	     val priv_key: BigInt,
	     val pub_key: List[BigInt]) {


  def pickStates(current_pub_key:BigInt, oldStates: Array[State], bid: Int, bids: Int): (BigInt,Array[State]) = {
   // println("p="+pub_key(0))
   // println("q="+pub_key(1))
   // println("g="+pub_key(2))
    val newStates = State.genStates(index, bids)
    println("Current pub key = " + current_pub_key)
    val new_pub_key = Cryptography.strip_pub_key(current_pub_key,
					         priv_key,
					         pub_key(2),
					         pub_key(0),
						 pub_key(1))
    println("new pub key = " +new_pub_key)
    val start = Platform.currentTime
    for (i <- 0 until newStates.length) {
      val newState = newStates(i) 
      val oldState = oldStates.find(_ == newState.afterBid(index, bid)).get
     // println("taking data from: "+ oldState.toString())
     // println("going into state: " + newState.toString())
      newState.winner_enc = oldState.winner_enc
      newState.price_enc = oldState.price_enc
      newState.c_1_winner = oldState.c_1_winner
      newState.c_1_price = oldState.c_1_price
      newState.reencryptState(priv_key, new_pub_key, pub_key(0),pub_key(1),pub_key(2))
      newStates(i) = newState
    }
    val finish = Platform.currentTime
    val elapsed = finish-start
    println("time elapsed: " + elapsed+"ms")

    (new_pub_key,newStates)
  }
}
