import java.net.ServerSocket
import scala.io.BufferedSource
import java.io.PrintStream
import scala.util.Random


object Server {
  def initialize(players: Int, bids: Int, server: ServerSocket): (Auction,(BigInt, BigInt, BigInt),BigInt) = {
    println("Initializing server keys")
    
    val pub_key = Cryptography.genPrime() //pub_key = (p, q=(p-1)/2, g)
    val rnd = new Random()
    val priv_key = Cryptography.priv_key(pub_key._2, rnd) 
    //poor name for thee server's public key [i.e. g^priv_key)
    val server_key = pub_key._3.modPow(priv_key,pub_key._1)
    print("PUB: ")
    println(pub_key)

    print("PRIV: ")
    println(priv_key)
    
    println("Awaiting player connections,,,")
    for(i<- 0 until players) {
      val s = server.accept()
      val in = new BufferedSource(s.getInputStream()) 
      val out = new PrintStream(s.getOutputStream())
      println("Player " + i + " connected")
      out.println(i)
      out.println(bids)
      out.println(pub_key._1) //p
      out.println(pub_key._2) //q 
      out.println(pub_key._3) //g
      out.flush()
      s.close()
    }


    println("Initializing keys for players")
    val player_keys = for {i <- 0 until players} yield {
      val s = server.accept()
      val in = new BufferedSource(s.getInputStream()).getLines()
      val key = BigInt(in.next())
      println("Received player " + i + " key")
      s.close()
      key
    }

    player_keys.zipWithIndex.foreach{ x =>
      val (k,i) = x
      println(i+" " + k)
    }
    
   

    println("Initializing auction with " + players + " players and " + bids + " bids")
    println()
    (new Auction(players, bids, player_keys.toList, server_key, pub_key._1, pub_key._3),
    pub_key,
    priv_key
    )
  }


  def takeTurn(auction:Auction,
	       player: Int,
               bids: Int,
               server: ServerSocket): Array[State] = {
    val s = server.accept()
    println("Sending bids to player " + player)
    val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())
    out.println(auction.current_pub_key)
    auction.states.foreach(out.println)
    out.println("EOM")
    out.flush()
    s.close()
    
    val s2 = server.accept()
    val in2 = new BufferedSource(s2.getInputStream()).getLines()
 
    var bidStrings = List[String]()
    auction.current_pub_key = BigInt(in2.next())
    var bidString = in2.next()
    while (bidString != "EOM") {
      bidStrings = bidString :: bidStrings
      bidString = in2.next()
    }
    s2.close()
    bidStrings.map(State.fromString).toArray
  }

  def runAuction(auction: Auction, server: ServerSocket,
      		 p:BigInt,q:BigInt, g:BigInt, priv_key:BigInt) {
    println("Running auction")
 
    for (i <- 0 until auction.players) {
      val player = auction.players - 1 - i
      println("Waiting for answer from player " + player)
      auction.states =  takeTurn(auction, player, auction.bids, server)
  //    auction.states.foreach(x => println(x.winner_enc + " ::: "+ x.price_enc))
    }
    println("Hit enter to do the final decryption")
    readLine
    println("Results: ")
    for(state <- auction.states){
      auction.current_pub_key = Cryptography.strip_pub_key(auction.current_pub_key, priv_key, g, p, q)
      state.reencryptState(priv_key, auction.current_pub_key,p,q, g)
      println("WINNER [g^winner]:d " + state.winner_enc)
      println("PRICE [g^price]: " + state.price_enc)
      println("WINNER: " + findPow(state.winner_enc,g,p))
      println("PRICE: " + findPow(state.price_enc,g,p))
    }
  }

  def findPow(value:BigInt,g:BigInt,p:BigInt):Int = { 
    var i = 0
    var number = BigInt(1)
    var found = false
    while(!found){ 
      if(number==value){ 
	found = true
      }
      else{ 
	number = (number*g)%p
	i=i+1
      }
    }
    i //return
  }
  def main(args: Array[String]) {
    println("Initializing server")
    val server = new ServerSocket(8888)
    val players = args(0).toInt
    val bids = args(1).toInt
    val auction = initialize(players, bids, server)
    println("Encrypting tree values")
    auction._1.encryptTree
    runAuction(auction._1, server, auction._2._1,auction._2._2,auction._2._3, auction._3)
  }
}
