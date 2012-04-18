import java.net.ServerSocket
import scala.io.BufferedSource
import java.io.PrintStream

object Server {
  def initialize(players: Int, bids: Int, server: ServerSocket): Auction = {
    println("Initializing keys for players")
    val player_keys = for {i <- 0 until players} yield {
      val s = server.accept()
      val in = new BufferedSource(s.getInputStream()).getLines()
      val key = in.next().toInt
      s.close()
      key
    }
    println("Initializing server keys")
    val server_key = (1, 1) // genkeys

    println("Initializing auction with " + players + " players and " + bids + " bids")
    println()
    new Auction(players, bids, player_keys.toList, server_key)
  }

  def takeTurn(results: Array[State],
               player: Int,
               bids: Int,
               server: ServerSocket): Array[State] = {
    val s = server.accept()
    println("Sending bids to player " + player)
    val in = new BufferedSource(s.getInputStream()).getLines()
    val out = new PrintStream(s.getOutputStream())
    
    results.foreach(out.println)
    out.println("EOM")
    out.flush()

    var bidStrings = List[String]()
    var bidString = in.next()
    while (bidString != "EOM") {
      bidStrings = bidString :: bidStrings
      bidString = in.next()
    }
    s.close()
    bidStrings.map(State.fromString).toArray
  }

  def runAuction(auction: Auction, server: ServerSocket) {
    println("Running auction")
    for (i <- 0 until auction.players) {
      val player = auction.players - 1 - i
      println("Waiting for answer from player " + player)
      auction.states = takeTurn(auction.states, player, auction.bids, server)
    }
    println("Results: " + auction.states(0))
  }

  def main(args: Array[String]) {
    println("Initializing server")
    val server = new ServerSocket(8888)

    val players = args(0).toInt
    val bids = args(1).toInt
    val auction = initialize(players, bids, server)

    println("Encrypting tree values")
    auction.encryptTree

    runAuction(auction, server)
  }
}
