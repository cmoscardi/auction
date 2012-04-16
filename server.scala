object Server {
  def initialize(players: Int, bids: Int): Auction = {
    println("Initializing keys for players")
    val player_keys = (for (i <- 0 until players) yield 1).toList
    println("Initializing server key")
    val server_key = 1 

    println("Initializing auction with " + players + " players and " + bids + " bids")
    println()
    new Auction(players, bids, player_keys, server_key)
  }

  def takeTurn(results: Array[(Int, Int)], player: Int, bids: Int): Array[(Int, Int)] = {
    results.foreach(println)
    println("Please input your reencrypted results list.")
    var newResults = new Array[(Int, Int)](results.size / bids)
    for (i <- 0 until newResults.size) {
      newResults(i) = (readInt, readInt)
    }
    return newResults
  }

  def runAuction(auction: Auction) {
    println("Running auction")
    var results = auction.collectNodes(auction.players).map(x => (x.winner, x.price)).toArray
    for (i <- 0 until auction.players) {
      val player = auction.players - 1 - i
      println("Waiting for answer from player " + player)
      results = takeTurn(results, player, auction.bids)
    }
    println("Results: " + results.toList)
  }

  def main(args: Array[String]) {
    val players = 2
    val bids = 3
    val auction = initialize(players, bids)
    println("Encrypting tree values")
    auction.encryptTree()
    runAuction(auction)
  }
}
