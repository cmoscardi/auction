object State {
  def genStates(index: Int, bids: Int): Array[State] =
    if (index > 0)
      (for (i <- 0 until index;
            j <- 0 until bids;
            k <- 0 to j)
          yield new State(i, j, k)).toArray
    else
      Array(new State(0, 0, 0))

  def fromString(string: String): State = {
    val values = string.split(" ")
    new State(values(0).toInt, values(1).toInt, values(2).toInt, BigInt(values(3)), BigInt(values(4))) 
  }

}

class State(
    val high_bidder: Int,
    val high_bid: Int,
    val next_bid: Int,
    var winner_enc: BigInt,
    var price_enc: BigInt) {
  def this(high_bidder: Int, high_bid: Int, next_bid: Int) =
    this(high_bidder, high_bid, next_bid, BigInt(0), BigInt(0))

  def encryptState(key: BigInt, modulus:BigInt) {
    winner_enc = Cryptography.encrypt(key, modulus, BigInt(high_bidder))
    price_enc = Cryptography.encrypt(key, modulus, BigInt(next_bid))
  }

  def reencryptState(key: BigInt, modulus:BigInt, generator:BigInt) {
    winner_enc = Cryptography.decrypt(key,generator, modulus, winner_enc)
    price_enc = Cryptography.decrypt(key,generator, modulus, price_enc)
  }

  def afterBid(player: Int, bid: Int): State = {
    var highBidder = high_bidder
    var highBid = high_bid
    var nextBid = next_bid
    if (bid >= high_bid) {
      nextBid = high_bid
      highBidder = player
      highBid = bid
    } else if (bid > next_bid) {
      nextBid = bid
    }
    new State(highBidder, highBid, nextBid)
  }

  override def toString(): String =
    List(high_bidder, high_bid, next_bid, winner_enc, price_enc).map(_.toString).reduceLeft((x, y) => x + " " + y)

  override def equals(other: Any): Boolean = {
    other match {
      case that: State => this.high_bidder == that.high_bidder &&
                       this.high_bid == that.high_bid &&
                       this.next_bid == that.next_bid
      case _ => false
    }
  }
}
