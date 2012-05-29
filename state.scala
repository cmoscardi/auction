object State {
  def genStates(index: Int, bids: Int): Array[State] =
    if (index > 0)
      (for (i <- 0 until index;
            j <- 1 to bids;
            k <- 1 to j)
          yield new State(i, j, k)).toArray
    else
      Array(new State(0, 1, 1))

  def fromString(string: String): State = {
    val values = string.split(" ")
    new State(values(0).toInt, values(1).toInt, values(2).toInt, BigInt(values(3)), BigInt(values(4)),BigInt(values(5)),BigInt(values(6)),BigInt(values(7))) 
  }

}

class State(
    val high_bidder: Int,
    val high_bid: Int,
    val next_bid: Int,
    var winner_enc: BigInt,
    var price_enc: BigInt,
    var c_1_winner: BigInt,
    var c_1_price: BigInt,
    var pub_key:BigInt) {
  def this(high_bidder: Int, high_bid: Int, next_bid: Int) =
    this(high_bidder, high_bid, next_bid, BigInt(high_bidder), BigInt(next_bid),BigInt(0),BigInt(0),BigInt(0))

  def encryptState(key: BigInt, modulus:BigInt,generator:BigInt) {
    winner_enc = Cryptography.encrypt(key, modulus, generator, BigInt(high_bidder))
    price_enc = Cryptography.encrypt(key, modulus, generator, BigInt(next_bid))
  }

  def decryptState(key:BigInt, modulus:BigInt, generator:BigInt) { 
   
    winner_enc = Cryptography.decrypt(c_1_winner,key,generator,modulus,winner_enc)
    price_enc = Cryptography.decrypt(c_1_price,key,generator,modulus,price_enc)
  }

  def reencryptState(key: BigInt, modulus:BigInt,q:BigInt, generator:BigInt) {
    //println("this state is == " + toString())
    val winner_things = Cryptography.recrypt(c_1_winner,
					 key,
					 pub_key,
					 q,
					 generator, 
					 modulus,
					 
					 winner_enc)

    val price_enc_things = Cryptography.recrypt(c_1_price,
					    key,
					    pub_key,
					    q,
					    generator, 
					    modulus, 
					    price_enc)

    winner_enc = winner_things._1
    c_1_winner = winner_things._2

    price_enc = price_enc_things._1
    c_1_price = price_enc_things._2

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
    List(high_bidder, high_bid, next_bid, winner_enc, price_enc,c_1_winner,c_1_price,pub_key).map(_.toString).reduceLeft((x, y) => x + " " + y)

  override def equals(other: Any): Boolean = {
    other match {
      case that: State => this.high_bidder == that.high_bidder &&
                       this.high_bid == that.high_bid &&
                       this.next_bid == that.next_bid
      case _ => false
    }
  }
}
