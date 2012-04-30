class Auction(
      val players: Int,
      val bids: Int,
      val player_keys: List[BigInt],
      val server_key: BigInt,
      val modulus: BigInt) {
  var states = State.genStates(players, bids)

  def encryptTree {
    val big_key = (server_key :: player_keys).reduceLeft( (a,b) => (a*b)%modulus)
    for (state <- states) {
      state.encryptState(big_key, modulus)
    }
  }
    
}
