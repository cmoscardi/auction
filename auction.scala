class Auction(
      val players: Int,
      val bids: Int,
      val player_keys: List[BigInt],
      val server_key: BigInt,
      val modulus: BigInt,
      val generator: BigInt){ 
      
  var states = State.genStates(players, bids)
  var current_pub_key = BigInt(0)

  def encryptTree {
    current_pub_key = (server_key :: player_keys).reduceLeft( (a,b) => (a*b)%modulus)
    for (state <- states) {
      state.encryptState(current_pub_key, modulus, generator)
      //i am assuming this is acceptable
      //it sets y=1, since c1 = g^y (in this case g^1)
      state.c_1_winner = generator
      state.c_1_price = generator
    }
  }
    

}
