class Auction(
      val players: Int,
      val bids: Int,
      val player_keys: List[Int],
      val server_key: (Int, Int)) {
  var states = State.genStates(players, bids)

  def encryptTree =
    for (state <- states)
      state.encryptState((server_key._1 :: player_keys).sum)
}
