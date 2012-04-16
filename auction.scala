class Auction(
      val players: Int,
      val bids: Int,
      val player_keys: List[Int],
      val server_key: Int) {
  val tree = genTree()

  def genTree(): Node = genTreeRec(null, 0, -1, -1, -1)
  def genTreeRec(parent: Node, n: Int, first: Int, second: Int, winner: Int): Node = {
    if (n < players) {
      val node = new Node(parent, new Array[Node](bids), winner, second)
      for (bid <- 0 until bids) {
        var newWinner = if (bid > first) n else winner
        var newFirst = if (bid > first) bid else first
        var newSecond = if (bid > first) first else if (bid > second) bid else second
        node.children(bid) = genTreeRec(node, n + 1, newFirst, newSecond, newWinner)
      }
      node
    } else {
      new Node(parent, null, winner, second)
    }
  }

  def collectNodes(level: Int): List[Node] = collectNodesRec(tree, level)
  def collectNodesRec(node: Node, level: Int): List[Node] = {
    if (level == 0) {
      node :: Nil
    } else {
      node.children.map(collectNodesRec(_, level - 1)).reduceLeft(_ ::: _)
    }
  }

  def encryptTree() {
    for (node <- collectNodes(players)) {
      node.initState()
      for (key <- server_key :: player_keys)
        node.encryptState(key)
    }
  }
}
