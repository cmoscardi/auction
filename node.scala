
/**
 * Should children be an array or list? A
 * question for the ages
 * */
class Node(
    val parent: Node,
    val children: Array[Node],
    val high_user: Int,
    val next_bid: Int) { 
  var winner = 0
  var price  = 0

  def initState() {
    winner = high_user
    price = next_bid
  }
  def encryptState(key: Int) {
    winner = Cryptography.encrypt(key, winner)
    price = Cryptography.encrypt(key, price)
  }
  
  def decryptState(key: Int) { 
    winner = Cryptography.decrypt(key, winner)
    price = Cryptography.decrypt(key, price)
  }
}
