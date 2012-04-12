
/**
 * Should children be an array or list? A
 * question for the ages
 * */
class Node(parent:Node, children:Array[Node]) { 
  //plaintext state variables
  var p = parent
  var c = children
  
  
  //state variables to be encrypted
  //they will be large numbers
  var high_user = 0
  var high_bid = 0 
  var next_bid = 0


  //obviously it won't be an Int...
  def encryptState(key: Int):void { 
    

  }
  //same here
  def decryptState(privateKey: Int):void { 
    

  }

  /*
   * @args bids the number of bidding options.
   * It is up to you to ensure that bidding amounts
   * map to the integers 0..(number-1)
   * [or should it be 1..number?]
   *
  *  @args users the number of users you will have
  *
  *
  *
  *  This is the brute force way which isn't very
  *  efficient, namely because it's exponential
  *  (bids ^ users)
  *
  *
  * 
   * */
  def generateTree(bids:Int, users:Int):void { 
    if(users == 0){ 
      return;
    }
    for(i <- 0 until bids){
      c[i] = new Node(this,new Node[bids])
      c[i].generateTree(bids, users-1)
    }
  }

  def collectLevel(int level, int position):Array[Node] { 
    if(position==level){ 
      
    }
    
  }


}
