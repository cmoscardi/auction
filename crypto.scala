import scala.util.Random

object Cryptography {
  val BIT_SIZE = 512
  def encrypt(key: BigInt, modulus:BigInt, generator:BigInt, message: BigInt) = {
    (key * generator.modPow(message,modulus)) % modulus
  }

  def decrypt(c_1:BigInt, priv_key: BigInt, generator:BigInt, modulus:BigInt, value: BigInt) = {
    val q = ((modulus-BigInt(1))/2) - priv_key
    val multiply = c_1.modPow(q,modulus)
    (value * multiply)%modulus
  }
  def strip_pub_key(priv_key:BigInt, pub_key:BigInt, p:BigInt,q:BigInt,g:BigInt) = { 
    val inv = g.modPow(q-priv_key,p)
    (pub_key*inv) % p
  }
  //strips and re-randomizes
  def recrypt(c_1:BigInt,
	      value:BigInt, 
	      key: BigInt, 
	      pub_key:BigInt,
	      p:BigInt,
	      q:BigInt,
	      g:BigInt):(BigInt,BigInt) = { 
    val inv = c_1.modPow(q-key,p)
    val rnd = new Random()
    //somewhat abusive of the priv_key function
    val y = priv_key(q,rnd)
    val new_c_1 = (c_1 * g.modPow(y,p))%p
    val new_encryption = ((value * inv) * pub_key.modPow(y,p))%p    
    (new_c_1,new_encryption)
  }
  
  //given g^x, finds x. 
  def findPow(value:BigInt,
	      g:BigInt,
	      p:BigInt):Int = { 
      var number = g
      var power = 1
      var found = false
      while(!found){
	if(number==value){
	  found = true
	}
	else {
	  number = number * g
	  power = power +1
	}
      }
      power
  }
  

  //returns (p,q,g)   
  def genPrime():(BigInt , BigInt, BigInt) = {
    val rnd = new Random()
    var prime = BigInt.probablePrime(BIT_SIZE, rnd)
    var safe = false
    while(!safe){
      if(((prime-BigInt(1))/BigInt(2)).isProbablePrime(100))
        safe = true
      else
        prime = BigInt.probablePrime(BIT_SIZE,rnd)
    }
    (prime,(prime-BigInt(1))/BigInt(2), findGenerator(prime))

  }

  //finds a generator for q-order multiplicative 
  //subgroup, not p. in practice almost always 2 or 3
  def findGenerator(prime: BigInt):BigInt = {
    var generator = false
    var counter = 2
    while(!generator){
        if(BigInt(counter).modPow((prime-BigInt(1))/BigInt(2),prime)==BigInt(1)){
	  generator = true
 	}
	else {
	  counter=counter+1
	}
    }
    BigInt(counter)
  }

  def priv_key(prime: BigInt, rnd:Random):BigInt = {
    val key = BigInt(prime.bitLength,rnd)
    if(key<prime)
      key
    else
      priv_key(prime,rnd)

  }

}
