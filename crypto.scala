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

  //strips and re-randomizes
  def recrypt(c_1:BigInt, 
	      key: BigInt, 
	      pub_key:BigInt,
	      generator:BigInt, 
	      modulus:BigInt, 
	      value:BigInt):(BigInt,BigInt,BigInt) = { 
    val q = (modulus-BigInt(1))/BigInt(2)
    val inv = c_1.modPow(q-key,modulus)
    val rnd = new Random()
    //somewhat abusive of the priv_key function
    val y = priv_key(q,rnd)
    val new_c_1 = (c_1 * generator.modPow(y,modulus))%modulus
    val new_pub_key = (pub_key * generator.modPow(q-key,modulus))%modulus
    val new_encryption = ((value * inv) * new_pub_key.modPow(y,modulus))%modulus    
    (new_encryption, new_c_1, new_pub_key)
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
