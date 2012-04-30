import scala.util.Random

object Cryptography {
  val BIT_SIZE = 256
  def encrypt(key: BigInt, modulus:BigInt, value: BigInt) = {
    (key * value) % modulus
  }

  def decrypt(priv_key: BigInt, generator:BigInt, modulus:BigInt, value: BigInt) = {
    val q = ((modulus-BigInt(1))/2) - priv_key
    val multiply = generator.modPow(q,modulus)
    (value * multiply)%modulus
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
