import scala.util.Random

object Cryptography {
  val BIT_SIZE = 128
  def encrypt(key: BigInt, modulus:BigInt, value: BigInt) = {
    (key * value) % modulus
  }

  def decrypt(c_1:BigInt, priv_key: BigInt, generator:BigInt, modulus:BigInt, value: BigInt) = {
    val q = ((modulus-BigInt(1))/2) - priv_key
    val multiply = generator.modPow(q,modulus)
    (value * multiply)%modulus
  }

  //decrypts and re-randomizes, returns 1) h^(r1*r2), and 2) the new message with one x_i removed
  def recrypt(c_1:BigInt, 
	      key: BigInt, 
	      generator:BigInt, 
	      modulus:BigInt, 
	      value:BigInt):(BigInt,BigInt) = { 
    val inv = c_1.modPow(((modulus-BigInt(1))/BigInt(2))-key,modulus)
    val rnd = new Random()
    //somewhat abusive of the priv_key function
    val y = priv_key(modulus,rnd)

    val new_c_1 = c_1.modPow(y,modulus)
    val new_encryption = (value * inv).modPow(y,modulus)
    (new_encryption, new_c_1)
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
