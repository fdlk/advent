package day4

object day4 {
  import java.security.MessageDigest

  val digest = MessageDigest.getInstance("MD5")   //> digest  : java.security.MessageDigest = MD5 Message Digest from SUN, <initia
                                                  //| lized>
                                                  //| 

	def hash(text:String):String = digest.digest(text.getBytes).map("%02x".format(_)).mkString
                                                  //> hash: (text: String)String

  //Quick MD5 of text
  val text = "pqrstuv1048970"                     //> text  : String = pqrstuv1048970
  val md5hash1 = hash(text)                       //> md5hash1  : String = 000006136ef2ff3b291c85725f17325c

	val tries = (1 to 1000000000).toStream    //> tries  : scala.collection.immutable.Stream[Int] = Stream(1, ?)

	tries.find { x => hash("iwrupvqb" + x).startsWith("000000") }
                                                  //> res0: Option[Int] = Some(9958218)
}