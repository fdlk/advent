package day4

object day4 {
  import java.security.MessageDigest

  val digest = MessageDigest.getInstance("MD5")   //> digest  : java.security.MessageDigest = MD5 Message Digest from SUN, <initia
                                                  //| lized>
                                                  //| 
	def hash(text:String):String = digest.digest(text.getBytes).take(6).map("%02x".format(_)).mkString
                                                  //> hash: (text: String)String
	Stream.from(1).find { x => hash("iwrupvqb" + x).startsWith("000000") }
                                                  //> res0: Option[Int] = Some(9958218)
}