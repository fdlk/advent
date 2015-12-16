package day4

object day4 {
  import java.security.MessageDigest

  val digest: MessageDigest = MessageDigest.getInstance("MD5")
                                                  //> digest  : java.security.MessageDigest = MD5 Message Digest from SUN, <initia
                                                  //| lized>
                                                  //| 
  digest.update("iwrupvqb".getBytes)
  def initializedDigest: MessageDigest = digest.clone.asInstanceOf[MessageDigest]
                                                  //> initializedDigest: => java.security.MessageDigest
  def hash1(text: String): Boolean = initializedDigest.digest(text.getBytes).map("%02x".format(_)).mkString.startsWith("00000")
                                                  //> hash1: (text: String)Boolean
  Stream.from(1) map (_.toString) find hash1      //> res0: Option[String] = Some(346386)
  def hash2(text: String): Boolean = initializedDigest.digest(text.getBytes).take(3).sameElements(Array(0, 0, 0))
                                                  //> hash2: (text: String)Boolean
  Stream.from(1) map (_.toString) find hash2      //> res1: Option[String] = Some(9958218)
}