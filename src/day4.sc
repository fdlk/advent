import java.security.MessageDigest
object day4 {
  val digest: MessageDigest = MessageDigest.getInstance("MD5")
  digest.update("iwrupvqb".getBytes)
  def initializedDigest = digest.clone.asInstanceOf[MessageDigest]

  def hash1(text: String): Boolean = initializedDigest.digest(text.getBytes).map("%02x".format(_)).mkString.startsWith("00000")
  val hash1: Option[String] = Stream.from(1) map (_.toString) find hash1
  def hash2(text: String): Boolean = initializedDigest.digest(text.getBytes).take(3).sameElements(Array(0, 0, 0))
  Stream.from(hash1.get.toInt) map (_.toString) find hash2
}