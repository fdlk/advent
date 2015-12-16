package day4

object day4 {
  import java.security.MessageDigest

  val digest: MessageDigest = MessageDigest.getInstance("MD5")
                                                  //> digest  : java.security.MessageDigest = MD5 Message Digest from SUN, <initia
                                                  //| lized>
                                                  //| 
  digest.update("iwrupvqb".getBytes)
  digest                                          //> res0: java.security.MessageDigest = MD5 Message Digest from SUN, <in progres
                                                  //| s>
                                                  //| 
  def hash(text: String): Boolean = digest.clone.asInstanceOf[MessageDigest].digest(text.getBytes).take(3).sameElements(Array(0,0,0))
                                                  //> hash: (text: String)Boolean
  Stream.from(1).find { x => hash(x.toString) }   //> res1: Option[Int] = Some(9958218)
}