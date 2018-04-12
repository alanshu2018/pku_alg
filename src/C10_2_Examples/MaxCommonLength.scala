package C10_2_Examples

import scala.collection.mutable

class MaxCommonLength(val s1:String, val s2: String) {
  val cache = new mutable.HashMap[(Int,Int),Int]()

  /**
    * Recursive to get the max common string length
    * @param l1
    * @param l2
    * @return
    */
  def mcl(l1:Int, l2:Int): Int = {
    if(l1 <=0 || l2 <=0) 0
    else {
      cache.get((l1,l2)) match {
        case Some(x) => x
        case None => {
          val ret = if(s1(l1-1) == s2(l2-1))
            mcl(l1-1,l2-1) + 1
          else
            List(mcl(l1-1,l2),mcl(l1,l2-1)).max
          cache.put((l1,l2),ret)
          ret
        }
      }
    }
  }

  def get = {
    mcl(s1.length,s2.length)
  }
}

object MaxCommonLength {
  def MaxLen(s1: String, s2: String): Int = {
   val mcl = new MaxCommonLength(s1,s2)
    //println("Max common length of " + s1 + " and " + s2 + " is:" +mcl.get())
    mcl.get
  }

  def main(args: Array[String]): Unit = {
    val tests = List(
      ("abcfbc","abfcab"),
      ("programming","contest"),
      ("abcd","mnp")
    )

    for(test <- tests){
      val (s1,s2) = test
      println("mcl("+s1+","+s2+")="+MaxLen(s1,s2))
    }
  }
}
