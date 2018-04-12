package C10_2_Examples

import scala.util.Sorting

object SnowSki {
  val N = 5
  val H = Array(
    Array(1,2,3,4,5),
    Array(16,17,18,19,6),
    Array(15,24,25,20,7),
    Array(14,23,22,21,8),
    Array(13,12,11,10,9)
  )

  /**
    * 得到排序之后的索引值
    *
    * @return
    */
  def FindSortedIndex: Array[(Int,Int)] = {
    val elems = H.toList.flatten.sorted
    println(elems)
    val sorted_index = Array.fill(N*N)((0,0))
    for{
      i <- 0 until N
      j <- 0 until N
    } yield{
      val idx = elems.indexOf(H(i)(j))
      sorted_index(idx) = (i,j)
    }
    sorted_index
  }

  /**
    * Update element at (i,j) with surroundings ( By the people)
    * 人人为我
    */
  def Update(L:Array[Array[Int]], i:Int, j:Int):Unit = {
    //left
    if(i >0 && H(i)(j)>H(i-1)(j)) {
      if (L(i)(j) < L(i-1)(j) +1)
        L(i)(j) = L(i-1)(j) + 1
    }

    //up
    if(j >0 && H(i)(j)>H(i)(j-1)) {
      if (L(i)(j) < L(i)(j-1) +1)
        L(i)(j) = L(i)(j-1) + 1
    }

    //right
    if(i < N-1 && H(i)(j)>H(i+1)(j)) {
      if (L(i)(j) < L(i+1)(j) +1)
        L(i)(j) = L(i+1)(j) + 1
    }

    //down
    if(j < N -1 && H(i)(j)>H(i)(j+1)) {
      if (L(i)(j) < L(i)(j+1) +1)
        L(i)(j) = L(i)(j+1) + 1
    }
  }

  /**
    * Update surrounding element with (i,j) ( For the people, 我为人人)
    *
    */
  def Update1(L:Array[Array[Int]], i:Int, j:Int):Unit = {
    //left
    if(i >0 && H(i)(j) < H(i-1)(j)) {
      if (L(i)(j) + 1 > L(i-1)(j))
        L(i-1)(j) = L(i)(j) + 1
    }

    //up
    if(j >0 && H(i)(j)<H(i)(j-1)) {
      if (L(i)(j) + 1 > L(i)(j-1) )
        L(i)(j-1) = L(i)(j) + 1
    }

    //right
    if(i < N-1 && H(i)(j)<H(i+1)(j)) {
      if (L(i)(j) + 1 > L(i+1)(j) )
        L(i+1)(j) = L(i)(j) + 1
    }

    //down
    if(j < N -1 && H(i)(j)<H(i)(j+1)) {
      if (L(i)(j)+1 > L(i)(j+1) )
        L(i)(j+1) = L(i)(j) + 1
    }
  }

  def CalculateMaxLength= {
    val L = Array.fill(N,N)(1)
    val indexes = FindSortedIndex
    for((i,j) <- indexes) {
      //检查四周的点
      Update1(L, i,j)
    }

    println("\nH=")
    for(i<- 0 until N; j <- 0 until N){
      if (j == 0 && i > 0) println()
      print(H(i)(j) + " ")
    }
    println("\nL=")
    for(i<- 0 until N; j <- 0 until N){
      if (j == 0 && i > 0) println()
      print(L(i)(j) + " ")
    }
  }
  def main(args: Array[String]): Unit = {
    CalculateMaxLength
  }
}
