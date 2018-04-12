package C8_03_chessboard_split

//Video at http://www.bilibili.com/video/av10046345/index_8.html#page=8
object ChessBoardSplit {
  val N = 3 // Number of split
  val Row = 8
  val Col = 8
  val Board = Array[Array[Int]](
    Array(1,1,1,1,1,1,1,3), //Row 1
    Array(1,1,1,1,1,1,1,1), //Row 2
    Array(1,1,1,1,1,1,1,1), //Row 3
    Array(1,1,1,1,1,1,1,1), //Row 4
    Array(1,1,1,1,1,1,1,1), //Row 5
    Array(1,1,1,1,1,1,1,1), //Row 6
    Array(1,1,1,1,1,1,1,0), //Row 7
    Array(1,1,1,1,1,1,0,3) //Row 8
  )

  var sums = Board
  for {
    i <- 0 until Row
    j <- 0 until Col
  } {
   sums(i)(j) = if (i==0 && j ==0) Board(i)(j)
   else if(i==0) sums(i)(j-1) + Board(i)(j)
   else if(j==0) sums(i-1)(j) + Board(i)(j)
   else sums(i)(j-1) + sums(i-1)(j) + Board(i)(j) -sums(i-1)(j-1)

    if (j == 0) println()
    print(" "+ sums(i)(j))
  }
  println()

  /**
    * Sum up the scores given the left-up corner and right-down corner of the board
    *
    * @param x1
    * @param y1
    * @param x2
    * @param y2
    * @return
    */
  def calc_sum(x1:Int, y1: Int, x2:Int, y2:Int): Int ={
    if(x1==0 && y1 ==0) sums(x2)(y2)
    else if(x1 ==0) sums(x2)(y2) - sums(x2)(y1-1)
    else if(y1 ==0) sums(x2)(y2) - sums(x1-1)(y2)
    else
      sums(x2)(y2) - sums(x2)(y1-1) - sums(x1-1)(y2) + sums(x1-1)(y1-1)
  }

  val score_map = scala.collection.mutable.Map[(Int,Int,Int,Int,Int),Int]()

  def set_score(n:Int, x1: Int, y1: Int, x2:Int, y2:Int, score: Int) = score_map.put((n,x1,y1,x2,y2),score)
  def get_score(n:Int, x1: Int, y1: Int, x2:Int, y2:Int) = score_map.get((n,x1,y1,x2,y2))

  /**
    * Split the board of given left-up and right-down corners into
    * n pieces.
    * @param n
    * @param x1
    * @param y1
    * @param x2
    * @param y2
    */
  def split(n: Int, x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    get_score(n,x1,y1,x2,y2) match {
      case Some(x) => x
      case None => {
        if (n == 1) {
          val sum = calc_sum(x1,y1,x2,y2)
          val s = sum * sum
          set_score(n,x1,y1,x2,y2,s )
          s
        }else{
          val score_1 = try_plan_1(n, x1, y1, x2, y2)
          val score_2 = try_plan_2(n, x1, y1, x2, y2)
          val score_3 = try_plan_3(n, x1, y1, x2, y2)
          val score_4 = try_plan_4(n, x1, y1, x2, y2)

          val s: Int = List(score_1, score_2, score_3, score_4).min
          set_score(n, x1, y1, x2, y2, s)
          s
        }
      }
    }
  }

  def try_plan_1(n: Int, x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    if (x1 == x2) Int.MaxValue // No way to split with this plan
    else {
      val scores = for {
        x <- x1 until x2
      } yield {
        //drop x1->x, keep x+1 ->x2
        split(1, x1, y1, x, y2) + split(n - 1, x + 1, y1, x2, y2)
      }
      scores.min
    }
  }

  def try_plan_2(n: Int, x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    if (x1 == x2) Int.MaxValue
    else {
      val scores = for {
        x <- x1 until x2
      } yield {
        //keep x1->x, drop x+1 ->x2
        split(n - 1, x1, y1, x, y2) + split(1, x + 1, y1, x2, y2)
      }
      scores.min
    }
  }

  def try_plan_3(n: Int, x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    if (y1 == y2) Int.MaxValue
    else {
      val scores = for {
        y <- y1 until y2
      } yield {
        //drop y1->y, keep y+1 ->y2
        split(1, x1, y1, x2, y) + split(n - 1, x1, y + 1, x2, y2)
      }
      scores.min
    }
  }

  def try_plan_4(n: Int, x1: Int, y1: Int, x2: Int, y2: Int): Int = {
    if (y1 == y2) Int.MaxValue
    else {
      val scores = for {
        y <- y1 until y2
      } yield {
        //keep y1->y, drop y+1 ->y2
        split(n - 1, x1, y1, x2, y) + split(1, x1, y + 1, x2, y2)
      }
      scores.min
    }
  }

  def main(args: Array[String]): Unit = {
    val squared_sum = split(N,0,0,Row-1,Col-1)
    val sum = sums(Row-1)(Col-1)
    val ret = N * squared_sum - sum * sum
    println("squared_sum="+squared_sum)
    println("sum="+sum)
    println("ret="+ret)
    println("result="+ math.sqrt(ret.toFloat/(N*N).toFloat))
  }
}
