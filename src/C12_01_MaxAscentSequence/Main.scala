package C12_01_MaxAscentSequence

object Main {
  def mas(numbers: List[Int]) = {
   val maxlen = new scala.collection.mutable.ArrayBuffer[Int]()

    //Initialize all maxlen to 1
    numbers.foreach(x=>maxlen.append(1))

    //update maxlen, by the people
    /*
    for(i <- 1 until numbers.size){
      for(j <- 0 until i){
        if (numbers(i) >= numbers(j)){
          maxlen(i) = if(maxlen(j)+1 > maxlen(i)) maxlen(j) +1 else maxlen(i)
        }
      }
    }*/

    //update maxlen, for the people
    for(i <- 0 until numbers.size){
      for(j <- i+1 until numbers.size ){
        if(numbers(j)> numbers(i)){
          maxlen(j) = if (maxlen(i)+1 > maxlen(j)) maxlen(i) + 1 else maxlen(j)
        }
      }
    }
    print(maxlen)
  }

  def main(args: Array[String]): Unit = {
   val numbers = List(1, 7, 3, 5 ,9, 4, 8)
    val ret = mas(numbers)
    println(ret)
  }
}
