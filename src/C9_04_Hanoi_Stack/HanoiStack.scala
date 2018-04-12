package C9_04_Hanoi_Stack

import scala.collection.mutable

case class Problem(n:Int, A: Char, B: Char, C:Char)

object HanoiStack {
  val stack = scala.collection.mutable.Stack[Problem]()

  def DecomposeProblem(p: Problem): List[Problem] = {
   List(
     Problem(p.n-1,p.A,p.C,p.B),
     Problem(1,p.A,p.B,p.C),
     Problem(p.n-1,p.B,p.A,p.C)
   )
  }

  def Hanoi(stack: mutable.Stack[Problem]): Unit = {
    if(!stack.isEmpty) {
      val p = stack.pop()
      if (p.n == 1) {
        println(p.A + "->" + p.C)
        Hanoi(stack)
      } else {
        val problems = DecomposeProblem(p).reverse
        for (i <- 0 until problems.length) {
          stack.push(problems(i))
        }
        Hanoi(stack)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val problem = Problem(3, 'A', 'B', 'C')

    stack.push(problem)

    Hanoi(stack)
  }

}
