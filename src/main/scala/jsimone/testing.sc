
def isPrime(n: Int): Boolean = {
        import scala.math
        val oddDivisors = for (i <- 3 to n by 2 if i <= Math.sqrt(n)) yield(i)
        val divisors = Vector(2) ++ oddDivisors
        val divisableBy = for (d <- divisors if n>1) yield n>d && n%d == 0
        if (divisableBy.isEmpty) false else ! divisableBy.reduceLeft((a:Boolean, b:Boolean) => a || b)
}

//def sum(list: List[Int]): Int = list.foldLeft(0)((r,c) => r+c)
//def sum(list: List[Int]): Int = list.foldLeft(0)(_+_)

val a = List(0,4,7,5,2,6,1,3)
val positions: List[(Int,Int)] = a.zipWithIndex

val row = 4
val col = 1
val aboveRight = Range(row+1, a.size)
val belowRight = Range(row-1, a.size)

val aboveLeft = Range(row+1, 0, -1)
val belowLeft = Range(row-1, 0, -1)

val next = 1 + 1


positions.foreach { case (row, col) =>
        println(s"($row, $col)")
        val q = positions.foldLeft(true)((left, right) => left &&
                  right._2 > col &&  // bext obe over
                  right._1 + 1 < 8 &&
                  right._1 - 1 > -1 &&
                  right._1 + 1 != row &&
                  right._1 - 1 != row)
        println(q)
}