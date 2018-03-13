
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

(0 until 8).foreach[Boolean]{ i => if (i == 5)  true else false}

def f(list: List[Int]): Boolean = {
    for (row <- 0 until list.size) {
        for (col <- 1 until list.size) {
            val above = list(row) + col
            val below = list(row) - col
            //println(s"code=${list(row)} above=$above below=$below ($row,$col)->${list(col)} ${row+col} ${row-col}")
            if (//row != col &&
                ( row+col< list.size && above == list(row+col)) ||
                ( row+col<list.size && below == list(row+col))) {
                return true
            }
        }
    }
    false
}

f(List(3,4))

f(List(0,4,7,5,2,6,1,3))

f(List(0,1))
f(List(1,0))
f(List(1,0,2))
f(List(1,2,0))


a(1)

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