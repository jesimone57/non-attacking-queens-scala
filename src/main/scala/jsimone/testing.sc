
val a = List(0,4,7,5,2,6,1,3)
val positions: List[(Int,Int)] = a.zipWithIndex

val row = 4
val col = 1
val aboveRight = Range(row+1, a.size)
val belowRight = Range(row-1, a.size)

val aboveLeft = Range(row+1, 0, -1)
val belowLeft = Range(row-1, 0, -1)

positions.foreach{ case (row, col) =>
        println(s"($row, $col)")







}