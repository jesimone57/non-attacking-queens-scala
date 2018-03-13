package jsimone

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object BoardUtil {

    def flipYAxis(list: List[Int]): List[Int] =
        list.map(i => (list.size - 1) - i)

    def flipXAxis(list: List[Int]): List[Int] =
        list.reverse

    /**
      * Rotate a board clock-wise by 90 degrees
      *
      * @param list List of Integers representing board
      * @return
      */
    def rotate(list: List[Int]): List[Int] = {
        val col = ArrayBuffer.fill(list.size)(-1)
        list.zipWithIndex.foreach { case (row, i) => col(row) = list.size - 1 - i }
        col.toList
    }

    def rotate(list: String): String = {
        rotate(list.map(_.asDigit).toList).mkString
    }

    def isAttackingOnDiagonal(list: List[Int]): Boolean = {
        for (row <- 0 until list.size) {
            for (col <- 1 until list.size) {
                val above = list(row) + col
                val below = list(row) - col
                val offSet = row + col
                //println(s"code=${list(row)} above=$above below=$below ($row,$col)->${list(col)} ${row+col} ${row-col}")
                if (offSet < list.size && (above == list(offSet) || below == list(offSet))) return true
            }
        }
        false
    }
}
