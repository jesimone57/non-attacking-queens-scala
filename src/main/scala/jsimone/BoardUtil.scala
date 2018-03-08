package jsimone

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object BoardUtil {

    def flipYAxis(list: List[Int]): List[Int] =
        list.map(i => (list.size-1) - i)

    def flipXAxis(list: List[Int]): List[Int] =
        list.reverse

    /**
      * Rotate a board clock-wise by 90 degrees
      * @param list List of Integers representing board
      * @return
      */
    def rotate(list: List[Int]): List[Int] = {
        val col = ArrayBuffer.fill(list.size)(-1)
        list.zipWithIndex.foreach{ case (row,  i) => col(row) = list.size-1 - i}
        col.toList
    }

    def rotate(list: String): String = {
        rotate(list.map(_.asDigit).toList).mkString
    }
}
