package jsimone

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object BoardUtil {

    def flipYAxis(list: List[Int], boardSize: Int): List[Int] =
        list.map(i => boardSize-1 - i)

    def flipXAxis(list: List[Int]): List[Int] =
        list.reverse

    def rotate(list: List[Int], boardSize: Int): List[Int] = {
        val col = ArrayBuffer.fill(list.size)(-1)
        list.zipWithIndex.foreach{ case (row,  i) => col(row) = i }
        col.toList
    }
}
