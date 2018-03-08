import scala.collection.mutable._

val list = List(0, 2, 1)

var col = new ListBuffer[Int]
val col2 = new ArrayBuffer[Int]()
val col3 = ArrayBuffer.fill(list.size)(-1)
col3(0)

list
list.zipWithIndex
list.zipWithIndex.foreach{ case (row,  i) => println(s"col($row) = $i") }
list.zipWithIndex.foreach{ case (row,  i) => col3(row) = i }

col3
col3.toList

import jsimone.BoardUtil._
rotate(List(2,0,1))
rotate("201".map(_.asDigit).toList).mkString