package jsimone


import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object BoardUtil {

    def flipYAxis(list: List[Int]): List[Int] =
        list.map(i => (list.size - 1) - i)

    def flipYAxis(list:String): String =
        list.map(_.asDigit).map(i => (list.size - 1) - i).mkString

    def flipXAxis(list: List[Int]): List[Int] =
        list.reverse

    def flipXAxis(list: String): String =
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
            for (index <- 1 until list.size - row) {
                val above = list(row) + index
                val below = list(row) - index
                val offSet = row + index
                //println(s"code=${list(row)} above=$above below=$below ($row,$index)->${list(offSet)} $offSet")
                if (offSet < list.size && (above == list(offSet) || below == list(offSet))) return true
            }
        }
        false
    }

    def isAttackingOnDiagonal(list: String): Boolean = {
        isAttackingOnDiagonal(list.map(_.asDigit).toList)
    }

    def solve(boardSize: Int): List[String] = {
        require( boardSize <= 10 && boardSize >= 2)
        val code = Range(0, 9).mkString.take(boardSize)
        code.permutations.filter( ! isAttackingOnDiagonal(_)).toList
    }

    private def addVariationToSet(variation: String, variationSet:  mutable.TreeSet[String], resultsAlreadyAccountedFor: mutable.TreeSet[String]): Unit = {
        if (! resultsAlreadyAccountedFor.contains(variation) && !variationSet.contains(variation)) {
            variationSet += variation
            resultsAlreadyAccountedFor += variation
        }
    }

    def mapSolutionsIntoUniqueSets(solutions: List[String]): mutable.TreeMap[String, mutable.TreeSet[String]] = {
        val pruned = new mutable.TreeMap[String, mutable.TreeSet[String]]()
        val resultsAlreadyAccountedFor = new mutable.TreeSet[String]()
        for (solution <- solutions) {
            if (! resultsAlreadyAccountedFor.contains(solution)) {
                val variationSet = new mutable.TreeSet[String]()

                // add all the variations for this particular result, if they have not already been accounted for
                addVariationToSet(solution, variationSet, resultsAlreadyAccountedFor)
                addVariationToSet(flipXAxis(solution), variationSet, resultsAlreadyAccountedFor)
                addVariationToSet(flipYAxis(solution), variationSet, resultsAlreadyAccountedFor)
                addVariationToSet(flipXAxis(flipYAxis(solution)), variationSet, resultsAlreadyAccountedFor)
                addVariationToSet(rotate(solution), variationSet, resultsAlreadyAccountedFor)
                addVariationToSet(flipXAxis(rotate(solution)), variationSet, resultsAlreadyAccountedFor)
                addVariationToSet(flipYAxis(rotate(solution)), variationSet, resultsAlreadyAccountedFor)
                addVariationToSet(flipXAxis(flipYAxis(rotate(solution))), variationSet, resultsAlreadyAccountedFor)

                println(s"$solution $variationSet")
                pruned += (solution -> variationSet)
            }
        }
        pruned
    }
}
