package jsimone

import scala.collection.mutable

object DisplayBoard {

    def displayRow(rowOrdinal: Int, boardSize: Int): Unit = {
        val sbRow = new StringBuilder
        sbRow.append(s" $rowOrdinal ")
        (0 until boardSize).foreach(i => if (i == rowOrdinal) sbRow.append("| Q ") else sbRow.append("|   "))
        sbRow.append("|")
        println(sbRow.toString())
        displayDivider(boardSize)
    }

    def displayBoard(solution: String): Unit = {
        val boardSize = solution.size
        require(boardSize >= 1 && boardSize <= 10)
        displayDivider(boardSize)
        solution.map(_.asDigit).foreach(i => displayRow(i, boardSize))
        println("\n")
    }

    def displayDivider(boardSize: Int): Unit = {
        val sbDivider = new StringBuilder
        sbDivider.append("   ")
        (0 until boardSize).foreach(i => sbDivider.append("|---"))
        sbDivider.append("|")
        println(sbDivider.toString())
    }

    def displaySolutions(variationsMap: mutable.TreeMap[String, mutable.TreeSet[String]]): Unit = {
        var i = 1
        val numUniqueSolutions = variationsMap.keys.size
        for (solution <- variationsMap.keys) {
            println(s"Unique Solution $i of $numUniqueSolutions:  $solution  All possible rotations and reflections/mirrors give: [${variationsMap(solution).mkString(", ")}]")
            displayBoard(solution)
            i += 1
        }
    }

}
