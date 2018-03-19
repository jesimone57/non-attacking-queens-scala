import jsimone.{BoardUtil, DisplayBoard}

class Application

object Application {
    def main(args: Array[String]): Unit = {

        var boardSize = 8
        if (args.length == 1) {
            try
                boardSize = args(0).toInt
            catch {
                case ex: NumberFormatException =>
                    println("Usage is: java Application n")
                    println("Where n = board size.  n must be in the range of 1 through 10.  n is optional and if omitted defaults to 8")
                    return
            }
            if (boardSize < 1 || boardSize > 10) {
                println(s"board size $boardSize is not in the range of 1 through 10")
                return
            }
        }

        val ordinalEncoding = (0 until boardSize).mkString
        println(s"For a $boardSize x $boardSize board ...")
        println("Ordinal Encoding of Queen Positions: " + ordinalEncoding)

        val solutions = BoardUtil.solve(boardSize)
        val uniqueSolutions = BoardUtil.mapSolutionsIntoUniqueSets(solutions)

        println(s"There are ${solutions.size} solutions.\n")
        println("NOTES:  Using an index origin of 0 labels the leftmost file/column of the chess board as 0.")
        println("\tEach solution has its board labled on the left with the ordinal encoding.")
        println("\tSolutions encoded by the file/column ordinal of the Queen's position on the board:")
        println(s"\t[ ${solutions.mkString(", ")}]")

        println(s"\nHowever, there are only ${uniqueSolutions.size} unique solutions by taking into account all possible rotations and reflections/mirrors of the board.\n")

        DisplayBoard.displaySolutions(uniqueSolutions)
    }
}
