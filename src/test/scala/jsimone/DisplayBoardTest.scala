package jsimone

import org.scalatest.{FlatSpec, Matchers}

class DisplayBoardTest extends FlatSpec with Matchers {

    "Display Row" should "display row correctly for board size 4" in {
       DisplayBoard.displayRow(0, 4)
    }

    "Display Board" should "display board correctly for a 4x4 board" in {
        DisplayBoard.displayBoard("1302")
    }

    "Display Solutions" should "display all solutions correctly for a 4x4 board" in {
        val solutions = BoardUtil.solve(4)
        val uniqueSolutions = BoardUtil.mapSolutionsIntoUniqueSets(solutions)
        DisplayBoard.displaySolutions(uniqueSolutions)
    }

    it should "display all solutions correctly for a 5x5 board" in {
        val solutions = BoardUtil.solve(5)
        val uniqueSolutions = BoardUtil.mapSolutionsIntoUniqueSets(solutions)
        DisplayBoard.displaySolutions(uniqueSolutions)
    }

    it should "display all solutions correctly for an 8x8 board" in {
        val solutions = BoardUtil.solve(8)
        val uniqueSolutions = BoardUtil.mapSolutionsIntoUniqueSets(solutions)
        DisplayBoard.displaySolutions(uniqueSolutions)
    }

}
