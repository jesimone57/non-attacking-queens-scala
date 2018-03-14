package jsimone

import org.scalatest.{FlatSpec, Matchers}

class BoardUtilTest extends FlatSpec with Matchers {

    "Flip Y" should "flip the board about Y axis" in {
        val list = List(0,2,1)
        BoardUtil.flipYAxis(list) shouldBe List(2,0,1)
    }

    "Flip X" should "flip the board about X axis" in {
        val list = List(0,2,1)
        BoardUtil.flipXAxis(list) shouldBe List(1,2,0)
    }

    "Rotate" should "rotate a 3x3 board clock-wise 90 degrees" in {
        BoardUtil.rotate(List(0,2,1)) shouldBe List(2,0,1)
        BoardUtil.rotate(List(2,0,1)) shouldBe List(1,0,2)
        BoardUtil.rotate(List(1,0,2)) shouldBe List(1,2,0)
        BoardUtil.rotate(List(1,2,0)) shouldBe List(0,2,1)
    }

    it should "rotate a 3x3 board string representation clock-wise 90 degrees" in {
        BoardUtil.rotate("021") shouldBe "201"
        BoardUtil.rotate("201") shouldBe "102"
        BoardUtil.rotate("102") shouldBe "120"
        BoardUtil.rotate("120") shouldBe "021"
    }

    it should "rotate a 5x5 board clock-wise 90 degrees" in {
        BoardUtil.rotate(List(0,2,4,1,3)) shouldBe List(4,1,3,0,2)
        BoardUtil.rotate(List(4,1,3,0,2)) shouldBe List(1,3,0,2,4)
        BoardUtil.rotate(List(1,3,0,2,4)) shouldBe List(2,4,1,3,0)
        BoardUtil.rotate(List(2,4,1,3,0)) shouldBe List(0,2,4,1,3)
    }

    it should "rotate a 5x5 board string representation clock-wise 90 degrees" in {
        BoardUtil.rotate("02413") shouldBe "41302"
        BoardUtil.rotate("41302") shouldBe "13024"
        BoardUtil.rotate("13024") shouldBe "24130"
        BoardUtil.rotate("24130") shouldBe "02413"
    }

    it should "rotate an 8x8 board clock-wise 90 degrees" in {
        BoardUtil.rotate(List(0,4,7,5,2,6,1,3)) shouldBe List(7,1,3,0,6,4,2,5)
        BoardUtil.rotate(List(7,1,3,0,6,4,2,5)) shouldBe List(4,6,1,5,2,0,3,7)
        BoardUtil.rotate(List(4,6,1,5,2,0,3,7)) shouldBe List(2,5,3,1,7,4,6,0)
        BoardUtil.rotate(List(2,5,3,1,7,4,6,0)) shouldBe List(0,4,7,5,2,6,1,3)
    }

    it should "rotate an 8x8 board string representation clock-wise 90 degrees" in {
        BoardUtil.rotate("04752613") shouldBe "71306425"
        BoardUtil.rotate("71306425") shouldBe "46152037"
        BoardUtil.rotate("46152037") shouldBe "25317460"
        BoardUtil.rotate("25317460") shouldBe "04752613"
    }

    "isAttackingOnDiagonal" should "return true for a diagonal 2x2 attack" in {
        BoardUtil.isAttackingOnDiagonal(List(0,1)) shouldBe true
        BoardUtil.isAttackingOnDiagonal(List(1,0)) shouldBe true
    }

    it should "return true for a diagonal 3x3 attack" in {
        BoardUtil.isAttackingOnDiagonal(List(1,0,2)) shouldBe true
        BoardUtil.isAttackingOnDiagonal(List(1,2,0)) shouldBe true
        BoardUtil.isAttackingOnDiagonal(List(0,2,2)) shouldBe true
        BoardUtil.isAttackingOnDiagonal(List(2,0,0)) shouldBe true
    }

    it should "return true for a diagonal 4x4 attack" in {
        BoardUtil.isAttackingOnDiagonal(List(0,2,1,3)) shouldBe true
        BoardUtil.isAttackingOnDiagonal(List(3,1,2,0)) shouldBe true
        BoardUtil.isAttackingOnDiagonal(List(0,2,1,3)) shouldBe true
    }

    it should "return false for not a diagonal attack of only 2 queens" in {
        BoardUtil.isAttackingOnDiagonal(List(0,4)) shouldBe false

        BoardUtil.isAttackingOnDiagonal(List(1,4)) shouldBe false
        BoardUtil.isAttackingOnDiagonal(List(2,4)) shouldBe false

        BoardUtil.isAttackingOnDiagonal(List(6,4)) shouldBe false
        BoardUtil.isAttackingOnDiagonal(List(7,4)) shouldBe false

        BoardUtil.isAttackingOnDiagonal(List(4,0)) shouldBe false
        BoardUtil.isAttackingOnDiagonal(List(4,1)) shouldBe false
        BoardUtil.isAttackingOnDiagonal(List(4,2)) shouldBe false

        BoardUtil.isAttackingOnDiagonal(List(4,6)) shouldBe false
        BoardUtil.isAttackingOnDiagonal(List(4,7)) shouldBe false

    }

    it should "return true for a diagonal attack of only 2 queens" in {
        BoardUtil.isAttackingOnDiagonal(List(3,4)) shouldBe true // one file over 3 + 1 = 4 -> attack
        BoardUtil.isAttackingOnDiagonal(List(5,4)) shouldBe true // one file over 5 - 1 = 4 -> attack

        BoardUtil.isAttackingOnDiagonal(List(4,3)) shouldBe true // one file over 4 - 1 = 3 -> attack
        BoardUtil.isAttackingOnDiagonal(List(4,5)) shouldBe true // one file over 4 + 1 = 5 -> attack
    }
    
    it should "return false for not a diagonal attack of only 3 queens" in {
        BoardUtil.isAttackingOnDiagonal(List(0,4,1)) shouldBe false

        BoardUtil.isAttackingOnDiagonal(List(0,4,6)) shouldBe false
        BoardUtil.isAttackingOnDiagonal(List(0,4,7)) shouldBe false

        BoardUtil.isAttackingOnDiagonal(List(1,4,0)) shouldBe false

        BoardUtil.isAttackingOnDiagonal(List(6,4,0)) shouldBe false
        BoardUtil.isAttackingOnDiagonal(List(7,4,0)) shouldBe false
    }

    it should "return true for a diagonal attack of only 3 queens" in {
        BoardUtil.isAttackingOnDiagonal(List(0,4,2)) shouldBe true // two files over 0 + 2 = 2 -> attack
        BoardUtil.isAttackingOnDiagonal(List(2,4,0)) shouldBe true // two files over 2 - 2 = 0 -> attack
    }
    
    it should "return correct result for a standard 8x8 chess board with 8 queens" in {
        BoardUtil.isAttackingOnDiagonal("04752613") shouldBe false // 1st unique solution on an 8x8 board

        // Q in col 0 -> diagonal attack Q in col 1
        BoardUtil.isAttackingOnDiagonal("01752643") shouldBe true

        // Q in col 0 -> diagonal attack Q in col 2
        BoardUtil.isAttackingOnDiagonal("04257613") shouldBe true

        // Q in col 0 -> diagonal attack Q in col 3
        BoardUtil.isAttackingOnDiagonal("04732615") shouldBe true

        // Q in col 0 -> diagonal attack Q in col 4
        BoardUtil.isAttackingOnDiagonal("02754613") shouldBe true

        // Q in col 0 -> diagonal attack Q in col 5
        BoardUtil.isAttackingOnDiagonal("04762513") shouldBe true

        // Q in col 0 -> diagonal attack Q in col 6
        BoardUtil.isAttackingOnDiagonal("04752163") shouldBe true

        // Q in col 0 -> diagonal attack Q in col 7
        BoardUtil.isAttackingOnDiagonal("04352617") shouldBe true
    }

    "Solve" should "throw IllegalArgumentException for invalid board sizes" in {
        assertThrows[IllegalArgumentException] {
            BoardUtil.solve(0)
        }
        assertThrows[IllegalArgumentException] {
            BoardUtil.solve(1)
        }
        assertThrows[IllegalArgumentException] {
            BoardUtil.solve(11)
        }
    }

    it should "solve for all solutions of the n queen problem for board size 4" in {
        val solutions = BoardUtil.solve(4)
        println("board size 4 solutions -> "+solutions)
        solutions.size shouldBe 2

        validateRotationsAndMirrors("1302", solutions)
        validateRotationsAndMirrors("2031", solutions)
    }

    it should "solve for all solutions of the n queen problem for board size 5" in {
        val solutions = BoardUtil.solve(5)
        println("board size 5 solutions -> "+solutions)
        solutions.size shouldBe 10

        validateRotationsAndMirrors("02413", solutions)
        validateRotationsAndMirrors("03142", solutions)
        validateRotationsAndMirrors("13024", solutions)
        validateRotationsAndMirrors("14203", solutions)
        validateRotationsAndMirrors("20314", solutions)
        validateRotationsAndMirrors("24130", solutions)
        validateRotationsAndMirrors("30241", solutions)
        validateRotationsAndMirrors("31420", solutions)
        validateRotationsAndMirrors("41302", solutions)
        validateRotationsAndMirrors("42031", solutions)
    }

    private def validateRotationsAndMirrors(solution: String, solutions: List[String]): Unit = {
        solutions.contains(solution) shouldBe true
        solutions.contains(solution.reverse) shouldBe true
        solutions.contains(BoardUtil.rotate(solution)) shouldBe true
        solutions.contains(BoardUtil.rotate(solution.reverse)) shouldBe true
    }
}
