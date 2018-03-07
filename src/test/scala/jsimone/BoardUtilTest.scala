package jsimone

import org.scalatest.{FlatSpec, Matchers}

class BoardUtilTest extends FlatSpec with Matchers {

    "Flip Y" should "flip the board about Y axis" in {
        val list = List(0,2,1)
        BoardUtil.flipYAxis(list, 3) shouldBe List(2,0,1)
    }

    "Flip X" should "flip the board about X axis" in {
        val list = List(0,2,1)
        BoardUtil.flipXAxis(list) shouldBe List(1,2,0)
    }

    /*
        assertEquals("021", NonAttackingQueens.rotate("021", 3));
        assertEquals("012", NonAttackingQueens.rotate("012", 3));

        assertEquals("102", NonAttackingQueens.rotate("102", 3));
        assertEquals("201", NonAttackingQueens.rotate("120", 3));

        assertEquals("120", NonAttackingQueens.rotate("201", 3));
        assertEquals("210", NonAttackingQueens.rotate("210", 3));
     */

    "Rotate" should "rotate a 3x3 board clock-wise 90 degrees" in {
        List(0,2,1) shouldBe BoardUtil.rotate(List(0,2,1), 3)
        List(0,1,2) shouldBe BoardUtil.rotate(List(0,1,2), 3)

        List(1,0,2) shouldBe BoardUtil.rotate(List(1,0,2), 3)
        List(2,0,1) shouldBe BoardUtil.rotate(List(1,2,0), 3)

        List(1,2,0) shouldBe BoardUtil.rotate(List(2,0,1), 3)
        List(2,1,0) shouldBe BoardUtil.rotate(List(2,1,0), 3)
    }

    "Rotate" should "rotate an 8x8 board clock wise 90 degrees" in {
        List(0,6,4,7,1,3,5,2) shouldBe BoardUtil.rotate(List(0,4,7,5,2,6,1,3), 8)
        List(0,4,7,5,2,6,1,3) shouldBe BoardUtil.rotate(List(0,6,4,7,1,3,5,2), 8)
    }

}
