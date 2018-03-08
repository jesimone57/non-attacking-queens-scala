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

}
