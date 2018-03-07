package jsimone

import org.scalatest.{FlatSpec, Matchers}

class PermutationTest extends FlatSpec with Matchers {

    def factorial(x: Int): Int = if (x==0) 1 else x * factorial(x-1)

    "Permutation of A" should "be same" in {
        val list = List("A")
        list.permutations.toList shouldBe List(list)
    }

    "Permutation of A B" should "be list of 2" in {
        val list = List("A", "B")
        list.permutations.toList shouldBe List(List("A", "B"), List("B","A"))
    }

    "Permutation of A B C" should "be list of 6" in {
        val list = List("A", "B", "C")
        list.permutations.toList should have size factorial(list.size)
    }

    "Permutation of A B C D" should "be list of 24" in {
        val list = List("A", "B", "C", "D")
        list.permutations.toList should have size factorial(list.size)
    }

    "Permutation of A B C D E" should "be list of 120" in {
        val list = List("A", "B", "C", "D", "E")
        list.permutations.toList should have size factorial(list.size)
    }


}
