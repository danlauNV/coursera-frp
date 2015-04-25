package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  /**
   *  If you insert any two elements into an empty heap,
   *  finding the minimum of the resulting heap should
   *  get the smallest of the two elements back.
   */
  property("Minimum of two") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  /**
   * If you insert an element into an empty heap, then
   * delete the minimum, the resulting heap should be
   * empty.
   */
  property("Delete from one produce empty") = forAll{ a: Int =>
    val notEmpty = insert(a, empty)
    val emp = deleteMin(notEmpty)
    isEmpty(emp) && !isEmpty(notEmpty)
  }

  /**
   * Given any heap, you should get a sorted sequence of
   * elements when continually finding and deleting minima.
   *
   * (Hint: recursion and helper functions are your friends.)
   */
  property("Removing minimum produces sorted") = forAll{ h : H =>
    isSorted(h, (x: Int, y: Int) => x.compareTo(y))
  }

  /**
   * Finding a minimum of the melding of any two heaps
   * should return a minimum of one or the other.
   */
  property("Minimum of meld is minimum of either") = forAll{ (h1 : H, h2: H) =>
    val m1 = meld(h1, h2)
    val m2 = meld(h2, h1)

    val minMeld1 = findMin(m1)
    val minMeld2 = findMin(m2)
    val minPartial : Int = Math.min(findMin(h1), findMin(h2))

    minMeld1 == minMeld2 && minPartial == minMeld1
  }

  /**
   * Melding without minimum
   */
  property("Melding without minima") = forAll{ (h1 : H, h2 : H) =>
    val m1 = if(findMin(h1).compareTo(findMin(h2)) <= 0) meld(deleteMin(h1), h2) else meld(deleteMin(h2), h1)
    val m2 = deleteMin(meld(h1, h2))

    findMin(m1) == findMin(m2)
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  def isSorted(h: H, cmp : (Int, Int) => Int): Boolean = {
    isEmpty(h) || ((h: H, x: Int) => (isEmpty(h) || cmp(x, findMin(h)) <= 0) && isSorted(h, cmp))(deleteMin(h), findMin(h))
  }

}
