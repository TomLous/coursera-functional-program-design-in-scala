package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  property("The smallest of 2 elements should be the smallest in a previously empty Heap") = forAll { (n1: A, n2: A) =>
    val h = insert(n1, insert(n2, empty))
    findMin(h) == min(n1, n2)
  }

  property("Deleting the minimal value in a Heap should result in an empty Heap") = forAll { (n: A) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  property("Recusivly finding and deleting elements in a Heap should return same elements") = forAll { (h: H) =>
    def isSorted(h: H): Boolean =
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    isSorted(h)
  }

  property("The minimal value of a melded Heap should be the min of the min of both heaps") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }


  property("Two heaps should be equal if recursivly removing min elements result in same elements until empty") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("The minimal value of 2 heaps should be the minimal after dispacing it from heap 1 to 2 and melding both") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = min(m1,m2)
    findMin(meld(deleteMin(h1), insert(m, h2))) == m
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
