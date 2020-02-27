package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {

    val numGen: Gen[Int] = arbitrary[Int]

    val retHeap: Gen[H] = for {
      num: Int <- numGen
      currentHeap <- oneOf[H](genHeap, empty)
    } yield insert(num, currentHeap)

    retHeap
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get
  // the smallest of the two elements back.
  property("minOfTwoElementsInEmptyHeap") =
    forAll { (elem1: Int, elem2: Int) =>
      val heap = insert(elem1, empty)
      val resultHeap = insert(elem2, heap)
      val expectedMin = if (elem1 < elem2) elem1 else elem2
      findMin(resultHeap) == expectedMin
    }

  // If you insert an element into an empty heap,
  // then delete the minimum, the resulting heap should be empty.
  property("insertAndDeleteElementFromEmptyHeap") =
    forAll { elem: Int =>
      val heap = insert(elem, empty)
      val resultHeap = deleteMin(heap)
      resultHeap == empty
    }

  // Given any heap, you should get a sorted sequence of elements when
  // continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("contDeleteSortHeap") =
    forAll { heap: H =>
      def deleteAndGetElements(heap: H): List[Int] = {
        if (isEmpty(heap)) List.empty
        else {
          val updatedHeap = deleteMin(heap)
          findMin(heap) :: deleteAndGetElements(updatedHeap)
        }
      }

      val deletedSortedElements: List[Int] = deleteAndGetElements(heap)
      deletedSortedElements == deletedSortedElements.sorted
    }

  // Finding a minimum of the melding of any two heaps
  // should return a minimum of one or the other.
  property("minOfMergedHeaps") =
    forAll { (heap1: H, heap2: H) =>

      val mergedHeap = meld(heap1, heap2)
      val min1 = findMin(heap1)
      val min2 = findMin(heap2)
      val minMerged = findMin(mergedHeap)
      minMerged == min1 || minMerged == min2
    }

  property("deleteMin") = forAll { (heap: H) =>
    val min = findMin(heap)
    val heapWithAnotherMin = insert(min, heap)
    deleteMin(heapWithAnotherMin) == heap
  }

  // Merged heaps should be equals after taking min element
  // from one heap and move it to the other (and then merge again)
  property("meldMinMove") = forAll { (heap1: H, heap2: H) =>
    def getHeapElements(heap: H): List[Int] = {
      if (isEmpty(heap)) List.empty
      else {
        val updatedHeap = deleteMin(heap)
        findMin(heap) :: getHeapElements(updatedHeap)
      }
    }

    val mergedHeap1 = meld(heap1, heap2)
    val min1 = findMin(heap1)
    val updatedHeap1 = deleteMin(heap1)
    val updatedHeap2 = insert(min1, heap2)
    val mergedHeap2 = meld(updatedHeap1, updatedHeap2)

    getHeapElements(mergedHeap1) == getHeapElements(mergedHeap2)
  }
}
