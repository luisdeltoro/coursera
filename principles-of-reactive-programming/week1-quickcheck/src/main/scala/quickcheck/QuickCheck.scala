package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genTinyInt = Gen.choose(0, 500)
  lazy val genSmallInt = Gen.choose(500,1000)
  lazy val genBigInt = Gen.choose(1000,2000)

  lazy val genHeap: Gen[H] =
    for {
      value <- arbitrary[Int]
      heap <- Gen.frequency[H]((1, empty), (3, genHeap))
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("insert on empty heap and find min afterwards => retrieved element should be the inserted one") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert two elements on empty heap and find min afterwards => retrieved element should be the minimum one") = forAll(genSmallInt, genBigInt) { (s: Int, b: Int) =>
    val h = insert(b, insert(s, empty))
    findMin(h) == s
  }

  property("insert on empty heap and delete min afterwards => heap should be empty") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    h == empty
  }

  property("given a heap, when finding and deleting minima => the generated sequence should be sorted") = forAll { h: H =>
    isSorted(deleteAllMins(h))
  }

  property("when melting one heap with an empty heap => the minimum of the resulting heap is the minium of the non-empty heap") = forAll { h: H =>
    findMin(meld(h, empty)) == findMin(h)
  }

  property("when melting two heaps => the minimum of the resulting heap is the minium of the one of the original ones") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == findMin(h1) ||  findMin(meld(h1, h2)) == findMin(h2)
  }

  property("when melting two heaps of bigger numbers and inserting a tiny int => the minimum of the resulting heap is the inserted tiny int") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == findMin(h1) ||  findMin(meld(h1, h2)) == findMin(h2)
  }

  property("when melting two empty heaps => the resulting heap is empty") = forAll { (h: H) =>
    isEmpty(meld(empty, empty))
  }

  property("when inserting 3 elements into an empty heap and deleting the minimum => the next minimum should be second smallest element") = forAll(genTinyInt, genSmallInt, genBigInt) { (t: Int, s: Int, b: Int) =>
    findMin(deleteMin(insert(b, insert(s, insert(t, empty))))) == s
  }

  def deleteAllMins(h: H): Seq[Int] = {
    if (isEmpty(h)) {
      Nil
    } else {
      findMin(h) +: deleteAllMins(deleteMin(h))
    }
  }

  def isSorted(sequence: Seq[Int]): Boolean = sequence match {
    case Nil => true
    case x1 :: Nil => true
    case x1 :: x2 :: xs => (x1 <= x2) && isSorted(xs)
  }

}
