package com.raddi

object Functional extends App {
  /*
  1. apply function
  2. How exactly functions are implemented
  3. Syntactic Sugar for functions
  4. Higher Order Function -> map
  5. Collections ->
      List (head, tail), ::, :+, +:
      Sequences Seq -> It is a trait, Seq(1, 2, 3) gives out a derived type of sequence
      Can get the index in a sequence
   */

  class Rudransh {
    def apply(n: Int): Int = {
      2 * n
    }
  }
  val rudransh = new Rudransh();
  rudransh.apply(2)



  val aList = List(1, 2, 3, 4, 5, 6, 7)
  val firstElement = aList.head
  val restElements = aList.tail
  val prependedList = 1 :: aList
  val extendedList = 1 +: aList :+ 2
  aList.map(x => x * 2)
  println(extendedList)

  val aSeq = Seq(1, 2, 3, 4, 5)
  println(aSeq(1)) // can access them by index

  val aVector = Vector(1, 2) // Good for large data
  println(aVector(1)) // Similar to seq

  val aSet = Set(1, 2, 3, 4, 5, 1, 3) // can + to a set or - from the set
  // has .contain method to check if a Set contains something

  val aTuple = ("Rudransh", 19)

  // Pattern Matching in Scala
  val pat = aVector match {
    case Vector(a, b) => s"$a-$b"
    case _ => "Nothing"
  }
  class A
  class B

  val listString = aList match {
    case h :: t => s"$h"
    case _ => "Nothing"
  }
  println(listString);

  def filterList[A](aList: List[A], fun: (A) => Boolean):List[A] = {
    aList match {
      case h :: t => if (fun(h)) {
        h +: filterList(t, fun)
      }
      else {
        filterList(t, fun)
      }
      case Nil => List[A]()
    }
  }
  println(filterList[Int](aList, x => x < 4))

  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[A](value:A, left: Tree[A], right: Tree[A]) extends Tree[A]

  val tree = Node[Int](5, Node[Int](6, Node[Int](9, Node[Int](7, Empty, Empty), Empty), Node[Int](10, Empty, Empty)), Empty)
  def traversal[A](tree: Tree[A]): String = {
    tree match {
      case Empty => s""
      case Node(value, left, right) => s"${traversal(left)} $value ${traversal(right)}"
    }
  }
  println(traversal(tree))
  // Function1, Function2, Function3, Function4... Function22

}
