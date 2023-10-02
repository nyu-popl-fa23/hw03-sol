package popl

object hw03 extends App:
  /*
   * CSCI-UA.0480-055: Homework 3
   */

  /*
   * Replace the '???' expressions with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * leaving any failing asserts.
   *
   * Your solution will _not_ be graded if it does not compile!!
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  // Declaration of list data type
  enum List:
    case Nil
    case ::(hd: Int, tl: List)

    def ::(hd: Int): List = List.::(hd, this)
    override def toString: String = this match
      case Nil => "Nil"
      case hd :: tl => s"$hd :: $tl"

  import List._

  // List reversal (useful for implementing tail-recursive versions of functions)

  def reverseLoop(l: List, acc: List): List = l match
    case Nil => acc
    case hd :: tl => reverseLoop(tl, hd :: acc)

  def reverse(l: List) = reverseLoop(l, Nil)

  // Tail-recursive functions append and filter to be completed by you

  def filter(n: Int, l: List): List =
    def loop(l: List, r: List): List = l match
      case Nil => reverse(r)
      case hd :: tl => loop(tl, if (hd < n) r else hd :: r)
    loop(l, Nil)

  def append(l1: List, l2: List): List =
    def loop(r1: List, l2: List): List = r1 match
      case Nil => l2
      case hd :: tl => loop(tl, hd :: l2)
    loop(reverse(l1), l2)

