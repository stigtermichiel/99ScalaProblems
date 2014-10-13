import com.example.scalaproject.NinetyNineProblems

import org.scalatest._
import NinetyNineProblems._


class ProgramTest extends FunSuite {

  test("Last element of List") {
      assert(last(List(1,2,3,4,5)) === 5)
    }

  test("Last but one element") {
    assert(penultimate(List(1, 1, 2, 3, 5, 8)) === 5)
  }

  test("Find the Kth element of a list") {
    assert(nth(4, List(1, 1, 2, 3, 5, 8)) === 5)
  }

  test("Find the number of elements in a list") {
    assert(length(List(1, 1, 2, 3, 5, 8, 2, 3, 4)) === 9)
  }

  test("Reverse a List") {
    assert(reverse(List(1,1,2,3,5,8)) === List(8, 5, 3, 2, 1, 1) )
  }

  test("Find out whether a list is a palindrome.") {
    assert(isPalindrome(List(1, 2, 3, 2, 1)) === true)
  }

  test("Flatten a nested list structure.") {
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) === List(1, 1, 2, 3, 5, 8) )
  }

  test("Eliminate consecutive duplicates of list elements.") {
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List('a, 'b, 'c, 'a, 'd, 'e))
  }

  test("Pack consecutive duplicates of list elements into sublists.") {
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)) )
  }

  test("Run-length encoding of a list.") {
    assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)) )
  }






}

