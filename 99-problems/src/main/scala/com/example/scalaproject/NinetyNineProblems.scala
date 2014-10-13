package com.example.scalaproject

import java.util.NoSuchElementException

object NinetyNineProblems {

  def last[A](x: List[A]): A = x match {
    case Nil => throw new NoSuchElementException()
    case head :: Nil => head
    case head :: tail => last(tail)
  }

  def penultimate[A](ls: List[A]): A = ls match {
    case Nil => throw new NoSuchElementException()
    case head :: tail if tail.length == 1 => head
    case head :: tail => penultimate(tail)
  }

  def nth[A](n: Int, ls: List[A]): A = (n, ls) match {
    case (_, Nil) => throw new NoSuchElementException()
    case (0, head :: tail) => head
    case (_, head :: tail) => nth(n-1, tail)
  }

  def length[A](ls: List[A]): Int = {
    def lengthRec(rs: List[A], counter: Int): Int = rs match {
      case Nil => counter
      case head :: tail => lengthRec(tail, counter + 1)
      }
    lengthRec(ls, 0)
  }

  def reverse[A](ls: List[A]): List[A] = {
    def reverseRec(rs: List[A], result: List[A]): List[A] = rs match {
      case Nil => result
      case head :: tail => reverseRec(tail, result.+:(head))
    }
    reverseRec(ls, List())
  }

  def isPalindrome[A](ls: List[A]): Boolean = {
    ls == ls.reverse
  }

  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
  }

  def compress[A](ls: List[A]): List[A] = {
    def compressRec(ms: List[A], result: List[A]): List[A] = (ms, result) match {
      case (Nil, result) => result.reverse
      case (head :: tail, h :: t) if head == h => compressRec(tail, h :: t)
      case (head :: tail, result) => compressRec(tail, head :: result )
    }
    compressRec(ls, List())
  }

  def pack[Symbol](ls: List[Symbol]): List[List[Symbol]] = {
    if (ls.isEmpty) List(List())
    else {
      val (done, todo) = ls span { _ == ls.head }
      if (todo == Nil) List(done)
      else done :: pack(todo)
    }
  }

  def encode[Symbol](ls: List[Symbol]): List[(Int, Symbol)] = {
   pack(ls) map {x => (x.length, x.head)}
  }

}
