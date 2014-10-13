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

  def encodeModified[Symbol](ls: List[Symbol]): List[Any] = {
    pack(ls) map {x => if (x.length == 1) x.head else (x.length, x.head) }
  }

  def decode(ls: List[(Int, Symbol)]): List[Symbol] = {
    ls flatMap  {x => List.make(x._1, x._2)}
    }

  def encodeDirect[Symbol](ls: List[Symbol]): List[(Int, Symbol)] = {
    if (ls.isEmpty) List()
    else {
      val (done, next) = ls span { _ == ls.head }
      (done.length, done.head) :: encodeDirect(next)
    }
  }

  def duplicate[Any](ls: List[Any]): List[Any] = ls flatMap {x => List(x,x)}

  def duplicateN[Any](n: Int, ls: List[Any]): List[Any] = ls flatMap {x => List.make(n,x)}

  def drop[Any](n: Int, ls: List[Any]): List[Any] = {
    def dropRec(c: Int, ms: List[Any], result: List[Any]): List[Any] = (c, ms) match {
      case (_, Nil) => result.reverse
      case (1, head :: tail) => dropRec(n, tail, result)
      case (c, head :: tail) => dropRec(c-1, tail, head :: result)
    }
    dropRec(n, ls, List())
  }

  def split[Any](n: Int, ls: List[Any]): (List[Any], List[Any]) = {
    def splitRec(c: Int, ms: List[Any], result: List[Any]): (List[Any], List[Any]) = (c, ms) match {
      case (_, Nil) => (result, Nil)
      case (0, head :: tail) => (result.reverse, ms)
      case (c, head :: tail) => splitRec(c-1, tail, head :: result)
    }
    splitRec(n, ls, List())
  }

  def slice[Any](i: Int, j: Int,  ls: List[Any]): List[Any] = {
    def sliceRec(x: Int, y: Int, ms: List[Any], result: List[Any]): List[Any] = (x, y, ms) match {
      case (0, _, head :: tail) => sliceRec(x-1, y-1, tail, head :: result)
      case (x, y, head :: tail) if x < 1 && y > 1 => sliceRec(x-1, y-1, tail, head :: result)
      case (_, 1, head :: tail) => (head ::result).reverse
      case (_, _, head :: tail) => sliceRec(x-1, y-1, tail, result)
    }
    sliceRec(i, j, ls, List())
  }

  def rotate[Any](n: Int, ls: List[Any]): List[Any] = {
    def rotateRec(c: Int, ms: List[Any]): List[Any] = (c, ms) match {
      case (0, ms) => ms
      case (i, head :: tail) if i > 0 => rotateRec(c-1, tail.:+(head))
      case (i, head :: tail) if -ms.length == i => ms
      case (i, head :: tail) if i < 0 => rotateRec(c-1, tail.:+(head))
    }
    rotateRec(n, ls)
  }

  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = {
    def removeAtRec(c: Int, ms: List[A], result: List[A]): (List[A], A) = (c, ms) match {
      case (0, head :: tail) => (result ++ tail, head)
      case (i, head :: tail) => removeAtRec(c-1, tail, head :: result)
    }
    removeAtRec(n, ls, List())
  }

  














}
