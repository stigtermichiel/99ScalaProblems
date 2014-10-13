package com.example.scalaproject

import java.util.NoSuchElementException

object NinetyNineProblems {

  def last[A](x: List[A]): A = x match {
    case Nil => throw new NoSuchElementException()
    case head :: Nil => head
    case head :: tail => last(tail)
  }

}
