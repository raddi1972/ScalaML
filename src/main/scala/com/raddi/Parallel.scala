package com.raddi

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Parallel extends App {
  val powerOf2 = (n: Int) => {
    val result = 1;

    @tailrec
    def loop(acc: Int, it: Int): Int = {
      if (it == 0) {
        acc
      } else {
        if (it == 5 && n == 10) {
          return 0
        }
        loop(acc * 2, it - 1)
      }
    }
    loop(result, n)
  }
  val future1 = Future {
    powerOf2(11)
  }
  val future2 = Future{
    powerOf2(10)
  }
  val result1 = Await.result(future1, Duration.Inf)
  val result2 = Await.result(future2, Duration.Inf)
  println(result1 + result2)
}