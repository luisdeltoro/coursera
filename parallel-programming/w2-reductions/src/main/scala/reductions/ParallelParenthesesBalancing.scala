package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def count(acc: Int, c: Char) = {
      if (c == '(') acc + 1
      else if (c == ')') acc - 1
      else acc
    }

    @tailrec
    def loop(acc: Int, i: Int): Boolean = {
      if (i >= chars.size) {
        if (acc == 0) true
        else false
      }
      else if (acc < 0) false
      else loop(count(acc, chars(i)), i + 1)
    }

    loop(0, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def reduceOperation(ra: (Int, Int), rb: (Int, Int)) = {
      val (ra0, ra1) = ra
      val (rb0, rb1) = rb
      (Math.min(ra0, ra1 + rb0), ra1 + rb1)
    }

    def traverse(from: Int, until: Int): (Int, Int) = {
      var i = from
      var min = 0
      var count = 0
      while (i < until) {
        if (chars(i) == '(') count += 1
        else if (chars(i) == ')') count -= 1
        if (count < min) min = count
        i += 1
      }
      (min, count)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if (until <= from || until - from <= threshold) traverse(from, until)
      else {
        val middle = (from + until) / 2
        val (r1, r2) = parallel(reduce(from, middle), reduce(middle, until))
        reduceOperation(r1, r2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
