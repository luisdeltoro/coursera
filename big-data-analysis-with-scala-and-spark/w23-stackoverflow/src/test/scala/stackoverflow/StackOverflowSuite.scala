package stackoverflow

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

import stackoverflow.StackOverflow._

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120

    val lines = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv")
    val raw = rawPostings(lines)
    val grouped = groupedPostings(raw)
    val scored = scoredPostings(grouped)
    val vectors = vectorPostings(scored)
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  test("groupPostings works as expected") {
    val (qId, cSharpAs) = testObject.grouped.filter(p => p._1 == 563549).collect()(0)
    assert(cSharpAs.size == 8)
  }

  test("scoredPostings works as expected") {
    val scoredSize = testObject.scored.count()
    val qs = testObject.scored.filter {case (q, hs) => List(6, 42, 72, 126, 174).contains(q.id)}.collect()
    assert(scoredSize == 2121822)
    assert(qs.size == 5)
  }

  test("vectorPostings works as expected") {
    val vs = testObject.vectors.filter(v => List((350000, 67), (100000, 89), (300000, 3), (50000,  30), (200000, 20)).contains(v)).collect()
    assert(testObject.vectors.count() == 2121822)
    assert(vs.distinct.size == 5)
  }

  test("clusterResults works as expected") {
    val means   = kmeans(sampleVectors(testObject.vectors), testObject.vectors, debug = true)
    val results = clusterResults(means, testObject.vectors)
    assert(results(0) == ("Groovy", 100.0, 1631, 0))
    assert(results(1) == ("MATLAB", 100.0, 3725, 0))
  }

}
