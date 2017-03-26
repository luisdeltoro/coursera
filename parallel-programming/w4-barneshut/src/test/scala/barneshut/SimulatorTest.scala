package barneshut

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

import scala.collection.parallel.{ForkJoinTaskSupport, TaskSupport}

@RunWith(classOf[JUnitRunner])
class SimulatorTest extends FunSuite {

  def fixture =
    new {
      val sim = new Simulator(null, null) // updateBoundaries does not make use of member parameters
      val boundaries = new Boundaries()
      boundaries.minX = 2.0f
      boundaries.minY = 2.0f
      boundaries.maxX = 3.0f
      boundaries.maxY = 3.0f
    }

  test("update boundaries should work for body at 4.0 4.0") {
    val f = fixture
    val body = new Body(1.0f, 4.0f, 4.0f, 0.0f, 0.0f)
    f.sim.updateBoundaries(f.boundaries, body)
    assert(f.boundaries.minX == 2.0f, s"${f.boundaries.minX} should be 2.0f")
    assert(f.boundaries.minY == 2.0f, s"${f.boundaries.minX} should be 2.0f")
    assert(f.boundaries.maxX == 4.0f, s"${f.boundaries.minX} should be 4.0f")
    assert(f.boundaries.maxY == 4.0f, s"${f.boundaries.minX} should be 4.0f")
  }

  test("update boundaries should work for body at 1.0 1.0") {
    val f = fixture
    val body = new Body(1.0f, 1.0f, 1.0f, 0.0f, 0.0f)
    f.sim.updateBoundaries(f.boundaries, body)
    assert(f.boundaries.minX == 1.0f, s"${f.boundaries.minX} should be 1.0f")
    assert(f.boundaries.minY == 1.0f, s"${f.boundaries.minX} should be 1.0f")
    assert(f.boundaries.maxX == 3.0f, s"${f.boundaries.minX} should be 3.0f")
    assert(f.boundaries.maxY == 3.0f, s"${f.boundaries.minX} should be 3.0f")
  }

  test("update boundaries should work for body at 1.0 4.0") {
    val f = fixture
    val body = new Body(1.0f, 1.0f, 4.0f, 0.0f, 0.0f)
    f.sim.updateBoundaries(f.boundaries, body)
    assert(f.boundaries.minX == 1.0f, s"${f.boundaries.minX} should be 1.0f")
    assert(f.boundaries.minY == 2.0f, s"${f.boundaries.minX} should be 2.0f")
    assert(f.boundaries.maxX == 3.0f, s"${f.boundaries.minX} should be 3.0f")
    assert(f.boundaries.maxY == 4.0f, s"${f.boundaries.minX} should be 4.0f")
  }

  test("merge boundaries should work") {
    val f = fixture
    val boundaries2 = new Boundaries()
    boundaries2.minX = 4.0f
    boundaries2.minY = 4.0f
    boundaries2.maxX = 5.0f
    boundaries2.maxY = 5.0f
    val result = f.sim.mergeBoundaries(f.boundaries, boundaries2)
    assert(result.minX == 2.0f, s"${result.minX} should be 2.0f")
    assert(result.minY == 2.0f, s"${result.minY} should be 2.0f")
    assert(result.maxX == 5.0f, s"${result.maxX} should be 5.0f")
    assert(result.maxY == 5.0f, s"${result.maxY} should be 5.0f")
  }

  test("computeSectorMatrix should work") {
    val sim = new Simulator(new ForkJoinTaskSupport(), new TimeStatistics())
    val b1 = new Body(1.0f, 6.5f, 0.5f, 0.0f, 0.0f)
    val b2 = new Body(1.0f, 1.5f, 3.5f, 0.0f, 0.0f)
    val b3 = new Body(1.0f, 4.5f, 4.5f, 0.0f, 0.0f)
    val b4 = new Body(1.0f, 5.0f, 5.0f, 0.0f, 0.0f)
    val bodies = Seq(b1, b2, b3, b4)
    val boundaries = new Boundaries()
    boundaries.minX = 0.0f
    boundaries.minY = 0.0f
    boundaries.maxX = 8.0f
    boundaries.maxY = 8.0f
    val result = sim.computeSectorMatrix(bodies, boundaries)
    assert(result.matrix(6).size == 1, s"${result.matrix(8)} should be 1")
    assert(result.matrix(25).size == 1, s"${result.matrix(25)} should be 1")
    assert(result.matrix(36).size == 1, s"${result.matrix(36)} should be 1")
    assert(result.matrix(45).size == 1, s"${result.matrix(45)} should be 1")
  }

}
