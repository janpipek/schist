package cz.vzdusne.schist
import scala.reflect.{ClassTag, classTag}


abstract class Histogram[T : Numeric](frequencies : Array[T], binEdges : List[Array[Double]]) /* (implicit num: Numeric[T]) */ {
  private val numerics = implicitly[Numeric[T]]

  protected var frequencies_ : Array[T] = frequencies
  protected var binEdges_ : List[Array[Double]] = binEdges

  protected def frequenciesFlat : Seq[T] = frequencies_
  protected def frequenciesAsDouble : Array[Double] = frequencies_.map(x => numerics.toDouble(x))
  protected def frequenciesAsInt : Array[Int] = frequencies_.map(x => numerics.toInt(x))
  protected def frequenciesAsFloat : Array[Float] = frequencies_.map(x => numerics.toFloat(x))
  protected def frequenciesAsLong : Array[Long] = frequencies_.map(x => numerics.toLong(x))

  // def frequenciesAsInt : Array[Int] = frequencies_.map((x : T) => num.toInt(x))*/
  // def binEdges : Seq[Seq[U]] = binEdges_
}

class Histogram1D[T : Numeric](frequencies : Array[T], binEdges : List[Array[Double]]) extends Histogram[T](frequencies, binEdges) {
  def this(frequencies : Array[T], binEdges : Array[Double]) = this(frequencies, List(binEdges))
  def binEdges : Seq[Double] = binEdges_.head
  def frequencies = frequencies_

  def asDouble : Histogram1D[Double] = new Histogram1D[Double](frequenciesAsDouble, binEdges_)
}

object Histogram
{


  private def make_edge_params(values : Array[Double], binCount : Int) = {
    val minEdge = values.min
    val maxEdge = values.max
    val step = (maxEdge - minEdge) / binCount
    (minEdge, maxEdge, step)
  }

  private def make_edges(minEdge : Double, maxEdge : Double, step : Double, binCount : Int) : Array[Double] = {
    ((0 until binCount - 1).map { i => i * step + minEdge }.toList ::: List(maxEdge)).toArray
  }

  private def findBin(value : Double, minEdge : Double, maxEdge: Double, step: Double, binCount : Int) : Int =
    if (value == maxEdge) binCount - 1 else ((value - minEdge) / step).toInt

  def h1(values : Array[Double], binCount : Int): Histogram1D[Int] =
  {
    val (minEdge, maxEdge, step) = make_edge_params(values, binCount)
    val binEdges = make_edges(minEdge, maxEdge, step, binCount)
    val frequencies : Array[Int] = Array.fill(binCount)(0)

    values.foreach { x =>
      val bin = findBin(x, minEdge, maxEdge, step, binCount)
      frequencies(bin) = frequencies(bin) + 1
    }

    new Histogram1D(frequencies, binEdges)
  }

  def h1(values : Array[Double], weights : Array[Double], binCount : Int): Histogram1D[Double] =
  {
    val (minEdge, maxEdge, step) = make_edge_params(values, binCount)
    val binEdges = make_edges(minEdge, maxEdge, step, binCount)
    val frequencies : Array[Double] = Array.fill(binCount)(0.0)

    val pairs = values zip weights
    pairs.foreach {
      case (value, weight) =>
        val bin = findBin(value, minEdge, maxEdge, step, binCount)
        frequencies(bin) = frequencies(bin) + weight
    }

    new Histogram1D(frequencies, List(binEdges))
  }

  def h1(values : Array[Double], weights : Array[Double]): Histogram1D[Double] = h1(values, weights, 10)
  def h1(values : Array[Double]): Histogram1D[Int] = h1(values, 10)
}