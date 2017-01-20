package cz.vzdusne.schist
import java.util.Random

package object example {
  def normalH1(size : Int = 10000, bins : Int = 100) : Histogram1D[Int] = {
    val rnd = new Random()
    val values = (1 to size).map( _ => rnd.nextGaussian()).toArray
    Histogram.h1(values, bins)
  }
}
