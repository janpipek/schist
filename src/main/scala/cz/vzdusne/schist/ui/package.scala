package cz.vzdusne.schist

import org.jfree.chart._
import org.jfree.data.xy.DefaultXYDataset

package object ui {
  def plot[T: Numeric](histogram: Histogram1D[T]) : Unit = {
    val dataset = new DefaultXYDataset
    val x = histogram.binEdges.toArray
    val y = histogram.asDouble.frequencies

    dataset.addSeries("Title", Array(x, y))

    val frame = new ChartFrame(
      "Title",
      ChartFactory.createScatterPlot(
        "Plot",
        "X Label",
        "Y Label",
        dataset,
        org.jfree.chart.plot.PlotOrientation.HORIZONTAL,
        false,false,false
      )
    )
    frame.pack()
    frame.setVisible(true)
  }
}
