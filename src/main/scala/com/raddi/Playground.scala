package com.raddi

import com.raddi.DataParser.{All, Dataframe, Index, Range}
import com.raddi.NdArray.{Array1, Array2, transpose}

import scala.io.Source

object Playground extends App {
  val readfile = (fileName: String) => {
    val bufferedSource = Source.fromFile(fileName);
    val lines = bufferedSource.getLines().foldLeft(List[String]()) ((list, string) => list :+ string)
    bufferedSource.close();
    lines
  }

  val fileContent = readfile("data.csv").map(x => {
    x.split(',').toList
  })
  val df: Dataframe = DataParser.makeDataframe(fileContent)
  println(df(Index(0), All))
  val arr = NdArray.make2dArray[Float](df, "float")
  val a = Array2[Float](Vector(Vector(1.0f, 2.0f), Vector(3.0f, 4.0f)))
  val b = Array2[Float](Vector(Vector(5.0f, 6.0f), Vector(7.0f, 8.0f)))
  println(arr)
  println(NdArray.*(a, b))
}
