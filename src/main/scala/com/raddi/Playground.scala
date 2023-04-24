package com.raddi

import com.raddi.DataParser.{All, Dataframe, Index, Range}

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
  println(NdArray.makeNdArray[Float](df, "float"))
}
