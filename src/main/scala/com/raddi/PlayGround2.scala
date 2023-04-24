package com.raddi

import com.raddi.DataParser.{All, Dataframe, Index, Range}

import scala.io.Source

object PlayGround2 extends App {
  val readfile = (fileName: String) => {
    val bufferedSource = Source.fromFile(fileName);
    val lines = bufferedSource.getLines().foldLeft(List[String]()) ((list, string) => list :+ string)
    bufferedSource.close();
    lines
  }

  val fileContent = readfile("Salary_dataset.csv").map(x => {
    x.split(',').toList
  })
  val df: Dataframe = DataParser.makeDataframe(fileContent)
  //  val x=df(All, Index(1)).data
  //  val y=df(All,Index(2)).data
  //  private def compress(y:Vector[Vector[DataParser.Type]]): Vector[DataParser.Type] = {
  //    y.head.head+:compress(y.tail)
  //  }
  //  val y_comp=compress(y)
  val x=Vector(Vector(1.2),Vector(1.4),Vector(1.6),Vector(2.1),Vector(2.3),Vector(3.0),Vector(3.1),Vector(3.3),Vector(3.3),Vector(3.8),Vector(4.0),Vector(4.1),Vector(4.1),Vector(4.2),Vector(4.6),Vector(5.0),Vector(5.2),Vector(5.4),Vector(6.0),Vector(6.1),Vector(7.2),Vector(8.0),Vector(8.3),Vector(8.8),Vector(9.1),Vector(9.6),Vector(9.7),Vector(10.4),Vector(10.6))
  val y=Vector(39344.0
    , 46206.0
    , 37732.0
    , 43526.0
    , 39892.0
    , 56643.0
    , 60151.0
    , 54446.0
    , 64446.0
    , 57190.0
    , 63219.0
    , 55795.0
    , 56958.0
    , 57082.0
    , 61112.0
    , 67939.0
    , 66030.0
    , 83089.0
    , 81364.0
    , 93941.0
    , 91739.0
    , 98274.0
    , 101303.0
    , 113813.0
    , 109432.0
    , 105583.0
    , 116970.0
    , 112636.0
    , 122392.0
    , 121873.0
  )
  val weights=LinearRegression.fit(x,y)
  val pred=LinearRegression.predict(weights,Vector(4.0))
  println(pred)
}
