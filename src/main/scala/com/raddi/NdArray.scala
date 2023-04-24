package com.raddi

import com.raddi.DataParser.Type.{dType, getDefaultValue}
import com.raddi.DataParser.{Dataframe, Type}

object NdArray {

  trait NArray[A]
  case class Array2[A](data: Vector[Vector[A]]) extends NArray[A]
  /**
   *
   * @param dataframe
   */
  def makeNdArray[A](dataframe: Dataframe, dataType: String): NArray[A] = {
    val filtered = dataframe.data.map(row => {
      row.zip(dataframe.columns.map(tuple => tuple._2)).filter(element => {
//        println(dataType, element._2, Type.getTypePrecedence(dataType, element._2))
         if(Type.getTypePrecedence(dataType, element._2)) true else false
      }).map(pair => pair._1)
    })
    val data = filtered.map(row => row.map {
      case DataParser.FrameString(value) => value
      case DataParser.FrameInt(value) => value
      case DataParser.FrameFloat(value) => value
      case DataParser.Null => getDefaultValue(dataType)
    })
    Array2[A](data.asInstanceOf[Vector[Vector[A]]])
  }
}
