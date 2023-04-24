package com.raddi

import com.raddi.DataParser.{Dataframe, Type}

object NdArray {

  trait NArray[A]
  case class Array2[A](data: Vector[Vector[A]]) extends NArray[A]
  /**
   *
   * @param dataframe
   */
  def makeNdArray[A](dataframe: Dataframe, dataType: Type): NArray[A] = {
    val filtered = dataframe.data.map(row => {
      row.filter(element => {
        dataType match {
          case DataParser.FrameFloat(_) => element.isInstanceOf[DataParser.FrameFloat]
          case DataParser.FrameInt(_) => element.isInstanceOf[DataParser.FrameInt]
          case DataParser.FrameString(_) => element.isInstanceOf[DataParser.FrameString]
          case _ => false
        }
      })
    })
    val data = filtered.map(row => row.map {
      case DataParser.FrameString(value) => value
      case DataParser.FrameInt(value) => value
      case DataParser.FrameFloat(value) => value
    })
    Array2[A](data.asInstanceOf[Vector[Vector[A]]])
  }
}
