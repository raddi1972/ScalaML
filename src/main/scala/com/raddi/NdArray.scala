package com.raddi

import com.raddi.DataParser.Type.{dType, getDefaultValue}
import com.raddi.DataParser.{Dataframe, Type}

import scala.math.Numeric.Implicits.infixNumericOps

object NdArray {

  trait NArray[A]
  case class Array1[A](data: Vector[A]) extends NArray[A]
  case class Array2[A](data: Vector[Vector[A]]) extends NArray[A]
  /**
   *
   * @param dataframe
   */
  def make2dArray[A](dataframe: Dataframe, dataType: String): Array2[A] = {
    val filtered = dataframe.data.map(row => {
      row.zip(dataframe.columns.map(tuple => tuple._2)).filter(element => {
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

  def *[A: Numeric](left: Array1[A], right: Array1[A]): Option[A] = {
    if(left.data.length != right.data.length) None
    else {
      Some(left.data.zip(right.data).map(pair => pair._1 * pair._2).sum)
    }
  }

  def transpose[A: Numeric](array: Array2[A]): Array2[A] = {
    val data = array.data
    def makeVec(n:Int) : Vector[Vector[A]] = {
      if(n == 1) Vector(Vector())
      else Vector() +: makeVec(n-1)
    }
    Array2(data.foldLeft(makeVec(data(0).length))((acc, row) => {
      val input = makeVec(row.length)
      val cols = row.zip(input).map(it => {
        it._2 :+ it._1
      })
      cols.zip(acc).map(pair => pair._2 ++ pair._1)
    }))
  }

  def *[A: Numeric](left: Array2[A], right: Array2[A]): Option[Array2[A]] = {
    // Matrix Multiplication
    if(left.data(0).length != right.data.length) None
    else {
      val rightT = transpose(right)
      Some(Array2(left.data.map(row => rightT.data.map(col => *(Array1(row),Array1(col)).get))))
    }
  }
}
