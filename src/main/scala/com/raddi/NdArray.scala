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
  def make2dArray[A](dataframe: Dataframe, dataType: String): NArray[A] = {
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

  def *[A: Numeric](left: A, right: Array1[A]): Array1[A] = {
    def  mult[A: Numeric](left: A, right: Vector[A]): Vector[A] = {
      if(right.length>1){
        (left * right.head) +: mult(left, right.tail)
      }
      else{
        Vector(left * right.head)
      }
    }
    Array1(mult(left,right.data))
  }

  def +[A: Numeric](left: Array1[A], right: Array1[A]): Option[Array1[A]] = {
    if(left.data.length!=right.data.length){
      None
    }
    else{
      Some(Array1(left.data.zip(right.data).map(pair => pair._1 + pair._2)))
    }
  }

  def -[A: Numeric](left: Array1[A], right: Array1[A]): Option[Array1[A]] = {
    if (left.data.length != right.data.length) {
      None
    }
    else {
      Some(Array1(left.data.zip(right.data).map(pair => pair._1 - pair._2)))
    }
  }

  def randArr(length: Int, max:Double): Array1[Double]= {
    val r = scala.util.Random
    def loop(length: Int, max: Double): Vector[Double] = {
      if (length == 1) {
        Vector(max * r.nextDouble)
      }
      else {
        (max * r.nextDouble) +: loop(length - 1, max)
      }
    }
    Array1(loop(length, max))
  }

  def transpose()

  def *[A](left: Array2[A], right: Array2[A]): Option[Array2[A]] = {
    // Matrix Multiplication
    val leftData = left.data
    val rightData = right.data
    if(leftData(0).length != rightData.length) None
    else {
      val newRightData = rightData
    }
  }
}
