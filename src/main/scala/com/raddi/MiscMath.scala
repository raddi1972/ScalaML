package com.raddi

object MiscMath {
  def dot(v1: Vector[Double], v2: Vector[Double]): Double = {
    if(v1.length>1){
      (v1.head * v2.head) +dot(v1.tail,v2.tail)
    }
    else{
      v1.head * v2.head
    }
  }

  def addVector(v1: Vector[Double], v2:Vector[Double]): Vector[Double] = {
    if(v1.length>1){
      (v1.head+v2.head)+:addVector(v1.tail,v2.tail)
    }
    else{
      Vector(v1.head+v2.head)
    }
  }

  def subVector(v1: Vector[Double], v2: Vector[Double]): Vector[Double] = {
    if (v1.length > 1) {
      (v1.head - v2.head) +: subVector(v1.tail, v2.tail)
    }
    else {
      Vector(v1.head - v2.head)
    }
  }

  def multScalar(v1: Vector[Double], c: Double): Vector[Double] = {
    if (v1.length > 1) {
      (v1.head * c) +: multScalar(v1.tail, c)
    }
    else {
      Vector(v1.head * c)
    }
  }

  def divScalar(v1: Vector[Double], c: Double): Vector[Double] = {
    if (v1.length > 1) {
      (v1.head / c) +: divScalar(v1.tail, c)
    }
    else {
      Vector(v1.head / c)
    }
  }

  def randVector(length : Int, max: Int): Vector[Double] = {
    val r = scala.util.Random
    def loop(length: Int, max: Int): Vector[Double] = {
      if(length == 1){
        Vector(max*r.nextDouble)
      }
      else{
        (max*r.nextDouble)+:loop(length-1, max)
      }
    }
    loop(length, max)
  }
}
