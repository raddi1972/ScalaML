package com.raddi

object LinearRegression {
  private val learningRate: Double = 0.001

  private def Diff(weight: Vector[Double], x:Vector[Vector[Double]], y:Vector[Double]): Vector[Double] = {
    def loop(weight: Vector[Double], x:Vector[Vector[Double]], y:Vector[Double]): Vector[Double] = {
      val curr=(y.head - predict(weight, x.head))+:MiscMath.multScalar(x.head, y.head - predict(weight, x.head))
      if (x.length > 1) {
        MiscMath.addVector(curr,loop(weight, x.tail, y.tail))
      }
      else {
        curr
      }
    }
    MiscMath.multScalar(MiscMath.divScalar(loop(weight,x,y),x.length),-2)
  }

  def fit(x:Vector[Vector[Double]], y:Vector[Double]): Vector[Double] = {
    val weight: Vector[Double] = MiscMath.randVector(x.head.length+1, 20000)
    def loop(weight: Vector[Double],x:Vector[Vector[Double]], y:Vector[Double],epochs: Int): Vector[Double] = {
      if(epochs==0){
        weight
      }
      else{
        loop(MiscMath.subVector(weight,MiscMath.multScalar(Diff(weight, x, y),learningRate)), x, y, epochs-1)
      }
    }
    loop(weight,x,y,10000)
  }

  def predict(weight: Vector[Double], x: Vector[Double]): Double = {
    MiscMath.dot(weight.tail, x) + weight.head
  }
}
