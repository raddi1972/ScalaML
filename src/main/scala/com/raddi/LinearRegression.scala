package com.raddi

object LinearRegression {
  val learningRate: Float = 0

  def loss_root(weight: Vector[Float], x: Vector[Float], y:Float): Float ={
    y-MiscMath.dot(weight.tail,x)-weight.head
  }

  def slope(loss_root: Vector[Float], x: Vector[Float]): Vector[Float] = {
    val avg_loss=loss_root.sum/loss_root.length
    MiscMath.multScalar(x,avg_loss)
  }

  def iteration(weight: Vector[Float], x:Vector[Vector[Float]], y:Vector[Float]): Vector[Float] = {
    if(y.length > 1){
      loss_root(weight,x.head,y.head) +: iteration(weight,x.tail,y.tail)
    }
    else{
      Vector(loss_root(weight,x.head,y.head))
    }
  }

  def fit(x:Vector[Float], y:Vector[Float]): Vector[Float] = {
    val weight: Vector[Float] = MiscMath.randVector(x.length+1, 1)
    weight
  }
}
