//package com.raddi
//
//object MiscMath {
//  def dot(v1: Vector[Float], v2: Vector[Float]): Float = {
//    if(v1.length>1){
//      v1.head * v2.head +dot(v1.tail,v2.tail)
//    }
//    else{
//      v1.head * v2.head
//    }
//  }
//
//  def multScalar(v1: Vector[Float], c: Float): Vector[Float] = {
//    if(v1.length>1) {
//      (v1.head*c)+multScalar(v1.tail,c)
//    }
//    else{
//      Vector(v1.head * c)
//    }
//  }
//
//  def randVector(length : Int, max: Int): Vector[Float] = {
//    val r = scala.util.Random
//    def loop(length: Int, max: Int): Vector[Float] = {
//      if(length == 1){
//        Vector(max*r.nextFloat)
//      }
//      else{
//        (max*r.nextFloat)+loop(length-1, max)
//      }
//    }
//    loop(length, max)
//  }
//}
