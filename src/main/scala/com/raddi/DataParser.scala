package com.raddi

case object DataParser {
  trait Type;
  case class FrameString(value: String) extends Type
  case class FrameInt(value: Int) extends Type
  case class FrameFloat(value: Float) extends Type

  case class Dataframe(data: Vector[Vector[Type]], columns: Vector[String])

  def makeDataframe(data: List[List[String]]): Dataframe = {
    val columns = data.head.toVector
    val rows = data.tail // Need to convert this to data
    val rowData = rows.map(tuple => {
      tuple.map(element => {
        val maybeInt = element.toIntOption
        val maybeFloat = element.toFloatOption
        val result: Type  = maybeInt match {
          case Some(number) => FrameInt(number)
          case None => maybeFloat match {
            case Some(number) => FrameFloat(number)
            case None => FrameString(element)
          }
        }
        result
      }).toVector
    })
    Dataframe(rowData.toVector, columns)
  }
}
