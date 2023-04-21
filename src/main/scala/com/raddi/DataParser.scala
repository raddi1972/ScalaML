package com.raddi

object DataParser {
  trait Type {
    def toString: String
  }
  case class FrameString(value: String) extends Type {
    override def toString: String = {
      value
    }
  }
  case class FrameInt(value: Int) extends Type {
    override def toString: String = {
      s"$value"
    }
  }
  case class FrameFloat(value: Float) extends Type {
    override def toString: String = {
      s"$value"
    }
  }

  case class Dataframe(data: Vector[Vector[Type]], columns: Vector[String]) {
    def head(): Vector[Vector[Type]] = {
      val headData = if(data.length >= 5) {
        data.slice(0, 5);
      } else {
        data
      }
      headData
    }

    // This function has side effects!
    def printHead(): Unit = {
      val head = this.head()
      val len = columns.foldLeft[Int](0)((max, string) => if(max < string.length) string.length else max)
      val maxLength = Math.min(head.foldLeft[Int](len)((max, data) => {
        data.foldLeft(max)((acc, tp) => {
          val string = tp.toString
          if(max < string.length) string.length else max
        })
      }), 15)
      println(maxLength)
      def makeFormatString(column: Vector[String]): String = {
        column.foldLeft[String]("")((acc, a) => {
          s"$acc | %-${maxLength}s |"
        })
      }
      val formatString = makeFormatString(columns)
      printf(formatString+"\n", columns:_*)
      head.foreach((row) => {
        val trunkatedRow = row.map(str => str.toString.substring(0, Math.min(str.toString.length,maxLength - 1)))
        printf(formatString+"\n", trunkatedRow:_*)
      })
    }
  }

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
