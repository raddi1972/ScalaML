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

  trait Indexes
  case object All extends Indexes
  case class Index(value: Int) extends Indexes
  case class Range(i: Int, j: Int) extends Indexes

  case class Dataframe(data: Vector[Vector[Type]], columns: Vector[(String, Type)]) {

    /**
     * This function returns the row or column based on the given string
     * @param i: index of row (Use the Indexes Type)
     * @return the row with the given index
     */
    def apply(i: Indexes): Dataframe = {
      i match {
        case Range(i, j) => Dataframe(data.slice(i, j), columns)
        case Index(value) => Dataframe(Vector(data(value)), columns)
        case All => Dataframe(data, columns)
      }
    }
    def apply(i: Indexes, j: Indexes) : Dataframe = {
      val rowData = i match {
          case Range(rowStart, rowEnd) => data.slice(rowStart, rowEnd)
          case Index(value) => data.slice(value, value+1)
          case All => data
        }
      j match {
        case Range(colStart, colEnd) =>
          val colData = rowData.map(row => row.slice(colStart, colEnd))
          Dataframe(colData, columns.slice(colStart, colEnd))

        case Index(value) =>
          val newData = rowData.map(row => row.slice(value, value + 1))
          Dataframe(newData, columns.slice(value, value + 1))

        case All =>
          val newData = rowData
          Dataframe(newData, columns)

      }
    }

    def head(): String =
      Dataframe(if(data.length >= 5)
        data.slice(0, 5)
      else
        data, columns).toString



    // This function has side effects!
    override def toString: String = {
      val columnNames = columns.map(col => col._1)
      val len = columnNames.foldLeft[Int](0)((max, string) => if(max < string.length) string.length else max)
      val maxLength = Math.min(data.foldLeft[Int](len)((max, data) => {
        data.foldLeft(max)((acc, tp) => {
          val string = tp.toString
          if(acc < string.length) string.length else max
        })
      }), 15)
      println(maxLength)
      def makeFormatString(column: Vector[String]): String = {
        column.foldLeft[String]("")((acc, _) => {
          s"$acc | %-${maxLength}s |"
        })
      }
      val formatString = makeFormatString(columnNames)
      printf(formatString+"\n", columnNames:_*)
      data.foreach(row => {
        val truncatedRow = row.map(str => str.toString.substring(0, Math.min(str.toString.length,maxLength - 1)))
        printf(formatString+"\n", truncatedRow:_*)
      })
      ""
    }
  }

  def makeDataframe(data: List[List[String]]): Dataframe = {
    val columnNames = data.head.toVector
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
    def makeColumns(list1: List[String], list2: List[Type], i : Int): List[(String, Type)] = {
      list1 match {
        case List() => List()
        case head::tail => (head, list2.head) :: makeColumns(tail, list2.tail, i+1)
      }
    }
    Dataframe(rowData.toVector, makeColumns(columnNames.toList, rowData.head.toList, 0).toVector)
  }

}
