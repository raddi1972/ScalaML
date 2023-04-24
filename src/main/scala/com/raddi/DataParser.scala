package com.raddi

object DataParser {
  trait Type {
    def toString: String
  }
  object Type {
    val predTable = Map(
      "string" -> 0,
      "float" -> 1,
      "int" -> 2,
      "null" -> 100
    )
    val defaultValues = Map(
      "string" -> "",
      "float" -> 0.0f,
      "int" -> 0
    )
    val baseType = "int" // Worst precedence can be easily converted to other types
    def getDefaultValue(typ: String) : Any = {
      defaultValues(typ)
    }
    def getTypePrecedence(e1: Type, e2: Type): Boolean = {
      val type1 = dType(e1)
      val type2 = dType(e2)
      getTypePrecedence(type1, type2)
    }
    def getTypePrecedence(e1: String, e2: String): Boolean = {
     predTable(e1) <= predTable(e2)
    }
    def dType(element: Type): String = {
      element match {
        case FrameInt(_) => "float"
        case FrameString(_) => "string"
        case FrameFloat(_) => "int"
      }
    }
  }
  case object Null extends Type {
    val stringDefault: FrameString = FrameString("")
    val intDefault: FrameInt = FrameInt(0)
    val floatDefault: FrameFloat = FrameFloat(0.0f)
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

  case class Dataframe(data: Vector[Vector[Type]], columns: Vector[(String, String)]) {

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

  def identifyColumnType(df:List[List[String]], columnNames: List[String]): Vector[String] = {
    val baseTypes = columnNames.map(it => Type.baseType)
    df.map(row => {
      row.map(element => {
        val maybeInt = element.toIntOption
        val maybeFloat = element.toFloatOption
        val result: String = maybeInt match {
          case Some(number) => "int"
          case None => maybeFloat match {
            case Some(number) => "float"
            case None => "string"
          }
        }
        if(element.isEmpty)
          "null"
        else result
      })
    }).foldLeft(baseTypes) ((acc, list) => {
      acc.zip(list).map{
        case (typ, lsType)=>{
          if(Type.getTypePrecedence(typ, lsType)) typ else lsType
        }
      }
    }).toVector
  }

  def makeDataframe(data: List[List[String]]): Dataframe = {
    val columnNames = data.head.toVector
    val columnTypes = identifyColumnType(data.tail, columnNames.toList)
    val rows = data.tail // Need to convert this to data
    val rowData = rows.map(tuple => {
      tuple.zip(columnTypes).map {
        case (data, typ) => {
          if (data.isEmpty) Null
          else typ match {
            case "int" => FrameInt(data.toInt)
            case "float" => FrameFloat(data.toFloat)
            case "string" => FrameString(data)
          }
        }
      }.toVector
    })
    Dataframe(rowData.toVector, columnNames.zip(columnTypes))
  }

}
