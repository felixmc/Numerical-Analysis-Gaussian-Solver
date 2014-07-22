import scala.collection.mutable

class Matrix3 {
  var rows: Map[Int, Row3] = Map((1, new Row3()), (2, new Row3()), (3, new Row3()))

  def apply(i: Int, row: Row3) {
    assert(i > 0 && i <= 3)
    rows += ((i, row))
  }

  def apply(i: Int): Row3 = {
    assert(i > 0 && i <= 3)
    return rows(i)
  }
  
  def switch(r1: Int, r2: Int) {
    assert(r1 > 0 && r1 <= 3)
    assert(r2 > 0 && r2 <= 3)
    val temp: Row3 = rows(r1)
    rows += ((r1, rows(r2)))
    rows += ((r2, temp))
  }

  override def toString(): String = {
    val sb = new StringBuilder
    
    rows.foreach(r => {
    	sb ++= s"[ ${r._2}  ]\n"
    })
    
    return sb.toString
  }

}

class Row3(val x: Double, val y: Double, val z: Double, val r: Double = 0) {

  def this() = this(0, 0, 0, 0)

  def *(s: Double): Row3 = new Row3(x * s, y * s, z * s, r * s)
  def /(s: Double): Row3 = new Row3(x / s, y / s, z / s, r / s)
  def +(row: Row3): Row3 = new Row3(x + row.x, y + row.y, z + row.z, r + row.r)
  def -(row: Row3): Row3 = new Row3(x - row.x, y - row.y, z - row.z, r - row.r)

  def apply(i : Int): Double = {
    assert(i >= 0 && i < 4)
    return i match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => r
    }
  }
  
  override def toString() : String = {
    return s"${ts(x)} ${ts(y)} ${ts(z)} | ${ts(r)}"
  }
  
  private def ts(n: Double) :String = {
    return (if (Math.abs(n) < 10) " " else "") + (if (n >= 0) " " else "") + n.toString + " "
  }
  
}