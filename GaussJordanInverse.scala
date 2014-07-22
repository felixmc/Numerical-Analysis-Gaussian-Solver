object GaussJordanInverse {

  def idMatrix() : Matrix3 = {
    val matrix = new Matrix3
    
    matrix(1, new Row3(1, 0, 0))
    matrix(2, new Row3(0, 1, 0))
    matrix(3, new Row3(0, 0, 1))
    
    return matrix
  }
  
  def solve(matrix: Matrix3): (Double, Double, Double) = {
    var x : Double = 0
    var y : Double = 0
    var z : Double = 0
    
    val id = idMatrix()
    
    println("Problem: \n" + matrix.toString)
    
    normalize(matrix, id, 1, 0)
    eliminate(matrix, id, 1, 2, 0)
    normalize(matrix, id, 2, 1)

    eliminate(matrix, id, 1, 3, 0)
    eliminate(matrix, id, 2, 3, 1)
    normalize(matrix, id, 3, 2)
    
    eliminate(matrix, id, 3, 2, 2)
    eliminate(matrix, id, 3, 1, 2)
    eliminate(matrix, id, 2, 1, 1)
    
    println("Final: ")
    println()
    println(id)
    
    z = matrix(3)(3) / matrix(3)(2)
    y = (matrix(2)(3) - (matrix(2)(2) * z)) / matrix(2)(1)
    x = (matrix(1)(3) - (matrix(1)(1) * y) - (matrix(1)(2) * z)) / matrix(1)(0)
    
    return (x,y,z)
  }
  
  private def normalize(matrix: Matrix3, id: Matrix3, row: Int, col: Int) {
    matrix(row, matrix(row) / matrix(row)(col))
    id(row, id(row) / matrix(row)(col))
  }
  
  private def eliminate(matrix: Matrix3, id: Matrix3, base: Int, target: Int, col: Int) {
    val er = matrix(base) / matrix(base)(col) * matrix(target)(col)
    val ider = id(base) / matrix(base)(col) * matrix(target)(col)
    if (matrix(target)(col).signum == er(col).signum) {
      matrix(target, matrix(target) - er)
      id(target, id(target) - ider)
    } else {
      matrix(target, matrix(target) + er)
      id(target, id(target) + ider)
    }
  }
  
}