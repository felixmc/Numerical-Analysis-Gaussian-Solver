object GaussJordanSolver {

  
  def solve(matrix: Matrix3): (Double, Double, Double) = {
    var x : Double = 0
    var y : Double = 0
    var z : Double = 0
    
    println("Problem: \n" + matrix.toString)
    
    normalize(matrix, 1, 0)
    eliminate(matrix, 1, 2, 0)
    normalize(matrix, 2, 1)
        
    eliminate(matrix, 1, 3, 0)
    eliminate(matrix, 2, 3, 1)
    normalize(matrix, 3, 2)
    
    eliminate(matrix, 3, 2, 2)
    eliminate(matrix, 3, 1, 2)
    eliminate(matrix, 2, 1, 1)
    
    println("Final: ")
    println()
    println(matrix)
    
    z = matrix(3)(3) / matrix(3)(2)
    y = (matrix(2)(3) - (matrix(2)(2) * z)) / matrix(2)(1)
    x = (matrix(1)(3) - (matrix(1)(1) * y) - (matrix(1)(2) * z)) / matrix(1)(0)
    
    println(s"Solution: \n   x = $x\n   y = $y\n   z = $z")
    
    return (x,y,z)
  }
  
  private def normalize(matrix: Matrix3, row: Int, col: Int) {
    matrix(row, matrix(row) / matrix(row)(col))
  }
  
  private def eliminate(matrix: Matrix3, base: Int, target: Int, col: Int) {
    val er = matrix(base) / matrix(base)(col) * matrix(target)(col)
    matrix(target, if (matrix(target)(col).signum == er(col).signum)
      matrix(target) - er
      else
        matrix(target) + er)
  }
  
}