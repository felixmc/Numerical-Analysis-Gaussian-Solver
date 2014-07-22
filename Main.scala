object Main extends App {

  val matrix = new Matrix3()
  
  matrix(1, new Row3(1, 1, 1, 0))
  matrix(2, new Row3(2, -1, -4, 15))
  matrix(3, new Row3(1, -2, -1, 7))
  
  GaussJordanSolver.solve(matrix)
}