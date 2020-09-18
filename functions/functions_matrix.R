transposeMatrix = function(mat)
{
  t(mat);
}
rotateMatrix90 = function(mat)
{
  offDiagonal = matrix(c(0,0,1,    # Exchange matrix 
                         0,1,0,
                         1,0,0), nrow = 3, byrow = T) 
  
  transposeMatrix(mat) %*% offDiagonal;
}


rotateMatrix180 = function(mat)
{
  offDiagonal = matrix(c(0,0,1,    # Exchange matrix
                         0,1,0,
                         1,0,0), nrow = 3, byrow = T)
  
  transposeMatrix(rotateMatrix90(mat)) %*% offDiagonal;
}


rotateMatrix270 = function(mat)
{
  offDiagonal = matrix(c(0,0,1,   # Exchange matrix
                         0,1,0,
                         1,0,0), nrow = 3, byrow = T)
  
  transposeMatrix(rotateMatrix180(mat)) %*% offDiagonal
  
}