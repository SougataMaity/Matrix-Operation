
# Matrix multiplication
mat_mul <- function(mat1, mat2) {
  if (dim(mat1)[2] == dim(mat2)[1]) {
    c = matrix(0, nrow = dim(mat1)[1], ncol = dim(mat2)[2])
    for (i in 1:dim(mat1)[1]) {
      for (j in 1:dim(mat2)[2]) {
        for (k in 1:dim(mat1)[2]) {
          c[i,j] = c[i,j] + mat1[i,k]*mat2[k,j]
        }
        
      }
      
    }
  }else{
    return('Matrix 1 dim not equal matrix2')
  }
  return(c)
}

A = matrix(c(1,2,3,4,5,6), nrow = 2)
B = matrix(c(seq(10,18,1)), nrow = 3)
mat_mul(A,B)   
mat_mul(B,A)

# transpose of a matrix
transpose <- function(mat1) {
  mat = matrix(0, nrow = dim(mat1)[2], ncol = dim(mat1)[1])
  for (i in 1:dim(mat1)[2]) {
    for (j in 1:dim(mat1)[1]) {
      mat[i,j] = mat1[j,i]
      
    }
    
  }
  return (mat)
}

transpose(A)

# trace value of a matrix

trace <- function(mat1) {
  if (dim(mat1)[1] == dim(mat1)[2] ){
    count = 0
    for (i in 1:dim(mat1)[1]){
      for (j in 1:dim(mat1)[2]){
        if (i == j){
          count = count + mat1[i,j]
        }
      }
    }
  }else{
    return ('Non Square matrix Trace does not exist')
  }
  return (count)
}

trace(B)
trace(A)

# Off diagonal element sum

off_dsum <- function(mat1) {
  count = 0
  for (i in 1:dim(mat1)[1]){
    for (j in 1:dim(mat1)[2]){
      if(i != j){
        count = count + mat1[i,j]
      }
    }
  }
  return (count)
}
off_dsum(B)

# matrix elements sum
mat_sum <- function(mat1) {
  count = 0
  for(i in 1:dim(mat1)[1]){
    for(j in 1:dim(mat1)[2]){
      count = count + mat1[i,j]
    }
  }
  return(count)
}
mat_sum(B)

# Symmetric matrix
mat_sym <- function(mat1) {
  row = dim(mat1)[1]
  col = dim(mat1)[2]
  if(row == col){
    for(i in 1:row){
      for(j in 1:col){
       if(i != j){
         if(mat1[i,j] == mat1[j,i]){
           return ('Yes this is a Symmetric Matrix')
         }else{
           return('No this is not a symmetric Matrix')
         } 
       }
      }
    } 
  }else{
    return('This is not a square matrix')
  }
}
mat_sym(A)
mat_sym(B)

C = matrix(c(1,2,3,2,3,4,3,4,5), nrow = 3, byrow = T)
C
mat_sym(C)

# Determinant of a matrix
det


