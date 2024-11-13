
#
mdiag <- function(matrix) {
  if (is.vector(matrix)){
    return(diag(matrix))
  } else if (nrow(matrix) == 0 && ncol(matrix) == 0) {
    return(matrix(0,0,0))
  } else if (nrow(matrix) == 1 && ncol(matrix) == 1) {
    return(matrix)
  } else if ((nrow(matrix) > 1 && ncol(matrix) == 1) || (nrow(matrix) == 1 && ncol(matrix) > 1)) {
    return(diag(as.vector(matrix)))
  } else if (nrow(matrix) > 0 && ncol(matrix) > 0) {
    return(diag(matrix))
  } else {
    message("Error in the mdiag function!")
  }
}
 
nmatrix <- function(x, nrow, ncol) {
  if (nrow <0 || ncol < 0){
    return(matrix(0, 0, 0))
  } else {
    return(matrix(x, nrow, ncol))
  }
}

mcbind <- function(A, B) {
  if (is_empty(A)) {
    return(cbind(matrix(0,nrow(B),0), B))
  } else {
    return(cbind(A,B))
  }
}

m3cbind <- function(A, B, C) {
  if (is_empty(A)) {
    return(cbind(matrix(0,nrow(C),0), B, C))
  } else {
    return(cbind(A,B,C))
  }
}







