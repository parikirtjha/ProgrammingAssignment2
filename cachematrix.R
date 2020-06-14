##function to store value of inverse if already calculated
makeCacheMatrix<-function(x=matrix())
{
  i<-NULL ##initialising the inverse 
  set<-function(matrix) {
    x<<-matrix                    ##setting value of matrix in parent environment
    #if new value of matrix is set cached value is made NULL
    i<<-NULL
  }
  get<-function()
  {
    x                             ##function returning value of matrix 
  }
  setInverse<-function(inverse)   ##function to set inverse of matrix 
  {
    i<<-inverse                   ##setting value of inverse in parent environment
  }
  
  getInverse<-function()
  {
    i                             ##function returning value of inverse matrix
  }
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

##function to check if cached value present if not calculates inverse and if present returns cached value
cacheSolve<-function(x, ...)
{
  i<-x$getInverse()
  
  if(!is.null(i)){
    message('getting cached value')                  ##return inverse if already cached
    return(i) 
  }
  data<-x$get()
  i<-solve(data) %*% data           ##calculate inverse using matrix multiplication
  x$setInverse(i) 
  i##return matrix    
}
