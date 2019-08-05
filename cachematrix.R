## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#creates a special matrix that cache the inverse of the matrix
makeCacheMatrix<-function(y=matrix()){
  # initialize the property of matrix i (cache) to NULL
  i<-NULL
  #method to set(create) matrix denoted by m
  set<-function(m){
    y<<-m
    i<<-NULL
  }
  #method to get value of matrix 
  get<-function(){
    #return the value of matrix y
    y
  }
  #method to set inversr of matrix and store in "i"
  setInverse<-function(inverse){
    i<<-inverse
  }
  #method to get inversr of matrix from "i" 
  getInverse<-function(){
    #return the inverse property matrix i
    i
  }
  #return a list of the method that created function in working directory
  list(set=set, get=get, 
       setInverse=setInverse, 
       getInverse=getInverse)
}

#compute inverse of matrix makeCacheMatrix by "cacheSolve" function and store in "i"
cacheSolve<-function(x, ...){
  #return the inverse of matrix y stored in "i"
  i<-x$getInverse()
  #just return the inverse if its already set and print message "getting cached data"
  if(!is.null(i)){
    message("getting cached data")
    #display matrin inverse in console
    return(i)
  }
  # get(create) matrixr from the given object as it does not exist(be sure matrix is square)
  data<-x$get()
  # calculate the inverse from matrix and store in "i"
  i<-solve(data, ...)
  # set inverse to object
  x$setInverse(i)
  #returm to matrix i
  i
}
