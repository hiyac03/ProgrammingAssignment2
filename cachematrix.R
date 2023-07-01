## Put comments here that give an overall description of what your
## functions do

## library(MASS) is used to calculate inverse for matrix 
## makeCacheMatrix consists of set, get, setinv, getinv
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<<-NULL
  }
  
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x
  }
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
## used to get cache data 
cacheSolve <- function(x, ...) {
       inv<-x$getinv()
       if(!is.null(inv)){
         message("getting cache data")
         return(inv)
       }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
   ## Return a matrix that is the inverse of 'x'
}
