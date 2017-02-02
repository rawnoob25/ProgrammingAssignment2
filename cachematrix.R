## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#constructor funtion w/ mutators and accessors for a matrix and its inverse;
#
#the constructor function essentially serves as a wrapper for a matrix
#and facilitates setting and retrieving of its inverse
#
#the wrapped matrix can be reset
#
#default value of argument is an empty matrix
#
#function returns a list comprised of the 2 mutator and 2 accessor functions
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function(){
      x
    }
    setInverse<-function(i){
      inv<<-i
    }
    getInverse<-function(){
      inv
    }
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

##precondition: the datatype of x is list returned by makeCacheMatrix()
#
#function returns the inverse of the matrix wrapped in the makeCacheMatrix() instance
#(essentially a list),x
#
#the inverse has already been computed, it's retrieved from the cache; otherwise, it's computed
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     matrInv<-x$getInverse()
     if(!is.null(matrInv)){
        message("getting cached data")
        return (matrInv)
     }
     data<-x$get()
     matrInv<-solve(data)
     x$setInverse(matrInv)
     matrInv
}
