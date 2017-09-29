## renberg 
## september 29, 2019 

## makeCacheMatrix: 
## the funtion creates a list 
## set the value of matrix
## get the value of matrix
## set value of inverse 
## get value of inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## chacheSolve: 
## the function returns inverse of matrix 
## check if already computed -> yes -> skips computation 
## check if already comuted ->  no -> computes the inverse and sets the value in the cache 

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data)
  x$setinverse(inver)
  inver
}


## Examples of function: 
a <- rbind(c(1, -1/5), c(-1/5, 1))
mat <- makeCacheMatrix(a)
mat$get()

## when there is no cache ... it will compute 
cacheSolve(mat)

##  when there is cache  ... it retrives 
cacheSolve(mat)




