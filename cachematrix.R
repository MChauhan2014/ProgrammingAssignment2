## The first function,  makeVector  creates a special "vector", which is really a list containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean



makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
    } 
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
  } 



## Write a short comment describing this function
#cacheSolve : This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then  cacheSolve  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
 inv <- x$getinverse() 
 if(!is.null(inv)) { 
 message("getting cached data") 
 return(inv) 
 } 
data <- x$get() 
inv <- solve(data) 
 x$setinverse(inv) 
 inv 
 } 

## Sample Run 
#> x = rbind(c(1, -2), c(-2,1))
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1   -2
#[2,]   -2    1
#> cacheSolve(m)
#[,1]       [,2]
#[1,] -0.3333333 -0.6666667
#[2,] -0.6666667 -0.3333333
#> cacheSolve(m)
#getting cached data
#[,1]       [,2]
#[1,] -0.3333333 -0.6666667
#[2,] -0.6666667 -0.3333333
