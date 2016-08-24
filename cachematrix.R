######################################################
##                                                  ##
##  Assignment 2: Caching the Inverse of a Matrix 	##
##                                                  ##
######################################################

# Below are two functions that are used to create a special object that 
# stores a numeric matrix and caches its inverted matrix.
# This file also contains a section at the end with tests one can run to check the functions, 
# just cut-n-paste to the console as required.


# The function `makeCacheMatrix` creates a list containing a function to:
# 1.  set the values of the matrix
# 2.  get the value of the matrix 
# 3.  set the value of the inverted matrix
# 4.  get the value of the inverted matrix
makeCacheMatrix <- function(x = matrix()) {
  
  #Try and see if the matrix is singular first, no point in going further...
  if (try(det(x))==0){
    message("Your matrix is singular and cannot be inverted...")    
    return
  }
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  inv
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function calculates the inverse of the special matrix 
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setinverse` function.
cacheSolve <- function(x, ...) 
{

  inv<-x$getinverse()#   ## Return a matrix that is the inverse of 'x'
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Little testers below...
## -----------------------
##
## > m1<-rbind(c(1,2),c(1,1)) # this is 'normal' i.e. invertible matrix
## > mcm1<-makeCacheMatrix(m1)
## > cacheSolve(mcm1)         # see expected results below
##  [,1] [,2]
## [1,]   -1    2
## [2,]    1   -1
## > cacheSolve(mcm1)         # you should now see cached results below
##Getting cached data...
##  [,1] [,2]
##[1,]   -1    2
##[2,]    1   -1
##
##> m1<-rbind(c(1,2),c(2,2))  #now change the original matrix, no more cached results  
##> mcm1<-makeCacheMatrix(m1)
##> cacheSolve(mcm1)
##[,1] [,2]
##[1,]   -1  1.0
##[2,]    1 -0.5
##> cacheSolve(mcm1)          #...back to the cached inverse...
##Getting cached data...
##[,1] [,2]
##[1,]   -1  1.0
##[2,]    1 -0.5
## 
## > m1<-rbind(c(5,5),c(5,5)) # now try a singular matrix... and see the warning message 
## > mcm1<-makeCacheMatrix(m1)
## Your matrix is singular and cannot be inverted...