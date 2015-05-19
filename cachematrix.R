## Put comments here that give an overall description of what your
## functions do



## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # define mxi as NULL
  mxi <- NULL
  # push original data to global environment to cache.
  # pls note that calling function won't use the set method
  set <- function(y) {
    x <<- y
    mxi <<- NULL
  }
  # return the cached original matrix
  get <- function() x
  
  # obtains inversed matrix data from the calling function
  # pushes to 'mxi' in the global environment
  setmxinverse <- function(mxinverse) mxi <<- mxinverse
  getmxinverse <- function() mxi
  
  # return a list of method names for calling function to refer to
  list(set = set, get = get,
       setmxinverse = setmxinverse,
       getmxinverse = getmxinverse)
}





## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. 
##
## If the inverse has already been calculated (and the matrix
## has not changed), then this function should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # try x from the global environment
  # the method of x is $getmxinverse in cached data
  mxi <- x$getmxinverse()
  
  # if not cached mix is NULL
  # but !is.null == true indicates that cached data exists
  if(!is.null(mxi)) {
    message("... CACHED matrix-inverse")
    # return and exit the function.
    return(mxi)
  }
  
  # when cached data is NULL proceed with calculation of inverse
  # request data from the local scope of the makeCacheMatrix.R
  data <- x$get()
  # calculate the inverse 
  im <- solve(data, ...)
  # and save it to global environment to cache
  x$setmxinverse(im)
  
  # set the message regarding initial calculation, not cached yet
  message("... NEW instance of matrix-inverse")
  print(im)  #return inversed matrix by printing to terminal (test purposes)
}







## ================================================================
## FOLLOWING IS A TEST OF BOTH CACHE AND INVERSE (SOLVE) OPERATIONS


m=matrix(c(1,5,7,8,9,4,6,2,3), nrow=3, ncol=3)
message("... ORIGINAL test matrix")
print(m)

nm = makeCacheMatrix(m)
o1 <- cacheSolve(nm) # initial: should be new instance of inverse
im <- cacheSolve(nm) # second call :  should be the cached version 
print(im) 

## another test: return to the original by inversing the inversed
om <- makeCacheMatrix(im)
## and inverse the inversed matrix => original matrix
om1 <- cacheSolve(om) # initial: should be new instance of inverse of inverse = original
om2 <- cacheSolve(om) # second call :should be the cached version of the original
print(om2)


## ======================================
## TEST OUTPUT IS AS FOLLOWS



# ... ORIGINAL test matrix
#       [,1] [,2] [,3]
# [1,]    1    8    6
# [2,]    5    9    2
# [3,]    7    4    3

# ... NEW instance of matrix-inverse
#              [,1]       [,2]       [,3]
# [1,] -0.076923077  0.0000000  0.1538462
# [2,]  0.004048583  0.1578947 -0.1133603
# [3,]  0.174089069 -0.2105263  0.1255061

# ... CACHED matrix-inverse
#              [,1]       [,2]       [,3]
# [1,] -0.076923077  0.0000000  0.1538462
# [2,]  0.004048583  0.1578947 -0.1133603
# [3,]  0.174089069 -0.2105263  0.1255061


# RETURN to ORIGINAL MATRIX

# ... NEW instance of matrix-inverse
#       [,1] [,2] [,3]
# [1,]    1    8    6
# [2,]    5    9    2
# [3,]    7    4    3

# ... CACHED matrix-inverse
#       [,1] [,2] [,3]
# [1,]    1    8    6
# [2,]    5    9    2
# [3,]    7    4    3

## ======================================

