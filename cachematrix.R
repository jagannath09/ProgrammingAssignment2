## Caching the Inverse of a Matrix
## This function creates a special "matrix" object that can cache its inverse

## This function is intended to perform the followng 4 sub functions.
## set the value of the matrix
## get the value of the matrix
## set the inverse of matrix
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
             inv <- NULL
             set <- function(y) {
             
                     x <<- y
                     inv <<- NULL
             }
             get <- function() x
             setInverse <- function(inverse) inv <<- inverse
             getInverse <- function() inv
             list(set = set
                , get = get
                , setInverse = setInverse
                , getInverse = getInverse
                 )
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## x: reference of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()
        
cacheSolve <- function(x=matrix(), ...) {             
            ## Return a matrix that is the inverse of 'x'
              inv <- x$getInverse()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		# else calculate the inverse
    message("calculate inverse")
		data <- x$get()
		inv <- solve(data, ...)
		x$setInverse(inv)
    return(inv)
}


CheckCache=function(i_matrix)
{
  for ( i in 1:3) # test the cashe in 3 attempts
  {
    if (i==1)
    {   
      message("Before Cache")
      temp = makeCacheMatrix(i_matrix)
    }
      else
      {
        message(sprintf("After Cache, attempt: %s", i-1))
       }
    PrevTime = Sys.time()  
    cacheSolve(temp)
    duration= Sys.time() - PrevTime
    print(duration)
  }
}
 
 set.seed(2250000) # to generate consistent numbers in successive attempts
 r=rnorm(2250000)  # to generate random number to fill 1500 X 1500 matrix
 m1=matrix(r,nrow=1500, ncol= 1500)
 CheckCache(m1)
