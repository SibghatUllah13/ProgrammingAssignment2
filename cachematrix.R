


###### This function creates a special "matrix" object that will cache its inverse ### 
makeCacheMatrix <- function(x = matrix()) { 
  #### Set the Variables inv to Null for now, and set the matrix x ####### 
  inv <- NULL 
  set <- function(y) { 
    x <<- y 
    inv <<- NULL 
  } 
  ###### Function to get the value of matrix x ##########
  get <- function() x 
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv 
  #### A special list which will be sent to and used by function cacheSolve ########
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
} 

############## Function to compute the value of matrix sent by above function ####
cacheSolve <- function(x, ...) { 
  
  ### if the inverse has already been computed,it will retrieve the inverse from cache ##
  inv <- x$getinverse() 
  if(!is.null(inv)) { 
    message("getting cached data.") 
    return(inv) 
  } 
  ### else, it will compute it itself ####
  data <- x$get() 
  inv <- solve(data) 
  x$setinverse(inv) 
  inv 
  ### return the calculated inverse ####
} 
