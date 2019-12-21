## This is for Programming Assignment #2 on JHU R Programming course in Coursera

## This assigment is to learn about and observe how lexical scoping is used in R
##  The two functions will essentially create a special matrix, that when it is 
##  is initialized its inverse is computed and cached so that later the inverse
##  can be pulled form the cache memory rather than computing it again



## This funciton will create a special "matrix" object that will set (in parent)
## environement and get said values as a method to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## This is the beginning of the cachematrix fucntion.
  ## It will define default value of x as empty matrix
  
  matInv <- NULL ## Initilize cached inverse variable as null
  set <- function(y) { 
    ## This assigns values to variables listed to specified objects in parent 
    ## environment
    x<<-y
    matInv<<-NULL ## This will clear cached inverse if matrix is changed
  }
  get <- function() x
  setInv <- function(solve) matInv<<-solve
  getInv <- function() matInv ## returns matrix inverse if getInv is called
  list( ## This list allows us to give names to each sub-function to be called
        ##    by name later in 'cacheSolve'
       set=set,
       get=get,
       setInv=setInv,
       getInv=getInv
  )
}


## The function below will check to see if the inverse of a matrix is already
##    solved and retrieves it from the cache. If not already solved, it will 
##    calculate and return the value if there is nothing in the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    matInv <- x$getInv() ## This calles the cached inverse if available
    if(!is.null(matInv)){ ## This checkes to see if cached inverse is NULL
                          ##    and if it is already cached a message will be 
                          ##    displayed stating as such
        message("getting cached data") ## Display message in console to 
                                       ## demonstarte that the inverse is 
                                       ## already calculated
        return(matInv) ## returns cached inverse of matrix
    }
    ## Below is what will happen if the inverse is not already cached
    data <- x$get() ## Gets cached matrix data
    matInv <- solve(data,...) ## calcuates matrix inverse
    x$setInv(matInv) ## sets the matrix inverse within object x that is a part
                     ## defined with function 'makeCacheMatrix'
    matInv ## Returns inverse of the matrix
}
