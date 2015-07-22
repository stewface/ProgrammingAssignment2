## First function creates a matrix 
## this is a list containing a function to set then get the value of matrix
## then to set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        #set is a function that changes the matrix stored in the main function.
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        #get the value of matrix 
        get <- function() x
        #setmatrix and getmatrix are functions very similar to set and get. 
        setmatrix <- function(solveM) s <<- solveM
        getmatrix <- function() s
        
        #They don't calculate the mean, they simply store the value of the 
        #input in a variable m into the main function makeVector (setmean) and return it (getmean).
        
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## This function calucates the inverse of the special matrix
## we created above

## First it checks if the inverse has been calcuated. 
## If so grabs it from cache and skips to calculation
## if not it will run the calcuation and set the value in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## check if we have it in cache and grab it
        
        s <- x$getmatrix()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        #The first thing cachesolve does is to verify the value s, stored previously with getmatrix, 
        #exists and is not NULL. If it exists in memory, 
        #it simply returns a message and the value s, that is supposed to be the inverse, but not necessarily.
        
        # get data
        data <- x$get()
        # cacluate inverse
        s <- solve(data, ...)
        x$setmatrix(s)
        #return result
        s
}

