## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Here is an example of how the code is used. 
#You use the function makeCacheMatrix on a matrix. This generates a list of functions pertaining to the matrix x
#Then you use the function cacheinverse on the previous output to obtain the mean or the cached mean (Dependent on if the function has already been run).
#The example command of "x$get()" takes the list x and uses the get function. The get output  is simply the value of x

"
> m <- matrix(c(-1, -2, 1, 1), 2,2)
> x <- makeCacheMatrix(m)
> x$get()
                             [,1] [,2]
                        [1,]   -1    1
                        [2,]   -2    1
> inv <- cacheinverse(x)
> print (inv)
                             [,1] [,2]
                        [1,]    1   -1
                        [2,]    2   -1
> inv <- cacheinverse(x)
                        getting cached data
> print (inv)
                             [,1] [,2]
                        [1,]    1   -1
                        [2,]    2   -1
"

makeCacheMatrix <- function(x = matrix()) { #This function can be called to generate a list of functions based upon an input number.
        m <- NULL #This declares the variable m to be NULL initially so that it can be used later in logic
        set <- function(y) {#This could be used to set a new value for x
                x <<- y
                m <<- NULL
        }
        get <- function() x #stores the cached matrix
        setinverse <- function(inverse) m <<- inverse #stores the cached inverse
        getinverse <- function() m #stores the chached inverse value
        list(set = set, get = get, #generates a list of the functions defined above. This list is refrencable by using the command "makeMatrixCache$desiredfunction"
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheinverse <- function(x, ...) {
        m <- x$getinverse() #This loads the value of the cached inverse and stores it as m
        if(!is.null(m)) { #This tests to see if a cached inverse has been generated. Since makeCacheMatrix initially stores m as "NULL" it is able to determine this.
                message("getting cached data") #Prints the message notifying the user that x's inverse has been previously stored in the cache
                return(m) #Returns the cached inverse
        }
        data <- x$get() #This loads the matrix x into the variable data
        m <- solve(data, ...) #This finds the inverse of data and stores it in m
        x$setinverse(m) #This sets the cached inverse to be the new value for m
        m #This returns the value of the inverse m

        
        ## Return a matrix that is the inverse of 'x'
}

