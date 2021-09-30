
## Function to make matrix and inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL 
        set<-function(y){ #Function to set matrix based on function argument
                x<<-y
                inv<<-NULL
        }
        get<-function()x #Get matrix
        setinverse <- function(inverse) inv <<- inverse #Function to set the inverse of matrix
        getinverse <- function() inv #Get the inverse of matrix
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


## Function to cache the inverse of matrix

cacheSolve <- function(x, ...) {
        inv<-x$getinverse() 
        if(!is.null(inv)) { #Checks if the inverse is already cached
                message("getting cached data") #Returns message and inverse matrix if already cached
                return(inv)
        }
        data <- x$get() #If inverse matrix not cached, retrieves data
        inv<-solve(data,...) #Cache inverse of matrix x
        x$setinverse(inv)
        inv ## Return a matrix that is the inverse of 'x'
}
