#Input a vector by a <- makeCacheMatrix(my_matrix). Now enter cachesolve(a). first time, inverse of a is
#calculated in cachesolve. Now, when we enter cachesolve(a) again, since the inverse was calculated 
#before, it is simply returned without calling the solve function again

## makeCacheMatrix is used to take the input, store the inverse and pass the inverse stored to cachesolve


makeCacheMatrix <- function(x = matrix()) 
{
        inverse <- NULL  #defining inverse and assigning NULL to inverse
        set <- function(y)   #can be added in cachesolve to change x in case new data is entered to cal inverse
        {
                x <<- y      #replacing x with y. <<- makes the change to the parent environment makecachematrix
                inverse <<- NULL   #since x has been changed, so resetting inverse 
        }
        get <- function() x  #gets matrix originally entered
        setinverse <- function(inv) inverse <<- inv  #assigns the inverse calculated by cachesolve
        getinverse <- function() inverse               #Retriving the value stored in inverse
        list(set = set, get = get,            #Storing all 4 sub fns as a list in makecachematrix
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Checks if we have something stored in ínverse for the given input. If YES, then it is directly returned.
## Otherwise solve function is used to calculate the inverse

cacheSolve <- function(x, ...) 
{
        inverse <- x$getinverse()   #getting the previously stored inverse 
        if(!is.null(inverse)) #in case the previously stored value inverse is not NULL, it is simply returned
        {
                message("getting cached data")
                return(inverse)   #the function terminates with return (inverse)
        }
        data <- x$get()     #otherwise solve function is used to calculate inverse by assigning our input to data
        inverse <- solve(data, ...) #assigning 'inverse' the calculated inverse
        x$setinverse(inverse)         #stores inverse in makecachematrix
        inverse
}
