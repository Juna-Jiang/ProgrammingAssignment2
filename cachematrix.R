## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function make Cache Matrix creates a list of 4 components, which containing
## the input matrix and the inverse matrix. 
rm(list=ls())
makeCacheMatrix <- function(x = matrix()){ 
        cache<-NULL
        set<- function(y){
                x<<-y
                m<<- NULL
        }
        get<- function ()x
        setMatrix<- function(inverse) cache<<-inverse
        getInverse<- function() cache
        list(set =set, get=get,
             setMatrix = setMatrix,
             getInverse = getInverse)    
}


## The function cacheSolve will check if the inverse was calculated.If yes,the function will
## capture the inverse matrix from cache date. Otherwise it captures the original matrix through get
## function and calculate the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache<- x$getInverse()     # passing the value of x of in the function getMatrix to cache
        if(!is.null(cache)) {
                message("getting cashed data") 
                return(cache)
        }
        matrix<- x$get()
        cache<- solve(matrix,...)
        finally = {x$setMatrix(cache)  }
        return(cache)
}
# Test run
        x<- matrix(c(1,3,5,6,4,8,2,3,45),3,3)
        cache<- makeCacheMatrix(x)
        cacheSolve(cache)

        