## Author : Thierry Elnikoff
## Version : 0.1
##
## Description : This file contains functions necessary to :
##   * compute and cache the inverse of invertible matrix
##     to prevent heavy and multiple computing of inverses
##   * test and validate the reliability of the code.
##
## Tests is done without using any Unit Testing framework.
## However, in production running environment, this kind of unit control
## should be implemented in external files.
## Here, only some simple tests are done.
##


## makeCacheMatrix is a function used to effectively build a caching system
## for inverted matrix caching.
## It assumes input matrix is invertible.
## input : m must be a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        ## set.matrix will cache the value of the matrix given as input
        ## note that we do not compute inverse here, as this computation
        ## will have to occur only when we will need it
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }

        ## get.matrix return the cached matrix
        get <- function() x

        ## set.inverse will cache the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse

        ## get.inverse will get the cached inverse from parent environment
        getinverse <- function() inv

        ## give the ability to call members as this will return a vector of
        ## named functions
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve will compute and cache the inverse of a matrix.
## We assume input matrix is invertible
## dot-dot-dot argument will be passed to solve function.
## Not sure it's a good idea, as solve arguments allow to solve
## more than a single inverse (if b is given...)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Returning cached inverse")
                return(inv)
        }
        data <-x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}


## Usage :
##  1. Build a matrix named x (or whatever)
##  2. Call makeCacheMatrix on this matrix, and store the result as
##     "cached matrix"
##  3. Call cacheSolve on this cached matrix to get inverse
##     First run will need the computation of inverse, as this is not yet known
##     Subsequent runs will simply return cached result.
##  If we change the value of the matrix, and if the previous version is
##  different from the new, cached inverse is reset

## Testing functions
## First of all, we'll need a function to build a given sized squared matrix,
## with random numbers.
## make.RandomMatrix uses lexical scoping to give the ability to build any
## given sized matrix.
make.RandomMatrix <- function(matrixSize) {
        matrixBuilder <- function(...) {
                matrixElems <- rnorm(matrixSize^2,...)
                matrix(matrixElems,ncol=matrixSize, nrow=matrixSize)
#                rsparsematrix(nrow=matrixSize,ncol=matrixSize,density=1,...)
        }
        matrixBuilder
}

## We then use this builder to get some matrix builder of given sizes
## (we could have put these values in a list to automatically build those
## functions, but the aim is not there...)
make.sqM.3 <- make.RandomMatrix(3)
make.sqM.1000 <- make.RandomMatrix(1000)


## A matrix is invertible if and only if its determinant is different from 0
## So, this function will tell us if a given matrix can be inverted
is.invertible <- function(m) {
        out <- tryCatch(
                {
                        mm <- as.matrix(m)
                        det(mm) != 0
                },
                error=function(cond) {
                        message("Error occured during control:")
                        message(paste(cond))
                        return(FALSE)
                },
                warning=function(cond) {
                        message("A warning was given during control:")
                        message(paste(cond))
                        return(FALSE)
                })
        return(out)
}


## First test :
## We will build a 3x3 matrix and compute its inverse.
## Then, we will get it back using caching capabilities
test_1 <- function() {
        message ("Test 1 : 3x3 matrix inverse")
        x <- make.sqM.3(mean=25,sd=10)
        message ("********\n-> defined matrix:")
        print(mat1)
        message ("\n-> computing inverse just for proof :")
        inv.mat1 <- solve(mat1)
        print(inv.mat1)

        message ("\n-> storing matrix in cache")
        cache1 <- makeCacheMatrix(x)

        message ("\n-> getting inverse :")
        print(cacheSolve(cache1))

        message ("\n-> getting inverse (second run, should get cached data:")
        print(cacheSolve(cache1))

}

## Second test :
## We will build a 3x3 matrix and compute its inverse.
## Then, we will get it back using caching capabilities
test_2 <- function() {
        message ("\n\nTest2 : 1000x1000 matrix inverse")
        x <- make.sqM.1000(mean=25,sd=10)

        message ("\n-> storing matrix in cache")
        cache1 <- makeCacheMatrix(x)
        #time counter init for first solve
        ptm<-proc.time()
        message ("\n-> getting inverse :")
        inverse <- cacheSolve(cache1)
        #Results : time elapsed
        print(proc.time() - ptm)

        #time counter init for second reading
        ptm <- proc.time()
        message ("\n-> 2nd getting inverse :")
        inverse <- cacheSolve(cache1)
        #Results : time elapsed
        print(proc.time() - ptm)

}

## All tests in one run :
## This function will run all the tests in one command. Easier for
## lazy programmers !
test_all <- function() {
        test_1()
        test_2()
}

infos <- function() {
        message("This code implements matrix inversion in cached mode.")
        message()
        message("Please run test 'test_all()' to see the code in action !")
        message()
        message("Feel free to inspect and stress the code.")
        message("Any comment welcome at author's email : ")
        message("           --> thierry [at] elnikoff [dot] be")
}

infos()


