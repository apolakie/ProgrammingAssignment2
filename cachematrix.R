makeCacheMatrix <- function(x = matrix()){
        jcq <- NULL
        set <- function(y){
                x <<- y
                jcq <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) (jcq <<<- inverse}
        getInverse <- function() (jcq)
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        jcq <- x$getInverse()
        if(!is.null(jcq)){
                message("getting cache data")
        
        }
        mat <- x$get()
        jcq <- solve(mat , ...)
        x$setInverse(jcq)
        jcq
}
