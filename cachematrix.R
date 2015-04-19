## Cache an invertible matrix, along with its' inverse to avoid having to
## recompute the inverse multiple times.
##
makeCacheMatrix <- function(x = matrix()) {
    # Cache an invertible matrix together and define get/set 
    # functions for putting/retrieving matrices from the cache.
    #
    # Parameters:
    #   x   The matrix to cache.
    #
    # Return:
    #       List with cache get/set functions.
    
    m_cache     <- x     # cache the passed in matrix
    m_cache_inv <- NULL  # init the inverse to NULL
    
    # get/set the matrix
    get   <- function() m_cache
    set   <- function(new_m) {
        # note if we (re)set the matrix we must null
        # out the existing inverse to force a recompute
        # next time the inverse is requested 
        m_cache <<- new_m
        m_cache_inv <<- NULL
    }
    
    # get/set the inverse
    getinv <- function() m_cache_inv
    setinv <- function(inv) {
        # make sure that the provided matrix is actually the 
        # inverse of the cached matrix (the product of the 
        # current cached matrix and the potential inverse is
        # the identity matrix M*M' = I(.)
        if (!isTRUE(all.equal((inv %*% m),diag(nrow(m_cache)))))
        {
            m_inv <<- NULL
            warning("Provided matrix is not inverse for cached matrix.")
            return(NULL)
        }
        m_inv <<- inv
    }
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

cacheSolve <- function(x, ...) {
    # Solve for or retrieve the inverse of the cached matrix. 
    # If the inverse was previously computed it is used, otherwise
    # the inverse is computed, cached and returned. 
    # 
    # Parameters:
    #   x   The cache containing the matrix to invert.
    #
    # Return:
    #       The inverse of the cached matrix.
    msg <- "Using cached inverse"
    
    # try and get the inverse matrix from the cache
    xinv <- x$getinv()
    if (is.null(xinv))
    {
        # no cached version, compute it
        xinv <- solve(x$get())
        x$setinv(xinv)
        
        # change the message...
        msg <- "Computing inverse"
    }
    
    # return the inverse
    message(msg)
    xinv
}

## unit test
cache_test <- function()
{
    # unit test to check the following scenarios:
    # 1)    verify passed in matrix is properly cached
    # 2)    verify matrix is inverted
    # 3)    verify inverted matrix is cached correctly
    # 4)    verify the inverse is cleared if the matrix 
    #       is changed
    # 5)    verify the updated inverse is cached
    
    checkCache <- function(msg) {
        # helper function to pretty print the supplied
        # message followed by:
        #   m    - the cached matrix
        #   mi   - its computed ivers
        #   m*mi - if mi != NULL, the matrix product m*mi
        #          (should always be I) 
        message(msg)
        m <- cached_m$get()
        mi <- cached_m$getinv()
        cat("m = \n")
        print(m)
        cat("\nmi = \n")
        print(mi)
        if (!is.null(mi))
        {
            cat("\nm%*%mi = \n")
            print(m%*%mi)
        }
        cat("\n")
    }
    
    # create a couple of matrices to test with
    # fwiw we are ignoring non-square/non-invertible cases
    # in the assignment...
    m1 <- matrix(c(1,2,3,4),2,2)
    m2 <- matrix(c(7,6,5,4),2,2)
    
    # cache the first matrix
    cached_m <- makeCacheMatrix(m1)
    checkCache("Check cache before inverting:")
    
    # compute its inverse
    cacheSolve(cached_m)
    checkCache("Check cache after inverting:")
    
    message("Check we get the cached inverse:")
    cacheSolve(cached_m)
    checkCache("")
    
    cached_m$set(m2)
    checkCache("Update matrix and check cache:")
    
    cacheSolve(cached_m)
    checkCache("Check cache after inverting new matrix:")
    
    cached_m$setinv(m1)
    checkCache("Check cache after invalid inverse:")
}
