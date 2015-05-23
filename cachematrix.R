## --------------------------------------------------------------------------------
## Programming Assignement 2
## makeCacheMatrix
## This function creates a list of functions used to cache a matrix inversion
##
## The first two functions set and get the original square matrix (sm)
## The last two functions set and get the inverted matrix (im)

makeCacheMatrix <- function (sm = matrix()) {
  
  # Set inverted matrix to null when function is originally bound
  
  im <- NULL
  
  # This function (re)sets the the matrix to the value passed in. This function is used 
  # to change the matrix originally passed in when an object was bound to the function 
  
  set <- function (psm) {
    sm <<- psm               ## Set the matrix (sm) to the value passed in (psm)
    im <<- NULL              ## Set the inverse matrix (im) to null
  }
  
  # This function simply returns the current values of the matrix (sm). Use set to change.
  
  get <- function () {
    sm
  }
  
  # This function sets the cached value of the inverted matrix. It uses the 'deep assignment'
  # arrows <<- to set the value from the current envionment (i.e., within the function)
  
  setcache <- function (pim) {
    im <<- pim   # pim = parameter inverted matrix
  }
  
  # This function gets the cached value of the inverted matrix. 
  
  getcache <- function () {
    im
  }
  
  ## Finally, create a list for each of the four functions. Each list element is a function
  
  list (set = set, get = get, setcache = setcache, getcache = getcache)
  
}

## --------------------------------------------------------------------------------
## Programming Assignement 2
## cacheSolver
##
## This function returns the inversion of a matrix. It checks to determine if the
## matrix was previouly inverted; if so, it will return the cached version else it 
## will invert the matrix and then cache it for future use.
##

cacheSolve <- function (lst, ...) {
  
  # Assign the cached inverted matrix 
  
  im <- lst$getcache()
  
  # Now check to see if the inverted matrix is already cached. If so, return it
  
  if(!is.null(im)) {    # If the cache exists (i.e., not is null)
    message ("retrieved cached value ...")
    return (im)
  }
  
  # The inverted matrix is not cached so: (1) get the sequare matrix, (2) invert it
  # (3) set it in the cache and (4) return it
  
  sm <- lst$get()        # (1) get the square matrix
  
  im <- solve(sm)        # (2) call the solve function to invert the matrix
  
  lst$setcache(im)       # (3) set the inverted matrix in the cache
  
  return(im)             # (4) return m
  
}
