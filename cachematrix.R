#makeCacheMatrix - caches the inverse of a matrix passed to it

#cacheSolve - returns the inverse of a matrix that is passed to it. If the inverse
#is cached, the calculation step is skipped and the inverse is retrived from the
#cache

# A function that stores a set of functions to get or store the matrix or its inverse
makeCacheMatrix <- function( x = matrix() ) {
  inv <- NULL
  
  set_mat <- function( y ) {
    x <<- y
    inv <<- NULL
  }
  
  get_mat <- function() {
    return( x )
  }
  
  set_inv <- function( inverse ) { 
    inv <<- inverse 
  }
  
  get_inv <- function() {
    return( inv )
  }
  
  list( set_mat = set_mat, get_mat = get_mat, set_inv = set_inv, get_inv = get_inv )
}


#returns the inverse of a matrix passed to it. If the inverse is stored in the
#cache, it skips the computation and returns the cached inverse
cacheSolve <- function(x, ...) {
  mat_inv <- x$get_inv()
  if( !is.null( mat_inv ) ) {
    message( "getting cached data" )
    return( x$get_inv() )
  }
  
  matrix <- x$get_mat()
  cached_inv <- solve( matrix )
  x$set_inv( cached_inv )
  return( cached_inv )
}
