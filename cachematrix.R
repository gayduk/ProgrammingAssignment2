makeCacheMatrix <- function(x = matrix()) {
##'class' for holding a matrix and its inverse
    inverse=NULL
    set=function(y){
        x<<-y
        inverse=NULL
    }
    get=function() x
    set_inverse=function(y){inverse<<-y}
    get_inverse=function() inverse
    list(set = set, get = get,
        set_inverse = set_inverse,get_inverse=get_inverse)
}




cacheSolve <- function(x, ...) {
  ## returns an inverse of the makeCacheMatrix object using cached
  ## value if available
    inv=x$get_inverse()
    if(!is.null(inv))
    {
        return(x$get_inverse())
    }
    else
    {
        mat=x$get()
        inv=solve(mat)
        x$set_inverse(inv)
        return(inv)
    }
}
