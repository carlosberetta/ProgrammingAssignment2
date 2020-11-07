mv <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
gb <- function() {x}
setinv <- function(inverse) {inv <<- inverse}
gbinv <- function() {inv}
list(set = set, gb = gb, setinv = setinv, gbinv = gbinv)

}

cache <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  
tam <- x$gb()
inv <- solve(tam, ...)
x$setinv(inv)
inv
}

matrix <- mv(matrix(1:16, nrow=4, ncol=4))
matrix$gb()
