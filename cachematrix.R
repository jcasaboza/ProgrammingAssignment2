##Dada la complejidad de calcular la inversa de la matriz
##se suele beneficiar de capturar el cache de la ya resuelta

##Given that usually the computation of a matrix inversion is complex
##we could benefit from capturing the inverse rather than computing it
##every time

##My native language is spanish, so most of my variables
##are related to my language

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  conf <- function(y){
    inv <<- NULL
    x <<- y
  }
  tom <- function()x
  confinv <- function(inversa) inv <<- inversa
  tominv <- function() inv
  list(conf = conf, tom = tom, confinv = confinv, tominv= tominv)

}


## inv comes from inversa that translates to inverse
##tom comes from tomar algo that translates to get something
## conf comes from configurar as in darle un valor a algo 
## So, what I did was change the function that takes the mean 
## which they gave us as the example and rerun it as the inv
## This way, it does the same, except that it solves the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$tominv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$tom()
  inv <- solve(data)
  x$confinv(inv)
  inv
}
