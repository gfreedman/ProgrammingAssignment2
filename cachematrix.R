
# This function creates a matrix
# and provides setter and getter methods to
# return the function itself (with all methods therein)
# and to get and set the inverse of the matrix
# @author geoff.c.freedman@gmai.com
makeCacheMatrix <- function(myMatrix = matrix()) {
  
  # This is the inverse of the matrix, initially set to NULL
  # Originaly I was using an underscore char ("_") to let you know it was a private class member
  # but R did not like that, so I've substituted "my" for "_"
  myMatrixInverse <- NULL
  
  # This is a public setter method
  # You could almost think of this as the constructor in a class
  set <- function(theInputtedMatrix) {
    myMatrix <<- theInputtedMatrix
    myMatrixInverse <<- NULL # We set matrixInverse to null as we're effectively initializing the 'class member' inverseMatrix
  }
  
  # This is a public getter method that returns our instantiated object 'x'
  get <- function() myMatrix
  
  # This is a public setter method to assign _matrixInverse
  setMatrixInverse <- function(matrixInverse) myMatrixInverse <<- matrixInverse
  
  # This is a public getter method to retrieve _matrixInverse
  getMatrixInverse <- function() myMatrixInverse
  
  # Since this is the last statement in the function, then this is what get's returned
  # This seems to be how the R function can be aware of all of it's 'class' members even if
  # there is no such a thing as classes. Kind of like .prototype in JavaScript --> synthetic classes
  list(set = set, get = get,
       setMatrixInverse = setMatrixInverse,
       getMatrixInverse = getMatrixInverse)
}


## Write a short comment describing this function

# This function returns the inverse of a supplied matrix
# it first checks to see if we already have the answer
# if we do, it simply returns what was already computed
# but if we don't it will use the solve() function in R to calculate the inverse of a matrix
cacheSolve <- function(m, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  matrixInverse <- m$getMatrixInverse()
  
  # If the matrix inverse is NON NULL, that is probably an actual bonafide inverse of a matrix
  # then we return it immediately and exit the function body
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  
  # Get the 'class' instantiated instance ... AKA the matrix
  matrixInstance <- m$get()
  
  # This gives you the inverse of the matrix 
  # We assume that the matrix supplied is always invertible.
  matrixInverse <- solve(matrixInstance, ...)
  
  # This sets the cached inverse of the matrix for handy retrieval later on
  m$setMatrixInverse(matrixInverse)
  
  # Finally we can return the matrix
  matrixInverse
  
}

# This code here is just a simple test harness to show that I tested out the code and that
# it runs as expected
testHarness <- function(){
  
  #initialize the matrix
  a <- makeCacheMatrix()  
  
  #print the matrix --> nothing much to see since it's null
  print(a$get())       
  
  # set the matrix to an arbitrary vector
  a$set(c(1,2,3,4))     
  
  # get the matrix just to make sure we can see something simple
  print(a$get())            
  
  # now make a real matrix
  B = matrix( c(1,2,3,4), nrow=2, ncol=2) 
  
  # set the matrix to a real matrix B
  a$set(B)              
  
  # do we see a real matrix
  print(a$get())       
  
  # now test out our cacheSolve method:
  theInverse <- cacheSolve(a)
  
  # print output to console to verify that all is well
  print(theInverse)
  
}

# Run the test to make sure the code works as intended!
testHarness()
# Sample output from testHarness
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5