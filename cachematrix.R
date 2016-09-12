#Coursera - R Programming
#Authors - Kaushik M
#Programming Assignment 2: Lexical Scoping
#
#
#
# In the comments below i am mentioning how i understood the assignment and my approach to it. This is the toughest assignment of the lot. 
# Took me some time to understand it. Maybe i havent understood it completely. PFB - my attempt to understand it.
#
#
#------------------Copied from Assignment instruction - Begin---------------------#
#Write the following functions:
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#------------------Copied from Assignment instruction - End---------------------#
#
#
#
#By my understanding - we need to create 2 functions - One that can create inverse of a matrix and caches it. 
#And One thats able to fetch the cached matrix correctly. 
# The example given makes use of a vector and not a matrix. We need to substitute the vector for a matrix. It also doesnt calculate the inverse.

# The below function creates a matrix that can cache its own inverse.

#-----------------------make_Cache_Matrix - Begin----------------------#

make_Cache_Matrix <- function(a = matrix()) {
  
  #declaring an in_verse "variable". Naming it as in_verse as there seems to be a built in inverse function (good thing!!!)
  
  in_verse <- NULL
  
  #set the value
  
  set <- function(b) {
    a <<- b
    
    in_verse <<- NULL
    
  }
  
  #get the value
  
  get <- function() a
  
  #set the value of inverse
  
  set_Inverse <- function(inverse) in_verse <<- inverse
  
  #get the value of inverse
  
  get_Inverse <- function() in_verse
  
  # list it!
  
  list(set = set,
       get = get,
       set_Inverse = set_Inverse,
       get_Inverse = get_Inverse)
}
#-----------------------make_Cache_Matrix - End----------------------#

## The below function should process the inverse of the matrix created by the previous function. If the inverse is already cahced, 
# it should fetch it from the cache. 

#-------------------cache_Solve - Begin----------------------------#

cache_Solve <- function(a, ...) {
  
  ## Return the matrix that is the inverse of 'a'
  
  inv_erse <- a$get_Inverse()
  
  # Verify if there is cached data.Used the not null of inv_erse to see if there is cached data. If there is cached data then fetching it.
  
  if (!is.null(inv_erse)) {
    
          return(inv_erse)
      }
  
  #Assignment and inverse 
  
  mat_rix <- a$get()
  
  inv_erse <- solve(mat_rix, ...)
  
  a$set_Inverse(inv_erse)
  
  inv_erse
}

#-------------------cache_Solve - End----------------------------#