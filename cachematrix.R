# This set of codes defines a funciton that creates a matrix object.
# Then it builds four functions: set(g, et(), gets() and sets(). 
# The setters, set the related values for the objects x and s.
# Getters access the data within the objectsx and s. 

makeCacheMatrix<-function(x=matrix()) {
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL
  }
  get<-function() x
  sets<-function(inverse) s<<-inverse
  gets<-function() s
  list(set=set, get=get, sets=sets, gets=gets)
}

#This set of codes creates a function that first checks 
#if there is any cached value for the inverse of matrix.
#If there be inverse cached, then it calculates the inverse
#of the matrix and returns the value.

cacheSolve<-function(x,...){
  s<-x$gets
  if(!is.null(s)) {
    message("getting chached data")
    return(s)
  }
  data<-x$get()
  s<-solve(data, ...)
  x$sets(s)
  s
}