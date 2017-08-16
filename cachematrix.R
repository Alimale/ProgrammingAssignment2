makeCacheMatrix<-function(x=matrix()) {
  s<-NULL
  dim(x)<-c(sqrt(length(x)),sqrt(length(x)))
  set<-function(y){
    dim(y)<-c(sqrt(length(y)),sqrt(length(y)))
    x<<-y
    s<<-NULL
  }
  get<-function() x
  sets<-function(inverse) s<<-inverse
  gets<-function() s
  list(set=set, get=get, sets=sets, gets=gets)
}
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