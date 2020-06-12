#PART 1
makeCacheMatrix<-function(x=matrix())
{inver<-NULL
  set<-function(y){
    x<<-y
    inver<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inver<<-inverse
  getinverse<-function()inver
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#Part 2
cacheSolve<-function(x,...){
  inver<-x$getinverse()
  if(!is.null(inver)){
    message("getting cached data")
    return(inver)}
    dat<-x$get()
    inver<-solve(dat,...)
    x$setinverse(inver)
    inver
}