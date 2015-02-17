# makeVector takes a numeric vector, saved in the private variable x
makeVector <- function(x = numeric()) 
{
  # initialize the mean to NULL during the first call to makeVector
  # this is needed because ig getmean() is called immediately after
  # the makeVector funciton is constructed, without a call to setmean
  # we know we must first calculate the mean in cachemean.  
  m <- NULL 
  
  # funciton to set a new value for the underlying vector
  # this invalidates the cached mean, m
  # we use the <<- operator to set the value of x and m because we want 
  # to modify x and m defined in the enclosing environment (created 
  # when makeVector was first called), not in the environment local to set(),
  # in which x and m are undefined.
  # we must reset m to NULL since we are modifying the underlying
  # vector and the cached value is no longer the valid 
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  
  # getter function for underlying vector
  # in R the return value of a function is the last statement.
  # all of these functions could have been written as:
  # return(x), etc... as the last line.
  get <- function()
  {
    x
  }
  
  # set the mean of the vector x.  Called by cachemean,
  # this is pretty weird style, but then so is the whole set up.
  # again we use the <<- operator because we want to modify the m defined
  # in the enclosing function makeVector, not the m local to setmean,
  # which would be undefined.
  setmean <- function(mean) 
  {
    m <<- mean
  }
  
  # returns the mean.  Will be null if setmean has not been called or
  # if set is called after the last call to setmean
  getmean <- function() 
  {
    m
  }
  
  # return value of the makeVector function is a list
  # of functions (and variables if we wish) that we want to expose
  # as public.  these are accessed with the $ operator.  Any variables
  # declared inside makeVector but not exported as part of this list
  # are private...they are inaccessible to any caller of makeVector
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# cachemean takes a caching Vector created with makeVector
cachemean <- function(x, ...) 
{
  # get the mean of the vector defined inside x.
  # we can use the $ operator to access the function since it was
  # defined in the list of function pointers returned by the call to
  # makeVector
  m <- x$getmean()
  
  # if we've already computed the mean and stored it via setmean(),
  # and have not invalidated the cache by calling set(), return the cached
  # version of x
  if(!is.null(m)) 
  {
    message("getting cached data")
    # we have to explicily use return here otherwise we'd keep
    # executing the code after the if conditional ends.  Since
    # the cached version is good, just return it and we are done.
    return(m)
  }
  
  # either we havent computed the cached version yet, or we've called
  # set() previously and invalidated the cache.
  
  # call get() to get the underlying vector
  data <- x$get()
  
  # calculate the mean of the underlying vector, passing with it
  # any varargs passed to cachemean
  m <- mean(data, ...)
  
  # now set the mean in x so we cache it and dont need to needlessly
  # recompute it
  x$setmean(m)
  
  # return the caching vector
  m
}