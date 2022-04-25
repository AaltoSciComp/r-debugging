# Ensure that z is missing
if (exists("z")) {
  rm(z)
}
g <- function(x) {
  return(x + z)
}

f <- function(x,y = g(x)) {
  if (x>0) {
    return(x)
  } else {
    return(y)
  }
}

f(-1)