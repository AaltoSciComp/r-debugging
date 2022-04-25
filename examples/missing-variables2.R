i <- function() {
  return(z)
}
h <- function() {
  return(i())
}
f(-1, y=h())
