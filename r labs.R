factor <- function(x)
{
  val <- 1
  for(i in 1:x)
  {
    val <- val*i
  }
  return (val)
}
factor(6)
#2
e_val <- function(z)
{
  val <- (1+1/z)^z
  return (val)
}
e_val(1000)
#3
seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
seat
seat[seat$Roll==210877,]
seat$Roll==201877
