fact <- function(n)
{
  if(n <= 0){
    print("Factorial does not exist")
    return()
  }
  num <- 1
  for(i in 1:n)
  {
    num <- num*i
  }
  return(num)
}
expval <- function(n)
{
  val = (1 + 1/n)^n
  return(val)
}
expval(1000)
seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
length(seat)
seat[seat$Roll == 210877,]
