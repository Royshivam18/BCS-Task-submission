cricket <- read.csv("https://dvats.github.io/assets/course/mth208/battingbowling.csv")
heads = c(rbinom(n=1000, size = 1, prob = 0.5))
sum(heads)/1000
mean(heads)
#Biased case
heads1 = c(rbinom(n=1000, size = 1, prob =0.3))
heads1
sum(heads1)/1000
mean(heads1)
sample(x = 1:6, size = 1)
sample(x = 1:6, size = 1, prob = c(.1, .2, .1, .1, .3, .2))
runif(n = 1, min = 0, max = 1)      
sample(x = c('red','green','blue'), size = 1, prob = c(3, 2, 2)/7)       
A <- matrix(c(3, 1, -2, 4, 5, 3, -1, 2, -2), nrow = 3, ncol = 3)
new <- numeric(length = 3)
A1 = c(norm(A[,1], type = "2"),norm(A[,2], type = "2"),norm(A[,3], type = "2"))#we can also use loop
for(i in 1:3)
{
  new[i] = norm(A[,i], type = "2")
}
P = A1/sum(A1)
sample(x = 1:3, size = 1, prob = P)

runif(n = 1, min = 0, max = 5)
#question 3
new <- numeric(length = 1000)
countf <- function()
{
  sum = 0;
  for(i in 1:10000)
  {
    sum = sum + runif(n = 1, min = 0, max = 1)
    if(sum >= 1){
      return (i)
    }
  }
}
c = countf()
for(i in 1:1000)
{
  new[i] = countf()
}
mean(new)
#question 4
Numt <- function(x)
{
  n = 0;
  while(x>0)
  {
    x = x - sample(1:x, size = 1)
    n = n+1
  }
  return (n)
}
ne <- numeric(length = 1000)
for(i in 1:1000)
{
  ne[i] = Numt(25)
}
mean(ne)
