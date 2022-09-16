rbinom(n = 1000, size = 1, prob = 0.7)
sample(1:6, size = 1,prob = c(.1, .2, .1, .1, .3, .2))
runif(n = 3, min = 0, max = 1)

clr <- c('red', 'green', 'blue')
sample(clr,size = 1, prob = c(3/7, 2/7, 2/7))

ma = matrix(c(3, 1, -2, 4, 5, 3, -1, 2, -2), nrow = 3, ncol = 3)
su = 0
for(i in 1:3)
{
  su = su + norm(ma[,i],type = "2")
}
p = numeric(length =3)
for(i in 1:3)
{
  p[i] = norm(ma[,i], type = '2')/su
}


runif(1,0,5)

rand = function()
{
  sum = 0
  n = 0
  while(sum < 1)
  {
    sum = sum + runif(1,0,1)
    n = n+1
  }
  return (n)
}
rand()
vec = numeric(length = 1e3)
for(i in 1:1e3)
{
  vec[i] = rand()
}
mean(vec)

cand = function()
{
  sum = 0
  n = 0
  while(sum < 25)
  {
    sum = sum + sample(1:(25-sum),size = 1)
    n = n+1
  }
  return (n)
}
for(i in 1:1e3)
{
  vec[i] = cand()
}
mean(vec)
