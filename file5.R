library(imager)
dog <- load.image("dog.jpeg")
col.mat <- as.array(dog[,,1,])
dime <- dim(col.mat)
dog_180 <- array(0,c(dime[1],dime[2],3))

for(i in 1:dime[1])
{
  for(j in 1:dime[2])
  {
    dog_180[i,j,] = col.mat[dime[1]-i+1,dime[2]-j+1,]
  }
}
plot(as.cimg(dog))

#2
dog_90 <- array(0,c(dime[2],dime[1],3))

for(i in 1:dime[1])
{
  for(j in 1:dime[2])
  {
    dog_90[j,i,] = col.mat[dime[1]-i+1,j,]
  }
}
plot(as.cimg(dog_90))


#4
haha.mat <- as.array(dog[1:600,1:600,1,])
reduce = array(0,c(60,60,3))
for(i in 1:60)
{
  for(j in 1:60)
  {
    val = c(0,0,0)
    ind1 = (10*(i-1)+1):(10*i)
    ind2 = (10*(j-1)+1):(10*j)
    reduce[i,j,1] = mean(haha.mat[ind1,ind2,1])
    reduce[i,j,2] = mean(haha.mat[ind1,ind2,2])
    reduce[i,j,3] = mean(haha.mat[ind1,ind2,3])
  }
}
par(mfrow = c(1,2))

plot(dog)
plot(as.cimg(reduce))
save.image(as.cimg(reduce), file = "dog_300.jpeg")
