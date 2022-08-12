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



#12 aug
library(imager)
dog <- load.image("dog.jpeg")
dim(dog)
plot(dog)
graydog <- grayscale(dog)
plot(graydog)
dim(graydog)
gray.mat <- as.matrix(graydog[,,1,1])
dim(gray.mat)
col.mat <- as.array(dog[, ,1, ])
dim(col.mat)
cropped.mat <- col.mat[1:300, , ]
crop.dog <- as.cimg(cropped.mat)
plot(crop.dog)
dis.mat = array(dim = c(dim(gray.mat)[1],dim(gray.mat)[2],3))
dim(dis.mat)

for(i in 1:dim(gray.mat)[1])
{
  for(j in 1:dim(gray.mat)[2])
  {
    dis.mat[i,j,1] = norm(col.mat[i,j,] - c(0,1,0), "2")    #green
    dis.mat[i,j,2] = norm(col.mat[i,j,] - c(1,0,0), "2")    #red
    dis.mat[i,j,3] = norm(col.mat[i,j,] - c(0,0,1), "2")    #Blue
  }
}
k = c(min(dis.mat[ , ,1]), min(dis.mat[ , ,2]), min(dis.mat[ , ,3]))
di = rbind(which(dis.mat[ , ,1] == k[1], arr.ind = TRUE),which(dis.mat[ , ,2] == k[2], arr.ind = TRUE),which(dis.mat[ , ,3] == k[3], arr.ind = TRUE))
plot(dog)
points(di[1,1],di[1,2],col = "red")
points(di[2,1],di[2,2],col = "black")
points(di[3,1],di[3,2],col = "white")


#Q3
dog
