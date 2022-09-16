library(imager)
dog <- load.image('dog.jpeg')
dim(dog)
plot(dog)

graydog = grayscale(dog)
plot(graydog)
dim(graydog)

gray.mat = as.matrix(graydog[,,1,1])
dim(gray.mat)

col.mat = as.array(dog[,,1,])
dim(col.mat)

cropped.mat = col.mat[1:300, ,]
crop.dog = as.cimg(cropped.mat)
plot(crop.dog)

#1
pix_dist = array(0, c(nrow = dim(col.mat)[1], ncol = dim(col.mat)[2],3))

for(i in 1:dim(col.mat)[1])
{
  for(j in 1:dim(col.mat)[2])
  {
    pix_dist[i,j,1] = norm(col.mat[i,j,]-c(1,0,0),type = '2')
    pix_dist[i,j,2] = norm(col.mat[i,j,]-c(0,1,0),type = '2')
    pix_dist[i,j,3] = norm(col.mat[i,j,]-c(0,0,1),type = '2')
  }
}

ind1 = which(pix_dist[,,1] == min(pix_dist[,,1]), arr.ind = TRUE)
ind2 = which(pix_dist[,,2] == min(pix_dist[,,2]), arr.ind = TRUE)
ind3 = which(pix_dist[,,3] == min(pix_dist[,,3]), arr.ind = TRUE)
plot(dog)
for(i in 1:3)
{
  points(ind1,col = 'red')
  points(ind2,col = 'black')
  points(ind3,col = 'white')
}

#3
img1 = load.image('col1.png')
img2 = load.image('col2.png')
img3 = load.image('col3.png')
col = function(img)
{
  co.mat = as.array(img[,,1,])
  hi = c(norm(co.mat[1,1,] - c(1,0,0),'2'), norm(co.mat[1,1,] - c(0,1,0),'2'), norm(co.mat[1,1,] - c(0,0,1),'2'))
  ind = which(hi==min(hi),arr.ind = TRUE)
  if(ind == 1){
    return('Red')
  }
  else if(ind == 2){
    return('Green')
  }
  else{
    return('Blue')
  }
}
col(img3)

#4
pic1 <- load.image("land1.jpeg")
pic2 <- load.image("land2.jpeg")

# We can reuse function diff.col!
# measuring average distance to c(1,1,1) for both pics
distance.to.white1 <- mean(diff.col(pic1, c(1,1,1)))
distance.to.white2 <- mean(diff.col(pic2, c(1,1,1)))

ifelse(distance.to.white1 < distance.to.white2, "Land1", "Land2")
