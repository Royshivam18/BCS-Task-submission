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
# html
library(tidyverse)
library(rvest)
html <- read_html("https://www.iitk.ac.in/math/faculty")
name <- html_elements(html, ".head3")
name <- html_elements(name, "a")
name <- html_text(name)
name <- html_elements(html, ".head3 a")
name <- html_text(name)
name <- html %>% html_elements(".head3 a") %>% html_text()


#q1
file <- read_html("https://www.iitk.ac.in/math/visitors-post-doctoral-fellow")
phd <- html_elements(file, ".head2")%>%html_text()


#q2
movies <- read_html("https://www.imdb.com/chart/top/")
movies.name <- html_elements(movies,".titleColumn a")%>%html_text()
imdb <- as.data.frame(movies.name)
movies.year <- html_elements(movies,".secondaryInfo")%>%html_text()
imdb$name <- as.data.frame(movies.rating)
movies.rating <- html_elements(movies,"posterColumn")%>%html_text()
movies.votes <- html_elements(movies,css = ".ratingColumn strong")%>%html_attr('title')
movies.votes <- html_elements(movies,css = ".posterColumn")

substring(movies.year)
movies.rating = as.numeric(movies.rating)
movies.year = gsub("()", "", movies.year)
movies.year = as.numeric(movies.year)
