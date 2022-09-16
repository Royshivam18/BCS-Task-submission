library(tidyverse)
library(rvest)

html = read_html("https://www.imdb.com/chart/top/")
adder = html_elements(html,'.seen-widget')%>%
  html_attr('data-titleid')
t = 'https://www.imdb.com/title/t/ratings'
u = paste('https://www.imdb.com/title/',adder,'/ratings',sep = '')
html <- read_html("https://www.imdb.com/chart/top/")
movies <- html_elements(html,'.titleColumn a')%>%html_text()

year <- html_elements(html,'.titleColumn span')%>%
  html_text()%>%
  substring(.,2,5)%>%
  as.numeric()

rating <- html_elements(html,'.ratingColumn.imdbRating strong')%>%
  html_text()%>%as.numeric()
vote <- html_elements(html,'.posterColumn span')%>%
  html_attr('data-value')%>%
  as.numeric()
votes = numeric(length = 250)
for(i in 1:250){
  votes[i] = vote[5*(i-1)+4]
}


html <- read_html("https://www.imdb.com/title/tt0111161/ratings")
all_ratings <- html %>% html_table()
unlist(all_ratings[[1]][3])%>%gsub(',','',.)%>%as.numeric()
all_ratings[[2]][3,2]

all_ratings[[3]]

vote_rat = matrix(0,nrow = 250,ncol = 10)
vote_men = matrix(0,nrow = 250)
men_rat = matrix(0,nrow = 250)
vote_women = matrix(0,nrow = 250)
women_rat = matrix(0,nrow = 250)

for(i in 1:250)
{
  print(paste("chaalu hai",i))
  data <- read_html(u[i])%>%
    html_table()
  vote_ra = unlist(data[[1]][3])%>%gsub(',','',.)%>%as.numeric()
  vote_rat[i,] = vote_ra
  vote_men[i] = data[[2]][2,2]%>%
    gsub('\n','',.)%>%gsub(' ','',.)%>%substring(4,)%>%gsub(',','',.)%>%as.numeric()
  men_rat[i] = data[[2]][2,2]%>%
    gsub('\n','',.)%>%gsub(' ','',.)%>%substring(1,3)%>%as.numeric()
  vote_women[i] = data[[2]][3,2]%>%
    gsub('\n','',.)%>%gsub(' ','',.)%>%substring(4,)%>%gsub(',','',.)%>%as.numeric()
  women_rat[i] = data[[2]][3,2]%>%
    gsub('\n','',.)%>%gsub(' ','',.)%>%substring(1,3)%>%as.numeric()
}
data_struc = data.frame(movies,year,rating,men_rat,women_rat,vote_men,vote_women,vote_rat)



urls_img <- paste("https://www.imdb.com/title/", adder, sep = "")
images <- character(length = 100) # will store the vector of image locations
for(i in 1:100)
{
  print(paste("Starting movie", i))
  movie_html <- read_html(urls_img[i])
  images[i] <- movie_html %>% html_element(".ipc-image") %>% html_attr("src")
}
diff.col <- function(img, col)
{
  col.mat <- as.array(img[, , 1, ])
  dims <- dim(col.mat)
  
  # Calculate distance to given color
  dist <- matrix(0, nrow = dims[1], ncol = dims[2])
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      # distance from the col give by user
      dist[i,j] <- norm(col.mat[i,j, ] - col, "2")
    }
  }
  # return the mean distance from color
  return(mean(dist))
}


## Now calculating distance average distance black
library(imager)
black <- numeric(length = 100)
for(i in 1:100)
{
  img <- load.image(images[i])
  black[i] <- diff.col(img, col = c(0,0,0))
}

# Which movies are within .2 distance to black on average
index <- which(black < .5)
data_struc[index, ]
save(data_struc,file = 'struct.Rdata')
