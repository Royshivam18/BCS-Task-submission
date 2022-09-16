library(tidyverse)
library(rvest)

html <- read_html("https://www.imdb.com/chart/top/")
movies <- html_elements(html,'.titleColumn a')%>%html_text()

year <- html_elements(html,'.titleColumn span')%>%
  html_text()%>%
  substring(.,2,5)%>%
  as.numeric()

rating <- html_elements(html,'.ratingColumn.imdbRating strong')%>%
  html_text()
vote <- html_elements(html,'.posterColumn span')%>%
  html_attr('data-value')%>%
  as.numeric()
votes = numeric(length = 250)
for(i in 1:250){
  votes[i] = vote[5*(i-1)+4]
}

chart = data.frame(movies, year, rating, votes)
