library(tidyverse)
library(rvest)
library(imager)
movies <- read_html("https://www.imdb.com/chart/top/")
img = html_elements(movies,'.posterColumn a')%>%html_nodes('img')%>%html_attr('src')
trigger <- html_elements(movies,css = '.ratingColumn')
  html_elements('.seen-widget')%>%
  html_attr('data-titleid')
t  = "https://www.imdb.com/title/xyz/ratings"
###### Question 2
html <- read_html("https://www.imdb.com/chart/top/")

# getting a tag in titlecolumn class and the text in there
name <- html %>%  
  html_elements(".titleColumn a") %>% 
  html_text()

###### Question 2
# name is done, getting year
year <- html %>% 
  html_elements(".secondaryInfo") %>% 
  html_text() %>%
  substring(2,5) %>%  # removing brackets
  as.numeric()  # changing to number

# getting rating
rating <-  html %>% 
  html_elements(".ratingColumn.imdbRating") %>% 
  html_elements("strong") %>% 
  html_text() %>%
  as.numeric()  # convering to numeric

votes <- html %>% 
  html_elements(".ratingColumn strong") %>%
  html_attr("title") %>%
  substring(14) # removing the first 13 characters
vote = gsub(',','',votes)
vote = gsub(' user ratings','',vote)
as.numeric(vote)
html <- read_html("https://www.imdb.com/title/tt0111161/ratings")
all_ratings <- html %>% html_table()
length(all_ratings)
all_ratings[[1]][[3]]
gsub('\n','',all_ratings[[2]][[2]])%>%gsub(' ','',.)%>%substring(4) # Second item in list
all_ratings[[3]] # Third item in list
volis = list()
malevolis = list()
womvolis = list()
for(i in 1:250)
{
  mdet <- read_html(gsub('xyz',trigger[i],t))
  all_ratings <- mdet %>% html_table()
  volis[[i]] = as.numeric(gsub(',','',all_ratings[[1]][[3]]))
  malevolis[[i]] = as.numeric(gsub('\n','',all_ratings[[2]][[2]][2])%>%gsub(' ','',.)%>%gsub(',','',.)%>%substring(4))
  womvolis[[i]] = as.numeric(gsub('\n','',all_ratings[[2]][[2]][3])%>%gsub(' ','',.)%>%gsub(',','',.)%>%substring(4))
}
dat <- data.frame(name, year, rating, vote, unlist(malevolis),unlist(womvolis))
load.image(img[1])%>%plot
