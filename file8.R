
load('IMDB_movies.Rdata')
head(dat)

hist(dat$rating,main = 'histogram ratings',xlab = 'Ratings')
hist(dat$rating,main = 'histogram ratings',xlab = 'Ratings',col = "white")

par(mfrow = c(1,1))
boxplot(dat$rating,main = "boxplot of rating",col = "pink")

boxplot(dat$men_rating,dat$women_rating,names = c('men','women'),main = "bxplot rating by gender")

hist(dat$men_rating, main = "histogram ratings", xlab = "ratings", xlim = range(c(dat$men_rating, dat$women_rating))
     ,col = adjustcolor(col = 'yellow',alpha.f = 0.5))
hist(dat$women_rating, col = adjustcolor(col = "red",alpha.f = 0.5),add = TRUE)

legend('topright',c("men",'women'),fill = c(adjustcolor(col = 'yellow',alpha.f = 0.5),adjustcolor(col = "red", alpha.f = 0.5)))
