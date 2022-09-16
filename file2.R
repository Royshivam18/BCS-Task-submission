seat <- read.csv("https://dvats.github.io/assets/course/mth208/seating.csv")
seat[(seat$Roll)%%220000<10000,]
dim(seat[(seat$Roll)%%220000<10000,])


#2
players <- read.csv("https://dvats.github.io/assets/course/mth208/battingbowling.csv")
allrounds <- players[players$Bowling < 40 & players$Batting > 25,]
table(allrounds$Team)

#3

plot(x = 1:10, y = 1:10, type = "l", xlab = "x",ylab = 'y', main = 'Y = X Plot')
  

#4
fn <- function(x)
{
  val = (1 + 1/x)^x
  return (val)
}
plot(x = 1:1000, y = fn(1:1000), type = 'l', xlab = 'x', ylab = 'y', main = 'value of e')
abline(h = exp(1), col = 'red')
