# does the avg. age of the contestents effect the viewer ratings?

install.packages("bakeoff")
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyverse)
library(bakeoff)
bakers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-25/bakers.csv')

# preliminary information about the data 
summary(bakers)
summary(ratings)
head(ratings)
head(bakers)


# get the average age of the bakers
bakers <- as_tibble(bakers)

bakers <- select(bakers$series, bakers$age)
head(bakers)

bakers <- aggregate(bakers$age, list(bakers$series), mean)
head(bakers)
colnames(bakers)[1] <- "series"
colnames(bakers)[2] <- "average age"


# keep only relevant information in the ratings data
ratings <- as_tibble(ratings)
ratings <- ratings %>%
  select(series, viewers_7day, viewers_28day, network_rank)

ratings <- aggregate(ratings[2:4], list(ratings$series), mean)

colnames(ratings)[1] <- "series"
ratings


# combining the bakers data and ratings data into one tibble
gbb <- merge(bakers, ratings)
gbb <- as_tibble(gbb)
gbb


# scatterplot of age vs ratings

gbb_scatter <- plot(gbb$`average age`, gbb$viewers_7day, pch = 15, col = "maroon", ylim = c(0,14), xlab = "Average Age Per Series", ylab = "Average Viewers per Series")
    abline(lm(gbb$viewers_7day ~ gbb$`average age`), col = "maroon")
  points(gbb$`average age`, gbb$viewers_28day, pch = 16, col = "purple2")
    abline(lm(gbb$viewers_28day ~ gbb$`average age`), col="purple2")
  points(gbb$`average age`, gbb$network_rank, pch = 17, col = "pink1")
    abline(lm(gbb$network_rank ~ gbb$`average age`), col= "pink1")
  title("Average Age vs Viewer Ratings of the Great British Baking Show")
  legend("topleft", cex = .8,
         legend = c("Viewers Over 7 Days", "Viewers Over 28 Days", "Network Rank"),
         lwd = 3, col = c("maroon", "purple2", "pink1"))    

  
# bar plots of age and ratings broken down by series
      # to put the two plots side by side
par(mfrow = c(1:2))
    
      # bar plot one : ratings
ratingsbyseries <- group_by(ratings, series)
ratingsbyseries <- as_tibble(ratingsbyseries)
ratingsbyseries <- summarize(ratingsbyseries, viewers_7day, viewers_28day, network_rank)
ratingsbyseries <- as.matrix(ratingsbyseries)

seriescolors <- colorRampPalette(c("azure2", "navyblue"))
seriescolors(10)

gbb_bar1 <- barplot(ratingsbyseries, beside = TRUE, legend = TRUE, ylab = "Average Viewers Per Series", col = seriescolors(10), cex.names = .5)
  legend("topright", cex = .4,
         legend = c("Series 1", "Series 2","Series 3","Series 4","Series 5","Series 6","Series 7","Series 8","Series 9","Series 10"),
         lwd = 2, col = seriescolors(10))
  title("Average Viewers of the Great British Baking Show", cex.main = .6)
      
      # bar plot two : ages

agebyseries <- group_by(bakers, series)
agebyseries <- as_tibble(agebyseries)
agebyseries <- as.matrix(agebyseries)
agebyseries <- agebyseries[,-1]
agebyseries

agecolors <- colorRampPalette(c("deeppink4", "lightpink1"))

gbb_bar2 <- barplot(agebyseries, beside = TRUE, legend = TRUE, ylab = "Average Age Per Series", col = agecolors(10), cex.names = .6)
  legend("bottomright", inset(-.02, -.03), cex = .45,
       legend = c("Series 1", "Series 2","Series 3","Series 4","Series 5","Series 6","Series 7","Series 8","Series 9","Series 10"),
       lwd = 2, col = agecolors(10))
  title("Average Age of the Contestants on the Great British Baking Show", cex.main = .5)
  
# pairwise scatter plot 
par(mfrow = c(1:1))

gbb <- gbb %>%
  select(`average age`, viewers_7day, viewers_28day, network_rank)
gbbcolors <- colorRampPalette(c("plum", "darkorchid4"))

pairs(gbb, col = gbbcolors(5))
  title("Average Age and Viewership of the Great British Baking Show", line = 3, cex.main = .7)






