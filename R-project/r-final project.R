library(ggplot2)
library(corrgram)
library(dplyr)
library(ggrepel)
library(scales)
library(plotly)
library(formattable)
library(tidyr)
library(gplots)
library(gridExtra)


#1-to find relationship between director facebook likes and imdb score and also the relationship between 
#imdb scores and movie facebook likes
movie <- read.csv("movie_metadata.csv",header = T, stringsAsFactors = F)

plot(x = movie$imdb_score, y = movie$director_facebook_likes,
     col ="darkviolet",
     xlab ="IMDB Score",
     ylab =" director facebook likes",
     main ="IMDB Score vs director facebook likes",
     col.main ="blue")

plot(x = movie$imdb_score, y = movie$movie_facebook_likes,
     col ="darkorange",
     xlab ="IMDB Score",
     ylab =" facebook likes",
     main ="IMDB Score vs movie facebook likes",
     col.main ="blue")


#2-finding the most profittable director, movies and top films based on ROI
movie <- read.csv("movie_metadata.csv", header = TRUE, stringsAsFactors = FALSE)
movie <- movie[!duplicated(movie$movie_title),]
movie <- movie %>% 
  mutate(profit = gross - budget,
         return_on_investment_perc = (profit/budget)*100)

movie.profit.20 <- movie %>% 
  arrange(desc(profit)) %>% 
  top_n(20, profit)
ggplot(movie.profit.20, aes(x=budget/1000000, y=profit/1000000)) + geom_point(size = 1) + geom_text_repel(aes(label = movie_title), size = 2) + xlab("Budget $million") + ylab("Profit $million") + ggtitle("20 Most Profitable Movies")

movie.roi.20 <- movie %>% 
  filter(budget >10000) %>% 
  arrange(desc(return_on_investment_perc)) %>% 
  top_n(20, return_on_investment_perc)
ggplot(movie.roi.20, aes(x=budget/1000000, y=profit/1000000)) + geom_point(size = 1) + geom_text_repel(aes(label = movie_title), size = 2)  + xlab("Budget $million") + ylab("Profit $million") + ggtitle("top 20 Movies Based on ROI")

movie.directors.20 <- movie %>% 
  group_by(director_name) %>%
  select(director_name, budget, gross, profit) %>%
  na.omit() %>% 
  summarise(films = n(), budget = sum(as.numeric(budget)), gross = sum(as.numeric(gross)), profit = sum(as.numeric(profit))) %>% 
  arrange(desc(profit)) %>% 
  top_n(20, profit)
ggplot(movie.directors.20, aes(x=films, y=profit/1000000)) + geom_point(size = 1) + geom_text_repel(aes(label = director_name), size = 2) + xlab("Number of Films") + ylab("Profit $millions") + ggtitle("Most Profitable Directors")



#3-finding the trend of imdb scores over the years and top directors based on their Imdb scores.
movie <- read.csv('movie_metadata.csv',header=T,stringsAsFactors = F)
t1 <- movie %>% select(title_year,imdb_score)
t1 <- t1 %>% group_by(title_year)%>% summarise(score = mean(imdb_score))
p <- plot_ly(
  x = t1$title_year,
  y = t1$score,
  name = "Avg score by Rating")
p

t1 <- movie %>% select(director_name,imdb_score)
t1 <- t1 %>% group_by(director_name) %>% summarise(avg=mean(imdb_score))
t1 <- t1 %>% arrange(desc(avg))
t1 <- t1[1:20,]
t1 %>%
  formattable(list(avg = color_bar("cyan")), align = 'l')

#4-Here the most preferred movie genres and the most preffered story lines (i.e,keywords) by the fims makers are found.
movie <- read.csv('movie_metadata.csv', header=TRUE)
df <- as.data.frame(movie)
c1<-df[,c(4, 9:10, 12, 17:18, 20:24, 26:27)]
c1_wide<-as.data.frame(c1) %>% 
  separate(genres, into = paste("genres", 1:4, sep = "_")) %>%
  separate(plot_keywords, into = paste("plot_keywords", 1:8, sep = "_"))
head(c1_wide)

c1_wide_1<-c1_wide[,c(1,2,3,8,17,18,19,20)][complete.cases(c1_wide[,c(1,2,3,8,17,18,19,20)]),]
head(c1_wide_1)
s1 <-
  c1_wide_1 %>%
  group_by(genres_1) %>%
  summarise(avg.gross = mean(gross), avg.budget = mean(budget), number = n())
sorted <- s1 %>% 
  arrange(-number, -as.numeric(avg.gross), -as.numeric(avg.budget)) %>%
  mutate(rank=row_number())

x <-ggplot(sorted, aes(y=number, x=genres_1, fill=genres_1)) + 
  geom_bar(stat="identity")  + theme(legend.position = "none") + 
  coord_flip() 
y <-ggplot(sorted, aes(y=avg.gross, x=genres_1, fill=genres_1)) + 
  geom_bar(stat="identity") + theme(legend.position = "none") + 
  coord_flip() 
z <-ggplot(sorted, aes(y=avg.budget, x=genres_1, fill=genres_1)) + 
  geom_bar(stat="identity") + theme(legend.position = "none") + 
  coord_flip() 
grid.arrange(x, y, z, ncol=3)

s2 <-
  c1_wide_1 %>%
  group_by(plot_keywords_1) %>%
  summarise(avg.gross =  mean(gross), avg.budget = mean(budget), number = n())

sorted2 <- s2 %>% 
  arrange(-number, -as.numeric(avg.gross), -as.numeric(avg.budget)) %>%
  mutate(rank=row_number())  %>% filter(rank <= 30)

x <-ggplot(sorted2, aes(y=number, x=plot_keywords_1, fill=plot_keywords_1)) + 
  geom_bar(stat="identity")  + theme(legend.position = "none") + 
  coord_flip() 
y <-ggplot(sorted2, aes(y=avg.gross, x=plot_keywords_1, fill=plot_keywords_1)) + 
  geom_bar(stat="identity") + theme(legend.position = "none") + 
  coord_flip() 
z <-ggplot(sorted2, aes(y=avg.budget, x=plot_keywords_1, fill=plot_keywords_1)) + 
  geom_bar(stat="identity") + theme(legend.position = "none") + 
  coord_flip()
grid.arrange(x, y, z, ncol=3)

s4 <-
  c1_wide_1 %>%
  group_by(genres_1) %>%
  summarise(avg.gross =  mean(gross), avg.budget = mean(budget), 
            number = n(), ROI = sum(gross)/sum(budget))

ggplot(s4, aes(x=ROI, y=avg.budget, label=genres_1))+
  geom_point(aes(size=avg.gross, fill = "none"), shape=21)+ 
  scale_size_area(max_size = 6)+
  geom_text(size=2)+
  theme_bw()



#5-The movies which succeeded in getting both good reviews from critics and success at box-office are found out.
movie<-read.csv("movie_metadata.csv")
glimpse(movie)
plot.data <- movie%>%
  mutate(profit=gross- budget) %>% 
  top_n(10,profit)

plot.data

ggplot(plot.data, aes(x=imdb_score, y=gross/10^6, size=profit/10^6, colour= content_rating)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 600)) + 
  geom_vline(aes(xintercept = 7.75)) + 
  geom_text_repel(aes(label = movie_title), size =4) +
  xlab("Imdb score") + 
  ylab("Gross money earned in million dollars") + 
  ggtitle("Commercial success Vs Critical acclaim") +
  annotate("text", x = 8.5, y = 700, label = "High ratings \n & High gross")


#6-graphical representation of top 20 directors with high imdb scores in found ou
movie<-read.csv("movie_metadata.csv")
temp5 <- movie %>% select(director_name, gross, imdb_score)
temp5 <- temp5 %>% group_by(director_name)
temp5 <- temp5 %>% summarize(total_gross=sum(as.numeric(gross, na.rm=TRUE)), avg_imdb_score =mean(imdb_score))
temp5 <- temp5 %>% arrange(desc(total_gross))

ggplot(head(temp5, 20), aes(reorder(director_name, -total_gross), avg_imdb_score, group = 1)) +
  xlab("Director") +
  ylab("Avg IMDB Rating of 10 top directors") +
  geom_line(colour = "blue")

#7-corrgram is found out
movie<-read.csv("movie_metadata.csv")
corrgram(movie, order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the movie data")

##prediction:
df<-read.csv("movie_metadata.csv")
df <- na.omit(df)
df <- df[, c(3, 4, 5, 6, 8, 9, 13, 14, 16, 19, 23, 24, 25, 26, 27, 28)]
library(caret) 

split <- createDataPartition(df$imdb_score, p=0.67, list = FALSE)

train <- df[split,]
test <- df[-split,]

#imdb rating prediction
m1<-train(imdb_score~., data = train, method = "lm")
summary(m1)

m2 <- train(imdb_score~movie_facebook_likes+duration+num_critic_for_reviews+num_voted_users+director_facebook_likes, data=train, method="lm")
summary(m2)

m1Price <- predict(m1)
m2Price <- predict(m2)
m1Residual <- resid(m1)
m2Residual <- resid(m2)

# Testing using m1
m1testscore <- predict(m1, test)
m1Values <- data.frame(obs = test$imdb_score, pred=m1testscore)
defaultSummary(m1Values)

# Testing using m2
m2testscore <- predict(m2, test)
m2Values <- data.frame(obs = test$imdb_score, pred=m2testscore)
defaultSummary(m2Values)



#gross revenue prediction
m1<-train(gross~., data = train, method = "lm")
summary(m1)

m2 <- train(gross~imdb_score+movie_facebook_likes+duration+num_critic_for_reviews+num_voted_users+director_facebook_likes, data=train, method="lm")
summary(m2)

# Testing using m1
m1testscore <- predict(m1, test)
m1Values <- data.frame(obs = test$gross, pred=m1testscore)
defaultSummary(m1Values)

# Testing using m2
m2testscore <- predict(m2, test)
m2Values <- data.frame(obs = test$gross, pred=m2testscore)
defaultSummary(m2Values)



