library(data.table)
library(knitr)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(ggplot2)
  
rm(list=ls())
spotify = read.csv(file.choose(), header=T, stringsAsFactors=TRUE)
names(spotify)
attach(spotify)
summary(spotify)
str(spotify)

# Checking for acceptable range of values for numeric variable to identify outliers.
numeric_data <- spotify %>% dplyr::select(where(is.numeric))
summary(numeric_data)


# Checking for the number of missing or NA values in the dataframe
colSums(is.na(spotify))

# Exploratory Data Analysis
spotify <- spotify %>%
  mutate(uri = as.factor(spotify$uri),
         artist_names = as.factor(spotify$artist_names),
         track_name = as.factor(spotify$track_name),
         mode = as.factor(mode),
         key = as.factor(key))


# Creating New Variables
spotify$ranking[peak_rank <= 40] = "High"
spotify$ranking[peak_rank >40  & peak_rank <=120] = "Mid"
spotify$ranking[peak_rank > 120 & peak_rank <= 200] = "Low"

spotify$ranking <- as.factor(spotify$ranking)

table(spotify$peak_rank)

# Checking to see if there are any strong correlations between variables
library(corrplot)
corr_spotify <- select(spotify,peak_rank, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, tempo)
corrplot(cor(corr_spotify), type="lower")

# Assess the variable distribution
library(funModeling)
spotify_histograms <- spotify[,-c(1,2,3,4,5,6,7,8,11,13,20,22)]
plot_num(spotify_histograms)

## RANDOM FOREST USING ALL PREDICTORS=BAGGING
library(randomForest)
library(tree)
drop <- c("uri","time_signature","artist_names", "track_name", "ranking")
music1= spotify[,!(names(spotify) %in% drop)]

#create a training dataset
train = sample(1:nrow(music1), nrow(music1)/2)
spotify.test=music1[-train,"peak_rank"]
tree.spotify=tree(peak_rank~.,music1,subset=train)

set.seed(3)
model <- randomForest(peak_rank~., data=music1, subset=train)
model

which.min(model$mse)
# The model that produced the lowest test 
# mean squared error (MSE) used 234 trees.

sqrt(model$mse[which.min(model$mse)])
# The average difference between the predicted value for peak ranking and 
# the observed value was 46.8

set.seed(3)
bag.spotify=randomForest(peak_rank~., data=music1,subset=train, mtry=12,importance=TRUE)
bag.spotify

#Evaluate the performance of bagging by fitting it into the testing dataset. Then we calculate the MSE
perf.bag=predict(bag.spotify, newdata=music1[-train,])
mean((perf.bag-spotify.test)^2)
#Value of MSE is 2512.71

## RANDOM FOREST USING ONLY 5 PREDICTORS
set.seed(4)
rf.spotify=randomForest(peak_rank~.,data=music1,subset=train,mtry=5,importance=TRUE)
perf.rf = predict(rf.spotify,newdata=music1[-train,])
mean((perf.rf-spotify.test)^2)
#Value of MSE is 2432.19

#Two measures of variable importance are reported. One is
#based on the mean decrease of accuracy in predictions on the out of bag samples when a given variable
#is excluded from the model. The second is a measure of the total decrease in node impurity that
#results from splits over that variable, averaged over all trees
importance(rf.spotify)
varImpPlot(rf.spotify)

# With peak rank varying from 1 to 200, the accuracy of prediction might be off. We can also try doing the
# random forest but this time considering only songs that ranked 1-100.
music_mod <- filter(
  music1,peak_rank <= 100)

train2 = sample(1:nrow(music_mod), nrow(music_mod)/2)
spotify.test2=music_mod[-train,"peak_rank"]
tree.music2=tree(peak_rank~.,music_mod,subset=train)

set.seed(5)
rf.music2=randomForest(peak_rank~.,data=music_mod,subset=train2,mtry=5,importance=TRUE)
perf.rf2 = predict(rf.music2,newdata=music_mod[-train2,])
mean((perf.rf2-spotify.test2)^2)

#We notice the value of the MSE is 922, a lot lower now that we consider only songs
importance(rf.music2)
varImpPlot(rf.music2)

## Clustering
high <- subset(spotify,spotify$ranking == "High")

# Histogram displaying Speechiness of High Ranking Songs
par(mar=c(1,1,1,1))
hist(high$loudness, main = "Loudness of high songs")
hist(high$acousticness, main = "Acousticness of high songs")
hist(high$liveness, main = "Liveness of high songs")
hist(high$tempo, main = "Tempo of high songs")
hist(high$time_signature, main = "time sig of high songs")
hist(high$duration_ms, main = "duration of high songs")
hist(high$energy, main = "energy of high songs")
hist(high$speechiness, main = "speechiness of high songs")

# Histogram displaying Speechiness of Mid Ranking Songs
mid <- subset(spotify,spotify$ranking == "Mid")
hist(mid$loudness, main = "Loudness of mid songs")
hist(mid$acousticness, main = "Acousticness of mid songs")
hist(mid$liveness, main = "Liveness of mid songs")
hist(mid$tempo, main = "Tempo of mid songs")
hist(mid$time_signature, main = "time sig of mid songs")
hist(mid$duration_ms, main = "duration of mid songs")
hist(mid$energy, main = "energy of mid songs")
hist(mid$speechiness, main = "speechiness of mid songs")

# Histogram displaying Speechiness of Low Ranking Songs
low <- subset(spotify,spotify$ranking == "Low")
hist(low$loudness, main = "Loudness of low songs")
hist(low$acousticness, main = "Acousticness of low songs")
hist(low$liveness, main = "Liveness of low songs")
hist(low$tempo, main = "Tempo of low songs")
hist(low$time_signature, main = "time sig of low songs")
hist(low$duration_ms, main = "duration of low songs")
hist(low$energy, main = "energy of low songs")
hist(low$speechiness, main = "speechiness of low songs")


# HClusting
spotify.labs = spotify[,18]
spotify.data = (spotify[,4:17])
table(spotify.labs)

spotify.data

# Create a complete, average, and single hierarchal cluster to compare
data.dist=dist(spotify.data) 
hc1=hclust(data.dist)
hc2=hclust(data.dist, method="average")
hc3=hclust(data.dist, method="single")

hc1
hc2
hc3

#Plot the dendrogram for each cluster
par(mfrow=c(1,3))
plot(hc1, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hc2, main="Average Linkage", xlab="", sub="",ylab="")
plot(hc3, main="Single Linkage", xlab="", sub="",ylab="")

hc.clusters1=cutree(hc1,3)
hc.clusters2=cutree(hc2,3)
hc.clusters3=cutree(hc3,3)

table(hc.clusters1,spotify.labs)
table(hc.clusters2,spotify.labs)
table(hc.clusters3,spotify.labs)

# Determining the relationships within Clusters
names(spotify)
summary(spotify$speechiness)
summary(spotify$danceability)

par(mfrow=c(1,3))
cols <- c("blue", "orange", "black")
plot(danceability ~ energy, data=spotify, col= cols [spotify$ranking], main="danceability ~ energy")
legend("bottomleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=2, text.width = 1:10/5, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("pink", "orange", "red")
plot(danceability ~ speechiness, data=spotify, col= cols [spotify$ranking], main="danceability ~ speechiness")
legend("topright", legend=levels(spotify$ranking), col=cols, pch=1, ncol=1, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("pink", "blue", "purple")
plot(danceability ~ energy, data=spotify, col= cols [spotify$ranking], main="danceability ~ energy")
legend("bottomleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=1, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("pink", "blue", "purple")
plot(danceability ~ energy, data=spotify, col= cols [spotify$ranking], main="danceability ~ energy")
legend("bottomleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=3, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("red", "blue", "green")
plot(acousticness ~ energy, data=spotify, col= cols [spotify$ranking], main="acousticness ~ energy")
legend("bottomleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=3, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("red", "blue", "green")
plot(acousticness ~ loudness, data=spotify, col= cols [spotify$ranking], main="acousticness ~ loudness")
legend("bottomleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=3, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("red", "blue", "green")
plot(energy ~ tempo, data=spotify, col= cols [spotify$ranking], main="energy ~ tempo")
legend("bottomleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=3, text.width = 1:10/70, inset = .01, adj = c(0,.4))

spotify[order(spotify$rank),]

par(mfrow=c(1,3))
cols <- c("red", "blue", "green")
plot( tempo ~ energy, data=spotify, col= cols [spotify$ranking], main="tempo ~ energy")
legend("bottomleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=3, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("red", "blue", "green")
plot( liveness ~ energy, data=spotify, col= cols [spotify$ranking], main="liveness ~ energy")
legend("topleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=3, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("black", "blue", "yellow")
plot( loudness ~ energy, data=spotify, col= cols [spotify$ranking], main="liveness ~ energy")
legend("bottomleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=3, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("orange", "blue", "green")
plot( weeks_on_chart ~ energy, data=spotify, col= cols [spotify$ranking], main="weekonchart ~ energy")
legend("topright", legend=levels(spotify$ranking), col=cols, pch=1, ncol=3, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("black", "red", "blue")
plot( energy ~ duration_ms, data=spotify, col= cols [spotify$ranking], main="liveness ~ energy")
legend("bottomleft", legend=levels(spotify$ranking), col=cols, pch=1, ncol=3, text.width = 1:10/70, inset = .01, adj = c(0,.4))

par(mfrow=c(1,3))
cols <- c("black", "green", "red")
plot( energy ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="energy")

par(mfrow=c(1,3))
cols <- c("black", "green", "pink")
plot( danceability ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="daceability")

par(mfrow=c(1,3))
cols <- c("black", "red", "pink")
plot( speechiness ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="speechiness")

par(mfrow=c(1,3))
cols <- c("black", "green", "red")
plot( acousticness ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="acoustiness")

par(mfrow=c(1,3))
cols <- c("black", "green", "red")
plot( duration_ms ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="duration")

par(mfrow=c(1,3))
cols <- c("black", "green", "red")
plot( key ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="key")

par(mfrow=c(1,3))
cols <- c("black", "green", "red")
plot( instrumentalness ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="instrumental")

par(mfrow=c(1,3))
cols <- c("black", "green", "red")
plot( loudness ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="loud")

par(mfrow=c(1,3))
cols <- c("black", "green", "red")
plot( liveness ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="liveness")

par(mfrow=c(1,3))
cols <- c("black", "green", "red")
plot( tempo ~ spotify$ranking, data=spotify, col= cols [spotify$ranking], main="mode")

# Creating a model using only 3 variables 
music.data2 <- spotify[,c("loudness","duration_ms", "speechiness")]

data.dist2=dist(music.data2 ) 

newhc1=hclust(data.dist2)
newhc2=hclust(data.dist2, method="average")
newhc3=hclust(data.dist2, method="single")
newhc.clusters1=cutree(newhc1,3)
newhc.clusters2=cutree(newhc2,3)
newhc.clusters3=cutree(newhc3,3)

table(newhc.clusters1,spotify.labs)
table(newhc.clusters2,spotify.labs)
table(newhc.clusters3,spotify.labs)

par(mfrow=c(1,1))
plot(newhc3, labels=spotify.labs, cex = 0.001)
abline(h=1.3, col="red")
newhc3

levels(newhc3)

## KMEANS clustering
set.seed(6)

# KMeans Cluster using 12 variables (weeks_on_chart, danceability, energy, key, loudness, mode, speechiness, 
# acousticness, instrumentalness, liveliness, tempo, time_signature, and duration)
km.out1 =kmeans (spotify.data,3, nstart =1)

km.out1
km.out1$cluster

km.out1$betweenss
km.out1$withinss
km.out1$tot.withinss
km.out1$totss 

table(km.out1$cluster,spotify.labs)

set.seed(3)

km.out2 =kmeans (spotify.data,3, nstart =1)
km.out2$betweenss
km.out2$withinss
km.out2$tot.withinss
km.out2$totss
table(km.out2$cluster,spotify.labs)

# KMeans Cluster using only 3 variables (loudness, duration, and speechiness)
set.seed(1)
km.out3 =kmeans (spotify.data,3, nstart =20)
km.out3$betweenss
km.out3$withinss
km.out3$tot.withinss
km.out3$totss
table(km.out3$cluster,spotify.labs)

spotify.data2=spotify[,c("loudness","duration_ms", "speechiness")]

set.seed(1)
sd.data=scale(spotify.data2)
km.out4 =kmeans (sd.data,3, nstart =20)
km.out4$betweenss
km.out4$withinss
km.out4$tot.withinss
km.out4$totss
table(km.out4$cluster,spotify.labs)

km.out5 =kmeans (spotify.data2,3, nstart =20)
km.out5$betweenss
km.out5$withinss
km.out5$tot.withinss
km.out5$totss
table(km.out5$cluster,spotify.labs)

wss = km.out5$totss
for (i in 2:10) wss[i] = sum(kmeans(spotify.data2,centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="find the optimal value of K")

## Logistic Regression Model

#Split peak rank into only 2 categories, High and Low.
spotify <- data.frame(spotify)

spotify$ranking[peak_rank <= 40] = "High"
spotify$ranking[peak_rank > 40 & peak_rank <= 200] = "Low"
spotify$ranking <- as.factor(spotify$ranking)

#5-fold cross validation logistic regression model.
set.seed(1)
k=5
folds=sample(1:k,nrow(spotify),replace=TRUE) 

accuracy=rep(0,k)
for(i in 1:k)
{
  glm.fit<-glm(as.factor(ranking)~danceability+energy+key+mode+speechiness+acousticness+instrumentalness 
               +liveness+tempo+time_signature+duration_ms+loudness+weeks_on_chart,data=spotify[folds!=i,],
               family="binomial")
  spotify.test=spotify[folds==i,]
  
  glm.prob<-predict(glm.fit, spotify.test, type="response")
  glm.pred=rep("High", nrow(spotify[folds==i,]))
  
  glm.pred[glm.prob>.5]="Low"
  
  test.truevalue=spotify$ranking[folds==i]
  accuracy[i]=mean(glm.pred==test.truevalue)
} 
mean(accuracy) # 65.43%


# 10NN with logistic regression model

library(class)

# Normalize the data
st.dance=scale(danceability)
st.energy=scale(energy)
st.key=scale(key)
st.mode=scale(mode)
st.speech=scale(speechiness)
st.acoust=scale(acousticness)
st.instrum=scale(instrumentalness)
st.live=scale(liveness)
st.tempo=scale(tempo)
st.time=scale(time_signature)
st.duration=scale(duration_ms)
st.loud=scale(loudness)
st.week=scale(weeks_on_chart)
Input.standard=cbind(st.dance,st.energy,st.key,st.mode,st.speech,st.acoust,st.instrum,
                     st.live,st.tempo,st.time,st.duration,st.loud,st.week)
accuracy=matrix(0,10,5)

set.seed(2)
folds=sample(1:5,nrow(Input.standard),replace=TRUE)
for (j in 1:10)
{
  for(i in 1:5)
  {
    train.standard=Input.standard[folds!=i,]
    test.standard=Input.standard[folds==i,]
    train.truevalue=as.factor(spotify$ranking)[folds!=i]
    test.truevalue=as.factor(spotify$ranking)[folds==i]
    knn.pred=knn(train.standard,test.standard,train.truevalue,k=j)
    accuracy[j,i]=mean(knn.pred==test.truevalue)
  }
} 
cv.accuracy = apply(accuracy,1,mean)
mean(cv.accuracy) #56.24%

