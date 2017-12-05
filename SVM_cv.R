# SVM

#install.packages("e1071")
library("e1071")

# import the data and clean the data 

spotify <- read.table(file="Spotify4741_Pop_HipHop.csv", header=TRUE, sep=",")

# delete columns not needed
spotify <- spotify[, c("acousticness", "danceability", "energy", "liveness", "loudness", "speechiness", "tempo", "valence", "Like.or.not.like")]

spotify$Like.or.not.like[spotify$Like.or.not.like == -1] <- 0

spotify$Like.or.not.like <- as.factor(spotify$Like.or.not.like)

# get the index for training
set.seed(10)
crossvalid <- sample(1:nrow(spotify), 1/10*nrow(spotify))
spotify.crossvalid <- spotify[crossvalid, ]
spotify.remaining <- spotify[-crossvalid, ]
like.crossvalid <- spotify$Like.or.not.like[crossvalid]
like.remaining <- spotify$Like.or.not.like[-crossvalid]

#Product Run 
n = 500;
overall_accuracy = rep(0,n); overall_tprcol = rep(0,n); overall_recommended = rep(0,n); overall_weightedaverage = rep(0,n);
cv_accuracy = rep(0,n); cv_tprcol = rep(0,n); cv_recommended = rep(0,n); cv_weightedaverage = rep(0,n)

for (j in 1:n) {
  train <- sample(1:nrow(spotify.remaining), 2/3*nrow(spotify.remaining))
  spotify.test <- spotify.remaining[-train, ]
  like.test = like.remaining[-train]
  
  svm.spotify <- svm(Like.or.not.like~., data=spotify.remaining, subset= train)
  
  svm.predtest <- predict(svm.spotify, spotify.test, type = "class")
  svm.predcv <- predict(svm.spotify, spotify.crossvalid, type = "class")
  
  testtable = table(svm.predtest, like.test)
  cvtable = table(svm.predcv, like.crossvalid)
  #Check that table is 2x2 then only do the following
  if (dim(testtable)[1] == 2 && dim(testtable)[2] == 2 && ((testtable[1,1]+testtable[2,2])/(testtable[1,1]+testtable[2,2]+testtable[1,2]+testtable[2,1])) != 0 && (testtable[2,2]/(sum(testtable[,2]))) != 0 && (testtable[2,2]/(sum(testtable[2,]))) != 0) {
    
    #Go through each TPR weightage
    overall_accuracy[j] = (testtable[1,1]+testtable[2,2])/(testtable[1,1]+testtable[2,2]+testtable[1,2]+testtable[2,1]);
    overall_tprcol[j] = testtable[2,2]/(sum(testtable[,2]));
    overall_recommended[j] = testtable[2,2]/(sum(testtable[2,]));
    overall_weightedaverage[j] = (0.5*overall_tprcol[j] + 0.5*overall_recommended[j])
  }                  
  
  #Check that table is 2x2 then only do the following
  if (dim(cvtable)[1] == 2 && dim(cvtable)[2] == 2 && (cvtable[1,1]+cvtable[2,2])/(cvtable[1,1]+cvtable[2,2]+cvtable[1,2]+cvtable[2,1]) != 0 && cvtable[2,2]/(sum(cvtable[,2])) != 0 && cvtable[2,2]/(sum(cvtable[2,])) != 0 ) {
    
    #Go through each TPR weightage
    cv_accuracy[j] = (cvtable[1,1]+cvtable[2,2])/(cvtable[1,1]+cvtable[2,2]+cvtable[1,2]+cvtable[2,1]);
    cv_tprcol[j] = cvtable[2,2]/(sum(cvtable[,2]));
    cv_recommended[j] = cvtable[2,2]/(sum(cvtable[2,]));
    cv_weightedaverage[j] = (0.5*cv_tprcol[j] + 0.5*cv_recommended[j])
  }                  
}


mean(overall_accuracy);mean(overall_tprcol);mean(overall_recommended);mean(overall_weightedaverage);

mean(cv_accuracy);mean(cv_tprcol);mean(cv_recommended);mean(cv_weightedaverage)


