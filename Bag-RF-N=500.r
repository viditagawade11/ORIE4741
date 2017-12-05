# Regression experiments for 4741

#install.packages("randomForest")
# Decision Trees
library(randomForest)

# Import

spotify <- read.table(file="SpotifyHipHop600.csv", header=TRUE, sep=",")

# delete columns not needed
spotify <- spotify[, c("acousticness", "danceability", "energy", "liveness", "loudness", "speechiness", "tempo", "valence", "Like.or.not.like")]
# for decision trees purposes, create another column indicating like or not like
spotify$Like.or.not.like[spotify$Like.or.not.like == -1] <- 0
Like = ifelse(spotify$Like.or.not.like == 0, "No", "Yes")
spotify2 = data.frame(spotify, Like)

set.seed(10)
crossvalid <- sample(1:nrow(spotify), 1/10*nrow(spotify))
spotify2.crossvalid <- spotify2[crossvalid, ]
spotify2.remaining <- spotify2[-crossvalid, ]
Like.crossvalid <- Like[crossvalid]
Like.remaining <- Like[-crossvalid]

#Production Run 
n=100;
overall_accuracy = rep(0,n); overall_tprcol= rep(0,n); overall_recommended = rep(0,n); overall_weightedaverage = rep(0,n);
cv_accuracy= rep(0,n); cv_tprcol = rep(0,n); cv_recommended = rep(0,n); cv_weightedaverage = rep(0,n)


for (j in 1:n) {
    # get the index for training
    train <- sample(1:nrow(spotify2.remaining), 2/3*nrow(spotify2.remaining))
    spotify2.test <- spotify2.remaining[-train, ]
    Like.test = Like.remaining[-train]
  
    # bagging 500 trees
    bag.spotify2 = randomForest(Like~.-Like.or.not.like, data=spotify2.remaining, subset=train, mtry = 8, ntree = 500)
    
    bag.predtest = predict(bag.spotify2, spotify2.test, type = "class")
    bag.predcv = predict(bag.spotify2, spotify2.crossvalid, type = "class")

    testtable = table(bag.predtest, Like.test)
    cvtable = table(bag.predcv, Like.crossvalid)
    print(testtable)
    print(cvtable)
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
mean(cv_accuracy);mean(cv_tprcol);mean(cv_recommended);mean(cv_weightedaverage);


#sd(overall);sd(tprcol);sd(recommended);sd(weightedaverage);
#N1 = (sd(overall))^2*1.96^2/0.01^2; N2 = (sd(tprcol))^2*1.96^2/0.01^2;N3=(sd(recommended))^2*1.96^2/0.01^2;
#N4 = (sd(weightedaverage))^2*1.96^2/0.01^2
#print(N1)
#print(N2)
#print(N3)
#print(N4)

# > mean(overall);mean(tprcol);mean(recommended);mean(weightedaverage);
# [1] 0.6114433
# [1] 0.5900412
# [1] 0.616341
# [1] 0.6031911
# > sd(overall);sd(tprcol);sd(recommended);sd(weightedaverage);
# [1] 0.01135109
# [1] 0.02048186
# [1] 0.01062775
# [1] 0.01457182



