
# Regression experiments for 4741
#install.packages("tree")
# Decision Trees

library(tree)

# Import
# The version with cross-validation

spotify <- read.table(file="SpotifyHipHop800.csv", header=TRUE, sep=",")

# delete columns not needed
spotify <- spotify[, c("acousticness", "danceability", "energy", "liveness", "loudness", "speechiness", "tempo", "valence", "Like.or.not.like")]
# for decision trees purposes, create another column indicating like or not like
Like = ifelse(spotify$Like.or.not.like == 0, "No", "Yes")
spotify2 = data.frame(spotify, Like)

set.seed(10)
crossvalid <- sample(1:nrow(spotify), 1/10*nrow(spotify))
spotify2.crossvalid <- spotify2[crossvalid, ]
spotify2.remaining <- spotify2[-crossvalid, ]
Like.crossvalid <- Like[crossvalid]
Like.remaining <- Like[-crossvalid]


#Production Run 
n=500;
overall_accuracy = rep(0,n); overall_tprcol= rep(0,n); overall_recommended = rep(0,n); overall_weightedaverage = rep(0,n);
cv_accuracy= rep(0,n); cv_tprcol = rep(0,n); cv_recommended = rep(0,n); cv_weightedaverage = rep(0,n)

for (j in 1:n) {
    # get the index for training
    train <- sample(1:nrow(spotify2.remaining), 2/3*nrow(spotify2.remaining))
    spotify2.test <- spotify2.remaining[-train, ]
    Like.test = Like.remaining[-train]
    
    # create the tree
    tree.spotify2 = tree(Like~.-Like.or.not.like, spotify2.remaining, subset = train)
    
    tree.predtest = predict(tree.spotify2, spotify2.test, type="class")
    tree.predcv = predict(tree.spotify2, spotify2.crossvalid, type="class")
    
    testtable = table(tree.predtest, Like.test)
    cvtable = table(tree.predcv, Like.crossvalid)
    
    #Check that table is 2x2 then only do the following
    if (dim(testtable)[1] == 2 && dim(testtable)[2] == 2 && (testtable[1,1]+testtable[2,2])/(testtable[1,1]+testtable[2,2]+testtable[1,2]+testtable[2,1]) != 0 && testtable[2,2]/(sum(testtable[,2])) != 0 && testtable[2,2]/(sum(testtable[2,])) != 0) {

        #Go through each TPR weightage
        overall_accuracy[j] = (testtable[1,1]+testtable[2,2])/(testtable[1,1]+testtable[2,2]+testtable[1,2]+testtable[2,1]);
        overall_tprcol[j] = testtable[2,2]/(sum(testtable[,2]));
        overall_recommended[j] = testtable[2,2]/(sum(testtable[2,]));
        overall_weightedaverage[j] = (0.5*overall_tprcol[j] + 0.5*testtable[2,2]/sum(testtable[2,]))
    }                  

    #Check that table is 2x2 then only do the following
    if (dim(cvtable)[1] == 2 && dim(cvtable)[2] == 2 && (cvtable[1,1]+cvtable[2,2])/(cvtable[1,1]+cvtable[2,2]+cvtable[1,2]+cvtable[2,1]) != 0 && cvtable[2,2]/(sum(cvtable[,2])) != 0 && cvtable[2,2]/(sum(cvtable[2,])) != 0) {
      
      #Go through each TPR weightage
      cv_accuracy[j] = (cvtable[1,1]+cvtable[2,2])/(cvtable[1,1]+cvtable[2,2]+cvtable[1,2]+cvtable[2,1]);
      cv_tprcol[j] = cvtable[2,2]/(sum(cvtable[,2]));
      cv_recommended[j] = cvtable[2,2]/(sum(cvtable[2,]));
      cv_weightedaverage[j] = (0.5*cv_tprcol[j] + 0.5*cvtable[2,2]/sum(cvtable[2,]))
    }   
}

mean(overall_accuracy);mean(overall_tprcol);mean(overall_recommended);mean(overall_weightedaverage);
#sd(overall_accuracy);sd(overall_tprcol);sd(overall_recommended);sd(overall_weightedaverage);

mean(cv_accuracy);mean(cv_tprcol);mean(cv_recommended);mean(cv_weightedaverage);
#sd(cv_accuracy);sd(cv_tprcol);sd(cv_recommended);sd(cv_weightedaverage);


N1 = (sd(overall))^2*1.96^2/0.01^2; N2 = (sd(tprcol))^2*1.96^2/0.01^2;N3=(sd(recommended))^2*1.96^2/0.01^2;
N4 = (sd(weightedaverage))^2*1.96^2/0.01^2
print(N1)
print(N2)
print(N3)
print(N4)

# > mean(overall);mean(tprcol);mean(recommended);mean(weightedaverage);
# [1] 0.5659588
# [1] 0.52895
# [1] 0.5552717
# [1] 0.5421109


plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
title(main="main title",