spotify <- read.table(file="Crossvalidation_accuracy.csv", header=TRUE, sep=",")
print(spotify)
spotify_new <- spotify[c(2,3,4,5), c(1:8)]
print(spotify_new)  
  #modifiedDataFile = dataFile[c(1:100, 1000:5000), c(1:8)];

#setting margins for the plot window
par(mar=c(7,6.5,4,3)+.1)

#for logistic
logistic = spotify_new[c(1), c(2:8)]
logistic[1,3] = 0.34543
print(logistic[1,3] *100)
print(logistic)
barplot(logistic)

logistic_acc = 100*c(0.5965289 ,0.4263476 ,0.34543   ,  0.740612 , 0.5560539 , 0.6466841 ,0.7095576);
names(logistic_acc)=c( "HipHop800","Pop","Rock","Mellow","HipHop & Pop","All Genres","HipHop600")
# Horizontal Barplot:
barplot(logistic_acc, col=c(2,3,4,5,6,7,8), horiz=T , las=1., xlim = c(0,100), xlab = "Accuracy", main = "Cross Validation Accuracies for Logistic Regression", font.main = 2, cex.main = 1)

#for decision trees
decision_trees_accuracy = spotify_new[c(2), c(2:8)]
print(decision_trees_accuracy)
decision_trees_acc = 100*c(0.6054198,  0.4043027, 0.2067302  ,  0.6206963 , 0.5260113 , 0.3873805 ,0.5749067);
names(decision_trees_acc)=c( "HipHop800","Pop","Rock","Mellow","HipHop & Pop","All Genres","HipHop600")
# Horizontal Barplot:
barplot(decision_trees_acc, col=c(2,3,4,5,6,7,8), horiz=T , las=1., xlim = c(0,100), xlab = "Accuracy", main = "Cross Validation Accuracies for Decision Trees", font.main = 2, cex.main = 1)


#for random forests
random_forests_accuracy = spotify_new[c(3), c(2:8)]
print(random_forests_accuracy)
random_forests_accuracy[1,5] = 0.312143
random_forests_acc = 100*c(0.6389146, 0.3204232, 0.1167   , 0.5648695  ,  0.34543 , 0.4659416 ,0.6807644);
names(random_forests_acc)=c( "HipHop800","Pop","Rock","Mellow","HipHop & Pop","All Genres","HipHop600")
# Horizontal Barplot:
barplot(random_forests_acc, col=c(2,3,4,5,6,7,8), horiz=T , las=1., xlim = c(0,100), xlab = "Accuracy" , main = "Cross Validation Accuracies for Random Forests", font.main = 2, cex.main = 1)


#for SVM
svm_accuracy = spotify_new[c(4), c(2:8)]
print(svm_accuracy)
svm_accuracy[1,3] = 0.24541
svm_acc = 100*c(0.6217322, 0.02677857, 0.24541 ,   0.6482601 , 0.3617065 , 0.2766902 ,0.6657481);
names(svm_acc)=c( "HipHop800","Pop","Rock","Mellow","HipHop & Pop","All Genres","HipHop600")
# Horizontal Barplot:
barplot(svm_acc, col=c(2,3,4,5,6,7,8), horiz=T , las=1., xlim = c(0,100), xlab = "Accuracy" , main = "Cross Validation Accuracies for SVM", font.main = 2, cex.main = 1)
