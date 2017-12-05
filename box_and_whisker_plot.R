library(ggplot2)
install.packages("ggridges")
library(ggridges)
library(ggjoy)
spotify <- read.table(file="FinalVisuals.csv", header=TRUE, sep=",")

spotify$Genre[spotify$Genre == 1] <- "Pop"
spotify$Genre[spotify$Genre == 2] <- "Rock"
spotify$Genre[spotify$Genre == 3] <- "Mellow"
spotify$Genre[spotify$Genre == 4] <- "Hip Hop"

spotify$Genre <- as.factor(spotify$Genre)
head(spotify)



ggplot(spotify, aes(x = energy, y = Genre, fill = Genre)) + geom_density_ridges() + theme_ridges() +theme(legend.position = "none") +theme(legend.position = ("bottom")) + ggtitle("DIstribution of Acousticness Across Different Genres")+theme(plot.title = element_text(hjust = 0.3))



ggplot(spotify, aes(x=Genre, y=acousticness, fill=Genre))+geom_boxplot(alpha=.2)+theme(legend.position = ("bottom")) + ggtitle("Box-and-Whisker Plot for Acousticness across Different Genres")+theme(plot.title = element_text(hjust = 0.5))
ggplot(spotify, aes(x=Genre, y=danceability, fill=Genre))+geom_boxplot(alpha=.2)+theme(legend.position = ("bottom")) + ggtitle("Box-and-Whisker Plot for Danceability across Different Genres")+theme(plot.title = element_text(hjust = 0.5))
ggplot(spotify, aes(x=Genre, y=speechiness, fill=Genre))+geom_boxplot(alpha=.2)+theme(legend.position = ("bottom")) + ggtitle("Box-and-Whisker Plot for Speechiness across Different Genres")+theme(plot.title = element_text(hjust = 0.5))
ggplot(spotify, aes(x=Genre, y=valence, fill=Genre))+geom_boxplot(alpha=.2)+theme(legend.position = ("bottom")) + ggtitle("Box-and-Whisker Plot for Valence across Different Genres")+theme(plot.title = element_text(hjust = 0.5))



# legend(.75,.3,legend=c("Hip Hop","Pop","Rock","Sad","All"),col=c(rgb(1,0,0),rgb(.1,.5,.1),rgb(0,0,1),rgb(1,0,1),rgb(1,165/255,0)),pch=15,title="Genres")

