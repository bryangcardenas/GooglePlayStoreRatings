data <- read.csv(file="/Users/Bryan/Documents/R/googleplaystore.csv", header=TRUE, sep=",")
str(data)
#change reviews from factor to numeric
data$Reviews<-as.numeric(as.character(data$Reviews))
#delete any observations with missing data
data<-na.omit(data)
#remove observations with <50 reviews
data<-data[!(data$Reviews<50),]
#remove varies with device in size replace with median
data$Size[data$Size=="Varies with device"] <- 18

#remove dollar sign change price to numeric
install.packages("readr")
library(readr)
data$Price <- parse_number(data$Price)
#remove M sign change Size to numeric
data$Size <- parse_number(data$Size)
data$Size<-as.numeric(as.character(data$Size))
data$Size[is.na(data$Size)]<- 18
#Remove + in number of installs
data$Installs<- parse_number(data$Installs)
data$Installs<- as.numeric(data$Installs)
#remove columns current version, android version, last update
data <- data[, -c(10:13)]
#Change adults only 18+ and unrated to Mature 17+ 
data$Content.Rating[data$Content.Rating=="Adults only 18+"] <- "Mature 17+"
data$Content.Rating[data$Content.Rating=="Unrated"] <- "Mature 17+"
#Remove apps price higher than 100
data<-data[!(data$Price>100),]
#Lastley remove duplicate rows
install.packages("dplyr")
library(dplyr)
data <- distinct(data)

#install ggplot
install.packages("ggplot2")
library(ggplot2)
#Rating statistics bar chart
ggplot(data,aes(x=data$Rating, fill = Type))+
  geom_bar()+
  theme_bw()+
  labs(y="Application Count",x="Store-Rating"
  )

#Pie chart Content Rating
bp<- ggplot(data, aes(x="", y ="", fill = Content.Rating))+
  geom_bar(width = 1, stat = "identity")


pie <- bp + coord_polar("y", start=0)
pie

#paid vs free high vs low rating bar chart
ratdata$Rating[ratdata$Rating>=4]<- "High"
ratdata$Rating[ratdata$Rating<4]<- "Low"

ggplot(ratdata,aes(x=Rating, fill = Type))+
  geom_bar()+
  theme_bw()+
  labs(y="Application Count",x="Store-Rating"
  )

#Category vs rating box plot 
cat.rating <- ggplot(data,aes(x=Category,y = Rating))+
  geom_boxplot(fill = "#EC7063")+
  theme_bw()+
  labs(y="Store-Rating",x="Category"
  )
cat.rating + theme(axis.text.x = element_text(angle = 60, hjust = 1))

#Rating vs Price filterd by content rating
cat<- ggplot(data,aes(x=Reviews,y=Rating))+
  geom_point(fill = "#EC7063")+
  theme_bw()+
  labs(y="Store-Rating",x="Reviews"
  )
cat + geom_point(aes(colour = Content.Rating)) + theme(axis.text.x = element_text(angle=60,hjust=1))

#Rating vs Size
theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(data, aes(x=Size, y=Rating))
g + geom_jitter(width = .5, size=1) + geom_point(fill = "#EC7063")+
  labs( 
    y="Store-Rating", 
    x="Size (Mb)" 
  )+ggtitle('Title')

#Summary statistics mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
###########
linearMod <- lm(Rating ~ Reviews+Size+Installs+Price, data=data)  # build the model
summary(linearMod)
# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(filtdata), 0.8*nrow(filtdata))  # row indices for training data
trainingData <- filtdata[trainingRowIndex, ]  # model training data
testData  <- filtdata[-trainingRowIndex, ]
###
lmMod <- lm(Rating ~ Reviews+Size+Installs+Price, data=trainingData)  # build the model
RatingPred <- predict(lmMod, testData)  # predict rating
summary(lmMod)

#####
actuals_preds <- data.frame(cbind(actual=testData$Rating, predicted=RatingPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 8%
head(actuals_preds)
max(actuals_preds$predicted)
min(actuals_preds$predicted)