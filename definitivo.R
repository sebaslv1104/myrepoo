rm(list = ls())
setwd("C:/Users/chevi/Documents/GOETHE 2 CUATRI/Data science and marketing analytics")


#library
library(NLP)
library(tm)
library(ggplot2)
library(maps)
library(leaflet)
library(knitr)
library(mice)
library(tidyr)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(gtrendsR)
library(Hmisc)
library(doParallel)
library(caret)
library(smotefamily)

df_business=read.csv("business.csv",stringsAsFactors = F)

#Extract only open restaurants
restaurants <- df_business[grepl('Restaurant',df_business$categories) & df_business$is_open == "True",]

#Finding restaurants only in Akron
ak_restaurants1 <- restaurants %>% filter(city=="Akron")

#Loading all Akron businesses                                         
business_data=read.csv("akron_business.csv",header=TRUE, na.strings="NULL")

#Union of tables to keep only significant variables
ak_restaurants<-left_join(ak_restaurants1, business_data, by="business_id")
business_data=ak_restaurants[c(1:10, 18:21)]

#Dealing with the missings.
business_data$n_photo[is.na(business_data$n_photo)]=0
business_data1 <- subset(business_data,select = -c(business_id)) # removed this because MICE does not like imputing factors with more than 50 levels
library(mice)
#Inspect pattern of missings
md.pattern(business_data1)
#Below, the predictormatrix is specified.
#It is a square matrix of size ncol(data) containing 0/1 data specifying the set of predictors to be used for each target column. 
#Rows correspond to target variables (i.e. variables to be imputed), in the sequence as they appear in data. 
#A value of '1' means that the column variable is used as a predictor for the target variable (in the rows). 
#The diagonal of predictorMatrix must be zero.
predictorMatrix <- matrix(0,nrow = ncol(business_data1), ncol = ncol(business_data1)) # Make a matrix of zeros
colnames(predictorMatrix)=colnames(business_data1)
row.names(predictorMatrix)=colnames(business_data1)
predictorMatrix[c("business_price"),] <- 1 #Variables "business_price" can be explained by all other variables
diag(predictorMatrix) <- 0 #Diagonal must be zero

#Impute data
business_data1_data_imputed <- mice(business_data1, predictorMatrix = predictorMatrix, m=5, maxit = 50, seed = 500)
summary(business_data1_data_imputed)

#Get one of the complete data sets ( 2nd out of 5)
business_data_complete_data <- complete(business_data1_data_imputed,2)

#Bring back the business_id
business_data_complete_data=cbind(business_id=business_data$business_id,business_data_complete_data)

md.pattern(business_data_complete_data)
#No missing data

#Distribution of ratings
restaurants_and_stars <- business_data_complete_data %>% group_by(stars) %>% count()
ggplot(data=restaurants_and_stars, aes(x=cut(stars, c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)), y=n, fill=cut(stars, c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5)))) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Distribution of Ratings by Restaurants in Akron", y = "Count of Restaurants", x = "Star Category", fill = "Star Category") +
  theme(legend.position="none") +
  scale_x_discrete(labels=c("1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5"))

#Distributions of price range
restaurants_and_price_range <- business_data_complete_data %>% group_by(business_price) %>% count()
ggplot(data = restaurants_and_price_range, aes(x = business_price, y = n, fill = cut(business_price, c(0, 1, 2, 3)))) +
  geom_col() +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(title = "Distribution of Price Range by Restaurants in Akron", y = "Count of Restaurants", x = "Price Range", fill = "Star Category") +
  theme(legend.position = "none")

#Correlation of price on ratings
ggplot(data = subset(business_data_complete_data, !is.na(business_price)), 
       aes(x = factor(business_price), y = stars, color = factor(business_price))) + 
  geom_boxplot() +
  labs(title = "Ratings vs Price Range in Akron", y = "Average Rating", x = "Price Range", fill = "Price Range") + 
  scale_color_brewer(palette = "YlOrRd") + 
  theme(legend.position = "none")

#Interactive map of restaurants in Akron with their average ratings indicated by color
binpal <- colorBin(c("Red", "Orange", "Yellow", "Green"), business_data_complete_data$stars, 4, pretty = FALSE)
map1 <- leaflet(business_data_complete_data) %>% addProviderTiles("CartoDB.Positron") %>% addCircleMarkers(lng = ~longitude, lat = ~latitude, weight = 0, radius=5, fillOpacity = 0.7 , color = ~binpal(stars), popup = ~name) %>% addLegend("bottomleft", pal=binpal, values=stars, title = "Restaurants and their Average Ratings", labels=~binpal(stars),labFormat = labelFormat(prefix = ""), opacity = 0.7)
map1

#Search for the most common categories Akron
#Defining the custom NGramTokenizer function
NGramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), n = 1:3), paste, collapse = " "))
}
#Selects only the name and categories columns from the business_data_complete_data dataframe
all_categories <- business_data_complete_data %>%
  select(name, business_cat)
#Creation of the corpus
docs <- Corpus(VectorSource(all_categories$business_cat))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, c(stopwords("english"), "s", "ve"))
docs <- tm_map(docs, stripWhitespace)
#Creating the term matrix
tdm <- TermDocumentMatrix(docs, control = list(tokenize = NGramTokenizer))
#Calculation of term frequencies
freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
freq.df <- data.frame(word = names(freq), freq = freq)
#Visualization of the main categories with the creation of an interactive table
DT::datatable(as.data.frame(freq.df[1:10, "word"]))

#Correlation Price and Rating, coeficente di conntingenza
#Calculate the contingency table
contingency_table <- table(business_data_complete_data$business_price, business_data_complete_data$stars)
#Calculate the contingency coefficient
coefficiente_contingenza <- chisq.test(contingency_table)$statistic / sum(contingency_table)
#Create a data frame for the bar chart
df <- data.frame(
  Price = rep(rownames(contingency_table), ncol(contingency_table)),
  Stars = rep(colnames(contingency_table), each = nrow(contingency_table)),
  Frequency = as.vector(contingency_table)
)
#Create bar graph
bar_plot <- ggplot(df, aes(x = Price, y = Frequency, fill = Stars)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Business Price", y = "Frequency", fill = "Stars") +
  ggtitle("Correlation between price range and average number of stars of restaurants in Akron") +
  theme(legend.position = "top")
#View bar graph
print(bar_plot)
#Print the contingency coefficient
print(paste("Coefficient of Contingency:", coefficiente_contingenza))
#Significance test
test_chi_quadro <- chisq.test(contingency_table)
print(test_chi_quadro)

#Extracting data from Google trends on the most common categories for an understanding of which of these could be significant variables to the prediction
df_trend=gtrends(keyword = c("American", "Bars", "Sandwiches","Pizza","Burgers"), geo = "US", time = "2018-01-01 2018-11-14")
df_trend$interest_by_city
interest_by_city=df_trend$interest_by_city
interest_by_city %>% filter(location %in% "Ohio")
interest_over_over_the_time=df_trend$interest_over_time
interest_over_over_the_time$hits=as.numeric(interest_over_over_the_time$hits)
keywords <- c("American", "Bars", "Sandwiches", "Pizza", "Burgers")
ggplot(subset(interest_over_over_the_time, keyword %in% keywords), aes_string(x = "date", y = "hits", color = "keyword")) +
  geom_line() +
  theme_minimal()

#parte ML
#Merge with business data
DailyLevel_data <- read.csv("tips_2018-01-01-2018-11-14-Akron_data.csv",header=TRUE, na.strings="NA") #read csv file. Be aware of the parameters, such as the string for NA's. Usually either "" or "\\NA".
cum_u_names=DailyLevel_data$cum_u_names
Nobs=length(cum_u_names)

#Store the date in date column and remove the old name
DailyLevel_data$date <- as.Date(DailyLevel_data$date_tip) 
DailyLevel_data$date_tip <- NULL

#Join the tables
library(dplyr)
DailyLevel_data=DailyLevel_data%>%
  inner_join(business_data_complete_data,by="business_id")

#Make factors out of chr variables
for(j in 1:ncol(DailyLevel_data)){
  if(typeof(DailyLevel_data[,j])=="character")
    DailyLevel_data[,j]=as.factor(DailyLevel_data[,j])
}

#Limit the number of categories to American, Bars, Sandwiches, Pizza, Burgers and Others
cat_s=as.character(DailyLevel_data$business_cat)
new_cat_s=c("Others","American", "Bars", "Sandwiches","Pizza","Burgers")
changed=0
for(k in new_cat_s[-1]){
  cat_s[grepl(k,cat_s)]=k
  changed=changed+grepl(k,cat_s)
}
cat_s[changed==0]="Others"
DailyLevel_data$business_cat=as.factor(cat_s)

#n_photos==NA and cum_max_u_elite==NA are actually zeros, let's replace them with 0 before imputing.
DailyLevel_data$cum_max_u_elite[is.na(DailyLevel_data$cum_max_u_elite)]=0

#Some descriptive of the data
library(Hmisc)
describe(DailyLevel_data)
yelp_data=DailyLevel_data
str(yelp_data)
yelp_data$date = as.Date(yelp_data$date)
yelp_data$ch_in_string[yelp_data$ch_in>=1]="ch_in"
yelp_data$ch_in_string[yelp_data$ch_in==0]="Noch_in"
yelp_data$ch_in_string <- as.factor(yelp_data$ch_in_string)
yelp_data$ch_in_string <- relevel(yelp_data$ch_in_string,ref="ch_in") # since the performance evaluations are mainly made
#To check for the minority class - in our case Noch_in
yelp_data$business_open=as.factor(yelp_data$business_open)
yelp_data$business_cat=as.factor(yelp_data$business_cat)

# A simple regression analysis ----
m1=glm(ch_in~cum_n_tips+cum_max_friends+cum_max_us_fans+cum_max_us_tip+
         business_price+business_cat, data = yelp_data, family = "binomial")
car::vif(m1)
summary(m1)

library(doParallel)
library(caret)
library(smotefamily)



#Predictiv models
#Split randomly
set.seed(66)
yelp_data_na=yelp_data
#List of variables in our model
varsin=c("ch_in_string","ch_in","business_price","business_cat","n_photo","cum_n_tips","cum_max_friends","cum_max_u_elite","cum_max_us_fans","cum_max_us_tip", "stars")
yelp_data=subset(yelp_data,select=varsin)
datasetsize=nrow(yelp_data)/10 # would you like to work only  on a subset of your data? 
x <- yelp_data[sample(1:nrow(yelp_data), datasetsize, replace = F),]
x.train <- x[1:floor(nrow(x)*.75), ]
x.evaluate <- x[(floor(nrow(x)*.75)+1):nrow(x), ]

BaseFormula <- as.formula(paste0("ch_in_string~",paste(varsin[-c(1,2)],collapse = "+")))
BaseFormula1 <- as.formula(paste0("ch_in~",paste(varsin[-c(1,2)],collapse = "+")))

#Create dummies (required for SMOTE)
x.traindum=cbind(x.train[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.train),newdata = x.train))
x.evaluatedum=cbind(x.evaluate[,c("ch_in","ch_in_string")],predict(dummyVars(BaseFormula1,data=x.evaluate),newdata = x.evaluate))

#Class imbalance check.
temp=table(x.train[,"ch_in_string"])
print(temp)
# if yes, maybe you want do random over-sampling:
if(0){
  oversampled=x.train[x.train$ch_in_string==names(temp)[sort.int(temp,index.return=T,decreasing = T)$ix[1]],]
  minclass=names(temp)[sort.int(temp,index.return=T)$ix[1]]
  for(m in 1:(length(temp)-1)){
    minchclass=names(temp)[sort.int(temp,index.return=T)$ix[m]]
    minclassdat=x.train[x.train$ch_in_string==minchclass,]
    minclassdat=minclassdat[sample(1:nrow(minclassdat), sort(temp,decreasing = T)[1] , replace = T),]
    oversampled=rbind(oversampled,minclassdat)
  }
  x.train=oversampled
}

#Or do SMOTE:
if(1){
  x.traindum_smote=SMOTE(x.traindum[,-c(1,2)],x.traindum[,2])$data
  names(x.traindum_smote)[ncol(x.traindum_smote)]="ch_in_string"
  x.traindum_smote$ch_in=ifelse(x.traindum_smote$ch_in_string=="ch_in",1,0)
  x.traindum_smote$ch_in_string=as.factor(x.traindum_smote$ch_in_string)
  x.traindum=x.traindum_smote
  rm(x.traindum_smote)
}
temp=table(x.traindum[,"ch_in_string"])
print(temp)



############ Data for Heuristic machine learning methods
#Normalize data (very important for ML techniques, but not for logistic regression)
x.trainnorm=predict(preProcess(x.traindum, method = "range"), newdata=x.traindum)
x.evaluatenorm=predict(preProcess(x.evaluatedum, method = "range"), newdata=x.evaluatedum)

#Adjust Baseformulea to the dummy version of the data
varsin_dum=varsin[1:2]
for(i in 3:length(varsin)){
  if(!is.null(levels(x[,varsin[i]]))){
    for(j in 2:nlevels(x[,varsin[i]])){ # first level will be considered as the base-level
      varsin_dum=c(varsin_dum,paste(varsin[i],levels(x[,varsin[i]])[j],sep="."))
    }
  }else{
    varsin_dum=c(varsin_dum,varsin[i])
  }
}

#Redo the releveling:
x.traindum$ch_in_string=relevel(x.traindum$ch_in_string,ref="Noch_in") 
x.evaluatedum$ch_in_string=relevel(x.evaluatedum$ch_in_string,ref="Noch_in")
x.trainnorm$ch_in_string=relevel(x.trainnorm$ch_in_string,ref="Noch_in") 
x.evaluatenorm$ch_in_string=relevel(x.evaluatenorm$ch_in_string,ref="Noch_in")

BaseFormula_dum <- as.formula(paste0("ch_in_string~",paste(varsin_dum[-c(1,2)],collapse = "+")))
BaseFormula1_dum <- as.formula(paste0("ch_in~",paste(varsin_dum[-c(1,2)],collapse = "+")))

#Set threshold probability: usually .5, but better is to set it to the portion of 1's. 
probthres=mean(x.traindum$ch_in)

makeLiftPlot <- function(Prediction, Evaluate, ModelName){
  # plots the liftplot, and computes the GINI coefficient.
  iPredictionsSorted <- sort(Prediction,index.return=T,decreasing=T)[2]$ix #extract the index order according to predicted retention
  CustomersSorted <- Evaluate$ch_in_string[iPredictionsSorted] #sort the true behavior of customers according to predictions
  SumChurnReal<- sum(Evaluate$ch_in_string == "ch_in") #total number of real churners in the evaluation set
  CustomerCumulative=seq(nrow(Evaluate))/nrow(Evaluate) #cumulative fraction of customers
  ChurnCumulative=apply(matrix(CustomersSorted=="ch_in"),2,cumsum)/SumChurnReal #cumulative fraction of churners
  ProbTD = sum(CustomersSorted[1:floor(nrow(Evaluate)*.1)]=="ch_in")/floor(nrow(Evaluate)*.1) #probability of churn in 1st decile
  ProbOverall = SumChurnReal / nrow(Evaluate) #overall churn probability
  TDL = ProbTD / ProbOverall
  GINI = sum((ChurnCumulative-CustomerCumulative)/(t(matrix(1,1,nrow(Evaluate))-CustomerCumulative)),na.rm=T)/nrow(Evaluate)
  plot(CustomerCumulative,ChurnCumulative,type="l",main=paste("Lift curve of", ModelName),xlab="Cumulative fraction of check-ins (sorted by predicted check-in probability)",ylab="Cumulative fraction of check-ins")
  lines(c(0,1),c(0,1),col="blue",type="l",pch=22, lty=2)
  legend(.66,.2,c("According to model","Random selection"),cex=0.8,  col=c("black","blue"), lty=1:2)
  text(0.15,1,paste("TDL = ",round(TDL,2), "; GINI = ", round(GINI,2) ))
  return(data.frame(TDL,GINI))
}


# ----
#The analyses
######### LOGIT
ptm <- proc.time()
x.modelLogit <- glm(BaseFormula_dum , data = x.traindum, family = "binomial") # estimating the probability of "checkin"

summary(x.modelLogit)

x.evaluate$predictionlogit <- predict(x.modelLogit, newdata=x.evaluatedum, type = "response")
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit>probthres] <- "ch_in"
x.evaluate$predictionlogitclass[x.evaluate$predictionlogit<=probthres] <- "Noch_in"

x.evaluate$correctlogit <- x.evaluate$predictionlogitclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctlogit)))
LogitOutput <- makeLiftPlot(x.evaluate$predictionlogit,x.evaluate,"Logit")

TimeAux <- proc.time() - ptm 
#LogitOutput$summary=summary(x.modelLogit)
LogitOutput$TimeElapsed <- TimeAux[3] 
LogitOutput$PercCorrect <- mean(x.evaluate$correctlogit)*100
Logitconfmatrix <- table(x.evaluate$predictionlogitclass,x.evaluate$ch_in_string)
rm(TimeAux)


############ Naive Bayes
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelNB <- train(BaseFormula_dum, data = x.trainnorm, method="naive_bayes")

x.evaluate$predictionNB <- predict(x.modelNB, newdata=x.evaluatenorm,type="prob")


x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionNBclass[x.evaluate$predictionNB[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctNB <- x.evaluate$predictionNBclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNB)))

# the variable importance
print(varImp(x.modelNB))

# Extract the class probabilities.
x.evaluate$predictionNB <- x.evaluate$predictionNB[,'ch_in']

NBOutput <- makeLiftPlot(x.evaluate$predictionNB,x.evaluate,"NB")

TimeAux <- proc.time() - ptm 
NBOutput$TimeElapsed <- TimeAux[3]
NBOutput$PercCorrect <- mean(x.evaluate$correctNB)*100
NBconfmatrix <- table(x.evaluate$predictionNBclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


############ KNN
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
x.modelKNN <- knn3(BaseFormula_dum, data=x.trainnorm, k=23, prob=FALSE, use.all=TRUE)
x.evaluate$predictionKNN <- predict(x.modelKNN, newdata=x.evaluatenorm,type="prob")


x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionKNNclass[x.evaluate$predictionKNN[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctKNN <- x.evaluate$predictionKNNclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctKNN)))


# the variable importance
print(varImp(x.modelKNN))

# Extract the class probabilities.
x.evaluate$predictionKNN <- x.evaluate$predictionKNN[,'ch_in']

KNNOutput <- makeLiftPlot(x.evaluate$predictionKNN,x.evaluate,"KNN")

TimeAux <- proc.time() - ptm 
KNNOutput$TimeElapsed <- TimeAux[3]
KNNOutput$PercCorrect <- mean(x.evaluate$correctKNN)*100
KNNconfmatrix <- table(x.evaluate$predictionKNNclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)

############ SVM
cl <- makeCluster(detectCores())
registerDoParallel(cl)
ptm <- proc.time()
# fast trainer
x.trainnorm <- na.omit(x.trainnorm)
x.modelSVM <- train(BaseFormula_dum, data = x.trainnorm, method="svmRadial", cachesize=1000, tolerance=.1,
                    trControl = trainControl(classProbs =  TRUE))

x.evaluate$predictionSVM <- predict(x.modelSVM, newdata=x.evaluatenorm, type="prob")


x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']>probthres]="ch_in"
x.evaluate$predictionSVMclass[x.evaluate$predictionSVM[,'ch_in']<=probthres]="Noch_in"

x.evaluate$correctSVM <- x.evaluate$predictionSVMclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctSVM)))

# for fast trainer you can also get the variable importance
print(varImp(x.modelSVM))

# Extract the class probabilities.
x.evaluate$predictionSVM <- x.evaluate$predictionSVM[,'ch_in']

SVMOutput <- makeLiftPlot(x.evaluate$predictionSVM,x.evaluate,"SVM")

TimeAux <- proc.time() - ptm 
SVMOutput$TimeElapsed <- TimeAux[3]
SVMOutput$PercCorrect <- mean(x.evaluate$correctSVM)*100
SVMconfmatrix <- table(x.evaluate$predictionSVMclass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)

########## Neural network
cl <- makeCluster(detectCores())
registerDoParallel(cl)

library(NeuralNetTools) # required for plotting
# fast trainer using parallel computations
ptm <- proc.time()
mlp_grid = expand.grid(layer1 = 5,
                       layer2 = 0,
                       layer3 = 0)
x.modelNNet <- train(BaseFormula_dum, data=x.trainnorm, method='mlpML',tuneGrid=mlp_grid) 

x.evaluate$predictionNNet <- predict(x.modelNNet, newdata = x.evaluatenorm, type="prob")

x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionNNetclass[x.evaluate$predictionNNet[,"ch_in"]<=probthres]="Noch_in"


x.evaluate$correctNNet <- x.evaluate$predictionNNetclass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctNNet)))

print(varImp(x.modelNNet))
# plot NNet
if(0){
  NeuralNetTools::plotnet(x.modelNNet$finalModel)
}
x.evaluate$predictionNNet <- x.evaluate$predictionNNet[,"ch_in"]

NNetOutput <- makeLiftPlot(x.evaluate$predictionNNet,x.evaluate,"Neural Network")

TimeAux <- proc.time() - ptm 
#NNetOutput$summary=varImp(x.modelNNet)
NNetOutput$TimeElapsed <- TimeAux[3]
NNetOutput$PercCorrect <- mean(x.evaluate$correctNNet)*100
NNetconfmatrix <- table(x.evaluate$predictionNNetclass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)

########## TREE
# fast model using parallel computation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
x.modelTree <- train(BaseFormula_dum, data=x.trainnorm, method='ctree') 


x.evaluate$predictionTree <- predict(x.modelTree, newdata = x.evaluatenorm, type = "prob")

x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionTreeClass[x.evaluate$predictionTree[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionTreeClass <- factor(x.evaluate$predictionTreeClass, levels=c("Noch_in","ch_in"))

x.evaluate$correctTree <- x.evaluate$predictionTreeClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctTree)))

x.evaluate$predictionTree <- x.evaluate$predictionTree[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelTree))
plot(varImp(x.modelTree))
grafico_importanza <- ggplot(varImp(x.modelTree), aes(x = reorder(rownames(importanza_variabili), -Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Variables", y = "Importance") +
  ggtitle("Importance of Variables") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# plot tree, if desired 
if(0){
  plot(x.modelTree$finalModel)
}

TreeOutput <- makeLiftPlot(x.evaluate$predictionTree,x.evaluate,"Tree")

TimeAux <- proc.time() - ptm 
#TreeOutput$summary <- varImp(x.modelTree)
TreeOutput$TimeElapsed <- TimeAux[3]
TreeOutput$PercCorrect <- mean(x.evaluate$correctTree)*100
Treeconfmatrix <- table(x.evaluate$predictionTreeClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)

############ Bagging
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# fast training using parallel computation
x.modelBagging  <- train(BaseFormula_dum, data=x.trainnorm, method="treebag",importance=T)

# Use the model to predict the evaluation.
x.evaluate$predictionBagging <- predict(x.modelBagging, newdata=x.evaluatenorm, type="prob")

x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBaggingClass[x.evaluate$predictionBagging[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBaggingClass <- factor(x.evaluate$predictionBaggingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBagging <- x.evaluate$predictionBaggingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBagging)))

# Extract the class probabilities.
x.evaluate$predictionBagging <- x.evaluate$predictionBagging[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBagging))

BaggingOutput <- makeLiftPlot(x.evaluate$predictionBagging,x.evaluate,"Bagging")

TimeAux <- proc.time() - ptm
#BaggingOutput$summary <- varImp(x.modelBagging)
BaggingOutput$TimeElapsed <- TimeAux[3]
BaggingOutput$PercCorrect <- mean(x.evaluate$correctBagging)*100
Baggingconfmatrix <- table(x.evaluate$predictionBaggingClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)



############ Boosting
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using boosting ensemble algorithms
# fast trainer using parallel computation
x.modelBoosting  <- train(BaseFormula_dum, data=x.trainnorm, method = 'blackboost')#,  method = 'bstTree')

# Use the model to predict the evaluation.
x.evaluate$predictionBoosting <- predict(x.modelBoosting, newdata=x.evaluatenorm,type="prob")

x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionBoostingClass[x.evaluate$predictionBoosting[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionBoostingClass <- factor(x.evaluate$predictionBoostingClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctBoosting <- x.evaluate$predictionBoostingClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctBoosting)))

# Extract the class probabilities.
x.evaluate$predictionBoosting <- x.evaluate$predictionBoosting[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelBoosting))

# Make a lift curve
BoostingOutput <- makeLiftPlot(x.evaluate$predictionBoosting,x.evaluate,"Boosting")

TimeAux <- proc.time() - ptm 
#BoostingOutput$summary <- varImp(x.modelBoosting)
BoostingOutput$TimeElapsed <- TimeAux[3]
BoostingOutput$PercCorrect <- mean(x.evaluate$correctBoosting)*100
Boostingconfmatrix <- table(x.evaluate$predictionBoostingClass,x.evaluate$ch_in_string)
rm(TimeAux)

stopCluster(cl)

############ RANDOM FOREST
cl <- makeCluster(detectCores())
registerDoParallel(cl)

ptm <- proc.time()
# Create a model using "random forest and bagging ensemble algorithms
# a fast trainer using parallel computation
x.modelRF <- train(BaseFormula_dum, data=x.trainnorm, method="parRF") 

# Use the model to predict the evaluation.
x.evaluate$predictionRF <- predict(x.modelRF, newdata=x.evaluatenorm, type = "prob")

x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]>probthres]="ch_in"
x.evaluate$predictionRFClass[x.evaluate$predictionRF[,"ch_in"]<=probthres]="Noch_in"

x.evaluate$predictionRFClass <- factor(x.evaluate$predictionRFClass, levels=c("Noch_in","ch_in"))


# Calculate the overall accuracy.
x.evaluate$correctRF <- x.evaluate$predictionRFClass == x.evaluate$ch_in_string
print(paste("% of predicted classifications correct", mean(x.evaluate$correctRF)))

# Extract the class probabilities.
x.evaluate$predictionRF <- x.evaluate$predictionRF[,"ch_in"]

# to see the importance of the variables
print(varImp(x.modelRF))

RFOutput <- makeLiftPlot(x.evaluate$predictionRF,x.evaluate,"Random Forest")

TimeAux <- proc.time() - ptm 
#RFOutput$summary <- varImp(x.modelRF)
RFOutput$TimeElapsed <- TimeAux[3]
RFOutput$PercCorrect <- mean(x.evaluate$correctRF)*100
RFconfmatrix <- table(x.evaluate$predictionRFClass,x.evaluate$ch_in_string)
rm(TimeAux)
stopCluster(cl)


# SOME Summarizing plots:

OverallTDL <- c(LogitOutput$TDL,NBOutput$TDL,KNNOutput$TDL,SVMOutput$TDL,TreeOutput$TDL,BaggingOutput$TDL,BoostingOutput$TDL,RFOutput$TDL,NNetOutput$TDL)
OverallGINI <- c(LogitOutput$GINI,NBOutput$GINI,KNNOutput$GINI,SVMOutput$GINI,TreeOutput$GINI,BaggingOutput$GINI,BoostingOutput$GINI,RFOutput$GINI,NNetOutput$GINI)

ForGraph <- data.frame(OverallTDL,OverallGINI)

myLeftAxisLabs <- pretty(seq(0, max(ForGraph$OverallTDL), length.out = 10))
myRightAxisLabs <- pretty(seq(0, max(ForGraph$OverallGINI), length.out = 10))

myLeftAxisAt <- myLeftAxisLabs/max(ForGraph$OverallTDL)
myRightAxisAt <- myRightAxisLabs/max(ForGraph$OverallGINI)

ForGraph$OverallTDL1 <- ForGraph$OverallTDL/max(ForGraph$OverallTDL)
ForGraph$OverallGINI1 <- ForGraph$OverallGINI/max(ForGraph$OverallGINI)

op <- par(mar = c(5,4,4,4) + 0.1)

barplot(t(as.matrix(ForGraph[, c("OverallTDL1", "OverallGINI1")])), beside = TRUE, yaxt = "n", names.arg = c("Logit","Naive Baies","KNN","SVM","Tree","Bagging","Boosting","Random Forest","Neural Network"), ylim=c(0, max(c(myLeftAxisAt, myRightAxisAt))), ylab =	"Top Decile Lift", main="Performance of the Machine Learning Algorithms")

axis(2, at = myLeftAxisAt, labels = myLeftAxisLabs)

axis(4, at = myRightAxisAt, labels = myRightAxisLabs)

mtext("GINI Coefficient", side = 4, line = 3, cex = par("cex.lab"))

mtext(c(paste(round(LogitOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NBOutput$TimeElapsed,digits=2),"sec"),
        paste(round(KNNOutput$TimeElapsed,digits=2),"sec"),
        paste(round(SVMOutput$TimeElapsed,digits=2),"sec"),
        paste(round(TreeOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BaggingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(BoostingOutput$TimeElapsed,digits=2),"sec"),
        paste(round(RFOutput$TimeElapsed,digits=2),"sec"),
        paste(round(NNetOutput$TimeElapsed,digits=2),"sec")), side = 1, line = 3, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))
mtext(c(paste(round(LogitOutput$PercCorrect,digits=0),"%"),
        paste(round(NBOutput$PercCorrect,digits=0),"%"),
        paste(round(KNNOutput$PercCorrect,digits=0),"%"),
        paste(round(SVMOutput$PercCorrect,digits=0),"%"),
        paste(round(TreeOutput$PercCorrect,digits=0),"%"),
        paste(round(BaggingOutput$PercCorrect,digits=0),"%"),
        paste(round(BoostingOutput$PercCorrect,digits=0),"%"),
        paste(round(RFOutput$PercCorrect,digits=0),"%"),
        paste(round(NNetOutput$PercCorrect,digits=0),"%")), side = 1, line = 4, cex = par("cex.lab"), at = c(2,5,8,11,14,17,20,23,26))

mtext("Calc. time", side = 1, line = 3, cex = par("cex.lab"), at = -.8)
mtext("% correct", side = 1, line = 4, cex = par("cex.lab"), at = -.8)


lift_obj=lift(ch_in_string~predictionBagging+predictionBoosting+predictionTree+predictionNNet+predictionSVM+predictionlogit,data=x.evaluate,class="ch_in")

ggplot(lift_obj)

#per vedere il tuning del modello
x.modelLogit$bestTune
x.modelNB$bestTune
x.modelKNN$bestTune
x.modelSVM$bestTune
x.modelTree$bestTune
x.modelBagging$bestTune
x.modelBoosting$bestTune
x.modelRF$bestTune
x.modelNNet$bestTune
#visualizacion de los modelos ml

print(NBOutput)
print(BaggingOutput)
summary(yelp_data)
print(NBOutput)
print(LogitOutput)
print(KNNOutput)
print(SVMOutput)
print(TreeOutput)
print(BoostingOutput)
print(RFOutput)
print(NNetOutput)