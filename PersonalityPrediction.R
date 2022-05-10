#install needed packages
install.packages("Matrix")
install.packages("irlba")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("reshape2")

#matrix library required for construction of sparse matrix
require(Matrix)

#load data set files into table/dataframe
usersTable <- read.csv(file = "users.csv")
likesTable <- read.csv(file = "likes.csv")
ulTable <- read.csv(file = "users-likes.csv")

#dropping political column from users table due to many null data points and renaming column
usersTable$political <- NULL
names(likesTable)[2] <- 'text'

#verifying correct table formats
head(usersTable, n=5)
head(likesTable, n=5)
head(ulTable, n=5)

#joining with reference to row number in likes and users table
ulTable$user_index<-match(ulTable$userid,usersTable$userid)
ulTable$like_index<-match(ulTable$likeid,likesTable$likeid)
head(ulTable, n=5)

#creating sparse matrix between user and like
ulMatrix <- sparseMatrix(i = ulTable$user_index, j = ulTable$like_index, x = 1)

#change row and col names from row index to userid and text
rownames(ulMatrix) <- usersTable$userid
colnames(ulMatrix) <- likesTable$text

# Remove the values that are under threshold
repeat {                                       
  i <- sum(dim(ulMatrix))                             
  ulMatrix <- ulMatrix[rowSums(ulMatrix) >= 50, colSums(ulMatrix) >= 150] 
  if (sum(dim(ulMatrix)) == i) break                  
}

usersTable <- usersTable[match(rownames(ulMatrix), usersTable$userid), ]

library(irlba)

# Get 5 SVD dimensions for the user, like matrix
svdUL <- irlba(ulMatrix, nv = 5)

# Get SVD scores
u <- svdUL$u
v <- svdUL$v

# Get the rotated V matrix:
rotVmatrix <- unclass(varimax(svdUL$v)$loadings)

# The cross-product of user like Matrix and rotated V Matrix
rotUmatrix <- as.matrix(ulMatrix %*% rotVmatrix)

# Correlate user traits and their SVD scores
cor(rotUmatrix, usersTable[,-1], use = "pairwise")

#libraries to be loaded
library(corrplot)
library(ggplot2)
library(reshape2)

# SVD
top <- list()
bottom <-list()
for (i in 1:5) {
  f <- order(rotVmatrix[ ,i])
  temp <- tail(f, n = 10)
  top[[i]]<-colnames(ulMatrix)[temp]  
  temp <- head(f, n = 10)
  bottom[[i]]<-colnames(ulMatrix)[temp]  
}

# calculating correlations
correlation<-round(cor(rotUmatrix, usersTable[,-1], use="p"),2)

# Using ggplot2 to redesigning it to make it uncomplicated
Melt<-melt(correlation)
colnames(Melt)<-c("SVD", "Trait", "r")

# Generating the heatmap for SVD dimensions and understanding personality traits 
qplot(p=SVD, q=Trait, data=q, fill=r, geom="tile") +
  scale_fill_gradient2(limits=range(p), breaks=c(min(p), 0, max(p)))+
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14,face="bold"),
        panel.background = element_rect(fill='white', colour='white'))+
  labs(p=expression('SVD'[rot]), q=NULL)

# Performing k-fold cross validations to divide users in 10 groups
splits <- sample(1:10, size = nrow(usersTable), replace = T)



#################################################### Start Prediction ####################################################
# Start predictions
set.seed(seed=68)
number_of_folds<-10                         # define the number of folds needed
attributes<-colnames(users)[5:ncol(users)]  # choose prediction attributes from the data
k_values<-c(2:10,15,20,30,40,50)

# Preset an empty list to hold the results
predicted_values <- list()

folds <- sample(1:number_of_folds, size = nrow(users), replace = T)

results<-list()
g_data <- list()

# Checking the prediction values for different values of k and folds
for (k in k_values){
  for (fold in 1:number_of_folds){ 
    print(paste("Cross-validated predictions for the particular fold number:", fold))
    test <- folds == fold
    
    #Calculate the SVD dimensions
    Msvd <- irlba(ulMatrix[!test, ], nv = k)
    v_rot <- unclass(varimax(Msvd$v[, 1:k])$loadings)
    predictors <- as.data.frame(as.matrix(ulMatrix %*% v_rot))
    
    for (attribute in attributes){
      results[[attribute]]<-rep(NA, n = nrow(users))
      model_fit<-glm(users[,attribute]~., data = predictors, subset = !test)
      results[[attribute]][test] <- predict(model_fit, predictors[test, ])
      print(paste("k-val",k," Variable", attribute, "done."))
    }
  }
  
  # Function to calculate the accuracies from the predicted values
  compute_accuracy <- function(ground_truth, predicted){
    return(cor(ground_truth, predicted,use = "pairwise"))
  }
  
  accuracies<-list()
  for (attribute in attributes){
    accuracies[[attribute]]<-compute_accuracy(users[,attribute], results[[attribute]])
    if (attribute == 'ope'){
      predicted_values[[as.character(k)]] <- accuracies[[attribute]]
    }
  }
  print(accuracies)
  
}

#Plot the graph for different k and the predicted outcome values
library(ggplot2)
library(reshape2)
data<-data.frame(k=k_values, r=as.numeric(predicted_values))

ggplot(data=data, aes(x=k, y=r, group=1)) +
  theme_light() +
  stat_smooth(colour="orange", linetype="solid", size=1,se=F) +
  geom_point(colour="blue", size=2, shape=21, fill="white") +
  scale_y_continuous(breaks = seq(0, .5, by = 0.05))


############################################################### Calculate Leadership Abilities ####################################
install.packages("lsa")
#Manually assign a fold
test <- folds == 1
library(irlba)

# Get the best k value from the plot
svdUL <- irlba(ulMatrix[!test, ], nv = 50)

# Rotate Like SVD scores (V)
v_rot <- unclass(varimax(svdUL$v)$loadings)

# Rotate user SVD scores *for the entire sample*
u_rot <- as.data.frame(as.matrix(ulMatrix %*% v_rot))

# Build linear regression model for 5 personality traits
fit_ope <- glm(users$ope~., data = u_rot, subset = !test)
fit_con <- glm(users$con~., data = u_rot, subset = !test)
fit_ext <- glm(users$ext~., data = u_rot, subset = !test)
fit_agr <- glm(users$agr~., data = u_rot, subset = !test)
fit_neu <- glm(users$neu~., data = u_rot, subset = !test)

# Predict the personality traits
pred_ope <- predict(fit_ope, u_rot[test, ])
pred_con <- predict(fit_con, u_rot[test, ])
pred_ext <- predict(fit_ext, u_rot[test, ])
pred_agr <- predict(fit_agr, u_rot[test, ])
pred_neu <- predict(fit_neu, u_rot[test, ])

matched_users <- users[match(rownames(as.matrix(pred_o)),users$userid), ]
matched_users

#Combine the predicted values
data_new <- cbind(pred_ope, pred_con, pred_ext, pred_agr, pred_neu) 

data_new
#Define leadership traits
leader <- c(0.90, 0.90, 0.6, 0.5, -0.5)

res = list()

#Iterate over users to predict the leadership traits
library(lsa)

for (row in 1:nrow(data_new)) {
  traits <- list()
  for(col in 1:ncol(data_new)) {
    traits <- c(traits, data_new[row, col])
  }
  
  # Using cosine distance to calcualte leadership qualities
  traits <- as.numeric(traits)
  res[[matched_users[row,1]]] <- cosine(leader, traits)
  print("End")
}

res

############################################### Calculating the emotions #################################################
install.packages("syuzhet", repos = "http://cran.us.r-project.org")
library(syuzhet)
library(data.table)
library(plyr)

# Load files
likesTable<-read.csv("likes.csv")
ulTable<-read.csv("users-likes.csv")

like_sentiments = data.frame(matrix(ncol=11,nrow=0, dimnames=list(NULL, c("likeid", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive"))))

# for (row in 1:nrow(likesTable)){
for (row in 99000:100000){
  print(row)
  name <- likesTable[row, "name"]
  words <- unique(strsplit(name, split = " "))
  char_v <- unlist(words)
  nrc <- get_nrc_sentiment(char_v, cl = NULL, language = "english", lowercase = TRUE)
  sentiments <- colSums(nrc)
  x <- data.frame(sentiments)
  t <- transpose(x)
  colnames(t) <- rownames(x)
  temp <- cbind(likeid=likesTable[row, "likeid"], t)
  like_sentiments <- rbind(like_sentiments, temp)
}

like_sentiments

trimmed_ul_table <- ulTable[ulTable$likeid %in% like_sentiments$likeid,]
user_sentiments = data.frame(matrix(ncol=11,nrow=0, dimnames=list(NULL, c("userid", "anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust", "negative", "positive"))))

head(trimmed_ul_table)
# l = likesTable[likesTable$likeid == "220f40e9d4e1bd4ec84d90fe7c13319c",]
# 
# l

for (row in 1:nrow(trimmed_ul_table)){
  # for (row in 1:100000){
  print(row)
  likeid = trimmed_ul_table[row, "likeid"]
  userid = trimmed_ul_table[row, "userid"]
  
  like_sentiment = like_sentiments[like_sentiments$likeid == likeid,]
  like_sentiment
  if (dim(like_sentiment)[1] != 0){
    user_sentiment = user_sentiments[user_sentiments$userid == userid,]
    
    temp_list = list(userid=userid)
    temp <- append(temp_list, like_sentiment[,-1])
    
    if (dim(user_sentiment)[1] == 0) {
      user_sentiments <- rbind(user_sentiments, temp)
    }
    else {
      for(col in colnames(like_sentiment[,-1])){
        user_sentiment[[col]] <- like_sentiment[[col]] + user_sentiment[[col]]
      }
      user_sentiments[user_sentiments$userid == userid,] = user_sentiment
    }
  }
}

user_emotions = data.frame(matrix(ncol=2,nrow=0, dimnames=list(NULL, c("userid", "sentiments"))))
for (row in 1:nrow(user_sentiments)){
  # for (row in 99000:100000){
  user_sentiment = user_sentiments[row,]
  max <- do.call(`pmax`, user_sentiment[,-1])
  sentiments <- (apply(user_sentiment, 1, function(x) colnames(user_sentiment)[which(x == max)]))
  user_emotions[nrow(user_emotions) + 1,] = c(user_sentiments[row, "userid"], paste(unlist(sentiments), collapse=' '))
}

print(user_emotions)

