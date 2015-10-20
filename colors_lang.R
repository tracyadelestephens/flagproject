##
#Using colors of a flag to predict language spoken
#Languages:
#1=English, 2=Spanish, 3=French, 4=German, 5=Slavic, 
#6=Other Indo-European, 7=Chinese, 8=Arabic, 
#9=Japanese/Turkish/Finnish/Magyar, 10=Others

library(caret)
library(pROC)

data <- flagcols
data[ ,ncol(data) + 1] <- cc[,which(names(cc)=="lang", arr.ind = TRUE)]
names(data)[ncol(data)] <- "lang"

#adjust language variable
#1=English, 2=Spanish, 3=French, 4=German, 5=Slavic,
#6=Chinese, 7=Arabic, 8=Other
data$newlang <- "NA"
newlangs <- c(1:5, 8, 6, 7, 8, 8)
for (i in 1:10) {
  data$newlang[which(data$lang == i)] <- newlangs[i]
}
data$lang <- data$newlang
data <- data[ ,-ncol(data)]
#80 countries have "other", remove these from the data set 
withother <- data
data <- data[-which(data$lang == 8), ]
data$lang <- as.factor(data$lang)

#train & test
set.seed(123)
samplesize <- .75 * nrow(data)
train_index <- sample(seq_len(nrow(data)), size = samplesize)
tr <- data[train_index, ]
te <- data[-train_index, ]

#dealing with class imbalance (upsampling)
dataUp <- upSample(data[,-ncol(data)], data[ ,ncol(data)], list = FALSE, yname = "lang")
samplesizeUp <- .75 * nrow(dataUp)
train_indexUp <- sample(seq_len(nrow(dataUp)), size = samplesizeUp)
trUp <- dataUp[train_indexUp, ]
teUp <- dataUp[-train_indexUp, ]

#function to create responses for binary models
isthing <- function(vec, val) {
  newvec <- vector("logical", length(vec))
  newvec[which(vec == val)] <- TRUE
  return(as.numeric(newvec))
}

#logistic regression
tr <- apply(tr, 2, as.numeric)
te <- apply(te, 2, as.numeric)
glmdata <- data.frame(tr[,-which(colnames(tr)=="lang")])
newglmdata <- data.frame(te[,-which(colnames(te)=="lang")])
rocauc <- vector(mode = "list", length = 7)
glmpreds <- data.frame(matrix(NA, nrow = nrow(te), ncol = 7))
for (i in 1:7) {
  glmdata$class <- isthing(tr[ ,which(colnames(tr)=="lang")], i)
  newglmdata$class <- isthing(te[ ,which(colnames(te)=="lang")], i)
  logreg <- glm(glmdata$class ~ ., data = glmdata, model = TRUE)
  logregpreds <- predict.glm(logreg, newglmdata, type = "response")
  if(max(isthing(te[ ,which(colnames(te)=="lang")], i)) > 0) {
    rocauc[i] <- roc(isthing(te[ ,which(colnames(te)=="lang")], i), logregpreds)$auc
  }
  glmpreds[,i] <- logregpreds
}
glmpreds$lang <- apply(glmpreds, 1, which.max)
score <- length(which(glmpreds[, 8] == te[ ,10])==TRUE)/length((glmpreds[, 8] == te[ ,10])==TRUE)
trUp <- apply(trUp, 2, as.numeric)
glmdataUp <- data.frame(trUp[,-which(colnames(trUp)=="lang")])
rocaucUp <- vector(mode = "list", length = 7)
glmpredsUp <- data.frame(matrix(NA, nrow = nrow(te), ncol = 7))
for (i in 1:7) {
  glmdataUp$class <- isthing(trUp[ ,which(colnames(trUp)=="lang")], i)
  newglmdata$class <- isthing(te[ ,which(colnames(te)=="lang")], i)
  logreg <- glm(glmdataUp$class ~ ., data = glmdataUp, model = TRUE)
  logregpreds <- predict.glm(logreg, newglmdata, type = "response")
  if(max(isthing(te[ ,which(colnames(te)=="lang")], i)) > 0) {
    rocaucUp[i] <- roc(isthing(te[ ,which(colnames(te)=="lang")], i), logregpreds)$auc
  }
  glmpredsUp[,i] <- logregpreds
}
glmpredsUp$lang <- apply(glmpredsUp, 1, which.max)
scoreUp <- length(which(glmpredsUp[, 8] == te[ ,10])==TRUE)/length((glmpredsUp[, 8] == te[ ,10])==TRUE)

#find common combinations of colors




#remove the data variable
rm(data)
rm(tr)
rm(te)
