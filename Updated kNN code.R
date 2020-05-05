########################################################
# Danielle Senechal
# kNN code
# This code runs the kNN method using all of the data
########################################################
library(ggplot2)
library(class) # knn

setwd("~/Documents/ECSU/Spring 2020/Web Dev/The Final")
thedata <- read.csv(paste0("hmnist_8_8_RGB.csv"))
head(thedata)
# View(thedata) #read and view data

###################################################################################
# AKIEC: Actinic keratoses and intraepithelial carcinoma/Bowen's disease
# BCC: Basal cell carcinoma
# BKL: Benign keratosis-like lesions
  # including: solar lentigines/seborrheic keratoses and lichen-planus like keratoses
# DTF: Dermatofibroma
# MEL: Melanoma 
# NV: Melanocytic nevi
# VASC: Vascular lesions
  # including: angiomas, angiokeratomas, pyogenic granulomas and hemorrhage

# Use knn.cv(data,classes, k), where 
#   data - the data (probes in COLUMNS and samples in ROWS)
#   classes - the known classes corresponding to the data
#   k - value of k for the 'k' nearest neighbors
###################################################################################


########################### Naming the categories #################################
head(thedata$label)
thedata.coded <- factor(thedata$label, levels = c(0, 1, 2, 3, 4, 5, 6), 
                        labels = c("AKIEC", "BCC", "BKL", "DTF", "NV", 
                                   "VASC", "MEL")) # corresponding names
head(thedata.coded)
tail(thedata.coded)

# Frequency graph
ggplot(thedata, aes(x=thedata$label)) + geom_bar(aes(fill = thedata.coded)) +
  labs(x = "Type of Skin Cancer", y = "Number of Images in Dataset", 
       fill = "") + 
  ggtitle("Amounts in each category")

########################### Spliting into seven categories ########################

AKIEC <- subset(thedata, thedata$label == 0)
nrow(AKIEC) 
# View(AKIEC)

BCC <- subset(thedata, thedata$label == 1)
nrow(BCC)
# View(BCC)

BKL <- subset(thedata, thedata$label == 2)
nrow(BKL)
# View(BKL)

DTF <- subset(thedata, thedata$label == 3)
nrow(DTF)
# View(DTF)

NV <- subset(thedata, thedata$label == 4)
nrow(NV) 
# View(NV)

VASC <- subset(thedata, thedata$label == 5)
nrow(VASC) 
# View(VASC)

MEL <- subset(thedata, thedata$label == 6)
nrow(MEL)
# View(MEL)

nrow(AKIEC) + nrow(BCC) + nrow(BKL) + nrow(DTF) + nrow(MEL) + nrow(NV) + nrow(VASC)
# should equate to 10015

########################### Spliting into training/testing ########################
# Each category into 60% training and 40% testing

################# AKIEC #########################

six.AKIEC <- sample(1:nrow(AKIEC), 0.6 * nrow(AKIEC)) 
six.AKIEC
length(six.AKIEC) # 196 all good in the neighborhood

# train
AKIEC.train <- AKIEC[six.AKIEC,] 
# head(AKIEC.train)
nrow(AKIEC.train) 
# View(AKIEC.train)

#test
AKIEC.test <- AKIEC[-six.AKIEC,] 
# head(AKIEC.test)
nrow(AKIEC.test)
# View(AKIEC.test) 

################# BCC #########################

six.BCC <- sample(1:nrow(BCC), 0.6 * nrow(BCC)) 
six.BCC
length(six.BCC)

# train
BCC.train <- BCC[six.BCC,] 
# head(BCC.train)
nrow(BCC.train)
# View(BCC.train)

# test
BCC.test <- BCC[-six.BCC,] 
#head(BCC.test) 
nrow(BCC.test)
# View(BCC.test)

################# BKL #########################

six.BKL <- sample(1:nrow(BKL), 0.6 * nrow(BKL)) 
six.BKL
length(six.BKL) 

#train
BKL.train <- BKL[six.BKL,] 
# head(BKL.train)
nrow(BKL.train) 
# View(BKL.train)

#test
BKL.test <- BKL[-six.BKL,] 
# head(BKL.test)
nrow(BKL.test) 
# View(BKL.test)

################# DTF #########################

six.DTF <- sample(1:nrow(DTF), 0.6 * nrow(DTF)) 
six.DTF
length(six.DTF)

#train
DTF.train <- DTF[six.DTF,] 
# head(DTF.train)
nrow(DTF.train)
# View(DTF.train)

#test
DTF.test <- DTF[-six.DTF,] 
# head(DTF.test) 
nrow(DTF.test)
# View(DTF.test)

################# MEL #########################

six.MEL <- sample(1:nrow(MEL), 0.6 * nrow(MEL)) 
six.MEL
length(six.MEL) 

# train
MEL.train <- MEL[six.MEL,] 
# head(MEL.train)
nrow(MEL.train) 
# View(MEL.train)

#test
MEL.test <- MEL[-six.MEL,] 
# head(MEL.test)
nrow(MEL.test)
# View(MEL.test)

################# NV #########################

six.NV <- sample(1:nrow(NV), 0.6 * nrow(NV)) 
six.NV
length(six.NV) 

# train
NV.train <- NV[six.NV,] 
# head(NV.train)
nrow(NV.train)
# View(NV.train)

#test
NV.test <- NV[-six.NV,] 
# head(NV.test) 
nrow(NV.test)
# View(NV.test)

################# VASC #########################

six.VASC <- sample(1:nrow(VASC), 0.6 * nrow(VASC)) 
six.VASC
length(six.VASC)

# train
VASC.train <- VASC[six.VASC,] 
# head(VASC.train)
nrow(VASC.train)
# View(VASC.train)

#test
VASC.test <- VASC[-six.VASC,] 
# head(VASC.test) 
nrow(VASC.test)
# View(VASC.test)

#################################### Recombining ##################################

training <- rbind(AKIEC.train, BCC.train, BKL.train, DTF.train, MEL.train, NV.train, VASC.train)
training <- data.frame(training)
nrow(training)
# View(training)
testing <- rbind(AKIEC.test, BCC.test, BKL.test, DTF.test, MEL.test, NV.test, VASC.test)
testing <- data.frame(testing)
nrow(testing)
# View(testing)
nrow(training) + nrow(testing) # should equate 10015

#################################### Testing ######################################
target.category <- training[ncol(training)] 
# View(target.category)
nrow(target.category)

test.category <- testing[ncol(testing)] 
# View(test.category)
nrow(test.category)

cl = target.category[,1]

predictions <- knn(training, testing, cl, k = 3) #run knn, can change the value of k

test.category.vec = test.category[,1]
the.table <- table(test.category.vec, predictions)
the.table 

cm <- as.matrix(table(test.category.vec, predictions))
colnames(cm) <- c("AKIEC", "BCC", "BKL", "DTF", "NV", "VASC", "MEL")
rownames(cm) <- c("AKIEC", "BCC", "BKL", "DTF", "NV", "VASC", "MEL")
cm # this is the confusion matrix

sum(diag(cm))/length(cl) * 100 # gives the accuracy of the predictions

acc <- diag(cm)
tots <- data.frame(acc)
tots$totals <- c(nrow(AKIEC.test), nrow(BCC.test), nrow(BKL.test), nrow(DTF.test), 
                 nrow(NV.test), nrow(VASC.test), nrow(MEL.test))
# View(tots)
indiv.accs <- tots$acc / tots$totals * 100
indiv.accs <- data.frame(indiv.accs)
indiv.accs$type <- c("AKIEC", "BCC", "BKL", "DTF", "NV", "VASC", "MEL")
# View(indiv.accs)

akiec.acc <- indiv.accs[1,]
akiec.acc <- round(akiec.acc$indiv.accs, digits=2) 
bcc.acc <- indiv.accs[2,]
bcc.acc <- round(bcc.acc$indiv.accs, digits=2) 
bkl.acc <- indiv.accs[3,]
bkl.acc <- round(bkl.acc$indiv.accs, digits=2) 
dtf.acc <- indiv.accs[4,]
dtf.acc <- round(dtf.acc$indiv.accs, digits=2) 
nv.acc <- indiv.accs[5,]
nv.acc <- round(nv.acc$indiv.accs, digits=2) 
vasc.acc <- indiv.accs[6,]
vasc.acc <- round(vasc.acc$indiv.accs, digits=2) 
mel.acc <- indiv.accs[7,]
mel.acc <- round(mel.acc$indiv.accs, digits=2) 


ggplot(indiv.accs, aes(x=type, y=indiv.accs, fill=type)) +
  geom_bar(stat='identity', position='dodge') + 
  ggtitle("Categorical Accuracies") +
  labs(x = "Type of lesion", y = "Accuracy (%)") + ylim(0,100) +
  theme(legend.position = "none")








