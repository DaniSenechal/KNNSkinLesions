###############################################################
# Name: Danielle Senechal
# CSC-301
# Final Project
###############################################################

# Define a server for the Shiny app
function(input, output, session) {
  
  updateSelectInput(session, "datas",
                    label = paste("Which data set?"),
                    choices = c("hmnist_8_8_RGB.csv", "hmnist_28_28_RGB.csv"),
                    selected = "hmnist_8_8_RGB.csv")
  
  # choose between 1, 3, and 5 neighbors
  updateSelectInput(session, "neighbor",
                    label = paste("How many neighbors?"),
                    choices = c(1, 3, 5),
                    selected = 3 )
  
  # choose between 90%, 80%, 70%, 60%, and 50% for the training dataset
  updateSelectInput(session, "splits",
                    label = paste("What percent of data for training? (Multiply by 100)"),
                    choices=c(0.90, 0.80, 0.70, 0.60, 0.50), selected = 0.60 ) 
  
  observe({ 
    
    ######################## kNN function ########################
  
    setwd("~/Documents/ECSU/Spring 2020/Web Dev/The Final") # set working directory
    thedata <- read.csv(paste0(input$datas))
    
    ########## Naming the categories ##########
    head(thedata$label)
    thedata.coded <- factor(thedata$label, levels = c(0, 1, 2, 3, 4, 5, 6), 
                            labels = c("AKIEC", "BCC", "BKL", "DTF", "NV", 
                                       "VASC", "MEL")) # corresponding names
    
    ########## Spliting into seven categories ##########
    
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
    # should equate to 10,015
    
    
    ########## Spliting into training/testing ##########
  
    thesplit <- as.numeric(input$splits) # make input numeric
    
    ####### AKIEC #######
    per.AKIEC <- sample(1:nrow(AKIEC), thesplit * nrow(AKIEC)) 

    AKIEC.train <- AKIEC[per.AKIEC,] # training
    AKIEC.test <- AKIEC[-per.AKIEC,] #testing
  
    ####### BCC #######
    per.BCC <- sample(1:nrow(BCC), thesplit * nrow(BCC)) 
  
    BCC.train <- BCC[per.BCC,] # training
    BCC.test <- BCC[-per.BCC,] # testing
  
    ####### BKL #######
    per.BKL <- sample(1:nrow(BKL), thesplit * nrow(BKL)) 

    BKL.train <- BKL[per.BKL,] # training
    BKL.test <- BKL[-per.BKL,] # testing
  
    ####### DTF #######
    per.DTF <- sample(1:nrow(DTF), thesplit * nrow(DTF)) 
  
    DTF.train <- DTF[per.DTF,] # training
    DTF.test <- DTF[-per.DTF,] # testing
  
    ####### MEL #######
    per.MEL <- sample(1:nrow(MEL), thesplit * nrow(MEL)) 

    MEL.train <- MEL[per.MEL,] # training
    MEL.test <- MEL[-per.MEL,] # testing
  
    ####### NV #######
    per.NV <- sample(1:nrow(NV), thesplit * nrow(NV)) 

    NV.train <- NV[per.NV,] # training
    NV.test <- NV[-per.NV,] # testing
  
    ####### VASC #######
    per.VASC <- sample(1:nrow(VASC), thesplit * nrow(VASC)) 

    VASC.train <- VASC[per.VASC,] # training
    VASC.test <- VASC[-per.VASC,] # testing
  
    
    ########## Recombining ##########
  
    training <- rbind(AKIEC.train, BCC.train, BKL.train, DTF.train, MEL.train, NV.train, VASC.train)
    training <- data.frame(training) # data frame of all training data

    testing <- rbind(AKIEC.test, BCC.test, BKL.test, DTF.test, MEL.test, NV.test, VASC.test)
    testing <- data.frame(testing) # data frame of all testing data
  
    
    ########## Perform kNN function ##########
    
    target.category <- training[ncol(training)] # select last column which has target categories
    test.category <- testing[ncol(testing)] # select last column which has target categories
  
    cl = target.category[,1] # create classifiction variable
  
    predictions <- knn(training, testing, cl, k = input$neighbor) # kNN, k is chosen by user in ui
  
    test.category.vec = test.category[,1] # create vector
  
    # identify correct predictions in a confusion matrix
    cm <- as.matrix(table(test.category.vec, predictions))
    colnames(cm) <- c("AKIEC", "BCC", "BKL", "DTF", "NV", "VASC", "MEL") # give column names
    rownames(cm) <- c("AKIEC", "BCC", "BKL", "DTF", "NV", "VASC", "MEL") # give row names
  
    total.accuracy <- sum(diag(cm))/length(cl) * 100 # gives the accuracy of all predictions
  
    acc <- diag(cm) # select diagonal results, they are the predictions that are correct
    tots <- data_frame(acc) # create dataframe
    
    # add column of overall totals in each testing category
    tots$totals <- c(nrow(AKIEC.test), nrow(BCC.test), nrow(BKL.test), nrow(DTF.test), 
                     nrow(NV.test), nrow(VASC.test), nrow(MEL.test)) 
    
    # divide number correct by overall in each category, multiply by 100 to get percentage
    indiv.accs <- tots$acc / tots$totals * 100 
    indiv.accs <- data_frame(indiv.accs) # create dataframe
    
    # add column to specify type of lesion
    indiv.accs$type <- c("AKIEC", "BCC", "BKL", "DTF", "NV", "VASC", "MEL")
  
    akiec.acc <- indiv.accs[1,] # select AKIEC row
    bcc.acc <- indiv.accs[2,] # select BCC row
    bkl.acc <- indiv.accs[3,] # select BKL row
    dtf.acc <- indiv.accs[4,] # select DTF row
    nv.acc <- indiv.accs[5,] # select NV row
    vasc.acc <- indiv.accs[6,] # select VASC row
    mel.acc <- indiv.accs[7,] # select MEL row
  
    ######################## end kNN function ########################
  
  # output bar graph that updates with given input (set y axis to prevent confusion)
  output$accuracyPlot <- renderPlot({
    
    ggplot(indiv.accs, aes(x=type, y=indiv.accs, fill=type)) +
      geom_bar(stat='identity', position='dodge') + 
      ggtitle(paste0("Categorical Accuracies using ", as.numeric(input$splits) * 100,
                     "% of the data for training and k = ", input$neighbor, " (", input$datas, ")")) +
      labs(x = "Type of lesion", y = "Accuracy (%)") + ylim(0, 100) +
      theme(legend.position = "none")   })
  
  # output overall accuracy
  output$accuracyMessage <- renderText({
    paste('Overall accuracy is:', total.accuracy, "%")   })
  
  # output accuracy for AKIEC
  output$akiecMessage <- renderText({
    paste('Total accuracy for Actinic keratoses (AKIEC) is:', 
          round(akiec.acc$indiv.accs, digits=2), "%")   })
  
  # output accuracy for BCC
  output$bccMessage <- renderText({
    paste('Total accuracy for Basal cell carcinoma (BCC) is:', 
          round(bcc.acc$indiv.accs, digits=2), "%")   })
  
  # output accuracy for BKL
  output$bklMessage <- renderText({
    paste('Total accuracy for Benign keratosis-like lesions (BKL) is:', 
          round(bkl.acc$indiv.accs, digits=2), "%")   })
  
  # output accuracy for DTF
  output$dtfMessage <- renderText({
    paste('Total accuracy for Dermatofibroma (DTF) is:', 
          round(dtf.acc$indiv.accs, digits=2), "%")   })
  
  # output accuracy for MEL
  output$melMessage <- renderText({
    paste('Total accuracy for Melanoma (MEL) is:', 
          round(mel.acc$indiv.accs, digits=2), "%")   })
  
  # output accuracy for NV
  output$nvMessage <- renderText({
    paste('Total accuracy for Melanocytic nevi (NV) is:', 
          round(nv.acc$indiv.accs, digits=2), "%")   })
  
  # output accuracy for VASC
  output$vascMessage <- renderText({
    paste('Total accuracy for Vascular lesions (VASC) is:', 
          round(vasc.acc$indiv.accs, digits=2), "%")   })
  
  })
  
}