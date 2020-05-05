###############################################################
# Name: Danielle Senechal
# CSC-301
# Final Project
###############################################################

fluidPage(

  titlePanel("kNN Accuracies in Skin Lesion Prediction"), # title
  h4("Please be patient, it takes a while for kNN to run! :)", style = "color: green"), # header 4
  h6("Warning: It takes about 10 minutes to run the 28x28 images!", style = "color: red"), # header 6

  sidebarLayout(

    sidebarPanel(
     
      selectInput("datas", "Which data set?",
                  choices = c("hmnist_8_8_RGB.csv", "hmnist_28_28_RGB.csv"),
                  selected = "hmnist_8_8_RGB.csv"),
      
      # choose between 1, 3, and 5 neighbors
      selectInput("neighbor", "How many neighbors?",
                  c(1, 3, 5), selected = 3), # set initial to 3
      
      # choose between 90%, 80%, 70%, 60%, and 50% for the training dataset
      selectInput("splits", "What percent of data for training? (Multiply by 100)",
                  choices=c(0.90, 0.80, 0.70, 0.60, 0.50), selected = 0.60), # set initial to 60%
      
      hr(),
      
      p('Source: ', style = 'color:blue',
      helpText("Data from the Skin Cancer MNIST: HAM10000 dataset.
               The kNN function is ran using the RGB values of the 10,015 images from this dataset.
               You can choose to run the 8x8 images or the 28x28 images.", 
               style = 'display:inline') # reference the dataset
      )),
      
    mainPanel(
      plotOutput("accuracyPlot"), # overall accuracy plot
      verbatimTextOutput('accuracyMessage'), # overall accuracy output
      verbatimTextOutput('akiecMessage'), # AKIEC accuracy output
      verbatimTextOutput("bccMessage"), # BCC accuracy output
      verbatimTextOutput("bklMessage"), # BKL accuracy output
      verbatimTextOutput("dtfMessage"), # DTF accuracy output
      verbatimTextOutput("melMessage"), # MEL accuracy output
      verbatimTextOutput("nvMessage"), # NV accuracy output
      verbatimTextOutput("vascMessage") # VASC accuracy output
    )

  )
)

