# KNNSkinLesions
Diagnostic typing of skin lesion images using k-nearest neighbor. The ui, server, and global files display the results in a shiny application. 
Data is from: https://www.kaggle.com/kmader/skin-cancer-mnist-ham10000.
The objective of this research is to evaluate the performance of the k-nearest neighbor (kNN) algorithm 
on skin lesion typing.  
  
The poster created for this project can be viewed [here](https://www.easternct.edu/create/create-2020/_documents/poster_senechal_gd_garrett-dancik.pdf).

## Technologies
Project was created with:
* RStudio: R version 3.6.1 (2019-07-05).

## Installation
Install [RStudio](https://rstudio.com/products/rstudio/download/) to run the code.
Download the following packages by typing these commands in the terminal. These packages are loaded in the available code.
```bash
install.packages("ggplot2")
```
```bash
install.packages("class")
```
Download the data and read it in R by using the command:
```bash
proj1 <- read.csv("your directory")
```
where "your directory" is the pathname of the data file.

## Status
This project is finished, but I am hoping to explore other classification models on this data set.

## Inspiration
This project was done for my Senior Research project at ECSU in 2019. 

## Authors
Danielle Senechal
