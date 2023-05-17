EDA on R

# set working directory

rm(list = ls())  

setwd("C:/Users/moksh/OneDrive - The University of Texas at Dallas/UTD/SEM 2 Spring 2023/predictive analytics/Python")

# panel data in R

 #install.packages("plm")
    library(plm)
    library(AER)
    library(lmtest)
#install.packages("olsrr")
    library("olsrr")

# load data

    margarine1 <- read.csv("PROJECT UPDATEDt1.csv", header = TRUE, sep = ",")

    #is.data.frame(margarine)
   # dim(margarine)
    #str(margarine)
    #summary(margarine)
    margarine = margarine1
    
    # print column names
    names(margarine)

    View(head(margarine,100))

# select data with Tubs only
    margarine <- margarine[margarine$CAG_Form_Value == "Tubs",]
    # print unique values of CAG_Form_Value
    unique(margarine$CAG_Form_Value)
