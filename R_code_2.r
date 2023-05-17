# Code 2 ts.csv

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
    margarine1 <- read.csv("ts.csv", header = TRUE, sep = ",")
    #is.data.frame(margarine)
   # dim(margarine)
    #str(margarine)
    #summary(margarine)

    margarine = margarine1

    View(head(margarine)

# convert Time variable to factor
    margarine$Time <- as.factor(margarine$Time)


# Convert Time variable to date
margarine$Time <- as.Date(margarine$Time, format = "Week Ending %m-%d-%y")
head(margarine$Time)
# Add new column for week number with year
margarine$Week <- strftime(margarine$Time, format = "%Y-W%V")
head(margarine$Week)
# convert week to factor
margarine$Week <- as.factor(margarine$Week)

 View(head(margarine))


# Initial data cleaning

    # remove OZ from CAG.Ounces.Value
    margarine$CAG.Ounces.Value <- gsub(" OZ", "", margarine$CAG.Ounces.Value)
    head(margarine$CAG.Ounces.Value)
    # print unique values of CAG.Ounces.Value
    unique(margarine$CAG.Ounces.Value)
    # factor CAG.Ounces.Value
    margarine$CAG.Ounces.Value <- as.factor(margarine$CAG.Ounces.Value)


    # remove CT from count value
    margarine$CAG.Count.Value <- gsub(" CT", "", margarine$CAG.Count.Value)
    head(margarine$CAG.Count.Value)
    # print unique values of CAG.Count.Value
    unique(margarine$CAG.Count.Value)
    # factor CAG.Count.Value
    margarine$CAG.Count.Value <- as.factor(margarine$CAG.Count.Value)

# factor Form, tier, major brand, manufacturer, sub category name, category name
    margarine$CAG.Form.Value <- as.factor(margarine$CAG.Form.Value)
    # print unique values of CAG.Form.Value
    unique(margarine$CAG.Form.Value)

    # factor CAG.Tier.Value
    margarine$CAG.Tier.Value <- as.factor(margarine$CAG.Tier.Value)
    # print unique values of CAG.Tier.Value
    unique(margarine$CAG.Tier.Value)
    # count rows with value TBD
    sum(margarine$CAG.Tier.Value == "TBD")
    # 477 rows have TBD as value for CAG.Tier.Value out of 343627 rows, so we can remove these rows
    margarine <- subset(margarine, margarine$CAG.Tier.Value != "TBD")

    # factor CAG.Major.Brand.Value
    margarine$CAG.Major.Brand.Value <- as.factor(margarine$CAG.Major.Brand.Value)
    # print unique values of CAG.Major.Brand.Value
    unique(margarine$CAG.Major.Brand.Value)

    # factor CAG.Manufacturer.Value
    margarine$CAG.Manufacturer.Value <- as.factor(margarine$CAG.Manufacturer.Value)
    # print unique values of CAG.Manufacturer.Value
    unique(margarine$CAG.Manufacturer.Value)

    # factor Sub.Category.Name
    margarine$Sub.Category.Name <- as.factor(margarine$Sub.Category.Name)
    # print unique values of CAG.Sub.Category.Name
    unique(margarine$Sub.Category.Name)
    # remove Sub.Category.Name from margarine
    margarine$Sub.Category.Name <- NULL
    

    # factor CAG.Category.Value
    margarine$CAG.Category.Value <- as.factor(margarine$CAG.Category.Value)
    # print unique values of CAG.Category.Value
    unique(margarine$CAG.Category.Value)
    # remove CAG.Category.Value from margarine
    margarine$CAG.Category.Value <- NULL

    #View(head(margarine))

    # convert Category, Form, Tier

    # print all weeks
    levels(margarine$Week)

    #View(head(margarine))
