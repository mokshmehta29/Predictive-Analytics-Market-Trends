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
    margarine1 <- read.csv("new_dt.csv", header = TRUE, sep = ",")
    #is.data.frame(margarine)
   # dim(margarine)
    #str(margarine)
    #summary(margarine)
    margarine = margarine1
    
    # print column names
    names(margarine)

# Convert Time variable to date
margarine$Time <- as.Date(margarine$Time, format = "Week Ending %m-%d-%y")
head(margarine$Time)
# Add new column for week number with year
margarine$Week <- strftime(margarine$Time, format = "%Y-W%V")
head(margarine$Week)
# convert week to factor
margarine$Week <- as.factor(margarine$Week)

# View(head(margarine,100))
# print unique Week values
unique(margarine$Week)

# drop 2020 Year data
margarine <- margarine[margarine$Year != "2020",]
# print unique Year values
unique(margarine$Year)

# count null values for each column
colSums(is.na(margarine))



# remove OZ from CAG.Ounces.Value
    margarine$CAG.Ounces.Value <- gsub(" OZ", "", margarine$CAG.Ounces.Value)
    #head(margarine$CAG.Ounces.Value)
    # print unique values of CAG.Ounces.Value
    unique(margarine$CAG.Ounces.Value)
    # factor CAG.Ounces.Value
    margarine$CAG.Ounces.Value <- as.factor(margarine$CAG.Ounces.Value)

# print column names
names(margarine)

# run a panel regression with market share as dependent variable
    
# estimate the fixed effects regression with plm()
    attach(margarine)
    panelreg1 <- plm( Market_Share ~ Week +
CAG.Ounces.Value_8.OZ+
CAG.Ounces.Value_13.OZ+
CAG.Ounces.Value_15.OZ+

CAG.Form.Value_ALL.OTHER.FORM+
CAG.Form.Value_SPRAY.SQUEEZE+
CAG.Form.Value_STICKS+
CAG.Form.Value_TUBS+
CAG.Major.Brand.Value_BLUE.BONNET+
CAG.Major.Brand.Value_COUNTRY.CROCK+

CAG.Major.Brand.Value_I.CANT.BELIEVE.ITS.NOT.BUTTER+
CAG.Major.Brand.Value_IMPERIAL, data = margarine, index = c("Week"), model = "within")

# print the results
    summary(panelreg1)
