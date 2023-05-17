

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
    

    # select only rows with BLUE.BONNNET = 1 or COUNTRY.CROCK = 1
    margarine <- subset(margarine, BLUE.BONNET == 1 | PARKAY == 1 | EARTH.BALANCE == 1 | SMART.BALANCE == 1 )
    #View(head(margarine,100))
    nrow(margarine)

    # check data types for each column
    #sapply(margarine, class)

    # convert all numeric columns or integer columns to double type
    #margarine[, sapply(margarine, is.numeric)] <- lapply(margarine[, sapply(margarine, is.numeric)], as.double)

    # only select the important columns
    margarine <- margarine[, c("Time", "CAG_Major_Brand_Value", "Market_Share","Price_Dec_Merch", "Avg_Price_Per_Unit", "ACV_No_Merch","ACV_Any_Merch","Incremental_Volume")]  
    View(head(margarine,100))

    # create a stats summary for ACV_No_Merch
    summary(margarine$ACV_No_Merch)
    nrow(margarine)
    summary(margarine$ACV_Any_Merch)

    # create a distribution plot for ACV_No_Merch
    #install.packages("ggplot2")    
    #library(ggplot2)
    #ggplot(margarine, aes(x = ACV_No_Merch)) + geom_histogram(binwidth = 0.5, color = "black", fill = "white") + labs(title = "Distribution of ACV_No_Merch") + theme(plot.title = element_text(hjust = 0.5))
    
    # impute NA values with mean in ACV_No_Merch and ACV_Any_Merch
    margarine$ACV_No_Merch[is.na(margarine$ACV_No_Merch)] <- mean(margarine$ACV_No_Merch, na.rm = TRUE)
    margarine$ACV_Any_Merch[is.na(margarine$ACV_Any_Merch)] <- mean(margarine$ACV_Any_Merch, na.rm = TRUE)

    # convert NA in Incremental_Volume to 0
    margarine$Incremental_Volume[is.na(margarine$Incremental_Volume)] <- 0


    View(head(margarine,100))

    # group by CAG_Major_Brand_Value and average the numeric columns except market share which is summed
   
    #install.packages("dplyr")
    library(dplyr)
    grp_margarine <- margarine %>%
    group_by(Time,CAG_Major_Brand_Value) %>%
    summarise(
    Price_Dec_Merch = mean(Price_Dec_Merch),
    Avg_Price_Per_Unit = mean(Avg_Price_Per_Unit),
    ACV_No_Merch = mean(ACV_No_Merch),
    ACV_Any_Merch = mean(ACV_Any_Merch),
    Incremental_Volume = mean(Incremental_Volume),
    Market_Share = sum(Market_Share))

    View(grp_margarine)

    # print number of records
    nrow(grp_margarine)



# save as csv
#write.csv(grp_margarine, file = "proj_upd_t6_acv_vol.csv", row.names = FALSE)



# only select years that are not 2020

grp_margarine$Time <- as.Date(grp_margarine$Time, format = "Week Ending %m-%d-%y")
head(margarine$Time)

# select years 18 and 19 from Time column
#grp_margarine <- subset(grp_margarine, Time >= "2018-01-01" & Time <= "2020-01-10")

# create a new year column 
grp_margarine$Year <- format(grp_margarine$Time, "%Y")

# select only rows with year = 2018 or year = 2019 or year = 2021 or year = 2022
grp_margarine <- subset(grp_margarine, Year == "2018" | Year == "2019" | Year == "2021" | Year == "2022")

# print number of records
nrow(grp_margarine)

# save as csv
write.csv(grp_margarine, file = "proj_upd_t7_acv_vol_exclude 2020.csv", row.names = FALSE)

# select years - 21 and 22
grp_margarine <- subset(grp_margarine, Year == "2021" | Year == "2022")

# write as csv
write.csv(grp_margarine, file = "proj_upd_t8_acv_vol_21_22.csv", row.names = FALSE)



)