library("dplyr")
library(data.table)
library(zoo)

# df = read.csv("input.csv", fileEncoding = "big5") # including missing value
df = read.csv("hw1_case1.csv", fileEncoding = "big5") # given data
# df = read.csv("hw1input.csv", fileEncoding = "big5") # given data
# df = read.csv("output.csv", fileEncoding = "big5") # given data
data <- df %>%
  filter(df[,3] == "O3" |
         df[,3] == "PM2.5" |
         df[,3] == "PM10" |
         df[,3] == "CO" |
         df[,3] == "SO2" |
         df[,3] == "NO2")


DateUnique = unique(data[,1])
Hour = as.matrix(rep(c(0:23), times = length(DateUnique))) # fulfilled by Hours
Date = as.matrix(rep(DateUnique, each = 24)) # fulfilled by Date

# create an empty matrix
colName = c("Date", "Hour", "AQI", "O3(8hour)", "O3(1hour)", "PM2.5", "PM10", "CO", "SO2", "NO2")
originalData = matrix(NA, nrow = length(DateUnique)*24, ncol = length(colName))
colnames(originalData) = colName
originalData[,2] = Hour[,1]
originalData[,1] = Date[,1]
originalData <- as.data.frame(originalData)

# reverse original matrix
tmp <- t(data)
colnames(tmp) = tmp[3,]
tmp <- tmp[-c(1:3),]
rownames(tmp) = c(0:23)

IAQI <- matrix(NA, nrow = 7, ncol = 2)
IAQI <- cbind(c(0,51,101, 151, 201, 301, 401),c(50, 100, 150, 200, 300, 400, 500))
colnames(IAQI) <- c("IAQI_L0", "IAQI_Hi")

O3_8hr <- matrix(NA, nrow = 7, ncol = 2)
O3_8hr <- cbind(c(0.000,0.055,0.071, 0.086, 0.106, 0.405, 0.505),c(0.054, 0.070, 0.085, 0.105, 0.200, 0.504, 0.604 ))
colnames(O3_8hr) <- c("BP_L0", "BP_Hi")

O3 <- matrix(NA, nrow = 7, ncol = 2)
O3 <- cbind(c(NA, NA, 0.125, 0.165, 0.205, 0.405, 0.505),c(NA, NA, 0.164, 0.204, 0.404, 0.504, 0.604))
colnames(O3) <- c("BP_L0", "BP_Hi")

PM2.5 <- matrix(NA, nrow = 7, ncol = 2)
PM2.5 <- cbind(c(0.0, 15.5, 35.5, 54.5, 150.5, 250.5, 350.5),c(15.4, 35.4, 54.4, 150.4, 250.4, 350.4, 500.4))
colnames(PM2.5) <- c("BP_L0", "BP_Hi")

PM10 <- matrix(NA, nrow = 7, ncol = 2)
PM10 <- cbind(c(0, 55, 126, 255, 355, 425, 505),c(54, 125, 254, 354, 424, 504, 604))
colnames(PM10) <- c("BP_L0", "BP_Hi")

CO <- matrix(NA, nrow = 7, ncol = 2)
CO <- cbind(c(0, 4.5, 9.5, 12.5, 15.5, 30.5, 40.5),c(4.4, 9.4, 12.4, 15.4, 30.4, 40.4, 50.4))
colnames(CO) <- c("BP_L0", "BP_Hi")

SO2 <- matrix(NA, nrow = 7, ncol = 2)
SO2 <- cbind(c(0, 36, 76, 186, 305, 605, 805),c(35, 75, 185, 304, 604, 804, 1004))
colnames(SO2) <- c("BP_L0", "BP_Hi")

NO2 <- matrix(NA, nrow = 7, ncol = 2)
NO2 <- cbind(c(0, 54, 101, 361, 650, 1250, 1650),c(53, 100, 360, 649, 1249, 1649, 2049))
colnames(NO2) <- c("BP_L0", "BP_Hi")


# split matrix by cols(Date)
start <- seq(1, by = 6, length = ncol(tmp)/6)
sdf <- lapply(start, function(i, tmp) as.data.frame(tmp[,i:(i+5)]), tmp = tmp)
sdf <- rbindlist(sdf)

# assigned value into empty dataframe & use as.numeric(as.character()) to remove abnormal value
originalData$CO <- as.numeric(as.character(sdf$CO))
originalData$PM2.5 <- as.numeric(as.character(sdf$PM2.5))
originalData$PM10 <- as.numeric(as.character(sdf$PM10))
originalData$SO2 <- as.numeric(as.character(sdf$SO2))
originalData$NO2 <- as.numeric(as.character(sdf$NO2))
originalData$`O3(1hour)` <- as.numeric(as.character(sdf$O3))/1000 # ppb to ppm
# originalData$`O3(1hour)` <- as.factor(originalData$`O3(1hour)`)

# for ( i in which(originalData$SO2 < 0)){ # remove out of range value
#   originalData[i,9] <- NA
# }
for ( i in 1:nrow(originalData)){ # remove out of range value
  if(!is.na(originalData[i,5])){
    if(!is.na(originalData[i,5]) & originalData[i,5] < 0 | originalData[i,5] > O3[7,2]){
      originalData[i,5] <- NA
    }
  }
  if(!is.na(originalData[i,6])){
    if(originalData[i,6] < 0 | originalData[i,6] > PM2.5[7,2]){
      originalData[i,6] <- NA
    }
  }
  if(!is.na(originalData[i,7])){
    if(originalData[i,7] < 0 | originalData[i,7] > PM10[7,2]){
      originalData[i,7] <- NA
    }
  }
  if(!is.na(originalData[i,8])){
    if(originalData[i,8] < 0 | originalData[i,8] > CO[7,2]){
      originalData[i,8] <- NA
    }
  }
  if(!is.na(originalData[i,9])){
    if(originalData[i,9] < 0 | originalData[i,9] > SO2[7,2]){
      originalData[i,9] <- NA
    }
  }
  if(!is.na(originalData[i,10])){
    if(originalData[i,10] < 0 | originalData[i,10] > NO2[7,2]){
      originalData[i,10] <- NA
    } 
  }
}
################################
##        Interpolation       ##
################################
# Y = Ya + (Yb-Ya)*((X-Xa)/(Xb-Xa))

z <- zoo(originalData)
z$`O3(1hour)`<-na.approx(z$`O3(1hour)`)
z$PM2.5<-na.approx(z$PM2.5)
z$PM10<-na.approx(z$PM10)
z$CO<-na.approx(z$CO)
z$SO2<-na.approx(z$SO2)
z$NO2<-na.approx(z$NO2)

# measure O3(8Hour)
newData <- as.matrix(z)
newData[,c(4, 6, 7, 8)] <- NA
for (j in 1:nrow(newData)){
  if(j+7 < nrow(newData)){  # get the average of O3 per 8 hrs 
    newData[8+j, 4] <- mean(as.numeric(as.character(z[j:(j+7), 5])))
  }
  
  if(j+23 < nrow(newData)){ # get the average of PM2.5 per 24 hrs 
    newData[24+j, 6] <- mean(as.numeric(as.character(z[j:(j+23), 6])))
  }
  
  if(j+23 < nrow(newData)){ # get the average of PM10 per 24 hrs 
    newData[24+j, 7] <- mean(as.numeric(as.character(z[j:(j+23), 7])))
  }
  
  if(j+7 < nrow(newData)){ # get the average of CO per 8 hrs 
    newData[8+j, 8] <- mean(as.numeric(as.character(z[j:(j+7), 8])))
  }
}
newData <- newData[-c(1:24),]

################################
##       Calculate AQI        ##
################################

# measure AQI and IAQIp

output <- newData # copy the form of newData(start from 2nd)
output[,4:10] <- NA
for (i in 1:nrow(output)){

  n2 = which(O3[,2] >= round(as.numeric(newData[i,5]),3) & O3[,1] <= round(as.numeric(newData[i,5]),3))
  IAQI1_2 <- ((IAQI[n2,2]-IAQI[n2,1])/(O3[n2,2]-O3[n2,1])*(as.numeric(newData[i,5])-O3[n2,1])+IAQI[n2,1]) # O3(1Hour)
  if (length(n2) == 0){
    output[i,5] <- '-'
  }else{
    output[i,5] <- IAQI1_2
  }
  
  IAQI1_1 <- 0
  n1 = which(O3_8hr[,2] >= round(as.numeric(newData[i,4]),3) & O3_8hr[,1] <= round(as.numeric(newData[i,4]))) # get row number of certain value
  IAQI1_1 <- ((IAQI[n1,2]-IAQI[n1,1])/(O3_8hr[n1,2]-O3_8hr[n1,1])*(as.numeric(newData[i,4])-O3_8hr[n1,1])+IAQI[n1,1]) # O3(8Hour)
  if (length(IAQI1_1) == 0){
    output[i,4] <- IAQI1_2
  }else{
    output[i,4] <- IAQI1_1[1]
  }
 
  n3 = which(PM2.5[,2] >= round(as.numeric(newData[i,6]),1) & PM2.5[,1] <= round(as.numeric(newData[i,6]),1))
  IAQI2 <- ((IAQI[n3,2]-IAQI[n3,1])/(PM2.5[n3,2]-PM2.5[n3,1])*(as.numeric(newData[i,6])-PM2.5[n3,1])+IAQI[n3,1]) # PM2.5
  output[i,6] <- IAQI2
  
  n4 = which(PM10[,2] >= round(as.numeric(newData[i,7])) & PM10[,1] <= round(as.numeric(newData[i,7]))) # the indicator is an integer
  IAQI3 <- ((IAQI[n4,2]-IAQI[n4,1])/(PM10[n4,2]-PM10[n4,1])*(as.numeric(newData[i,7])-PM10[n4,1])+IAQI[n4,1]) # PM10 
  output[i,7] <- IAQI3
 
  n5 = which(CO[,2] >= round(as.numeric(newData[i,8]),1) & CO[,1] <= round(as.numeric(newData[i,8]),1))
  IAQI4 <- ((IAQI[n5,2]-IAQI[n5,1])/(CO[n5,2]-CO[n5,1])*(as.numeric(newData[i,8])-CO[n5,1])+IAQI[n5,1]) # CO
  output[i,8] <- IAQI4
 
  n6 = which(SO2[,2] >= round(as.numeric(newData[i,9])) & SO2[,1] <= round(as.numeric(newData[i,9])))  # the indicator is an integer
  IAQI5 <- ((IAQI[n6,2]-IAQI[n6,1])/(SO2[n6,2]-SO2[n6,1])*(as.numeric(newData[i,9])-SO2[n6,1])+IAQI[n6,1]) # SO2
  output[i,9] <- IAQI5
 
  n7 = which(NO2[,2] >= round(as.numeric(newData[i,10])) & NO2[,1] <= round(as.numeric(newData[i,10])))  # the indicator is an integer
  IAQI6 <- ((IAQI[n7,2]-IAQI[n7,1])/(NO2[n7,2]-NO2[n7,1])*(as.numeric(newData[i,10])-NO2[n7,1])+IAQI[n7,1]) # NO2
  output[i,10] <- IAQI6
  
  AQI <- max(c(IAQI1_1, IAQI1_2, IAQI2, IAQI3, IAQI4, IAQI5, IAQI6)) # get the Maximum of IAQI
  output[i,3] <- AQI 
}

write.csv(output,'output.csv')
