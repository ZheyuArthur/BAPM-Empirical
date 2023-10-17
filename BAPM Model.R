### Behavioral Asset Pricing Model ###
### Empirical Research of BAPM Model with evidence from Chinese Stock Market ###
### Author: ZENG Arthur @ ESSEC Business School & London School of Economics ###
# git remote add origin https://github.com/ZheyuArthur/Arthur-s-Research-on-Finance-and-Economics.git
# git branch -M main
# git push -u origin main
##------------------------------------------------------------------------##

### packages used ###
library(patchwork)   
library(readxl)
library(officer)    
library(tidyverse)   
library(dplyr)
library(rio)      
library(vars)     
library(tseries)
library(TSA)
library(forecast)
library(xts)
library(scales)
import(Math)

Bapmdata <- read_excel("C:/Users/arthu/Desktop/BAPMh.:f/Bapmdata.xlsx")

##------------------------------------------------------------------------##
## Part 1. I compare the regression results of CAPM and BAPM

## CAPM regression 
  #All periods
  vars <- c(colnames(Bapmdata)) # I use colnames for the For-loops
  result <- c()
  for (i in 2:96){
    fit <- lm(substitute((x-Rf) ~ 0 + (Rm-Rf), list(x=as.name(vars[i]))), data = Bapmdata)
    result <- rbind(result, c(vars[i], coef(summary(fit))[1,c(1,2,4)])) # Combine statistics
  }
  
  #Same process on 2019 only
  result_2019 <- c()
  for (i in 2:96){
    fit <- lm(substitute((x[1:243]-Rf[1:243]) ~ 0 + (Rm[1:243]-Rf[1:243]), list(x=as.name(vars[i]))), data = Bapmdata)
    result_2019 <- rbind(result_2019, c(vars[i], coef(summary(fit))[1,c(1,2,4)])) # Combine statistics
  }
  
  #Same process on 2020 only
  result_2020 <- c()
  for (i in 2:96){
    fit <- lm(substitute((x[244:487]-Rf[244:487]) ~ 0 + (Rm[244:487]-Rf[244:487]), list(x=as.name(vars[i]))), data = Bapmdata)
    result_2020 <- rbind(result_2020, c(vars[i], coef(summary(fit))[1,c(1,2,4)])) # Combine statistics
  }

  #Same process on 2020 only
  result_2021 <- c()
  for (i in 2:96){
    fit <- lm(substitute((x[488:729]-Rf[488:729]) ~ 0 + (Rm[488:729]-Rf[488:729]), list(x=as.name(vars[i]))), data = Bapmdata)
    result_2021 <- rbind(result_2021, c(vars[i], coef(summary(fit))[1,c(1,2,4)])) # Combine statistics
  }

## BAPM regression
  #All periods
  vars2 <- c(colnames(Bapmdata)) 
  result2 <- c()
  for (i in 2:96){
    fit2 <- lm(substitute((x-Rf) ~ 0 + (RBM-Rf), list(x=as.name(vars2[i]))), data = Bapmdata)
    result2 <- rbind(result2, c(vars2[i], coef(summary(fit2))[1,c(1,2,4)]))
  }
  
  #Same process on 2019 only
  result2_2019 <- c()
  for (i in 2:96){
    fit2 <- lm(substitute((x[1:243]-Rf[1:243]) ~ 0 + (RBM[1:243]-Rf[1:243]), list(x=as.name(vars2[i]))), data = Bapmdata)
    result2_2019 <- rbind(result2_2019, c(vars2[i], coef(summary(fit2))[1,c(1,2,4)]))
  }
  
  #Same process on 2020 only
  result2_2020 <- c()
  for (i in 2:96){
    fit2 <- lm(substitute((x[244:487]-Rf[244:487]) ~ 0 + (RBM[244:487]-Rf[244:487]), list(x=as.name(vars2[i]))), data = Bapmdata)
    result2_2020 <- rbind(result2_2020, c(vars2[i], coef(summary(fit2))[1,c(1,2,4)]))
  }
  
  #Same process on 2021 only
  result2_2021 <- c()
  for (i in 2:96){
    fit2 <- lm(substitute((x[488:729]-Rf[488:729]) ~ 0 + (RBM[488:729]-Rf[488:729]), list(x=as.name(vars2[i]))), data = Bapmdata)
    result2_2021 <- rbind(result2_2021, c(vars2[i], coef(summary(fit2))[1,c(1,2,4)]))
  }

## Combine the results and clean
  #Combine results and have one output first
  Result_Whole <- cbind(result[,2], result2[,2])
  write.table(Result_Whole,file="Beta_result.csv")

  #Continue to combine results for CAPM and clear 
  beta_c_reg <- as.numeric(result[,2]) 
    beta_c <- beta_c_reg[4:length(beta_c_reg)] #delete first three lines
  beta_c_reg_2019 <- as.numeric(result_2019[,2]) 
    beta_c_2019 <- beta_c_reg_2019[4:length(beta_c_reg_2019)]
  beta_c_reg_2020 <- as.numeric(result_2020[,2]) 
    beta_c_2020 <- beta_c_reg_2020[4:length(beta_c_reg_2020)]
  beta_c_reg_2021 <- as.numeric(result_2021[,2]) 
    beta_c_2021 <- beta_c_reg_2021[4:length(beta_c_reg_2021)]

  #Continue to combine results for BAPM and clear
  beta_b_reg <- as.numeric(result2[,2]) 
    beta_b <- beta_b_reg[4:length(beta_b_reg)] #delete first three lines
  beta_b_reg_2019 <- as.numeric(result2_2019[,2]) 
    beta_b_2019 <- beta_b_reg_2019[4:length(beta_b_reg_2019)]
  beta_b_reg_2020 <- as.numeric(result2_2020[,2]) 
    beta_b_2020 <- beta_b_reg_2020[4:length(beta_b_reg_2020)]
  beta_b_reg_2021 <- as.numeric(result2_2021[,2])
  beta_b_2021 <- beta_b_reg_2021[4:length(beta_b_reg_2021)]
  
## Make graph 1a scatter points of betas in CAPM and BAPM
  
  #Convert Type to a factor
  Beta_table$Type <- factor(Beta_table$Type)
  
  #Create a long format of the data
  long_data <- Beta_table %>%
    pivot_longer(cols = starts_with("Beta_"), names_to = "Year", values_to = "Beta")
  
  #Create the scatter plot
  ggplot(data = long_data, aes(x = Ind, y = Beta, color = Type, group = Type)) +
    geom_point() +
    facet_wrap(~ Year, scales = "free")

## Make graph 1b box plot and scatter plot
  Whole_Period <- (test_data %>%
                    ggplot(aes(x = group, y = beta_hat)) +
                    geom_boxplot()+
                    theme(legend.position="none")+
                    labs(x="2019-2021", y = "beta"))
  Period_2019 <- (test_data_2019 %>%
                    ggplot(aes(x = group, y = beta_hat)) +
                    geom_boxplot()+
                    theme(legend.position="none")+
                    labs(x="2019", y = "beta"))
  Period_2020 <- (test_data_2020 %>%
                    ggplot(aes(x = group, y = beta_hat)) +
                    geom_boxplot()+
                    theme(legend.position="none")+
                    labs(x="2020", y = "beta"))
  Period_2021 <- (test_data_2021 %>%
                    ggplot(aes(x = group, y = beta_hat)) +
                    geom_boxplot()+
                    theme(legend.position="none") +
                    labs(x="2021", y = "beta"))
  Whole_Period + Period_2019 + Period_2020 + Period_2021

  
## Compare the results of CAPM and BAPM
  #group by b/capm to obtain descriptive stat
  group_by(test_data, group) %>%
    summarise(
      count = n(),
      mean = mean(beta_hat, na.rm = TRUE),
      sd = sd(beta_hat, na.rm = TRUE)
    )
  
  # Shapiro-Wilk normality test
  with(test_data, shapiro.test(beta_hat[group == "BAPM"])) # p = 0.1095
  # Shapiro-Wilk normality test
  with(test_data, shapiro.test(beta_hat[group == "CAPM"])) # p = 0.284

  # variance test
  res.ftest <- var.test(beta_hat ~ group, data = test_data)


  

##------------------------------------------------------------------------##
## Part 2. Calculate and process Noise Trader Risk (NTR)

## NTR Estimate ##
  #Calculate NTR = beta_CAPM - beta_BAPM
  diff_beta <- (as.numeric(result[,2])-as.numeric(result2[,2]))
  ntr_set <- diff_beta[4:length(diff_beta)] # eliminate the first three non-sense numbers
  #Visualize the NTR curve
  plot(
    x=c(1:92), y=ntr_set,
    xlab="Security", ylab="NTR Value", 
    xlim=c(0,100), ylim=c(-0.2,0.4),
    )
  abline(h=0, col="grey") 
  
  #Take a look at the mean 
  ntr_aver <- mean(ntr_set)



## Calculating NTR for different periods and scenarios ##
  #Write a ntr calculator function
  ntr_cal <- function(data){
    vars <- c(colnames(data))
    vars2 <- c(colnames(data))
    result <- c()
    result2 <- c()
    for (i in 2:96){
      fit <- lm(substitute((x-Rf) ~ 0 + (Rm-Rf), list(x=as.name(vars[i]))), data = data)
      result <- rbind(result, c(vars[i], coef(summary(fit))[1,c(1,2,4)]))
    }
    for (i in 2:96){
      fit2 <- lm(substitute((x-Rf) ~ 0 + (RBM-Rf), list(x=as.name(vars2[i]))), data = data)
      result2 <- rbind(result2, c(vars2[i], coef(summary(fit2))[1,c(1,2,4)]))
    }
    diff_beta <- (as.numeric(result[,2])-as.numeric(result2[,2]))
    ntr_set <- diff_beta[4:length(diff_beta)] # eliminate the first three non-sense numbers
    ntr_aver <- mean(ntr_set)
    ntr_aver
  }

#Timedata <- read_excel("C:/Users/arthu/Desktop/BAPMh.:f/Time_news.xlsx")

  #Write NTR Calculator for one stock
  ntr_cal_one <- function(random_dataset, Rm_t, RBM_t, Rf_t){
    fit_capm <- lm(random_dataset ~ 0 + (Rm_t - Rf_t), data=Bapmdata)
    fit_bapm <- lm(random_dataset ~ 0 + (RBM_t - Rf_t), data=Bapmdata)
    ntr_each <- as.numeric(coef(summary(fit_capm))[1])-as.numeric(coef(summary(fit_bapm))[1])
    ntr_each
  }
  
  #Write NTR for all stocks in different time periods based on ntr_cal_one
  ntr_diff_series <- function(n_n, size){ 
    #n_n controls stocks; size controls time
    windows_store <- array()
    for (i in 1:(730-size)){
      subset_each <- subset(unlist(Bapmdata[,n_n]), Refer >=i & Refer <= (i+size-1))
      Rm_ts <- subset(Bapmdata$Rm, Refer >=i & Refer <= (i+size-1))
      RBM_ts <- subset(Bapmdata$RBM, Refer >=i & Refer <= (i+size-1))
      Rf_ts <- subset(Bapmdata$Rf, Refer >=i & Refer <= (i+size-1))
      result_each <- ntr_cal_one(subset_each, Rm_ts, RBM_ts, Rf_ts )
      windows_store[i] <- result_each
    }
    delta_ntr <- diff(windows_store)
    delta_ntr  ## the change of ntr values for each stock ##
  }

  
##------------------------------------------------------------------------##
## Part 3. Newsdata and Information Adjusted Noise Model (IANM)

## write a function regressing Newsdata on one stock ##
  IANM_num <- function(col_n, size_1, size_2){ 
    #size_1 is the window size for ntr calculation, size_2 is for IANM windows
    ntr_col_n <- ntr_diff_series(col_n, size_1)
    IE_col_n <- unlist(Timedata[(2+size_1):730,col_n])
    alpha_n <- c()
    beta_n <- c()
    mu_n <- c()
    for (i in 1:(730-size_1-size_2)){
      IANM_fit <- lm(ntr_col_n[i:(i+size_2-1)] ~  IE_col_n[i:(i+size_2-1)])
      alpha_n[i] <- coef(summary(IANM_fit))[1,1]
      beta_n[i] <- coef(summary(IANM_fit))[2,1] 
      mu_n[i] <- alpha_n[i] + beta_n[i]
    }
    para_n <- cbind(alpha_n, beta_n, mu_n)
    para_n
  }

## Run IANM Function for all stocks ##
  Result_IANM <- c()
  for (i in 7:98){
    Every_stock <- IANM_num(i,100,100)
    Result_IANM <- bind_cols(Result_IANM, Every_stock)
  }
  Result_IANM
  
  #Output IANM result
  write.table(Result_IANM,file="IANM.csv")


## Give a numerical example for explanation
  ntr_7 <- ntr_diff_series(7,30) #699 numbers
  IE_7 <- Timedata[33:731,7]
  # 699 numbers = 730 - n_n(windows_length) - 1
  # take 600000.SH as an example #
  # fit_att <- lm(unlist(ntr_diff_series(7,30)) ~ unlist(Timedata[33:731,2]))


##------------------------------------------------------------------------##
## Part 4. Calculation of daily NTR based on sliding window method

  #Write a sliding window based calculation function
  slide_win_cal <- function(data, size){
    windows_arr <- array()  # empty carrier for data
    for (i in 1:(730-size)){
      subset_day <- subset(data, Refer >= i & Refer <= (i+size-1)) # Window
      result_day <- ntr_cal(subset_day)    # run ntr calculation function
      windows_arr[i] <- result_day    # instill the data into carrier
    }
    windows_arr
  }

  result_daily <- slide_win_cal(Bapmdata, 10) # ref=10, 10-days-average-line


  #NTR in seasons
  
  ## Seasonal Test ##
  # 2019s1 (1,57)       2020s1 (244,279)      2021s1 (487,544)      2022s1?
  # 2019s2 (58,117)     2020s2 (280,340)      2021s2 (545,604)      2022s2?
  # 2019s3 (118,183)    2020s3 (341,404)      2021s3 (605,668)
  # 2019s4 (184,243)    2020s4 (405,486)      2021s4 (669,729)
  y2019_s1 <- subset(Bapmdata, Refer>=1&Refer<=57)
  ntr_1 <- ntr_cal(y2019_s1)
  y2019_s2 <- subset(Bapmdata, Refer>=58&Refer<=117)
  ntr_2 <- ntr_cal(y2019_s2)
  y2019_s3 <- subset(Bapmdata, Refer>=118&Refer<=183)
  ntr_3 <- ntr_cal(y2019_s3)
  y2019_s4 <- subset(Bapmdata, Refer>=184&Refer<=243)
  ntr_4 <- ntr_cal(y2019_s4)
  y2019_s5 <- subset(Bapmdata, Refer>=244&Refer<=279)
  ntr_5 <- ntr_cal(y2019_s5)
  y2019_s6 <- subset(Bapmdata, Refer>=280&Refer<=340)
  ntr_6 <- ntr_cal(y2019_s6)
  y2019_s7 <- subset(Bapmdata, Refer>=341&Refer<=404)
  ntr_7 <- ntr_cal(y2019_s7)
  y2019_s8 <- subset(Bapmdata, Refer>=405&Refer<=486)
  ntr_8 <- ntr_cal(y2019_s8)
  y2019_s9 <- subset(Bapmdata, Refer>=487&Refer<=544)
  ntr_9 <- ntr_cal(y2019_s9)
  y2019_s10 <- subset(Bapmdata, Refer>=545&Refer<=604)
  ntr_10 <- ntr_cal(y2019_s10)
  y2019_s11 <- subset(Bapmdata, Refer>=605&Refer<=668)
  ntr_11 <- ntr_cal(y2019_s11)
  y2019_s12 <- subset(Bapmdata, Refer>=669&Refer<=729)
  ntr_12 <- ntr_cal(y2019_s12)
  
  ntr_seasonal_data <- c(ntr_1,ntr_2,ntr_3,
                         ntr_4,ntr_5,ntr_6,
                         ntr_7,ntr_8,ntr_9,
                         ntr_10,ntr_11,ntr_12)
  #Visualize
  plot(ntr_seasonal_data,type="o",
    xlab="Period in seasons", ylab="NTR Value", 
  )

  #NTR in a shifting window
  #Obtain day-line result 
  slide_win_cal <- function(data, size){
    windows_arr <- array()  # empty carrier for data
    for (i in 1:(730-size)){
      subset_day <- subset(data, Refer >= i & Refer <= (i+size-1)) # Window
      result_day <- ntr_cal(subset_day)    # run ntr calculation function
      windows_arr[i] <- result_day    # instill the data into carrier
      }
    windows_arr
  }

  #Output result of daily NTR
  write.table(result_daily, file="result_daily.xlsx")
  Bapmdata$Rm[2:721]

  #Visualization
  result_daily
  Bapmdata$Rm[2:691]
  Time_1 <- c(1:630) 
  
  options(repr.plot.width=100, repr.plot.height=5)
  plot(Time_1, result_daily, type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time_1, unlist(SSE_Comp)[91:720], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("SSE Composte Index", side=4, line=3)
  legend("topleft", c("NTR", "SSE Composite Index"),
         col=c("red", "blue"), lty=c(1,1))
  
  
  plot(Time_1, result_daily, type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time_1, unlist(SSE_Comp)[91:720], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("SSE Composte Index", side=4, line=3)
  legend("topleft", c("NTR", "SSE Composite Index"),
         col=c("red", "blue"), lty=c(1,1))
  
  abline(h=0, col="grey")
  abline(v=205, col="grey")
  abline(v=425, col="grey")
  
  temp <- locator(1) 
  text(temp," Year 2019")
  
  temp2 <- locator(1) 
  text(temp2,"Year 2020")
  
  temp3 <- locator(1)
  text(temp3,"Year 2021")
  
  SSE_Comp <- read_excel("C:/Users/arthu/Desktop/BAPMh.:f/SSE_Composite.xlsx")
  plot(Time_1, unlist(SSE_Comp)[91:720], type="l",
       xlab="Time", ylab="NTR")

##------------------------------------------------------------------------##
## Part 5. Event Study about Covid-19 on NTR and behavioral biases
## Event Study ##

  # Events:                Date Refer:        Windows:
  # ------------------------------------------------------
  # Outbreak of covid-19     135               
  # Free vaccination         275
  # Death 1 million          326
  # Vaccination 20%          493

  Time <- c(1:630)                              # 630
  ntr_indicator <- slide_win_cal(Bapmdata, 100) # 630
  stock_return <- unlist(SSE_Comp)[91:720]      # 630
  
  #Visualize Event1 COVID-19 Outbreak
  plot(Time[110:140], ntr_indicator[110:140], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[110:140], stock_return[110:140], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("SSE Composte Index", side=4, line=3)
  legend("top", c("NTR", "SSE Composite Index"),
         col=c("red", "blue"), lty=c(1,1))
  title("COVID-19 Outbreak")

  #Visualize Event2 Free Vaccination
  plot(Time[265:295], ntr_indicator[265:295], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[265:295], stock_return[265:295], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("SSE Composte Index", side=4, line=3)
  title("Free Vaccination")

  #Visualize Event3 One Million Death
  plot(Time[320:350], ntr_indicator[320:350], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[320:350], stock_return[320:350], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("SSE Composte Index", side=4, line=3)
  title("1 Million Death")

  #Visualize Event4 Widespread Vaccination
  plot(Time[480:510], ntr_indicator[480:510], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[480:510], stock_return[480:510], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("SSE Composte Index", side=4, line=3)
  title("Widespread Vaccination")

  #Behavioral Biases
  Psy <- read_excel("C:/Users/arthu/Desktop/BAPMh.:f/COVID_Res.xlsx")
  
  #Different Biases
  ntr_indicator <- slide_win_cal(Bapmdata, 100) # 630
  OCI <- Psy$`Over-confidence Index`[81:710] #Over-confidence
  CSAD <- Psy$`|CSAD|`[81:710]               #Herding
  AI <- Psy$Aversion_Index[81:710]           #Risk Aversion

  #NTR
  #Visualize Event1 COVID-19 Outbreak
  plot(Time[110:140], ntr_indicator[110:140], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[110:140], OCI[110:140], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("OCI", side=4, line=3)
  legend("topleft", c("NTR", "OCI"),
         col=c("red", "blue"), lty=c(1,1))
  title("COVID-19 Outbreak")
  
  #Visualize Event2 Free Vaccination
  plot(Time[265:295], ntr_indicator[265:295], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[265:295], OCI[265:295], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("OCI", side=4, line=3)
  title("Free Vaccination")
  
  #Visualize Event3 One Million Death
  plot(Time[320:350], ntr_indicator[320:350], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[320:350], OCI[320:350], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("OCI", side=4, line=3)
  title("1 Million Death")
  
  #Visualize Event4 Widespread Vaccination
  plot(Time[480:510], ntr_indicator[480:510], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[480:510], OCI[480:510], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("OCI", side=4, line=3)
  title("Widespread Vaccination")


  #CSAD, follow the exactly same process as above to visualize
  plot(Time[110:140], ntr_indicator[110:140], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[110:140], CSAD[110:140], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("CSAD", side=4, line=3)
  legend("topleft", c("NTR", "CSAD"),
         col=c("red", "blue"), lty=c(1,1))
  title("COVID-19 Outbreak")
  
  plot(Time[265:295], ntr_indicator[265:295], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[265:295], CSAD[265:295], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("CSAD", side=4, line=3)
  title("Free Vaccination")
  
  plot(Time[320:350], ntr_indicator[320:350], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[320:350], CSAD[320:350], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("CSAD", side=4, line=3)
  title("1 Million Death")
  
  plot(Time[480:510], ntr_indicator[480:510], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[480:510], CSAD[480:510], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("CSAD", side=4, line=3)
  title("Widespread Vaccination")


  #Risk aversion, follow the exactly same process as above to visualize
  plot(Time[110:140], ntr_indicator[110:140], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[110:140], AI[110:140], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("AI", side=4, line=3)
  legend("topleft", c("NTR", "AI"),
         col=c("red", "blue"), lty=c(1,1))
  title("COVID-19 Outbreak")
  
  plot(Time[265:295], ntr_indicator[265:295], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[265:295], AI[265:295], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("AI", side=4, line=3)
  title("Free Vaccination")
  
  plot(Time[320:350], ntr_indicator[320:350], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[320:350], AI[320:350], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("AI", side=4, line=3)
  title("1 Million Death")
  
  plot(Time[480:510], ntr_indicator[480:510], type="l",
       xlab="Time", ylab="NTR",
       col="red")
  par(new="TRUE", oma=c(3,3,3,3))
  plot(Time[480:510], AI[480:510], type="l",
       xlab="Time", ylab="NTR",
       xaxt='n', yaxt='n',
       col="blue")  
  axis(side=4)
  mtext("AI", side=4, line=3)
  title("Widespread Vaccination")



# coeff <- 1 ## Outbreak of covid-19
# dd_1 <- data.frame(Time[110:140],ntr_indicator[110:140],stock_return[110:140])
# figure1 <- ggplot()+
#   geom_line(data = dd_1,aes(x = Time[110:140],y = ntr_indicator[110:140],colour = "ntr_indicator"),size=0.7)+
#   geom_line(data = dd_1,aes(x = Time[110:140],y = rescale(stock_return[110:140]/coeff, c(0,0.1)),colour ="stock_return"),size=0.7) + 
#   scale_y_continuous(
#     name="NTR",
#     sec.axis = sec_axis(~.*coeff, name="stock return")
#   )+
#   scale_colour_manual("",values = c("ntr_indicator" = "red","stock_return" = "blue"))+
#   xlab("Time")+ylab("NTR")+
#   theme(text=element_text(size=13, family="Comic Sans MS"))+
#   ggtitle("Outbreak of COVID-19")
# figure1
# 
# coeff <- 18000 ## Free vaccination
# dd_2 <- data.frame(Time[265:295],ntr_indicator[265:295],stock_return[265:295])
# figure2 <- ggplot()+
#   geom_line(data = dd_2,aes(x = Time[265:295],y = ntr_indicator[265:295],colour = "ntr_indicator"),size=0.7)+
#   geom_line(data = dd_2,aes(x = Time[265:295],y = rescale(stock_return[265:295]/coeff, c(0,0.1)),colour ="stock_return"),size=0.7) + 
#   scale_y_continuous(
#     name="NTR",
#     sec.axis = sec_axis(~.*coeff, name="stock return")
#   )+
#   scale_colour_manual("",values = c("ntr_indicator" = "red","stock_return" = "blue"))+
#   xlab("Time")+ylab("NTR")+
#   theme(text=element_text(size=13, family="Comic Sans MS"))+
#   ggtitle("Free Vaccination")
# figure2
# 
# coeff <- 18000 ## Death Surpass one million
# dd_3 <- data.frame(Time[320:350],ntr_indicator[320:350],stock_return[320:350])
# figure3 <- ggplot()+
#   geom_line(data = dd_2,aes(x = Time[320:350],y = ntr_indicator[320:350],colour = "ntr_indicator"),size=0.7)+
#   geom_line(data = dd_2,aes(x = Time[320:350],y = rescale(stock_return[320:350]/coeff, c(0,0.1)),colour ="stock_return"),size=0.7) + 
#   scale_y_continuous(
#     name="NTR",
#     sec.axis = sec_axis(~.*coeff, name="stock return")
#   )+
#   scale_colour_manual("",values = c("ntr_indicator" = "red","stock_return" = "blue"))+
#   xlab("Time")+ylab("NTR")+
#   theme(text=element_text(size=13, family="Comic Sans MS"))+
#   ggtitle("Death Surpass 1 million")
# figure3
# 
# coeff <- 18000 ## Death Surpass one million
# dd_4 <- data.frame(Time[480:510],ntr_indicator[480:510],stock_return[480:510])
# figure4 <- ggplot()+
#   geom_line(data = dd_2,aes(x = Time[480:510],y = ntr_indicator[480:510],colour = "ntr_indicator"),size=0.7)+
#   geom_line(data = dd_2,aes(x = Time[480:510],y = rescale(stock_return[320:350]/coeff, c(0,0.1)),colour ="stock_return"),size=0.7) + 
#   scale_y_continuous(
#     name="NTR",
#     sec.axis = sec_axis(~.*coeff, name="stock return")
#   )+
#   scale_colour_manual("",values = c("ntr_indicator" = "red","stock_return" = "blue"))+
#   xlab("Time")+ylab("NTR")+
#   theme(text=element_text(size=13, family="Comic Sans MS"))+
#   ggtitle("Widespread Vaccination")
# figure4
# 
# Whole_Period <- (test_data %>%
#                    ggplot(aes(x = group, y = beta_hat)) +
#                    geom_boxplot()+
#                    theme(legend.position="none")+
#                    labs(x="2019-2021", y = "beta"))
# 
# Period_2019 <- (test_data_2019 %>%
#                   ggplot(aes(x = group, y = beta_hat)) +
#                   geom_boxplot()+
#                   theme(legend.position="none")+
#                   labs(x="2019", y = "beta"))
# 
# Period_2020 <- (test_data_2020 %>%
#                   ggplot(aes(x = group, y = beta_hat)) +
#                   geom_boxplot()+
#                   theme(legend.position="none")+
#                   labs(x="2020", y = "beta"))
# 
# Period_2021 <- (test_data_2021 %>%
#                   ggplot(aes(x = group, y = beta_hat)) +
#                   geom_boxplot()+
#                   theme(legend.position="none") +
#                   labs(x="2021", y = "beta"))
# 
# Whole_Period + Period_2019 + Period_2020 + Period_2021



