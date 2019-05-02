#for the price prediction 
library('tseries')
library('forecast')
dataset = read.csv("C:/Users/AAA/Documents/bitcoin price prediction/final_data.csv",header=T)
dataset$DATES
dataset$DOW.JONES
dataset$DATES=as.Date(dataset$DATES,"%d-%m-%Y")
data_daily= ts(dataset[,-1], frequency = 1)

fit= NULL
fc_d = data.frame()
for (i in 1:ncol(data_daily))
{
  fit[i] = auto.arima(data_daily[,i],seasonal = T)
  x=forecast(fit[i],h=1)
  fc_d[1,i]=x$mean
}


fit1=auto.arima(data_daily[,1],seasonal = T)
fc_d1=forecast(fit1,h=1)
x1=fc_d1$mean

fit2=auto.arima(data_daily[,2],seasonal = T)
fc_d2=forecast(fit1,h=1)
x2=fc_d2$mean

fit3=auto.arima(data_daily[,3],seasonal = T)
fc_d3=forecast(fit1,h=1)
x3=fc_d3$mean

fit4=auto.arima(data_daily[,4],seasonal = T)
fc_d4=forecast(fit1,h=1)
x4=fc_d4$mean

fit5=auto.arima(data_daily[,5],seasonal = T)
fc_d5=forecast(fit5,h=1)
x5=fc_d5$mean

fit6=auto.arima(data_daily[,6],seasonal = T)
fc_d6=forecast(fit6,h=1)
x6=fc_d6$mean

fit7=auto.arima(data_daily[,7],seasonal = T)
fc_d7=forecast(fit7,h=1)
x7=fc_d7$mean

fit8=auto.arima(data_daily[,8],seasonal = T)
fc_d8=forecast(fit8,h=1)
x8=fc_d8$mean

fit9=auto.arima(data_daily[,9],seasonal = T)
fc_d9=forecast(fit9,h=1)
x9=fc_d9$mean

fit10=auto.arima(data_daily[,10],seasonal = T)
fc_d10=forecast(fit10,h=1)
x10=fc_d10$mean

fit11=auto.arima(data_daily[,11],seasonal = T)
fc_d11=forecast(fit11,h=1)
x11=fc_d11$mean

fit12=auto.arima(data_daily[,12],seasonal = T)
fc_d12=forecast(fit12,h=1)
x12=fc_d12$mean

prediction = dataset[FALSE,]
prediction[1,1]=dataset$DATES[31]+1
prediction[1,2]=x1
prediction[1,3]=x2
prediction[1,4]=x3
prediction[1,5]=x4
prediction[1,6]=x5
prediction[1,7]=x6
prediction[1,8]=x7
prediction[1,9]=x8
prediction[1,10]=x9
prediction[1,11]=x10
prediction[1,12]=x11
prediction[1,13]=x12

write.csv(prediction , "Z:/bitcoin price prediction/project/prediction.csv")
save.image("Z:/bitcoin price prediction/auto_arima.RData")


# for the imputation and regression 
data=data=read.csv("Z:/bitcoin price prediction/project/one_year_data.csv",header = T)
data$date=as.Date(data$date,"%d-%m-%Y")

library(imputeTS)
#dowjones impute
x_dow=ts(data$DOW_JONES,frequency = 7)
x_dow_na=na.kalman(x_dow)
x1=data.frame(x_dow_na)

#s&p imputation
x_sp=ts(data$S.P_500,frequency = 7)
x_sp_na=na.kalman(x_sp)
x2=data.frame(x_sp_na)
#nasdaq
x_nas=ts(data$NASDAQ,frequency = 7)
x_nas_na=na.kalman(x_nas)
x3=data.frame(x_nas_na)
#sensex
x_sen=ts(data$SENSEX,frequency = 7)
x_sen_na=na.kalman(x_sen)
x4=data.frame(x_sen_na)
#ftse
x_ft=ts(data$FTSE,frequency = 7)
x_ft_na=na.kalman(x_ft)
x5=data.frame(x_ft_na)
#nikkei
x_nk=ts(data$NIKKIE_225,frequency = 7)
x_nk_na=na.kalman(x_nk)
x6=data.frame(x_nk_na)

imputed_data= cbind(x1,x2,x3,x4,x5,x6)
write.csv(imputed_data,"Z:/bitcoin price prediction/project/imputed_data.csv")

#regression model 
data=read.csv("Z:/bitcoin price prediction/project/final_data.csv",header = T)

#finding out the correlation 
library(Hmisc)

corr=rcorr(as.matrix(data[,-1]))
corr=as.data.frame(corr$r)
write.csv(corr,"Z:/bitcoin price prediction/project/correlation.csv")

#removing S&p as it is highly correlated with NASDAQ . and NASDAQ is more correlated with bitcoin
#Removing FTSE as its highly correlated with NASDAQ 
##removing litcoin as its highly correlated with sensex 
colnames(data)
data1=data[,-c(3,7,10)]

corr1 =rcorr(as.matrix(data1[,-1]))
corr1=as.data.frame(corr1$r)
data2=data1[,-1]
reg=lm(BITCOIN~.,data = data2)
reg
summary(reg)
