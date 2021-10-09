#Import data and declare time series

library(readxl)
data<- read_excel("HousePrices_CA.xls")


#install.packages("dynlm") 
#Model I for fitting dynamic linear models 
library(dynlm) # use the dynlm package
g <- ts(data$`Growth Rate_ HousePrice CA`, frequency = 4, start=c(1975,1)) #declare g as (quarterly) time series 


#Fixed Scheme
fcast1<-numeric(20) #generate a vector of 20 zeros
model<-dynlm(g ~ lag(g,-1)+ lag(g,-3), start=c(1975,1), end=c(2002,4)) #fit AR(3)
for (i in 1:20){ #start a for loop
  fcast1[i]<-coef(model)[1]+coef(model)[2]*g[111+i]+coef(model)[3]*g[109+i] #fill in forecasted values at the end of each iteration
} #end the loop

#Recursive Scheme
fcast2<-numeric(20) #generate a vector of 20 zeros
for (i in 1:20){ 
  model<-dynlm(g ~ lag(g,-1)+ lag(g,-3), start=c(1975,1), end=c(2002,3+i)) #fit AR(3), note that "end" depends on i
  fcast2[i]<-coef(model)[1]+coef(model)[2]*g[111+i]+coef(model)[3]*g[109+i] #fill in forecasted values at the end of each iteration
} 

#Rolling Scheme
fcast3<-numeric(20) #generate a vector of 20 zeros
for (i in 1:20){ 
  model<-dynlm(g ~ lag(g,-1)+ lag(g,-3), start=c(1975,1+i), end=c(2002,3+i)) #fit AR(3), note that both "start" and "end" depend on i
  fcast3[i]<-coef(model)[1]+coef(model)[2]*g[111+i]+coef(model)[3]*g[109+i] #fill in forecasted values at the end of each iteration
} 

#Model II (Naive)
fcast4<-numeric(20) 
for (i in 1:20){ 
  fcast4[i]<-g[111+i] #naive forecast
} 

#Model IV (Average-4)
fcast5<-numeric(20) 
for (i in 1:20){ 
  fcast5[i]<-(g[111+i]+g[110+i]+g[109+i]+g[108+i])/4 #forecast is the average of the last 4 observations
} 
g0<-window(g,start=c(2003,1))
f1<-ts(fcast1, frequency = 4, start=c(2003,1))
f2<-ts(fcast2, frequency = 4, start=c(2003,1))
f3<-ts(fcast3, frequency = 4, start=c(2003,1))
f4<-ts(fcast4, frequency = 4, start=c(2003,1))
f5<-ts(fcast5, frequency = 4, start=c(2003,1))
plot(g0,col='black',main = "CA house price",ylab = "Growth rate",xlab = "Quarter")
lines(f1,col='red')
lines(f2,col='green')
lines(f3,col='brown')
lines(f4,col='purple')
lines(f5,col='blue')

