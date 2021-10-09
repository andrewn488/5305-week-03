data<-houseprices_mortgagerates
p<-ts(data$`US House Price Index (Freddie Mac)`,frequency = 1,start = 1971)
r<-ts(data$`30-year fixed mortgage rate (average)`,frequency = 1,start = 1971)

plot(p)
plot(r)

dp<-diff(log(p))
dr<-diff(r)

plot(dp)
plot(dr)


library(dynlm)
model1<-dynlm(dp~lag(dp,-1)+lag(dp,-2))
summary(model1)

model2<-dynlm(dp~lag(dp,-1)+lag(dp,-2)+lag(dr,-1)+lag(dr,-2))
summary(model2)

model3<-dynlm(dp~lag(dp,-1)+lag(dp,-2)+lag(dp,-3)+lag(dp,-4))
summary(model3)

model4<-dynlm(dp~lag(dp,-1)+lag(dp,-2)+lag(dp,-3)+lag(dp,-4)+lag(dr,-1)+lag(dr,-2)+lag(dr,-3)+lag(dr,-4))
summary(model4)

acf(p,lag.max=20)
acf(r,lag.max=20)
pacf(p,lag.max=20)
pacf(r,lag.max=20)

acf(p,lag.max=20,plot=FALSE)
pacf(r,lag.max=20,plot=FALSE)


acf(dp,lag.max=10)
acf(dr,lag.max=10)
pacf(dp,lag.max=10)
pacf(dr,lag.max=10)
