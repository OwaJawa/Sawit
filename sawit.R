library(reshape2)
library(MASS)
library(ggplot2)

DataSawit<- read.csv(file="DataSawit.csv",head=TRUE,sep=",")

Api <- scale(DataSawit$Api, center = T, scale = T)
Sawit <- scale(DataSawit$Sawit, center = T, scale = T)
D1 <- scale(DataSawit$D1, center = T, scale = T)
D2 <- scale(DataSawit$D2, center = T, scale = T)
D3 <- scale(DataSawit$D3, center = T, scale = T)
Year <- (DataSawit$Year)
Prov <- (DataSawit$Prov)

plot(Api, Sawit)

reg <- lm(Sawit ~ Api )
reg1 <- lm(Sawit ~ Api + D1 + D2 + D3)

summary(reg)
summary(reg1)

reg2 <- rlm(Sawit ~ Api + D1 + D2 + D3)
summary(reg2)

#  Plotting 


ggplot( DataSawit,  aes(x= Besar Lahan Hutan Terbakar (hektar), y= Besar Lahan Kelapa Sawit Tahun Depan (hektar))) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=rlm)

ggplot(DataSawit, aes(x= Api, y= Sawit)) +
  geom_point(shape=19,      # Use solid circles
             alpha=1/4) +
  geom_smooth(method=rlm) # 1/4 opacity

qplot(Api, Sawit) + geom_smooth( method = rlm)

# plotting time series differnce

Api_ts <- ts(Api)
plot(Api)
plot(Sawit)





#--- try Panel Dynamics
# plot Time Series in panel
library(foreign)

coplot(Api ~  Year|Prov , type="l")        # Lines
coplot(Api ~  Year|Prov , type="b")         # Points and line

library(lattice)

# xyplot(Sawit + Api ~ Year , type="smooth")
# xyplot(Sawit + Api ~ Year|Sawit , type="smooth")

#dat<-data.frame(subject=as.factor(rep(1:4, each=50)),time=rep(1:50,4),y1=rnorm(200,50,20),y2=rnorm(200,70,20),y3=rnorm(200,20,20))
#xyplot(y1+y2+y3~time|subject,dat,type='smooth')



library(car)

# scatterplot(  Api ~ Year|Prov,
#               boxplots = T,
#               smooth = F ,
#               reg.line = T)

# Panel

panel_plm <- plm(Sawit ~ Api + D1 + D2 + D3 , model = "within", data = DataSawit, )
summary(panel_plm)


data("Produc", package = "plm")
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
          data = Produc, index = c("state","year"))
summary(zz)




panel_dat <- data("EmplUK", package = "plm")

## Arellano and Bond (1991), table 4b 
z1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital) + lag(log(output), 0:1) | lag(log(emp), 2:99),
           data = EmplUK, effect = "twoways", model = "twosteps")
summary(z1)

Sawit_Panel <- pdata.frame(DataSawit, index = c("Prov", "Year"), drop.index = TRUE, row.names = TRUE)
head(Sawit_Panel)

z1 <- pgmm(log(emp) ~ log(wage) + log(capital) + log(output),
           lag.form = list(2,1,0,1), data = EmplUK, 
           effect = "twoways", model = "twosteps",
           gmm.inst = ~log(emp), lag.gmm = list(c(2,99)))
summary(z1)

pdim(Sawit_Panel)

z1 <- pgmm(log(Sawit) ~ log(Api) ,
          lag.form = 1,
          data = Sawit_Panel,
          effect = "twoways",
          model = "twosteps",
          gmm.inst = ~ log(Sawit),
          lag.gmm = list(c(2,99)))
summary(z1)