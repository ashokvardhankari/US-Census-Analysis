library(visreg)
library(lmtest)
library(arm)

# Exploratory Analysis


us2000 <- read.csv(file="US_Census_AllData_2000.csv",
                 header=T, as.is=TRUE)

us2010 <- read.csv(file="US_Census_AllData_2010.csv",
                 header=T, as.is=TRUE)

us2014 <- read.csv(file="US_Census_AllData_2014.csv",
                 header=T, as.is=TRUE)

head(us2000)
head(us2010)
head(us2014)

# Factors influencing population rise/ fall in 2000

lm1 <- lm(P_2000~GDP_2000+Cr_2000+CO2_2000+Av_2000, us2000)
summary(lm1)

# Factors influencing population rise/ fall in 2010
lm2 <- lm(P_2010~GDP_2010+Cr_2010+CO2_2010+Av_2010+BIRTHS2010+DEATHS2010+NETMIG2010+IncM_2010, us2010)
summary(lm2)

# Factors influencing population rise/ fall in 2014
lm3 <- lm(P_2014~GDP_2014+Cr_2014+CO2_2014+Av_2014+BIRTHS2014+DEATHS2014+NETMIG2014+IncM_2014, us2014)
summary(lm3)

# Factors influencing migration in 2010

lm4 <- lm(NETMIG2010~Cr_2010+E_2010+Av_2010+CO2_2010+IncM_2010, us2010)
summary(lm4)

lm5 <- lm(NETMIG2014~Cr_2014+E_2014+Av_2014+CO2_2014+IncM_2014, us2014)
summary(lm5)

par(mfrow=c(2,2))
plot(lm1, main = "Linear Regression - Year 2000")

plot(lm2, main = "Linear Regression - Year 2010")

plot(lm3, main = "Linear Regression - Year 2014")

plot(lm4, main = "Migration to its factors - Year 2010")

plot(lm5, main = "Migration to its factors - Year 2014")
dev.off()

lm10 <- lm(Pop._Change_1~GDP._Change_1+Crime._Change_1+Ave_Wage_.Ch_1+CO2_Em_Change_1, census1)
summary(lm10)
plot(lm10)

lm11 <- lm(Pop._Change_2~GDP._Change_2+Crime._Change_2+Ave_Wage_.Ch_2+CO2_Em_Change_2, census1)
summary(lm11)
plot(lm11)
visreg2d(lm10,"GDP._Change_1", "Ave_Wage_.Ch_1", type = "contrast", plot.type = "persp")
visreg2d(lm10,"GDP._Change_1", "Ave_Wage_.Ch_1",  plot.type = "rgl", color = "#20B2AA")
visreg2d(lm11,"GDP._Change_2", "Ave_Wage_.Ch_2",  plot.type = "rgl", color = "#20B2AA")

coefplot(lm10, add=TRUE, col.pts="blue", intercept=TRUE, offset=0.2)


