library(survival)
library(survminer)
library(dplyr)
library(ggplot2)
library(MASS)
library(pscl)
library(leaps)
library(stargazer)
military <- read.csv("D:/Jongwon's/Political Science/OH_Military_Data.csv")

military$region <- factor(military$region)
military$mpdummy <- factor(military$mpdummy)
military$majordummy <- factor(military$majordummy)
military$opec <- factor(military$opec)

str(military)
View(military)
options("scipen" = 100)

# 1. the whole military regime
## survival model: univariate

model1=survfit(formula=Surv(duryear,status)~1,data=military,conf.type="log-log")
summary(model1)
quantile(model1,probs=c(0.25,0.5,0.75),conf.int=T)
plot(model1,xlab='time',ylab='survival rate',mark.time=T)
legend(0.5,0.2,c("Product-limit estimator","95% CI"),lty=c(1,2))
ggsurvplot(model1,data=military)

w_model1=survreg(formula=Surv(duryear,status)~1,data=military,dist="weibull")
surv_prob <- seq(.99, .01, by = -.01)
surv_time <- predict(w_model1, type = "quantile", p = 1 - surv_prob, 
                     newdata = data.frame(1))
model1_df <- tibble(time = surv_time, surv = surv_prob, upper = NA, lower = NA, 
                    std.err = NA) %>% as.data.frame()
ggsurvplot_df(fit=model1_df, surv.geom=geom_line, pval=TRUE, conf.int=TRUE, 
              surv.median.line="v", break.time.by=5)

capture.output(summary(model1), 
               file="D:/Jongwon's/Political Science/overall military.doc", 
               append=TRUE)

# 2. comparison with party and without party
## survival model: univariate

model2=survfit(Surv(duryear,status)~mpdummy,data=military,conf.type="log-log")
summary(model2)
quantile(model2,probs=c(0.25,0.5,0.75),conf.int=T)
survdiff(Surv(duryear,status)~mpdummy, data=military)
ggsurvplot(model2, data=military, pval=TRUE, conf.int=TRUE, 
           surv.median.line="v", break.time.by=5)

capture.output(summary(model2), 
               file="D:/Jongwon's/Political Science/kaplan maier party.doc", 
               append=TRUE)

model2_2=survfit(Surv(duryear,status)~majordummy,data=military,
                 conf.type="log-log")
ggsurvplot(model2_2, data=military, conf.int=TRUE, surv.median.line="v",
           break.time.by=5)

# 3. multivariate analysis
## variable choice in Cox's model
model3 <- coxph(Surv(duryear,status)~mpdummy+loggdp1+loggpc10+gg100+mgr100+opec
                +ethnic100+region,data=military)
summary(model3)
ggforest(model3, data=military)

model4 <- coxph(Surv(duryear,status)~majordummy+loggdp1+loggpc10+gg100+mgr100
                +opec+ethnic100+region,data=military)
summary(model4)
ggforest(model4, data=military)

stargazer(model3,model4, type="html", digits=1, out="table1.doc",
          flip=TRUE)

# 4. linear regression duration and party
duration1 <- lm(duryear~mpdummy+loggdp1+loggpc10+gg100+mgr100+opec+region+
                        ethnic100,data=military)
car::vif(duration1)
sqrt(car::vif(duration1)) > 2
summary(duration1)

duration2 <- lm(duryear~seat100_1+loggdp1+loggpc10+gg100+mgr100+opec+
                        region+ethnic100,data=military)
car::vif(duration2)
sqrt(car::vif(duration2)) > 2
summary(duration2)

duration3 <- lm(duryear~majordummy+loggdp1+loggpc10+gg100+mgr100+opec+
                        region+ethnic100,data=military)
car::vif(duration3)
sqrt(car::vif(duration3)) > 2
summary(duration3)

stargazer(duration1,duration2,duration3,type="html", digits=1, out="table2.doc", 
          flip=TRUE)

plot(military$mpdummy, military$duryear, cex=.25, type="n", 
     xlab = "Military Party or Not", ylab = "Regime Duration")
points(military$mpdummy, military$duryear, cex=.5, col="blue")
abline(lm(military$duryear ~ military$mpdummy), lty=2, col="red", 
       untf = TRUE)

#5. linear regression seat share
seat1 <- lm(seat100_1~yeardiff+timediff+loggdp1+loggpc10+gg100+mgr100+opec
            +ethnic100,data=military)
car::vif(seat1)
sqrt(car::vif(seat1)) > 2
summary(seat1)

seat2 <- lm(seat100_1~yeardiff+seat100_2+timediff+loggdp1+loggpc10+gg100+mgr100
            +opec+ethnic100,data=military)
car::vif(seat2)
sqrt(car::vif(seat1)) > 2
summary(seat2)

seat3 <- lm(seat100_1~yeardiff+seat100_3+timediff+loggdp1+loggpc10+gg100+mgr100
            +opec+ethnic100,data=military)
car::vif(seat3)
sqrt(car::vif(seat3)) > 2
summary(seat3)

stargazer(seat1,seat2,seat3, type="html", digits=1, out="table3.doc", flip=TRUE)

plot(military$yeardiff, military$seat100_1, cex=.25, type="n", 
     xlab = "Year Difference between Nation Building and Military Dictatorship", 
     ylab = "Seat Share (%)")
points(military$yeardiff, military$seat100_1, cex=.5, col="blue")
abline(lm(military$seat100_1 ~ military$yeardiff), lty=2, col="red", 
       untf = TRUE)

military2 <- read.csv("D:/Jongwon's/Political Science/OH_Military_Data(2).csv")
stargazer(military2, type = "html", 
          summary.stat = c("min", "median", "max", "mean", "sd","n"), 
          title = "Descriptive Statistics", 
          digits = 1, out = "descriptive statistics.doc", flip = TRUE)
