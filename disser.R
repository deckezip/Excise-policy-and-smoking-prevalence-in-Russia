library(dplyr)
library(data.table)
library(survival)
library(knitr)
library(tidyverse)
library(lubridate)
library(survival)
library(survminer)






library(devtools)

install_github('kassambara/survminer')

dat <- for_model_start_smoke
colnames(dat)
smokers0 <- subset(for_model_start_smoke, subset = event == 1)
smokers0_before <- subset(smokers0, subset = tabak_tax_rise != 1)
smokers0_after <- subset(smokers0, subset = tabak_tax_rise == 1)
fit <- survfit(formula = Surv(dat$YEAR, dat$event == 1) ~ dat$tabak_tax_rise)
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"),
           ggtheme = theme_minimal())

plot(survfit(formula = Surv(smokers0$duration, smokers0$event == 1) ~ 1))
before <- survfit(formula = Surv(smokers0_before$duration, smokers0_before$event == 1) ~ 1)
plot(survfit(formula = Surv(smokers0_before$duration, smokers0_before$event == 1) ~ 1))
after <- survfit(formula = Surv(smokers0_after$duration, smokers0_after$event == 1) ~ 1)
plot(survfit(formula = Surv(smokers0_after$duration, smokers0_after$event == 1) ~ 1))

plot( before,  col="red" )
par(new=TRUE)
plot( after,  col="blue" )

survdiff(formula = Surv(duration, event) ~ tabak_tax_rise, data = dat)
survdiff(formula = Surv(duration, smokes) ~ tabak_tax_rise, data = for_model_start_smoke)

#####COR PLOT
fit <- survfit(Surv(duration, event) ~ H5, data = smokers0)



library(corrplot)
library(RColorBrewer)
M <-cor(smokers0[c('price','rosstat_price','event','duration',
              'POPUL','EDUC','is_parent','doctor_per_year',
              'active_hours_per_day','is_female', 'beh_1.0','beh_2.0', 'beh_3.0'
              ,'beh_4.0','kind_1.0' ,'kind_3.0','kind_4.0',  
                'kind_5.0','kind_6.0','REGION_1','REGION_9','REGION_10','REGION_12', 'REGION_14',
               # REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
              #  REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                #REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                #REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                #REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                #REGION_142 + REGION_161 + 
              'STATUS_2','STATUS_3','STATUS_4','job_0.0','job_1.0', 
              #  job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                'crisis','tabak_tax_rise')])
#M <- cor(smokers)
corrplot(M)
vif(M)



summary(dat)
survfit(Surv(dat$duration,dat$event ==1)~1)
Surv(dat$YEAR, dat$event==1)
coxph(Surv(YEAR, smokes) ∼ duration  + price + rosstat_price + POPUL + 
EDUC + salary + is_parent + doctor_per_year + active_hours_per_day + is_female + 
beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
 REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
 job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
 crisis + tabak_tax_rise, data = dat)

cess_res.cox <- coxph(Surv(duration, smokes) ~ rosstat_price + 
                   EDUC + is_parent + doctor_per_year + active_hours_per_day + is_female + 
                   beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                   kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                   REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                   REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                   REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                   REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                   REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                   REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                   job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                   crisis + tabak_tax_rise, data =  dat)
res.cox1 <- coxph(Surv(YEAR, event) ~ rosstat_price + POPUL + 
                   EDUC + salary + is_parent + doctor_per_year + active_hours_per_day + is_female + 
                   beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                   kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                   REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                   REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                   REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                   REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                   REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                   REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                   job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                   crisis + tabak_tax_rise, data =  dat,id =IDIND)
summary(cess_res.cox)
summary() 
??tidy
library(broom)

tidy(res.cox1)
stargazer(cess_res.cox,res.cox1,
          column.labels=c("cess","init"), align=TRUE
          , type="text",out = "fit.html"
)


################################################################3

ces <- subset(dat, subset = event == 1)
ces <- subset(ces, subset = duration >= 0)
S.KM <- survfit(Surv(duration, event) ~ 1, data = ces)
H.NA <- survfit(Surv(duration, event) ~ 1, data = ces,type = "fleming-harrington")

par(mfrow = c(1, 2))
plot(S.KM, xlab = "Time (years)", ylab = "Survival Probability of cessation",
     main = "Kaplan-Meier Estimator", mark.time = FALSE)
plot(H.NA, xlab = "Time (years)", ylab = "Cumulative Hazard of cessation",
     main = "Nelson-Aalen Estimator", fun = "cumhaz", mark.time = FALSE)
# 1998-2008
ces98 <- subset(ces, subset = YEAR <= 2008)
ces08 <- subset(ces, subset = YEAR >= 2008)
ces08 <- subset(ces, subset = YEAR <= 2013)
S.KM1 <- survfit(Surv(duration, event) ~ 1, data = ces98)
S.KM2 <- survfit(Surv(duration, event) ~ 1, data = ces08)

par(mfrow = c(1, 2))
plot(S.KM1, xlab = "Time (years)", ylab = "Survival Probability of cessation",
     main = "Kaplan-Meier Estimator 1998-2008", mark.time = FALSE)
plot(S.KM2, xlab = "Time (years)", ylab = "Survival Probability of cessation",
     main = "Kaplan-Meier Estimator 2008-2013", mark.time = FALSE)

# 2008-2014- 2020
int14 <- subset(ces, subset = YEAR <= 2014)
int14 <- subset(ces, subset = YEAR >= 2008)
int20 <- subset(ces, subset = YEAR <= 2020)
S.KM1 <- survfit(Surv(duration, event) ~ 1, data = int14)
S.KM2 <- survfit(Surv(duration, event) ~ 1, data = int20)

par(mfrow = c(1, 2))
plot(S.KM1, xlab = "Time (years)", ylab = "Survival Probability of cessation",
     main = "Kaplan-Meier Estimator 2008-2014", mark.time = FALSE)
plot(S.KM2, xlab = "Time (years)", ylab = "Survival Probability of cessation",
     main = "Kaplan-Meier Estimator 2014-2020", mark.time = FALSE)

# CESSATion
dat1 <- subset(dat, subset = event1 == 1)
dat1 <- subset(dat1, subset = duration1 >= 0)
# 1998-2008
ces98 <- subset(dat1, subset = YEAR <= 2008)
ces08 <- subset(dat1, subset = YEAR >= 2008)
ces08 <- subset(ces08, subset = YEAR <= 2013)
S.KM1 <- survfit(Surv(duration1, event1) ~ 1, data = ces98)
S.KM2 <- survfit(Surv(duration1, event1) ~ 1, data = ces08)

par(mfrow = c(1, 2))
plot(S.KM1, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator 1998-2008", mark.time = FALSE)
plot(S.KM2, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator 2008-2013", mark.time = FALSE)

# 2008-2014- 2020

int14 <- subset(dat1, subset = YEAR <= 2014)
int14 <- subset(int14, subset = YEAR >= 2008)
int20 <- subset(dat1, subset = YEAR <= 2020)
S.KM1 <- survfit(Surv(duration1, event1) ~ 1, data = int14)
S.KM2 <- survfit(Surv(duration1, event1) ~ 1, data = int20)

par(mfrow = c(1, 2))
plot(S.KM1, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator 2008-2014", mark.time = FALSE)
plot(S.KM2, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator 2014-2020", mark.time = FALSE)




#################################################################




library(stargazer)
stargazer(res.cox)
??survival
cox <- coxph(Surv(YEAR, event) ~ duration, data=dat)
cox
??survfit
tfit  <- survfit(Surv(YEAR, event) ~ duration, data=dat, id=STATUS, istate=)
dim(tfit)
plot(tfit, col=1:4, lty=1:4, lwd=2, ylab="Probability in state")


# строим стандыртный модели пул, рандомные, фискированные
ols <-lm(duration ~ rosstat_price + 
           EDUC + is_parent + doctor_per_year + active_hours_per_day + is_female + 
           beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
           kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
           REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
           REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
           REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
           REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
           REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
           REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
           job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
           crisis + tabak_tax_rise, data=smokers0)
summary(ols)
library(regclass)
VIF(ols)
fixed.dum <-lm(duration ~ rosstat_price + 
                 EDUC + is_parent + doctor_per_year + active_hours_per_day + is_female + 
                 beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                 kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                 REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                 REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                 REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                 REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                 REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                 REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                 job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                 crisis + tabak_tax_rise + factor(IDIND), data=smokers0)
summary(fixed.dum)

fixed.year <-lm(duration ~ rosstat_price + 
                  EDUC + is_parent + doctor_per_year + active_hours_per_day + is_female + 
                  beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                  kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                  REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                  REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                  REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                  REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                  REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                  REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                  job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                  crisis + tabak_tax_rise + factor(YEAR), data=smokers0)
summary(fixed.year)


library(plm)
fixed <- plm(duration ~  price + 
               EDUC  + is_parent + doctor_per_year + active_hours_per_day + is_female + 
               beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
               kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
               REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
               REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
               REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
               REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
               REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
               REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
               job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
               crisis + tabak_tax_rise, data=smokers0, index=c("IDIND", "YEAR"), model="within")
summary(fixed)

random <- plm(duration ~ rosstat_price + 
                EDUC + is_parent + doctor_per_year + active_hours_per_day + is_female + 
                beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                crisis + tabak_tax_rise, data=smokers0, index=c("IDIND", "YEAR"), model="random")
summary(random)
phtest(fixed, random)
stargazer(ols, random, fixed, 
          column.labels=c("OLS","Random","Fixed"), align=TRUE
          , type="text"
)



########start smoking for_model_start_smoke
dat <- for_model_start_smoke
colnames(dat)
smokers0 <- subset(dat, subset = event1 == 1)
smokers0_before <- subset(smokers0, subset = tabak_tax_rise != 1)
smokers0_after <- subset(smokers0, subset = tabak_tax_rise == 1)
survfit(formula = Surv(dat$YEAR, dat$event1 == 1) ~ 1)
plot(survfit(formula = Surv(smokers0$duration1, smokers0$event1 == 1) ~ 1))
plot(survfit(formula = Surv(smokers0_before$duration, smokers0_before$event == 1) ~ 1))
plot(survfit(formula = Surv(smokers0_after$duration, smokers0_after$event == 1) ~ 1))
fit <- survfit(Surv(duration, event) ~ H5, data = smokers0)

plot(survfit(formula = Surv(smokers0$duration, smokers0$event == 1) ~ 1))
before <- survfit(formula = Surv(smokers0_before$duration, smokers0_before$event == 1) ~ 1)
plot(survfit(formula = Surv(smokers0_before$duration, smokers0_before$event == 1) ~ 1))
after <- survfit(formula = Surv(smokers0_after$duration, smokers0_after$event == 1) ~ 1)
plot(survfit(formula = Surv(smokers0_after$duration, smokers0_after$event == 1) ~ 1))

plot( before,  col="red" )
par(new=TRUE)
plot( after,  col="blue" )

plot( before,  col="red" )
plot( after,  col="blue" , add = TRUE)


summary(dat)
survfit(Surv(dat$duration1,dat$event1 ==1)~1)
Surv(dat$YEAR, dat$event1==1)

res.cox <- coxph(Surv(YEAR, not_smokes) ~ duration1  + price + rosstat_price + POPUL + 
                   EDUC + salary + is_parent + doctor_per_year + active_hours_per_day + is_female + 
                   beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                   kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                   REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                   REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                   REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                   REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                   REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                   REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                   job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                   crisis + tabak_tax_rise, data =  dat)
res.cox1 <- coxph(Surv(YEAR, event1) ~ duration1  + rosstat_price + 
                    EDUC +  is_parent + doctor_per_year + active_hours_per_day + is_female + 
                    beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                    kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                    REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                    REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                    REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                    REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                    REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                    REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                    job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                    crisis + tabak_tax_rise, data =  dat)
summary(res.cox)
summary(res.cox1)
library(stargazer)
stargazer(res.cox)
??survival
cox <- coxph(Surv(YEAR, event) ~ duration, data=dat)
cox
??survfit
tfit  <- survfit(Surv(YEAR, event) ~ duration, data=dat, id=STATUS, istate=)
dim(tfit)
plot(tfit, col=1:4, lty=1:4, lwd=2, ylab="Probability in state")


stargazer(res.cox, res.cox1, 
          column.labels=c("Cessation","Initiation"), align=TRUE
          #, type="text"
          )

# строим стандыртный модели пул, рандомные, фискированные
ols <-lm(duration1 ~ YEAR + price + rosstat_price + POPUL + 
           EDUC + salary + is_parent + doctor_per_year + active_hours_per_day + is_female + 
           beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
           kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
           REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
           REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
           REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
           REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
           REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
           REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
           job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
           crisis + tabak_tax_rise, data=smokers0)
summary(ols)


fixed.dum <-lm(duration ~ YEAR + price + rosstat_price + POPUL + 
                 EDUC + salary + is_parent + doctor_per_year + active_hours_per_day + is_female + 
                 beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                 kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                 REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                 REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                 REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                 REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                 REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                 REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                 job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                 crisis + tabak_tax_rise + factor(IDIND), data=for_model2)
summary(fixed.dum)

fixed.year <-lm(duration ~ YEAR + price + rosstat_price + POPUL + 
                  EDUC + salary + is_parent + doctor_per_year + active_hours_per_day + is_female + 
                  beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                  kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                  REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                  REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                  REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                  REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                  REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                  REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                  job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                  crisis + tabak_tax_rise + factor(YEAR), data=smokers0)
summary(fixed.year)


library(plm)
fixed <- plm(duration ~ rosstat_price + 
               EDUC + salary + is_parent + doctor_per_year + active_hours_per_day + is_female + 
               beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
               kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
               REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
               REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
               REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
               REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
               REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
               REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
               job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
               crisis + tabak_tax_rise, data=smokers0, index=c("IDIND", "YEAR"), model="within")
summary(fixed)

random <- plm(duration ~  price + rosstat_price + POPUL + 
                EDUC + salary + is_parent + doctor_per_year + active_hours_per_day + is_female + 
                beh_1.0 + beh_2.0 + beh_3.0 + beh_4.0 + kind_1.0 + kind_3.0 + kind_4.0 + 
                kind_5.0 + kind_6.0 + REGION_1 + REGION_9 + REGION_10 + REGION_12 + REGION_14 + 
                REGION_33 + REGION_39 + REGION_45 + REGION_46 + REGION_47 + REGION_48 + REGION_52+
                REGION_58 + REGION_66 + REGION_67 + REGION_70 + REGION_71 + REGION_72 + REGION_73 +
                REGION_77 + REGION_84 + REGION_86 + REGION_89 + REGION_92 + REGION_93 + REGION_100 +
                REGION_105 + REGION_106 + REGION_107 + REGION_116 + REGION_117 + REGION_129 + 
                REGION_135 + REGION_136 + REGION_137 + REGION_138 + REGION_140 + REGION_141 +
                REGION_142 + REGION_161 + STATUS_2 + STATUS_3 + STATUS_4 + job_0.0 + job_1.0 + 
                job_2.0 + job_3.0 + job_4.0 + job_5.0 + job_6.0 + job_7.0 + job_8.0 + job_9.0 + 
                crisis + tabak_tax_rise, data=smokers0, index=c("IDIND", "YEAR"), model="random")
summary(random)


stargazer(ols,fixed.year,
          column.labels=c("OLS","Fixed"), align=TRUE
          #, type="text"
          )








################################################################################
#install_github('graemeleehickey/joineRML')
library('joineRML')
library("JM")

# random-intercepts model for the AIDS dataset
lmeFit.int <- lme(duration ~ YEAR, random = ~ 1 | IDIND,data = for_model1)
summary(lmeFit.int)

# marginal covariance matrix for Patient 12
margCov.int <- getVarCov(lmeFit.int, individuals = 12,
                         type = "marginal")

margCov.int
cov2cor(margCov.int[[1]])

# random-intercepts and random-slopes model for the AIDS dataset
lmeFit.slp <- lme(duration ~ YEAR, random = ~ YEAR | IDIND, data = for_model1)
summary(lmeFit.slp)
# marginal covariance matrix for Patient 12
margCov.slp <- getVarCov(lmeFit.slp, individuals = 12,
                         type = "marginal")

margCov.slp
cov2cor(margCov.slp[[1]])


#################
# Section 3.2.1 #
#################Surv(YEAR, event) ~ duration, data=dat
smokers0 <- subset(for_model_start_smoke, subset = event1 == 1)
S.KM <- survfit(Surv(duration1, event1) ~ 1, data = smokers0)
H.NA <- survfit(Surv(duration1, event1) ~ 1, data = smokers0,type = "fleming-harrington")

par(mfrow = c(1, 2))
plot(S.KM, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator", mark.time = FALSE)
plot(H.NA, xlab = "Time (years)", ylab = "Cumulative Hazard of initiation",
     main = "Nelson-Aalen Estimator", fun = "cumhaz", mark.time = FALSE)


smokers0 <- subset(for_model_start_smoke, subset = event == 1)
S.KM <- survfit(Surv(duration, event) ~ 1, data = smokers0)
H.NA <- survfit(Surv(duration, event) ~ 1, data = smokers0,type = "fleming-harrington")

par(mfrow = c(1, 2))
plot(S.KM, xlab = "Time (years)", ylab = "Survival Probability of cessation",
     main = "Kaplan-Meier Estimator", mark.time = FALSE)
plot(H.NA, xlab = "Time (years)", ylab = "Cumulative Hazard of cessation",
     main = "Nelson-Aalen Estimator", fun = "cumhaz", mark.time = FALSE)