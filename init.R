library(tidyverse)
library(lubridate)
library(survival)
library(survminer)

library(corrplot)
library(RColorBrewer)
M <-cor(for_model3[c('price','rosstat_price','event','duration',
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
plot(survfit(formula = Surv(for_model3$AGE, for_model3$event == 1) ~ 1))
#init <- 
res.cox1 <- coxph(Surv(AGE, event) ~ rosstat_price  + 
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
                    crisis + tabak_tax_rise, data =  for_model3, id =IDIND)
summary(res.cox1)

stargazer(res.cox1,
          column.labels=c("init"), align=TRUE
          , type="text"
) 




int <- subset(for_model3, subset = event == 1)
S.KM <- survfit(Surv(AGE, event) ~ 1, data = int)
H.NA <- survfit(Surv(AGE, event) ~ 1, data = int,type = "fleming-harrington")

par(mfrow = c(1, 2))
plot(S.KM, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator", mark.time = FALSE)
plot(H.NA, xlab = "Time (years)", ylab = "Cumulative Hazard of initiation",
     main = "Nelson-Aalen Estimator", fun = "cumhaz", mark.time = FALSE)
# 1998-2008
int98 <- subset(int, subset = YEAR <= 2008)
int08 <- subset(int, subset = YEAR >= 2008)
int08 <- subset(int, subset = YEAR <= 2013)
S.KM1 <- survfit(Surv(AGE, event) ~ 1, data = int98)
S.KM2 <- survfit(Surv(AGE, event) ~ 1, data = int08)

par(mfrow = c(1, 2))
plot(S.KM1, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator 1998-2008", mark.time = FALSE)
plot(S.KM2, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator 2008-2013", mark.time = FALSE)

# 2008-2014- 2020
int14 <- subset(int, subset = YEAR <= 2014)
int14 <- subset(int, subset = YEAR >= 2008)
int20 <- subset(int, subset = YEAR <= 2020)
S.KM1 <- survfit(Surv(AGE, event) ~ 1, data = int14)
S.KM2 <- survfit(Surv(AGE, event) ~ 1, data = int20)

par(mfrow = c(1, 2))
plot(S.KM1, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator 2008-2014", mark.time = FALSE)
plot(S.KM2, xlab = "Time (years)", ylab = "Survival Probability of initiation",
     main = "Kaplan-Meier Estimator 2014-2020", mark.time = FALSE)


# строим стандыртный модели пул, рандомные, фискированные
ols <-lm(AGE ~ rosstat_price + 
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
           crisis + tabak_tax_rise, data=int)
summary(ols)
library(regclass)
VIF(ols)


library(plm)
fixed <- plm(AGE ~  price + 
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
               crisis + tabak_tax_rise, data=int, index=c("YEAR"), model="within")
summary(fixed)

random <- plm(AGE ~ rosstat_price + 
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
                crisis + tabak_tax_rise, data=int, index=c( "YEAR"), model="random")
summary(random)
phtest(fixed, random)
stargazer(ols, random, fixed, 
          column.labels=c("OLS","Random","Fixed"), align=TRUE
          , type="text"
)
