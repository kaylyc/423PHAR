library(survival)
library(ggplot2)
library(tidyverse)
library(survminer)

df <- ovarian

#-----------Kaplan Meier Curve------

#Create Survival Curve
#~1 only draws 1 KM curve --> no grouping variable
km0 <- survfit(Surv(futime, fustat)~1, data = df)
summary(km0)


#Generate plot
plot(km0)
ggsurvplot(km0)

#Group by treatment (~rx)
#pval - Log Rank Test: tests null hypotethsis for sampling variability
km1 <- survfit(Surv(futime, fustat)~rx, data = df)
summary(km1)
ggsurvplot(km1, pval = T)


#Focus on futime and fustat (0=censored)
hist(df$futime)
summary(df$futime)

hist(df$age)
summary(df$age)

#Make age a binary outcome (>=50 OR <50 )
df <- df %>% mutate(age_group = ifelse(age >=50, 1, 0))

#Cox proportional - close to 0.3 --> unadjusted
cr0 <- coxph(Surv(futime, fustat)~rx, data = df)
summary(cr0)

#Add age as a confounder --> p-value OR Pr(>|z|) becomes significant
#Interpretation: maybe older people are more likely to get this treatment --> older people have worse outcomes
#Interpreation: naive comparisons make rx look bad 
cr1 <- coxph(Surv(futime, fustat)~rx+age_group, data = df)
summary(cr1)
df$rx2 <- ifelse(df$rx==2, 1, 0)

glm(rx~age_group, damily = binomial(link='logit'),data = df)


#Create cox regression
#~first one (rx) is variable of interest and control for other confounders
#Interpretation: rx_exp(coef) = 75% reduction in risk of ovarian cancer with this treatment after controlling for other variables
cr2 <- coxph(Surv(futime, fustat)~rx+age_group+resid.ds+ecog.ps, data = df)
summary(cr2)


#Modeling Interactions
#Account for interaction: if treatment and tumor severity grade where the combined effect is more than the sum of individual effect
#Only look at p-value --> when p value < 0.05 cannot reject that the combined interaction the non-significant (tumour grade does not affect outcome)
#Instead of stratification --> create another coefficient to model for combined interaction
crx <- coxph(Surv(futime, fustat)~rx+age_group+resid.ds+ecog.ps+rx*resid.ds, data = df)



x <- summary(cr2)
y <- x$coefficients

ln_hr <- y[1,1]
se_ln_hr <- y[1,3]

ln_ci_ul <- ln_hr+1.96*se_ln_hr
ln_ci_ll <- ln_hr-1.96*se_ln_hr

#paste0"HR:", 
#      round(exp(ln_hr),3),
#      " (95%CI ", 
#      round(exp(ln_ci_ll),0),
#      "-", 
#      round(exp(ln_ci_ul),")")


#Generate forest plot
ggforest(cr2)


