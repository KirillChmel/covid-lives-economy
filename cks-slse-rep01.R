################################################################################
################################################################################
### 1. Replication File - Tables

library(stargazer)
library(sandwich)
library(dplyr)
library(aod)


### Use CKS-Study1.Rdata to replicate Study 1
### Use CKS-Study2.Rdata to replicate Study 2

study1 <- readRDS('CKS-Study1.Rdata')
study2 <- readRDS('CKS-Study2.Rdata')



################################################################################
### Table 1 - Group means for each of the experimental conditions (Study 1)

### Group means and standard deviations
study1 |>
  group_by(t_lives, t_economy) |>
  summarise(mean = mean(dv_approval, na.rm = T), sd = sd(dv_approval, na.rm = T))

study1 |>
  group_by(t_lives, t_economy) |>
  summarise(mean = mean(dv_support, na.rm = T), sd = sd(dv_support, na.rm = T))

study1 |>
  group_by(t_lives, t_economy) |>
  summarise(mean = mean(dv_voting, na.rm = T), sd = sd(dv_voting, na.rm = T))

### F-statistics
summary(aov(dv_approval ~ as.factor(treatment), data = study1))
summary(aov(dv_support ~ as.factor(treatment), data = study1))
summary(aov(dv_voting ~ as.factor(treatment), data = study1))

### N in each group
study1 |>
  group_by(t_lives, t_economy) |>
  summarise(n = n())



################################################################################
### Table 2 - OLS regression analysis (Study 1)

### Estimate OLS models
tab_2_m1 <- lm(dv_approval ~ t_lives + t_economy + 
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_econpol + c_healthpol,
               data = study1)
tab_2_m2 <- lm(dv_approval ~ t_lives * t_economy + 
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_econpol + c_healthpol,
               data = study1)

tab_2_m3 <- lm(dv_support ~ t_lives + t_economy +
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_econpol + c_healthpol,
               data = study1)
tab_2_m4 <- lm(dv_support ~ t_lives * t_economy +
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_econpol + c_healthpol,
               data = study1)

tab_2_m5 <- lm(dv_voting ~ t_lives + t_economy +
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_econpol + c_healthpol,
               data = study1)
tab_2_m6 <- lm(dv_voting ~ t_lives * t_economy +
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_econpol + c_healthpol,
               data = study1)

### Robust standard errors
tab_2_m1_SE <- sqrt(diag(vcovHC(tab_2_m1, type = "HC2")))
tab_2_m2_SE <- sqrt(diag(vcovHC(tab_2_m2, type = "HC2")))
tab_2_m3_SE <- sqrt(diag(vcovHC(tab_2_m3, type = "HC2")))
tab_2_m4_SE <- sqrt(diag(vcovHC(tab_2_m4, type = "HC2")))
tab_2_m5_SE <- sqrt(diag(vcovHC(tab_2_m5, type = "HC2")))
tab_2_m6_SE <- sqrt(diag(vcovHC(tab_2_m6, type = "HC2")))

### Report the table
stargazer(tab_2_m1, tab_2_m2, tab_2_m3,
          tab_2_m4, tab_2_m5, tab_2_m6,
          se = list(tab_2_m1_SE, tab_2_m2_SE, tab_2_m3_SE,
                    tab_2_m4_SE, tab_2_m5_SE, tab_2_m6_SE),
          no.space = T, type = 'text',
          keep = c('t_lives1', 't_economy1', 'Constant'),
          star.char = c("†", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

### Wald tests
wald.test(b = coef(tab_2_m1), Sigma = vcov(tab_2_m1), Terms = 2:3)
wald.test(b = coef(tab_2_m3), Sigma = vcov(tab_2_m3), Terms = 2:3)
wald.test(b = coef(tab_2_m5), Sigma = vcov(tab_2_m5), Terms = 2:3)



################################################################################
### Table 3 - Group means for each of the experimental conditions (Study 2)

### Group means and standard deviations
study2 |>
  group_by(t_lives, t_economy) |>
  summarise(mean = mean(dv_approval, na.rm = T), sd = sd(dv_approval, na.rm = T))

study2 |>
  group_by(t_lives, t_economy) |>
  summarise(mean = mean(dv_support, na.rm = T), sd = sd(dv_support, na.rm = T))

study2 |>
  group_by(t_lives, t_economy) |>
  summarise(prop = mean(dv_voting, na.rm = T)*100)

### F-statistics
summary(aov(dv_approval ~ as.factor(treatment), data = study2))
summary(aov(dv_support ~ as.factor(treatment), data = study2))
chisq.test(study2$dv_voting, study2$treatment)

### N in each group
study2 |>
  group_by(t_lives, t_economy) |>
  summarise(n = n())



################################################################################
### Table 4 - OLS regression analysis (Study 2)

### Estimate OLS models
tab_4_m1 <- lm(dv_approval ~ t_lives + t_economy + 
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_policy, 
               data = study2)
tab_4_m2 <- lm(dv_approval ~ t_lives * t_economy + 
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_policy, 
               data = study2)

tab_4_m3 <- lm(dv_support ~ t_lives + t_economy + 
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_policy, 
               data = study2)
tab_4_m4 <- lm(dv_support ~ t_lives * t_economy + 
                 c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                 c_fear + c_financ + c_trust + c_policy, 
               data = study2)

### Estimate GLM models
tab_4_m5 <- glm(dv_voting ~ t_lives + t_economy + 
                  c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                  c_fear + c_financ + c_trust + c_policy,
                data = study2, family=binomial(link='logit'))
tab_4_m6 <- glm(dv_voting ~ t_lives * t_economy + 
                  c_gender + c_age + c_educ + c_empl + c_income + c_health + 
                  c_fear + c_financ + c_trust + c_policy,
                data = study2, family=binomial(link='logit'))

### Robust standard errors for OLS and standard errors for GLM
tab_4_m1_SE <- sqrt(diag(vcovHC(tab_4_m1, type = "HC2")))
tab_4_m2_SE <- sqrt(diag(vcovHC(tab_4_m2, type = "HC2")))
tab_4_m3_SE <- sqrt(diag(vcovHC(tab_4_m3, type = "HC2")))
tab_4_m4_SE <- sqrt(diag(vcovHC(tab_4_m4, type = "HC2")))
tab_4_m5_SE <- coef(summary(tab_4_m5))[,2]
tab_4_m6_SE <- coef(summary(tab_4_m6))[,2]

### Report the table
stargazer(tab_4_m1, tab_4_m2, tab_4_m3,
          tab_4_m4, tab_4_m5, tab_4_m6,
          se = list(tab_4_m1_SE, tab_4_m2_SE, tab_4_m3_SE,
                    tab_4_m4_SE, tab_4_m5_SE, tab_4_m6_SE),
          no.space = T, type = 'text',
          keep = c('t_lives', 't_economy', 'Constant'),
          star.char = c("†", "*", "**", "***"),
          star.cutoffs = c(.1, .05, .01, .001))

### Wald tests
wald.test(b = coef(tab_4_m1), Sigma = vcov(tab_4_m1), Terms = 2:3)
wald.test(b = coef(tab_4_m3), Sigma = vcov(tab_4_m3), Terms = 2:3)
wald.test(b = coef(tab_4_m5), Sigma = vcov(tab_4_m5), Terms = 2:3)
