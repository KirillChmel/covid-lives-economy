################################################################################
################################################################################
### 2. Replication File - Statistical Tests

library(dplyr)
library(mosaic)
library(effsize)


### Use CKS-Study1.Rdata to replicate Study 1
### Use CKS-Study2.Rdata to replicate Study 2

study1 <- readRDS('CKS-Study1.Rdata')
study2 <- readRDS('CKS-Study2.Rdata')



################################################################################
### Study 1

### Group means and standard deviations - Saving Lives
study1 |>
  group_by(t_lives) |>
  summarise(mean = mean(dv_approval, na.rm = T), sd = sd(dv_approval, na.rm = T))

study1 |>
  group_by(t_lives) |>
  summarise(mean = mean(dv_support, na.rm = T), sd = sd(dv_support, na.rm = T))

study1 |>
  group_by(t_lives) |>
  summarise(mean = mean(dv_voting, na.rm = T), sd = sd(dv_voting, na.rm = T))

### t-tests - Saving Lives
t.test(dv_approval ~ t_lives, data = study1, var.equal = TRUE)
t.test(dv_support ~ t_lives, data = study1, var.equal = TRUE)
t.test(dv_voting ~ t_lives, data = study1, var.equal = TRUE)

### Cohen’s d - Saving Lives
cohen.d(dv_approval ~ t_lives, data = study1)
cohen.d(dv_support ~ t_lives, data = study1)
cohen.d(dv_voting ~ t_lives, data = study1)


### Group means and standard deviations - Saving the Economy
study1 |>
  group_by(t_economy) |>
  summarise(mean = mean(dv_approval, na.rm = T), sd = sd(dv_approval, na.rm = T))

study1 |>
  group_by(t_economy) |>
  summarise(mean = mean(dv_support, na.rm = T), sd = sd(dv_support, na.rm = T))

study1 |>
  group_by(t_economy) |>
  summarise(mean = mean(dv_voting, na.rm = T), sd = sd(dv_voting, na.rm = T))

### t-tests - Saving the Economy
t.test(dv_approval ~ t_economy, data = study1, var.equal = TRUE)
t.test(dv_support ~ t_economy, data = study1, var.equal = TRUE)
t.test(dv_voting ~ t_economy, data = study1, var.equal = TRUE)

### Cohen’s d - Saving Lives
cohen.d(dv_approval ~ t_economy, data = study1)
cohen.d(dv_support ~ t_economy, data = study1)
cohen.d(dv_voting ~ t_economy, data = study1)



################################################################################
### Study 2

### Group means and standard deviations - Saving Lives
study2 |>
  group_by(t_lives) |>
  summarise(mean = mean(dv_approval, na.rm = T), sd = sd(dv_approval, na.rm = T))

study2 |>
  group_by(t_lives) |>
  summarise(mean = mean(dv_support, na.rm = T), sd = sd(dv_support, na.rm = T))

study2 |>
  group_by(t_lives) |>
  summarise(prop = mean(dv_voting, na.rm = T)*100)

### t-tests and z-rest - Saving Lives
t.test(dv_approval ~ t_lives, data = study2, var.equal = TRUE)
t.test(dv_support ~ t_lives, data = study2, var.equal = TRUE)
prop.test(~dv_voting | t_lives, data = study2, 
          conf.level = 0.95, success = 1, correct = F)

### Cohen’s d - Saving Lives
cohen.d(dv_approval ~ t_lives, data = study2)
cohen.d(dv_support ~ t_lives, data = study2)


### Group means and standard deviations - Saving the Economy
study2 |>
  group_by(t_economy) |>
  summarise(mean = mean(dv_approval, na.rm = T), sd = sd(dv_approval, na.rm = T))

study2 |>
  group_by(t_economy) |>
  summarise(mean = mean(dv_support, na.rm = T), sd = sd(dv_support, na.rm = T))

study2 |>
  group_by(t_economy) |>
  summarise(prop = mean(dv_voting, na.rm = T)*100)

### t-tests and z-test - Saving the Economy
t.test(dv_approval ~ t_economy, data = study2, var.equal = TRUE)
t.test(dv_support ~ t_economy, data = study2, var.equal = TRUE)
prop.test(~dv_voting | t_economy, data = study2, 
          conf.level = 0.95, success = 1, correct = F)

### Cohen’s d - Saving Lives
cohen.d(dv_approval ~ t_economy, data = study2)
cohen.d(dv_support ~ t_economy, data = study2)
