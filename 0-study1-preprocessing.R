########################################################################
### Raw Data Pre-processing / Study 1 - CATI

library(foreign)
library(dplyr)


### load the data with complete responses
dt.complete <- read.spss('spss_202103141419_fe39a77c6b918d90.sav',
                         to.data.frame = TRUE, use.value.labels = FALSE)

### load the data with incomplete responses
dt.partial  <- read.spss('spss_202103221018_47b9a88ff2d72105.sav',
                         to.data.frame = TRUE, use.value.labels = FALSE)

### subset partial completes (see AAPOR 2016)
dt.partial  <- subset(dt.partial, !is.na(dt.partial$Q1003))

### merge data
dt <- rbind(dt.complete, dt.partial)
dt[dt == 99] <- NA

### rm data objects
rm(dt.complete); rm(dt.partial)
s
### select variables for the analysis
dt <- dt %>% select(Q1001, Q361, Q362, Q363, Q364, Q37, Q38,
                    Q1, Q2, Q22, Q23, Q24_3, Q28_1, Q28_2, 
                    Q71, Q72, Q73, Q77, Q78, Q81)

### name the variables
names(dt) <- c('assignment', 'exp_gr1', 'exp_gr2', 'exp_gr3', 'exp_gr4', 
               'dv_2', 'dv_3', 'sex', 'age', 'covid-fear', 'ch-econ-status', 
               'pol-trust', 'policy-eval-econ', 'policy-eval-health', 'health-stat',
               'educ', 'employment', 'income', 'region', 'settlement-size')



