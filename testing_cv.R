
rm(list = ls())
library(tidyverse)
library(parallel)
library(MuMIn)
data(Mass)
data(MassClimate)
#source('R/slidingwin.R')
#source('R/otherfunctions.R')

# Test an absolute window, starting 20 May (refday = c(20, 5))
# Test for climate windows between 100 and 0 days ago (range = c(100, 0))
# Test both mean and max aggregate statistics (stat = c("mean", "max"))
# Fit a linear term (func = "lin")
# Test at the resolution of days (cinterval = "day")
#Mass <- head(Mass)

Mass$climate <- 1 
baseline <- lm( Mass ~ 1, data = Mass)
ix_baseline <- lm(Mass ~ Age*climate, data = Mass)

MassWin_AIC <- slidingwin(xvar = list(Temp = MassClimate$Temp),
                          cdate = MassClimate$Date,
                          bdate = Mass$Date,
                          baseline = ix_baseline,
                          cinterval = "day",
                          range = c(20, 0),
                          type = "absolute", refday = c(20, 05),
                          stat = "mean",
                          func = "lin", 
                          k = 0, cv_by_cohort = F)

MassWin_AIC[[1]]$Dataset %>% View 

MassWin_cv <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
                      bdate = Mass$Date, baseline = ix_baseline,
                      range = c(20, 0),
                      stat = c("mean"), func = "lin",
                      type = "absolute", refday = c(20, 5),
                      cmissing = FALSE, cinterval = "day", cv_by_cohort = T, ncores = 2)

climwin::plotdelta(MassWin_cv[[1]]$Dataset)

MassWin_AIC[[1]]$Dataset %>% head
MassWin_cv[[1]]$Dataset %>% head 
# Binary logistic regression 

Mass$Binary <- Mass$Mass > 130 
Mass$climate <- 1 
baseline_glm <- glm( Binary ~ 1, data = Mass, family = 'binomial')
ix_baseline_glm <- glm(Binary ~ Age*climate, data = Mass, family = 'binomial')

MassWin_cv_glm <- slidingwin(xvar = list(Temp = MassClimate$Temp), cdate = MassClimate$Date,
                         bdate = Mass$Date, baseline = ix_baseline_glm,
                         range = c(20, 0),
                         stat = c("mean"), func = "lin",
                         type = "absolute", refday = c(20, 5),
                         cmissing = FALSE, cinterval = "day", cv_by_cohort = T)

MassWin_AIC_glm <- slidingwin(xvar = list(Temp = MassClimate$Temp),
                          cdate = MassClimate$Date,
                          bdate = Mass$Date,
                          baseline = ix_baseline_glm,
                          cinterval = "day",
                          range = c(20, 0),
                          type = "absolute", refday = c(20, 05),
                          stat = "mean",
                          func = "lin", 
                          k = 0, cv_by_cohort = F)


MassWin_AIC_glm[[1]]$Dataset %>% head
MassWin_cv_glm[[1]]$Dataset %>% head 


# 
# # Examine tested combinations
# View( MassWin_cv[[1]]$Dataset )
# MassWin_cv[[1]]$Dataset[, 1:10] %>% head 
# MassWin_AIC[[1]]$Dataset[, 1:10] %>% head 
# 
# # 
# # MassWin[[1]]$Dataset %>%View 
# # plot( MassWin[[1]]$Dataset$deltaAICc, MassWin[[1]]$Dataset$deltaMSE)
# # 
# 
rm(list = ls())
load('climwinTest.RData')
library(tidyverse)
library(parallel)
library(MuMIn)
library(lme4)
library(lubridate)
library(optimx)

# LMER optimization options
control_lmer = lmerControl(
  optimizer = "optimx",
  calc.derivs = FALSE,
  optCtrl = list(
    method = "nlminb",
    starttests = FALSE,
    kkt = FALSE
  )
)
control_lmer$optCtrl$eval.max <- 1e8
control_lmer$optCtrl$iter.max <- 1e8

m_baseline2 <- lm( area ~ area0*climate + W.intra, data = growth)
#m_baseline <- lmer( area ~ area0*climate + W.intra, data = growth)

model_type2 <- "lm"
a <- Sys.time()

growthWin <- slidingwin(xvar = list(TMAX_scaled = daily_weather$TMAX_scaled),
                        cdate = daily_weather$date_reformat,
                        bdate = growth$date_reformat,
                        baseline = m_baseline,
                        cinterval = "month",
                        range = c(window_open_max, window_open_min),
                        #exclude = c(window_exclude_dur, window_exclude_max),
                        type = "absolute", refday = c(15, 06),
                        stat = 'mean',
                        func = c('lin'))

growthWin[[1]]$Dataset %>% View 


# Try with Survival model 
rm(list = ls())
load('testing_cv_logistic.RData')

library(tidyverse)
library(parallel)
library(MuMIn)
library(lme4)
library(lubridate)
library(optimx)

window_open_max <- 24
window_open_min <- 1

survivesWin <- slidingwin(xvar = list(TMAX_scaled = daily_weather$TMAX_scaled),
                          cdate = daily_weather$date_reformat,
                          bdate = survival$date_reformat,
                          baseline = m_baseline, 
                          cinterval = 'month',
                          range = c(window_open_max, window_open_min),
                          type = "absolute", 
                          refday = c(15, 06),
                          stat = 'mean', 
                          func = c('lin'))

survivesWin[[1]]$Dataset %>% arrange(ModelLogLoss) 

survivesWin[[1]]$Dataset %>% View 
climwin::plotdelta(survivesWin[[1]]$Dataset)
survivesWin[[1]]$Dataset %>% head 
