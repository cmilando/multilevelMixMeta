## shows how to get BLUP for multi-level models

library(mixmeta)
library(tidyverse)

head(school)

# STANDARD META-ANALYSIS (NB: random NOT STRICTLY NEEDED HERE)
yearcen2 <- with(school, year - mean(tapply(year, district, mean)))

mod4 <- mixmeta(effect ~ yearcen2, var, random= ~ 1|district/study, data=school,
                method="ml")

mod4$random
school

bp0 <- as.data.frame(blup(mod4, se = T, level = 2)) ## level 2 = study
bp0$study <- school$study
bp0$district <- school$district
bp0
dim(bp0)

bp1 <- as.data.frame(blup(mod4, se = T, level = 1)) ## level 1 = district
bp1$district <- school$district
bp1 <- unique(bp1)

bp1

# plot to make sure you aren't crazy
ggplot(school) +
  geom_pointrange(aes(x = factor(district), 
                      y = effect,
                      group = study,
                      ymin = effect - sqrt(var), 
                      ymax = effect + sqrt(var)),
                  position = position_dodge(0.5)) +
  geom_pointrange(data = bp0, 
    aes(x = factor(district), 
                      y = blup,
                      group = study,
                      ymin = blup - se, 
                      ymax = blup + se), color = 'red',
                  position = position_dodge(0.5)) +
  geom_pointrange(data = bp1, size = 1, shape = 19,
                  aes(x = factor(district), 
                      y = blup,
                      ymin = blup - se, 
                      ymax = blup + se), color = 'blue')






