library(tidyverse)
library(ggthemes)
library(ggpubr)

theme_set(theme_bw())

source("scripts/get-sneox-2024-data.R")
rm(list = setdiff(ls(), "sensofar.data"))
parameters = colnames(sensofar.data[,6:19])


### variation in measurements within artifacts
param.cov = sensofar.data %>% group_by(Id_number, Weathering_class, surface_class) %>%
  summarize(
    Spc = abs(sd(Spc)/mean(Spc)), 
    Spd = abs(sd(Spd)/mean(Spd)), 
    S5v = abs(sd(S5v)/mean(S5v)), 
    Sq = abs(sd(Sq)/mean(Sq)), 
    Sp = abs(sd(Sp)/mean(Sp)), 
    Sv = abs(sd(Sv)/mean(Sv)), 
    Sa = abs(sd(Sa)/mean(Sa)), 
    Ssk = abs(sd(Ssk)/mean(Ssk)), 
    Sal = abs(sd(Sal)/mean(Sal)), 
    Sk = abs(sd(Sk)/mean(Sk)), 
    Spk = abs(sd(Spk)/mean(Spk)), 
    Smr1 = abs(sd(Smr1)/mean(Smr1)), 
    Smr2 = abs(sd(Smr2)/mean(Smr2)), 
    Vvv = abs(sd(Vvv)/mean(Vvv))
  ) %>% 
  pivot_longer(c(4:17), names_to = "parameter", values_to = "CoV")

ggplot(param.cov) +
  geom_point(aes(x = Id_number, y = CoV, color = surface_class)) +
  facet_wrap(~parameter)

ggplot(param.cov) +
  geom_boxplot(aes(x = parameter, y = CoV, group = parameter)) +
  geom_boxplot(data=param.cov %>% filter(parameter %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvv")), 
               aes(x = parameter, y = CoV, group = parameter), color = "red") +
  facet_wrap(~surface_class)


pvals = psych::corr.test(sensofar.data %>% select_at(parameters))$p
corrplot::corrplot(cor(sensofar.data %>% select_at(parameters), use = "pairwise.complete.obs"),
                   method = "square", addCoef.col = "black", 
                   col = scales::alpha(corrplot::COL2("RdBu"), alpha = 0.85),
                   p.mat = pvals, insig="blank", sig.level=0.1, 
                   type = "lower", number.cex = 0.5, diag=F)

param.var = sensofar.data %>% group_by(Id_number, Weathering_class, surface_class) %>%
  summarize(
    Spc = var(Spc), 
    Spd = var(Spd), 
    S5v = var(S5v), 
    Sq = var(Sq), 
    Sp = var(Sp), 
    Sv = var(Sv), 
    Sa = var(Sa), 
    Ssk = var(Ssk), 
    Sal = var(Sal), 
    Sk = var(Sk), 
    Spk = var(Spk), 
    Smr1 = var(Smr1), 
    Smr2 = var(Smr2), 
    Vvv = var(Vvv)
  ) %>% 
  pivot_longer(c(4:17), names_to = "parameter", values_to = "var")

var.plot = ggplot(param.var) +
  geom_boxplot(aes(x = parameter, y = var, group = parameter)) +
  geom_boxplot(data=param.var %>% filter(parameter %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvv")), 
               aes(x = parameter, y = var, group = parameter), color = "red") +
  facet_wrap(~surface_class)
ggsave(filename = "figures/SM_parameter-variation.png", plot = var.plot, 
       dpi = 300, width = 8, height = 4)


