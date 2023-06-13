library(tidyverse)
library(ggthemes)
library(ggpubr)
library(factoextra)
library(nlme)
library(lme4)
library(broom)
library(broom.mixed)

theme_set(theme_bw())

data = read_csv("data/Sneox2_microscopy_artifacts.csv") %>%
  mutate(lw_pvdif_c = abs(lw_Sp_c - lw_Sv_c), 
         mw_pvdif_c = abs(mw_Sp_c - mw_Sv_c))

sdata = data  %>%
  pivot_longer(cols = 41:52, names_to = "measurement", values_to = "value") %>%
  separate(col = "measurement", into = c("scar_weathering", "measurement", "correction"), sep = "_")
sdata$measurement = factor(sdata$measurement, levels = c("Sq", "Sp", "Sv", "pvdif", "Spc", "Spd"))
sdata$Weathering_class = factor(sdata$Weathering_class, levels = c("strongly_weathered", "mildly_weathered", "weakly_weathered"))

####Data comparisons####

#Corrected Data comparison
ggplot(sdata) +
  geom_boxplot(aes(x = scar_weathering, y = value)) +
  facet_wrap(~measurement, scales = "free") +
  stat_compare_means(aes(x = scar_weathering, y = value))

ggplot(sdata) +
  geom_point(aes(x = scar_weathering, y = value, color = Weathering_class)) +
  geom_line(aes(x = scar_weathering, y = value, group = Id_number, color = Weathering_class)) +
  facet_grid(measurement~Id_number, scales = "free")


#####Comparisons -- corrected data#####
Spc_c = sdata %>% filter(measurement == "Spc") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(Spc_diff = mw - lw) %>%
  rename(Spc_mw = mw, 
         Spc_lw = lw)
Spd_c = sdata %>% filter(measurement == "Spd") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(Spd_diff = mw - lw) %>%
  rename(Spd_mw = mw, 
         Spd_lw = lw)
Sq_c = sdata %>% filter(measurement == "Sq") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(Sq_diff = mw - lw) %>%
  rename(Sq_mw = mw, 
         Sq_lw = lw)
pvdiff_c = sdata %>% filter(measurement == "pvdif") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(pvdif_diff = mw - lw) %>%
  rename(pvdif_mw = mw, 
         pvdif_lw = lw)

mwcomp_c = Sq_c %>% left_join(Spd_c, by = "Id_number") %>% left_join(Spc_c, by = "Id_number") %>% left_join(pvdiff_c, by = "Id_number") %>%
  select(Id_number, location.x, Weathering_class.x, Sq_mw, Spc_mw, Spd_mw, pvdif_mw) %>%
  pivot_longer(cols = 4:7, names_to = "measurement", values_to = "value") %>%
  rename(location = location.x, Weathering_class = Weathering_class.x)
mw.labs = c("Spc", "Spd", "Sq", "peak-valley difference")
names(mw.labs) = c("Spc_mw", "Spd_mw", "Sq_mw", "pvdif_mw")
mwcomp_c$measurement = factor(mwcomp_c$measurement, levels = c("Sq_mw", "pvdif_mw", "Spd_mw", "Spc_mw"))

cmw_plot1 = ggplot(mwcomp_c) +
  geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = mw.labs)) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Set1")
plot(cmw_plot1)

cmw_plot2 = ggplot(mwcomp_c) +
  geom_boxplot(aes(x = location, y = value, color = location)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = mw.labs)) +
  labs(x = "degree of weathering") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")
plot(cmw_plot2)


lwcomp_c = Sq_c %>% left_join(Spd_c, by = "Id_number") %>% left_join(Spc_c, by = "Id_number") %>% left_join(pvdiff_c, by = "Id_number") %>%
  select(Id_number, location.x, Weathering_class.x, Sq_lw, Spc_lw, Spd_lw, pvdif_lw) %>%
  pivot_longer(cols = 4:7, names_to = "measurement", values_to = "value") %>%
  rename(location = location.x, Weathering_class = Weathering_class.x)
lw.labs = c("Spc", "Spd", "Sq", "peak-valley difference")
names(lw.labs) = c("Spc_lw", "Spd_lw", "Sq_lw", "pvdif_lw")
lwcomp_c$measurement = factor(lwcomp_c$measurement, levels = c("Sq_lw", "pvdif_lw", "Spd_lw", "Spc_lw"))

clw_plot1 = ggplot(lwcomp_c) +
  geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = lw.labs)) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Set1")
plot(clw_plot1)

clw_plot2 = ggplot(lwcomp_c) +
  geom_boxplot(aes(x = location, y = value, color = location)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = lw.labs)) +
  labs(x = "degree of weathering") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")
plot(clw_plot2)


allcomp_c =  Sq_c %>% left_join(Spd_c, by = "Id_number") %>% left_join(Spc_c, by = "Id_number") %>% left_join(pvdiff_c, by = "Id_number") %>%
  select(Id_number, location.x, Weathering_class.x, Sq_diff, Spc_diff, Spd_diff, pvdif_diff) %>%
  pivot_longer(cols = 4:7, names_to = "measurement", values_to = "value") %>%
  rename(location = location.x, Weathering_class = Weathering_class.x)
diff.labs = c("Spc (more - less)", "Spd (more - less)", "Sq (more - less)", "peak-valley diff (more-less)")
names(diff.labs) = c("Spc_diff", "Spd_diff", "Sq_diff", "pvdif_diff")
allcomp_c$measurement = factor(allcomp_c$measurement, levels = c("Sq_diff", "pvdif_diff", "Spd_diff", "Spc_diff"))
 
cl_plot = ggplot(allcomp_c %>% filter(Weathering_class == "strongly_weathered")) +
  geom_boxplot(aes(x = location, y = value, color = location)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = diff.labs)) +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")
plot(cl_plot)

cw_plot = ggplot(allcomp_c) +
  geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = diff.labs)) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering", colors = "") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Set1")
plot(cw_plot)


###### comparison figures ######
ggsave(
  plot = ggarrange(
    cmw_plot2, clw_plot2,
    ncol = 2, common.legend = T, 
    labels = "AUTO", 
    legend = "none"
  ), 
  filename = "figures/base-measures_location.tiff", 
  dpi = 300, width = 10, height = 6
)

ggsave(
  plot = ggarrange(
    cmw_plot1, clw_plot1, 
    ncol = 2, common.legend = T, 
    labels = "AUTO", 
    legend = "none"
  ), 
  filename = "figures/base-measures_weathering-stage.tiff", 
  dpi = 300, width = 10, height = 6
)

ggsave(
  plot = ggarrange(
    cw_plot, cl_plot, 
    ncol = 2, 
    labels = "AUTO", 
    legend = "none"
  ), 
  filename = "figures/mw-lw-comparisons.tiff", 
  dpi = 300, width = 10, height = 6
)


##### regressions #####
Sq_u = Sq_u %>%
  separate(Id_number, into = c("artifact", "suffix"), sep = "_") %>%
  mutate(
    strongly_weathered = ifelse(Weathering_class == "strongly_weathered", TRUE, FALSE), 
    mildly_weathered = ifelse(Weathering_class == "mildly_weathered", TRUE, FALSE),
    weakly_weathered = ifelse(Weathering_class == "weakly_weathered", TRUE, FALSE), 
    p1 = ifelse(location == "P1", TRUE, FALSE),
    p2 = ifelse(location == "P2", TRUE, FALSE),
    p5 = ifelse(location == "P5", TRUE, FALSE),
    survey = ifelse(location == "survey", TRUE, FALSE),
  )

# fit1 = lmer(diff ~ location + Weathering_class + (1 | artifact), data = Sq_u)
# summary(fit1)
# fit2 = lmer(diff ~ location + Weathering_class + Flake_length + Flake_width + Flake_thickness + Weight + (1 | artifact), data = Sq_u)
# summary(fit2)

fit1.dum = lmer(diff ~ p1 + p2 + p5 + survey + strongly_weathered + mildly_weathered + weakly_weathered + (1 | artifact), data = Sq_u)
summary(fit1.dum)

fixed.sq = tidy(fit1.dum) %>% filter(effect == "fixed") %>%
  mutate(lower_est = estimate - std.error, 
         upper_est = estimate + std.error)
fixed.sq$term = factor(fixed.spc$term, levels = c("(Intercept)", "p1TRUE", "p2TRUE", "p5TRUE", "strongly_weatheredTRUE", "mildly_weatheredTRUE"))

sq.u.p = ggplot(fixed.sq) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = term, y = estimate, color = term)) +
  geom_linerange(aes(x = term, ymin = lower_est, ymax = upper_est, color = term)) +
  coord_flip() +
  ggtitle("Change in Sq") +
  theme(title = element_text(size = 10))
plot(sq.u.p)

Spd_u = Spd_u %>%
  separate(Id_number, into = c("artifact", "suffix"), sep = "_") %>%
  mutate(
    strongly_weathered = ifelse(Weathering_class == "strongly_weathered", TRUE, FALSE), 
    mildly_weathered = ifelse(Weathering_class == "mildly_weathered", TRUE, FALSE),
    weakly_weathered = ifelse(Weathering_class == "weakly_weathered", TRUE, FALSE), 
    p1 = ifelse(location == "P1", TRUE, FALSE),
    p2 = ifelse(location == "P2", TRUE, FALSE),
    p5 = ifelse(location == "P5", TRUE, FALSE),
    survey = ifelse(location == "survey", TRUE, FALSE),
  )

# fit3 = lmer(diff ~ location + Weathering_class + (1 | artifact), data = Spd_u)
# summary(fit3)
# fit4 = lmer(diff ~ location + Weathering_class + Flake_length + Flake_width + Flake_thickness + Weight + (1 | artifact), data = Spd_u)
# summary(fit4)

fit3.dum = lmer(diff ~ p1 + p2 + p5 + survey + strongly_weathered + mildly_weathered + weakly_weathered + (1 | artifact), data = Spd_u)
summary(fit3.dum)

fixed.spd = tidy(fit3.dum) %>% filter(effect == "fixed") %>%
  mutate(lower_est = estimate - std.error, 
         upper_est = estimate + std.error)
fixed.spd$term = factor(fixed.spc$term, levels = c("(Intercept)", "p1TRUE", "p2TRUE", "p5TRUE", "strongly_weatheredTRUE", "mildly_weatheredTRUE"))

spd.u.p = ggplot(fixed.spd) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = term, y = estimate, color = term)) +
  geom_linerange(aes(x = term, ymin = lower_est, ymax = upper_est, color = term)) +
  coord_flip() +
  ggtitle("Change in Spd") +
  theme(title = element_text(size = 10))
plot(spd.u.p)


Spc_u = Spc_u %>%
  separate(Id_number, into = c("artifact", "suffix"), sep = "_") %>%
  mutate(
    strongly_weathered = ifelse(Weathering_class == "strongly_weathered", TRUE, FALSE), 
    mildly_weathered = ifelse(Weathering_class == "mildly_weathered", TRUE, FALSE),
    weakly_weathered = ifelse(Weathering_class == "weakly_weathered", TRUE, FALSE), 
    p1 = ifelse(location == "P1", TRUE, FALSE),
    p2 = ifelse(location == "P2", TRUE, FALSE),
    p5 = ifelse(location == "P5", TRUE, FALSE),
    survey = ifelse(location == "survey", TRUE, FALSE),
  )

# fit5 = lmer(diff ~ location + Weathering_class + (1 | artifact), data = Spc_u)
# summary(fit5)
# fit6 = lmer(diff ~ location + Weathering_class + Flake_length + Flake_width + Flake_thickness + Weight + (1 | artifact), data = Spc_u)
# summary(fit6)

fit5.dum = lmer(diff ~ p1 + p2 + p5 + survey + strongly_weathered + mildly_weathered + weakly_weathered + (1 | artifact), data = Spc_u)
summary(fit5.dum)

fixed.spc = tidy(fit5.dum) %>% filter(effect == "fixed") %>%
  mutate(lower_est = estimate - std.error, 
         upper_est = estimate + std.error)
fixed.spc$term = factor(fixed.spc$term, levels = c("(Intercept)", "p1TRUE", "p2TRUE", "p5TRUE", "strongly_weatheredTRUE", "mildly_weatheredTRUE"))

spc.u.p = ggplot(fixed.spc) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = term, y = estimate, color = term)) +
  geom_linerange(aes(x = term, ymin = lower_est, ymax = upper_est, color = term)) +
  coord_flip() +
  ggtitle("Change in Spc") +
  theme(title = element_text(size = 10))
plot(spc.u.p)
  
ggarrange(sq.u.p, spd.u.p, spc.u.p, 
          ncol = 3, common.legend = T)
