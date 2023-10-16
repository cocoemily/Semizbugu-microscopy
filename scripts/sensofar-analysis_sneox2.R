library(tidyverse)
library(ggthemes)
library(ggpubr)
library(paletteer)
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
comp = sdata %>% filter(scar_weathering == "mw")

ggplot(comp) +
  geom_point(aes(x = Weathering_class, y = value)) +
  facet_wrap(~measurement, scales = "free")


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

# cmw_plot1 = ggplot(mwcomp_c) +
#   geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
#   geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
#   facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = mw.labs)) +
#   scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
#   labs(x = "degree of weathering", color = "weathering class") +
#   theme(legend.position = "none") +
#   scale_color_brewer(palette = "Set1")
# plot(cmw_plot1)
# 
# cmw_plot2 = ggplot(mwcomp_c) +
#   geom_boxplot(aes(x = location, y = value, color = location)) +
#   geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
#   facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = mw.labs)) +
#   labs(x = "location") +
#   theme(legend.position = "none") +
#   scale_color_brewer(palette = "Dark2")
# plot(cmw_plot2)


lwcomp_c = Sq_c %>% left_join(Spd_c, by = "Id_number") %>% left_join(Spc_c, by = "Id_number") %>% left_join(pvdiff_c, by = "Id_number") %>%
  select(Id_number, location.x, Weathering_class.x, Sq_lw, Spc_lw, Spd_lw, pvdif_lw) %>%
  pivot_longer(cols = 4:7, names_to = "measurement", values_to = "value") %>%
  rename(location = location.x, Weathering_class = Weathering_class.x)
lw.labs = c("Spc", "Spd", "Sq", "peak-valley difference")
names(lw.labs) = c("Spc_lw", "Spd_lw", "Sq_lw", "pvdif_lw")
lwcomp_c$measurement = factor(lwcomp_c$measurement, levels = c("Sq_lw", "pvdif_lw", "Spd_lw", "Spc_lw"))

# clw_plot1 = ggplot(lwcomp_c) +
#   geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
#   geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
#   facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = lw.labs)) +
#   scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
#   labs(x = "degree of weathering", color = "weathering class") +
#   theme(legend.position = "none") +
#   scale_color_brewer(palette = "Set1")
# plot(clw_plot1)
# 
# clw_plot2 = ggplot(lwcomp_c) +
#   geom_boxplot(aes(x = location, y = value, color = location)) +
#   geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
#   facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = lw.labs)) +
#   labs(x = "location") +
#   theme(legend.position = "none") +
#   scale_color_brewer(palette = "Dark2")
# plot(clw_plot2)

 
allcomp_c =  Sq_c %>% left_join(Spd_c, by = "Id_number") %>% left_join(Spc_c, by = "Id_number") %>% left_join(pvdiff_c, by = "Id_number") %>%
  select(Id_number, location.x, Weathering_class.x, Sq_diff, Spc_diff, Spd_diff, pvdif_diff) %>%
  pivot_longer(cols = 4:7, names_to = "measurement", values_to = "value") %>%
  rename(location = location.x, Weathering_class = Weathering_class.x)
diff.labs = c("Spc (more - less)", "Spd (more - less)", "Sq (more - less)", "peak-valley diff (more-less)")
names(diff.labs) = c("Spc_diff", "Spd_diff", "Sq_diff", "pvdif_diff")
allcomp_c$measurement = factor(allcomp_c$measurement, levels = c("Sq_diff", "pvdif_diff", "Spd_diff", "Spc_diff"))
 
# cl_plot = ggplot(allcomp_c %>% filter(Weathering_class == "strongly_weathered")) +
#   geom_boxplot(aes(x = location, y = value, color = location)) +
#   geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
#   facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = diff.labs)) +
#   theme(legend.position = "none") +
#   scale_color_brewer(palette = "Dark2")
# plot(cl_plot)
# 
# cw_plot = ggplot(allcomp_c) +
#   geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
#   geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
#   facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = diff.labs)) +
#   scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
#   labs(x = "degree of weathering", colors = "") +
#   theme(legend.position = "none") +
#   scale_color_brewer(palette = "Set1")
# plot(cw_plot)


mwcomp_c$surface_condition = "more weathered"
lwcomp_c$surface_condition = "less weathered"
lwmw.all = rbind(mwcomp_c, lwcomp_c) %>%
  separate(measurement, into = c("measurement", "sc_abv"), sep = "_")

facet.labs = c("Spc", "Spd", "Sq", "peak-valley difference")
names(facet.labs) = c("Spc", "Spd", "Sq", "pvdif")
lwmw.all$measurement = factor(lwmw.all$measurement, levels = c("Sq", "pvdif", "Spd", "Spc"))

#####Figure 6####
all_plot1 = ggplot(lwmw.all %>% filter(measurement %in% c("Spc", "Spd", "Sq"))) +
  geom_boxplot(aes(x = Weathering_class, y = value, group = interaction(surface_condition, Weathering_class), color = interaction(surface_condition, Weathering_class))) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = facet.labs), 
             ncol = 1, strip.position = "right") +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Paired")
plot(all_plot1)

all_plot2 = ggplot(lwmw.all %>% filter(measurement %in% c("Spc", "Spd", "Sq"))) +
  geom_boxplot(aes(x = location, y = value, group = interaction(surface_condition, location), color = interaction(surface_condition, location))) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = facet.labs), 
             ncol = 1, strip.position = "right") +
  #scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "location", color = "weathering class") +
  theme(legend.position = "none") +
  scale_color_paletteer_d("ggthemes::Summer", direction = -1)
plot(all_plot2)

ggsave(plot = ggarrange(all_plot1, all_plot2,ncol = 2,labels = "AUTO"), 
       filename = "figures/base-measure-comparisons.tiff", 
       dpi = 300, width = 10, height = 9)


###### comparison figures ######
# ggsave(
#   plot = ggarrange(
#     cmw_plot2, clw_plot2,
#     ncol = 2, common.legend = T, 
#     labels = "AUTO", 
#     legend = "none"
#   ), 
#   filename = "figures/base-measures_location.tiff", 
#   dpi = 300, width = 10, height = 6
# )
# 
# ggsave(
#   plot = ggarrange(
#     cmw_plot1, clw_plot1, 
#     ncol = 2, common.legend = T, 
#     labels = "AUTO", 
#     legend = "none"
#   ), 
#   filename = "figures/base-measures_weathering-stage.tiff", 
#   dpi = 300, width = 10, height = 6
# )

# ggsave(
#   plot = ggarrange(
#     cw_plot, cl_plot, 
#     ncol = 2, 
#     labels = "AUTO", 
#     legend = "none"
#   ), 
#   filename = "figures/mw-lw-comparisons.tiff", 
#   dpi = 300, width = 10, height = 6
# )

######point comparisons by artifact####
art.comp = sdata %>% filter(measurement %in% c("Spc", "Spd", "Sq"))
art.comp$measurement = factor(art.comp$measurement, levels = c("Sq", "Spd", "Spc"))
art.comp$Id_number = factor(art.comp$Id_number, 
                            levels = c("1777", "1962_1", "1962_2", "495", "83", "survey_1", "survey_2",
                                       "1556_1", "1556_2", "1843_1", "1843_2", "791", 
                                       "231", "336"))

point.comp = ggplot(art.comp) +
  geom_line(aes(x = Id_number, y = value, group = Id_number, color = Weathering_class)) +
  geom_point(aes(x = Id_number, y = value, group = Id_number, color = Weathering_class, shape = scar_weathering), size = 2) +
  facet_wrap( ~ measurement, scales = "free", labeller = labeller(measurement = facet.labs), 
              ncol = 1, strip.position = "right") +
  geom_bracket(xmin = "1777", xmax = "survey_2", y.position = 0, label = "strongly", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  geom_bracket(xmin = "1556_1", xmax = "791", y.position = 0, label = "mildly", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  geom_bracket(xmin = "231", xmax = "336", y.position = 0, label = "weakly", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  guides(color = "none") +
  labs(shape = "") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  scale_shape_manual(values = c(15, 16), labels = c("less weathered surface", "more weathered surface")) +
  scale_color_brewer(palette = "Set1")
plot(point.comp)

ggsave(
  point.comp, 
  filename = "figures/mw-lw-comparisons_weathering-class.tiff", 
  dpi = 300, width = 5, height = 9
)

art.comp$Id_number = factor(art.comp$Id_number, 
                            levels = c("83", "231", "336", "495", 
                                       "791",
                                       "1777", "1556_1", "1556_2", "1843_1", "1843_2", "1962_1", "1962_2",
                                       "survey_1", "survey_2"))
point.comp2 = ggplot(art.comp) +
  geom_line(aes(x = Id_number, y = value, group = Id_number, color = location)) +
  geom_point(aes(x = Id_number, y = value, group = Id_number, color = location, shape = scar_weathering), size = 2) +
  facet_wrap( ~ measurement, scales = "free", labeller = labeller(measurement = facet.labs), 
              ncol = 1, strip.position = "right") +
  geom_bracket(xmin = "83", xmax = "495", y.position = 0, label = "P1", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  geom_bracket(xmin = "791", xmax = "791", y.position = 0, label = "P2", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  geom_bracket(xmin = "1777", xmax = "1962_2", y.position = 0, label = "P5", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  geom_bracket(xmin = "survey_1", xmax = "survey_2", y.position = 0, label = "survey", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  guides(color = "none") +
  labs(shape = "") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom") +
  scale_shape_manual(values = c(15, 16), labels = c("less weathered surface", "more weathered surface")) +
  scale_color_brewer(palette = "Dark2")
plot(point.comp2)

ggsave(
  point.comp2, 
  filename = "figures/mw-lw-comparisons_location.tiff", 
  dpi = 300, width = 5, height = 9
)
  

##### regressions #####
reg_Sq_c = Sq_c %>%
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

hist(reg_Sq_c$Sq_diff)
fit1 = lm(Sq_diff ~ p1 + p2 + p5 + survey + strongly_weathered + mildly_weathered + weakly_weathered + Weight + Flake_length + Flake_width + Flake_thickness, data = reg_Sq_c)
summary(fit1)

fit1.dum = lmer(Sq_diff ~ p1 + p2 + p5 + survey + strongly_weathered + mildly_weathered + weakly_weathered + Weight + Flake_length + Flake_width + Flake_thickness + (1 | artifact), data = reg_Sq_c)
summary(fit1.dum)

fixed.sq = tidy(fit1.dum) %>% filter(effect == "fixed") %>%
  mutate(lower_est = estimate - std.error, 
         upper_est = estimate + std.error)
fixed.sq$term = factor(fixed.sq$term, levels = c("(Intercept)", "p1TRUE", "p2TRUE", "p5TRUE", "strongly_weatheredTRUE", "mildly_weatheredTRUE", "Weight", "Flake_length", "Flake_width", "Flake_thickness"))

sq.c.p = ggplot(fixed.sq) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = term, y = estimate, color = term)) +
  geom_linerange(aes(x = term, ymin = lower_est, ymax = upper_est, color = term)) +
  coord_flip() +
  ggtitle("Change in Sq") +
  theme(title = element_text(size = 10))
plot(sq.c.p)

reg_Spd_c = Spd_c %>%
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

hist(reg_Spd_c$Spd_diff)
fit3 = lm(Spd_diff ~ p1 + p2 + p5 + survey + strongly_weathered + mildly_weathered + weakly_weathered + Weight + Flake_length + Flake_width + Flake_thickness, data = reg_Spd_c)
summary(fit3)
# fit4 = lmer(diff ~ location + Weathering_class + Flake_length + Flake_width + Flake_thickness + Weight + (1 | artifact), data = Spd_u)
# summary(fit4)

fit3.dum = lmer(Spd_diff ~ p1 + p2 + p5 + survey + strongly_weathered + mildly_weathered + weakly_weathered + Weight + Flake_length + Flake_width + Flake_thickness + (1 | artifact), data = reg_Spd_c)
summary(fit3.dum)

fixed.spd = tidy(fit3.dum) %>% filter(effect == "fixed") %>%
  mutate(lower_est = estimate - std.error, 
         upper_est = estimate + std.error)
fixed.spd$term = factor(fixed.spd$term, levels = c("(Intercept)", "p1TRUE", "p2TRUE", "p5TRUE", "strongly_weatheredTRUE", "mildly_weatheredTRUE", "Weight", "Flake_length", "Flake_width", "Flake_thickness"))

spd.c.p = ggplot(fixed.spd) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = term, y = estimate, color = term)) +
  geom_linerange(aes(x = term, ymin = lower_est, ymax = upper_est, color = term)) +
  coord_flip() +
  ggtitle("Change in Spd") +
  theme(title = element_text(size = 10))
plot(spd.c.p)


reg_Spc_c = Spc_c %>%
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

hist(reg_Spc_c$Spc_diff)
fit5 = lm(Spc_diff ~ p1 + p2 + p5 + survey + strongly_weathered + mildly_weathered + weakly_weathered + Weight + Flake_length + Flake_width + Flake_thickness, data = reg_Spc_c)
summary(fit5)
# fit6 = lmer(diff ~ location + Weathering_class + Flake_length + Flake_width + Flake_thickness + Weight + (1 | artifact), data = Spc_u)
# summary(fit6)

fit5.dum = lmer(Spc_diff ~ p1 + p2 + p5 + survey + strongly_weathered + mildly_weathered + weakly_weathered + Weight + Flake_length + Flake_width + Flake_thickness + (1 | artifact), data = reg_Spc_c)
summary(fit5.dum)

fixed.spc = tidy(fit5.dum) %>% filter(effect == "fixed") %>%
  mutate(lower_est = estimate - std.error, 
         upper_est = estimate + std.error)
fixed.spc$term = factor(fixed.spc$term, levels = c("(Intercept)", "p1TRUE", "p2TRUE", "p5TRUE", "strongly_weatheredTRUE", "mildly_weatheredTRUE", "Weight", "Flake_length", "Flake_width", "Flake_thickness"))

spc.c.p = ggplot(fixed.spc) +
  geom_hline(aes(yintercept = 0)) +
  geom_point(aes(x = term, y = estimate, color = term)) +
  geom_linerange(aes(x = term, ymin = lower_est, ymax = upper_est, color = term)) +
  coord_flip() +
  ggtitle("Change in Spc") +
  theme(title = element_text(size = 10))
plot(spc.c.p)
  
ggarrange(sq.c.p, spd.c.p, spc.c.p, 
          ncol = 3, common.legend = T)
