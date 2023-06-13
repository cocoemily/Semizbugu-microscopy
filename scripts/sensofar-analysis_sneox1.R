library(tidyverse)
library(ggthemes)
library(ggpubr)
library(factoextra)
library(nlme)
library(lme4)
library(broom)
library(broom.mixed)

theme_set(theme_bw())

data = read_csv("data/Sneox1_microscopy_artifacts.csv") %>%
  mutate(lw_pvdif_u = abs(lw_Sp_u - lw_Sv_u), 
         mw_pvdif_u = abs(mw_Sp_u - mw_Sv_u), 
         lw_pvdif_c = abs(lw_Sp_c - lw_Sv_c), 
         mw_pvdif_c = abs(mw_Sp_c - mw_Sv_c))

sdata = data  %>%
  pivot_longer(cols = 41:64, names_to = "measurement", values_to = "value") %>%
  separate(col = "measurement", into = c("scar_weathering", "measurement", "correction"), sep = "_")
sdata$measurement = factor(sdata$measurement, levels = c("Sq", "Sp", "Sv", "pvdif", "Spc", "Spd"))
sdata$Weathering_class = factor(sdata$Weathering_class, levels = c("strongly_weathered", "mildly_weathered", "weakly_weathered"))

####Data comparisons####
uncorrected_sdata = sdata %>% filter(correction == "u")
corrected_sdata = sdata %>% filter(correction == "c")

#Corrected Data comparison
ggplot(corrected_sdata) +
  geom_boxplot(aes(x = scar_weathering, y = value)) +
  facet_wrap(~measurement, scales = "free") +
  stat_compare_means(aes(x = scar_weathering, y = value))

ggplot(corrected_sdata) +
  geom_point(aes(x = scar_weathering, y = value, color = Weathering_class)) +
  geom_line(aes(x = scar_weathering, y = value, group = Id_number, color = Weathering_class)) +
  facet_grid(measurement~Id_number, scales = "free")

#Uncorrected Data comparison
ggplot(uncorrected_sdata) +
  geom_boxplot(aes(x = scar_weathering, y = value)) +
  facet_wrap(~measurement, scales = "free_y") +
  stat_compare_means(aes(x = scar_weathering, y = value)) +
  theme(legend.position = "none")

ggplot(uncorrected_sdata) +
  geom_point(aes(x = scar_weathering, y = value, color = Weathering_class)) +
  geom_line(aes(x = scar_weathering, y = value, group = Id_number, color = Weathering_class)) +
  facet_grid(measurement~Id_number, scales = "free") +
  theme(legend.position = "none")


#####Comparisons -- corrected data#####
Spc_c = corrected_sdata %>% filter(measurement == "Spc") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(Spc_diff = mw - lw) %>%
  rename(Spc_mw = mw, 
         Spc_lw = lw)
Spd_c = corrected_sdata %>% filter(measurement == "Spd") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(Spd_diff = mw - lw) %>%
  rename(Spd_mw = mw, 
         Spd_lw = lw)
Sq_c = corrected_sdata %>% filter(measurement == "Sq") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(Sq_diff = mw - lw) %>%
  rename(Sq_mw = mw, 
         Sq_lw = lw)

mwcomp_c = Sq_c %>% left_join(Spd_c, by = "Id_number") %>% left_join(Spc_c, by = "Id_number") %>%
  select(Id_number, location, Weathering_class, Sq_mw, Spc_mw, Spd_mw) %>%
  pivot_longer(cols = 4:6, names_to = "measurement", values_to = "value")
mw.labs = c("Spc", "Spd", "Sq")
names(mw.labs) = c("Spc_mw", "Spd_mw", "Sq_mw")

cmw_plot = ggplot(mwcomp_c) +
  geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = mw.labs)) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering") +
  theme(legend.position = "none") +
  ggtitle("Corrected data") +
  scale_color_brewer(palette = "Set1")
plot(cmw_plot)


allcomp_c = Sq_c %>% left_join(Spd_c, by = "Id_number") %>% left_join(Spc_c, by = "Id_number") %>%
  select(Id_number, location, Weathering_class, Sq_diff, Spc_diff, Spd_diff) %>%
  pivot_longer(cols = 4:6, names_to = "measurement", values_to = "value")
diff.labs = c("Spc (more - less)", "Spd (more - less)", "Sq (more - less)")
names(diff.labs) = c("Spc_diff", "Spd_diff", "Sq_diff")
 
cl_plot = ggplot(allcomp_c %>% filter(Weathering_class == "strongly_weathered")) +
  geom_boxplot(aes(x = location, y = value, color = location)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = diff.labs)) +
  theme(legend.position = "none") +
  ggtitle("Corrected data") +
  scale_color_brewer(palette = "Dark2")

cw_plot = ggplot(allcomp_c) +
  geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = diff.labs)) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering") +
  theme(legend.position = "none") +
  ggtitle("Corrected data") +
  scale_color_brewer(palette = "Set1")


#####Comparisons -- uncorrected data######
Spc_u = uncorrected_sdata %>% filter(measurement == "Spc") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(Spc_diff = mw - lw) %>%
  rename(Spc_mw = mw, 
         Spc_lw = lw)
Spd_u = uncorrected_sdata %>% filter(measurement == "Spd") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(Spd_diff = mw - lw) %>%
  rename(Spd_mw = mw, 
         Spd_lw = lw)
Sq_u = uncorrected_sdata %>% filter(measurement == "Sq") %>% 
  pivot_wider(names_from = scar_weathering, values_from = value) %>%
  mutate(Sq_diff = mw - lw) %>%
  rename(Sq_mw = mw, 
         Sq_lw = lw)

mwcomp_u = Sq_u %>% left_join(Spd_u, by = "Id_number") %>% left_join(Spc_u, by = "Id_number") %>%
  select(Id_number, location, Weathering_class, Sq_mw, Spc_mw, Spd_mw) %>%
  pivot_longer(cols = 4:6, names_to = "measurement", values_to = "value")
mw.labs = c("Spc", "Spd", "Sq")
names(mw.labs) = c("Spc_mw", "Spd_mw", "Sq_mw")

umw_plot = ggplot(mwcomp_u) +
  geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = mw.labs)) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering") +
  theme(legend.position = "none") +
  ggtitle("Uncorrected data") +
  scale_color_brewer(palette = "Set1")
plot(umw_plot)


allcomp_u = Sq_u %>% left_join(Spd_u, by = "Id_number") %>% left_join(Spc_u, by = "Id_number") %>%
  select(Id_number, location, Weathering_class, Sq_diff, Spc_diff, Spd_diff) %>%
  pivot_longer(cols = 4:6, names_to = "measurement", values_to = "value")
diff.labs = c("Spc (more - less)", "Spd (more - less)", "Sq (more - less)")
names(diff.labs) = c("Spc_diff", "Spd_diff", "Sq_diff")

ul_plot = ggplot(allcomp_u %>% filter(Weathering_class == "strongly_weathered")) +
  geom_boxplot(aes(x = location, y = value, color = location)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = diff.labs)) +
  theme(legend.position = "none") +
  ggtitle("Uncorrected data") +
  scale_color_brewer(palette = "Dark2")

uw_plot = ggplot(allcomp_u) +
  geom_boxplot(aes(x = Weathering_class, y = value, color = Weathering_class)) +
  geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", labeller = labeller(measurement = diff.labs)) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering") +
  theme(legend.position = "none") +
  ggtitle("Uncorrected data") +
  scale_color_brewer(palette = "Set1")


#####all comparison plots#####
ggsave(
  plot = ggarrange(ul_plot, cl_plot), 
  filename = "figures/location-comparison_Sneox-measurements.tiff", 
  dpi = 300, width = 11, height = 6
)

ggsave(
  plot = ggarrange(uw_plot, cw_plot), 
  filename = "figures/weathering-class-comparison_Sneox-measurements.tiff", 
  dpi = 300, width = 11, height = 6
)

ggsave(
  plot = ggarrange(umw_plot, cmw_plot), 
  filename = "figures/weathered-surface_wc-comparison_Sneox-measurements.tiff", 
  dpi = 300, width = 11, height = 6
)




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


####PCA analysis####
umw_data = data[,c(1,3,39, 42, 44, 46, 48, 50, 62)]
groups = factor(umw_data$Weathering_class, levels = c("strongly_weathered", "mildly_weathered", "weakly_weathered"))

umw_pca = prcomp(umw_data[,c(4:9)])
fviz_pca_ind(umw_pca, repel = T, 
             col.ind = groups, 
             addEllipses = T, 
             ellipse.type = "convex")
fviz_pca_var(umw_pca, 
             col.var = "contrib", 
             repel = T, 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

groups = factor(umw_data$location)
fviz_pca_ind(umw_pca, repel = T, 
             col.ind = groups, 
             addEllipses = T, 
             ellipse.type = "convex")


cmw_data = data[,c(1,3,39, 52, 54, 56, 58, 60, 64)]
groups = factor(cmw_data$Weathering_class, levels = c("strongly_weathered", "mildly_weathered", "weakly_weathered"))

cmw_pca = prcomp(cmw_data[,c(4:9)])
fviz_pca_ind(cmw_pca, repel = T, 
             col.ind = groups, 
             addEllipses = T, 
             ellipse.type = "convex")
fviz_pca_var(cmw_pca, 
             col.var = "contrib", 
             repel = T, 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))


