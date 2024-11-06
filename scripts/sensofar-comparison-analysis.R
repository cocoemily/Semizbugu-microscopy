library(tidyverse)
library(ggthemes)
library(ggpubr)
library(ggprism)
library(rcompanion)
library(paletteer)
library(rstatix)
library(lme4)

theme_set(theme_bw())

source("scripts/get-sneox-2024-data.R")
rm(list = setdiff(ls(), "sensofar.data"))
parameters = colnames(sensofar.data[,6:21])

sdata = sensofar.data  %>%
  pivot_longer(cols = 6:21, names_to = "measurement", values_to = "value")
sdata$Weathering_class = factor(sdata$Weathering_class, 
                                levels = c("strongly_weathered", "mildly_weathered", "weakly_weathered"))


####order artifacts by values####
plot_by_param_value = function(param) {
  new = sensofar.data  %>% 
    mutate(new_Id = paste0(Id_number, "_", measurement_number)) %>% 
    filter(surface_class == "mw") %>%
    select_at(c("new_Id", "Weathering_class", param))
  
  new$new_Id = reorder(new$new_Id, new[,param])
  
  return(
    ggplot(new) +
      geom_point(aes(x = new_Id, y = !!sym(param), color = Weathering_class)) +
      scale_color_colorblind()
  )
  
}

plot(plot_by_param_value(parameters[1]))
plot(plot_by_param_value(parameters[2]))
plot(plot_by_param_value(parameters[3]))
plot(plot_by_param_value(parameters[4]))
plot(plot_by_param_value(parameters[5]))
plot(plot_by_param_value(parameters[6]))
plot(plot_by_param_value(parameters[7]))
plot(plot_by_param_value(parameters[8]))
plot(plot_by_param_value(parameters[9]))
plot(plot_by_param_value(parameters[10]))
plot(plot_by_param_value(parameters[11]))
plot(plot_by_param_value(parameters[12]))
plot(plot_by_param_value(parameters[13]))


####comparing weakly weathered to strongly/mildly weathered####
sdata$wc2 = ifelse(sdata$Weathering_class == "weakly_weathered", "weak", "strong/mild")

ggplot(sdata, 
       aes(x = wc2, y = value, color = wc2, group = wc2)) +
  geom_boxplot() +
  stat_compare_means(vjust = 2) +
  facet_wrap(~measurement, scales = "free")

compare_means(value ~ wc2, group.by = "measurement", data = sdata, p.adjust.method = "bonferroni")



w.labs = c("strongly", "mildly", "weakly")
names(w.labs) = c("strongly_weathered", "mildly_weathered", "weakly_weathered")

stats.all = compare_means(value ~ surface_class, group.by = c("Weathering_class", "measurement"), 
                          data = sdata, 
                          paired = T)

wc.all = compare_means(value ~ Weathering_class, group.by = "measurement", 
                       data = sdata, 
                       method = "anova")
####mw-lw comparison####
all.stats = compare_means(value ~ surface_class, group.by = c("Weathering_class", "measurement"), 
                          data = sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")), 
                          paired = T) %>%
  left_join(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")), by = c("measurement", "Weathering_class")) %>%
  group_by(Weathering_class, measurement) %>%
  summarize(p.format = first(p.format), 
            max.y = max(value))

all.mw.lw.wc = ggplot(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc"))) +
  geom_boxplot(aes(x = Weathering_class, y = value, 
                   group = interaction(surface_class, Weathering_class), 
                   color = interaction(surface_class, Weathering_class))) +
  geom_text(data = all.stats %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")), 
            mapping = aes(x = Weathering_class, y = max.y,label = paste0("p = ", p.format)),
            nudge_y = 0.5) +
  facet_wrap(~fct_rev(measurement), scales = "free", strip.position = "right", ncol = 1) +
  scale_x_discrete(labels = w.labs) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Paired")
plot(all.mw.lw.wc)

# ggsave(plot = all.mw.lw.wc,
#        filename = "figures/mw-lw-comparisons_wc.tiff",
#        dpi = 300, width = 5, height = 8)

all.stats2 = compare_means(value ~ surface_class, group.by = c("location", "measurement"), 
                           data = sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")), 
                           paired = T) %>%
  left_join(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")), by = c("measurement", "location")) %>%
  group_by(location, measurement) %>%
  summarize(p.format = first(p.format), 
            max.y = max(value))

all.mw.lw.loc = ggplot(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc"))) +
  geom_boxplot(aes(x = location, y = value, 
                   group = interaction(surface_class, location), 
                   color = interaction(surface_class, location))) +
  geom_text(data = all.stats2 %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")), 
            mapping = aes(x = location, y = max.y,label = paste0("p = ", p.format)),
            nudge_y = 0.5) +
  facet_wrap(~fct_rev(measurement), scales = "free", strip.position = "right", ncol = 1) +
  scale_x_discrete(labels = w.labs) +
  labs(x = "location", color = "weathering class") +
  theme(legend.position = "none") +
  scale_color_paletteer_d("ggthemes::Summer", direction = -1)
plot(all.mw.lw.loc)

# ggsave(plot = all.mw.lw.loc,
#        filename = "figures/mw-lw-comparisons_loc.tiff",
#        dpi = 300, width = 5, height = 8)


#### weathered surface comparison ####
sdata.sub = sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc"))
sdata.sub$measurement = factor(sdata.sub$measurement, 
                               levels = c("Sq", "Spc", "Smr2", "Ssk", "Vvc"))

table((sensofar.data %>% filter(surface_class == "mw"))$Weathering_class)
table((sensofar.data %>% filter(surface_class == "mw"))$location)

mwplot1 = ggplot(sdata.sub %>% filter(surface_class == "mw") %>% filter(location != "survey"), 
                 aes(x = Weathering_class, y = value, 
                     group = Weathering_class, 
                     color = Weathering_class)) +
  geom_boxplot() +
  stat_compare_means(method = "wilcox.test", comparisons = list( 
    c("strongly_weathered", "mildly_weathered"), 
    c("strongly_weathered", "weakly_weathered"), 
    c("mildly_weathered", "weakly_weathered")),
    label.x.npc = "center", vjust = 0.6, #hjust = 1, 
    label = "p.signif", hide.ns = T) +
  #geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", strip.position = "right", ncol = 2) +
  scale_x_discrete(labels = w.labs) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "none", panel.spacing.x = unit(5, "lines")) +
  scale_color_brewer(palette = "Set1")
plot(mwplot1)

mwplot2 = ggplot(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")) %>% filter(surface_class == "mw"), 
                 aes(x = location, y = value, 
                     group = location, 
                     color = location)) +
  geom_boxplot() +
  stat_compare_means(method = "anova", label.x.npc = "center", vjust = 1, hjust = 1) +
  facet_wrap(~measurement, scales = "free", strip.position = "right", ncol = 2) +
  labs(x = "location")  +
  theme(legend.position = "none", panel.spacing.x = unit(5, "lines"), 
        axis.text.x = element_text(size = 7)) +
  scale_color_brewer(palette = "Dark2")
plot(mwplot2)

ggsave(plot = mwplot1,
       filename = "figures/mw-comparisons_wc.tiff",
       dpi = 300, width = 8, height = 7)
ggsave(plot = mwplot2,
       filename = "figures/mw-comparisons_loc.tiff",
       dpi = 300, width = 8, height = 7)

####by artifact####
art.comp = sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")) %>%
  mutate(measurement_id = paste0(Id_number, "_", measurement_number))

#order by location
# art.comp$Id_number = factor(art.comp$Id_number,
#                             levels = c("83", "231", "336","495", 
#                                        "791",
#                                        "1556", "1777" , "1843", "1962",
#                                        "survey"))

#order by weathering stage
art.comp$Id_number = factor(art.comp$Id_number,
                            levels = c("231", "336",
                                       "791","1556","1843",
                                       "83","495", "1777", "1962", "survey"))

art.comp$measurement = factor(art.comp$measurement , 
         levels = c("Sq", "Spc", "Smr2", "Ssk", "Vvc"))

all.stats = compare_means(value ~ surface_class, group.by = c("Id_number", "Weathering_class", "measurement"), 
                          data = sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")), 
                          paired = F) %>%
  left_join(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")), by = c("Id_number", "measurement", "Weathering_class")) %>%
  #filter(surface_class == "mw") %>%
  group_by(Id_number, Weathering_class, measurement) %>%
  summarize(p.format = first(p.format), 
            p.signif = first(p.signif),
            max.y = max(value))

all.stats$measurement = factor(all.stats$measurement, 
                               levels = c("Sq", "Spc", "Smr2", "Ssk", "Vvc"))

point.comp = ggplot(art.comp) +
  geom_boxplot(aes(x = Id_number, y = value, group = interaction(Id_number, surface_class), fill = interaction(surface_class, Weathering_class)), position = "dodge2") +
  geom_text(data = all.stats %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvc")) %>% filter(p.signif != "ns"), 
            mapping = aes(x = Id_number, y = max.y, label = p.signif), size = 6) +
  facet_wrap( ~ measurement, scales = "free", ncol = 1, strip.position = "right") +
  guides(color = "none", fill = "none") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "bottom", 
        #panel.spacing.y = unit(1.5, "lines")
        ) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(expand = c(0.1, 0.1))
plot(point.comp)

ggsave(
  point.comp, 
  filename = "figures/mw-lw-comparisons_V2.tiff", 
  dpi = 300, width = 5, height = 7
)


#### comparison while accounting for variation within subject ####
sensofar.data$measurement_id = paste0(
  sensofar.data$Id_number, "-", 
  sensofar.data$measurement_number, "_",
  sensofar.data$surface_class
)
sensofar.data$Id_number = as.factor(sensofar.data$Id_number)
sensofar.data$surface_class = as.factor(sensofar.data$surface_class)
sensofar.data$Weathering_class = as.factor(sensofar.data$Weathering_class)

plotNormalHistogram(sensofar.data$Spc)
model = lmer(Spc ~ surface_class * Weathering_class * location + (1 | Id_number), data = sensofar.data)
summary(aov(Spc ~ surface_class * Weathering_class * location + Error(Id_number), data = sensofar.data))
# anova_test(data = sensofar.data, dv = Spc, wid = measurement_number, 
#            between = surface_class, within = Id_number)
spc.comp = sensofar.data %>%
  group_by(Weathering_class, Id_number) %>%
  pairwise_t_test(Spc ~ surface_class, p.adjust.method = "bonferroni") %>%
  select(-n1, -n2)

plotNormalHistogram(sensofar.data$Sq)
summary(aov(Sq ~ surface_class * Weathering_class * location + Error(Id_number), data = sensofar.data))
# anova_test(data = sensofar.data, dv = Sq, wid = measurement_number, 
#            between = surface_class, within = Id_number)
sq.comp = sensofar.data %>%
  group_by(Weathering_class, Id_number) %>%
  pairwise_t_test(Sq ~ surface_class, p.adjust.method = "bonferroni") %>%
  select(-n1, -n2)

plotNormalHistogram(sensofar.data$Ssk)
summary(aov(Ssk ~ surface_class * Weathering_class * location + Error(Id_number), data = sensofar.data))
# anova_test(data = sensofar.data, dv = Ssk, wid = measurement_number, 
#            between = surface_class, within = Id_number)
ssk.comp = sensofar.data %>%
  group_by(Weathering_class, Id_number) %>%
  pairwise_t_test(Ssk ~ surface_class, p.adjust.method = "bonferroni") %>%
  select(-n1, -n2)

plotNormalHistogram(sensofar.data$Smr2)
summary(aov(Smr2 ~ surface_class * Weathering_class * location + Error(Id_number), data = sensofar.data))
# anova_test(data = sensofar.data, dv = Smr2, wid = measurement_number, 
#            between = surface_class, within = Id_number)
smr2.comp = sensofar.data %>%
  group_by(Weathering_class, Id_number) %>%
  pairwise_t_test(Smr2 ~ surface_class, p.adjust.method = "bonferroni") %>%
  select(-n1, -n2)

plotNormalHistogram(sensofar.data$Vvc)
summary(aov(Vvc ~ surface_class * Weathering_class * location + Error(Id_number), data = sensofar.data))
# anova_test(data = sensofar.data, dv = Vvc, wid = measurement_number, 
#            between = surface_class, within = Id_number)
Vvc.comp = sensofar.data %>%
  group_by(Weathering_class, Id_number) %>%
  pairwise_t_test(Vvc ~ surface_class, p.adjust.method = "bonferroni") %>%
  select(-n1, -n2)



