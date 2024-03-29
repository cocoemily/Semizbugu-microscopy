library(tidyverse)
library(ggthemes)
library(ggpubr)
library(rcompanion)

theme_set(theme_bw())

source("scripts/get-sneox-data.R")

plotNormalHistogram(sensofar.data$Sq)
plotNormalHistogram(sensofar.data$Spc)
plotNormalHistogram(sensofar.data$Smr2)


sdata = sensofar.data  %>%
  pivot_longer(cols = 6:18, names_to = "measurement", values_to = "value")

sdata$Weathering_class = factor(sdata$Weathering_class, 
                                levels = c("strongly_weathered", "mildly_weathered", "weakly_weathered"))


####order artifacts by values####
parameters = colnames(sensofar.data[,6:18])

plot_by_param_value = function(param) {
  new = sensofar.data  %>% 
    mutate(new_Id = paste0(Id_number, "_", sample_number)) %>% 
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
test.stats = compare_means(value ~ surface_class, group.by = c("Weathering_class", "measurement"), 
                           data = sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "S5v")), 
                           paired = T) %>%
  left_join(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "S5v")), by = c("measurement", "Weathering_class")) %>%
  group_by(Weathering_class, measurement) %>%
  summarize(p.format = first(p.format), 
            max.y = max(value))

ggplot(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "S5v"))) +
  geom_boxplot(aes(x = Weathering_class, y = value, 
                   group = interaction(surface_class, Weathering_class), 
                   color = interaction(surface_class, Weathering_class))) +
  geom_text(data = test.stats %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "S5v")), 
            mapping = aes(x = Weathering_class, y = max.y,label = paste0("p = ", p.format)),
            nudge_y = 0.5) +
  facet_wrap(~fct_rev(measurement), scales = "free", strip.position = "right", ncol = 1) +
  scale_x_discrete(labels = w.labs) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Paired")

all_plot1 = ggplot(sdata %>% filter(measurement %in% c("Sq", "Smr2"))) +
  geom_boxplot(aes(x = Weathering_class, y = value, 
                   group = interaction(surface_class, Weathering_class), 
                   color = interaction(surface_class, Weathering_class))) +
  geom_text(data = test.stats %>% filter(measurement %in% c("Sq", "Smr2")), 
            mapping = aes(x = Weathering_class, y = max.y,label = paste0("p = ", p.format)),
            nudge_y = 0.5) +
  facet_wrap(~fct_rev(measurement), scales = "free", strip.position = "right", ncol = 1) +
  scale_x_discrete(labels = w.labs) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Paired")
plot(all_plot1)

ggsave(plot = all_plot1,
       filename = "figures/mw-lw-comparisons_wc.tiff",
       dpi = 300, width = 5, height = 6)


# all_plot2 = ggplot(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2"))) +
#   geom_boxplot(aes(x = location, y = value, 
#                    group = interaction(surface_class, location), 
#                    color = interaction(surface_class, location))) +
#   #geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
#   facet_wrap(~measurement, scales = "free", strip.position = "right", ncol = 1) +
#   #scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
#   labs(x = "location", color = "weathering class") +
#   theme(legend.position = "none") +
#   scale_color_paletteer_d("ggthemes::Summer", direction = -1)
# #plot(all_plot2)
# 
# test.stats2 = compare_means(value ~ surface_class, group.by = c("location", "measurement"), 
#                            data = sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2")), 
#                            paired = T)

# ggsave(plot = ggarrange(all_plot1, all_plot2,ncol = 2,labels = "AUTO"), 
#        filename = "figures/base-measure-comparisons_V2.tiff", 
#        dpi = 300, width = 10, height = 9)

##### weathered surface comparison ####
sdata.sub = sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "S5v"))
sdata.sub$measurement = factor(sdata.sub$measurement, 
                               levels = c("Sq", "Spc", "Smr2", "Ssk", "S5v"))

mwplot1 = ggplot(sdata.sub %>% filter(surface_class == "mw"), 
                 aes(x = Weathering_class, y = value, 
                     group = Weathering_class, 
                     color = Weathering_class)) +
  geom_boxplot() +
  stat_compare_means(method = "anova", label.x.npc = "center", vjust = 2) +
  #geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", strip.position = "right", ncol = 2) +
  scale_x_discrete(labels = w.labs) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "none", panel.spacing.x = unit(5, "lines")) +
  scale_color_brewer(palette = "Set1")
plot(mwplot1)

mwplot2 = ggplot(sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "S5v")) %>% filter(surface_class == "mw"), 
                 aes(x = location, y = value, 
                     group = location, 
                     color = location)) +
  geom_boxplot() +
  stat_compare_means(method = "anova", label.x.npc = "center", vjust = 2) +
  facet_wrap(~measurement, scales = "free", strip.position = "right", ncol = 1) +
  labs(x = "location") +
  theme(legend.position = "none") +
  scale_color_brewer(palette = "Dark2")
plot(mwplot2)


ggsave(plot = mwplot1,
       filename = "figures/mw-comparisons_wc.tiff",
       dpi = 300, width = 8, height = 7)


ggsave(plot = mwplot2,
       filename = "figures/mw-comparisons_loc.tiff",
       dpi = 300, width = 5, height = 9)

# ggsave(plot = ggarrange(mwplot1, mwplot2,ncol = 2,labels = "AUTO"),
#        filename = "figures/mw-comparisons.tiff",
#        dpi = 300, width = 10, height = 9)

######abrasion parameters####
ggplot(sdata %>% filter(measurement %in% c("Sk", "Spk"))) +
  geom_boxplot(aes(x = Weathering_class, y = value, 
                   group = interaction(surface_class, Weathering_class), 
                   color = interaction(surface_class, Weathering_class))) +
  #geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", strip.position = "right", ncol = 1) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Paired")

######feature height#####
ggplot(sdata %>% filter(measurement %in% c("Sp", "Sv"))) +
  geom_boxplot(aes(x = Weathering_class, y = value, 
                   group = interaction(surface_class, Weathering_class), 
                   color = interaction(surface_class, Weathering_class))) +
  #geom_hline(aes(yintercept = 0), color = "grey20", linetype = 2) +
  facet_wrap(~measurement, scales = "free", strip.position = "right", ncol = 1) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  labs(x = "degree of weathering", color = "weathering class") +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Paired")



####by artifact####
art.comp = sdata %>% filter(measurement %in% c("Sq", "Spc", "Smr2"))
#art.comp$measurement = factor(art.comp$measurement, levels = c("Sq", "Spd", "Spc"))
art.comp$new_Id = paste0(art.comp$Id_number, "_", art.comp$sample_number)
art.comp$new_Id = factor(art.comp$new_Id, 
                         levels = c("83_002", "495_002", "1777_003", "1962_001", "1962_002", "survey_002", "survey_003",
                                    "791_002","1556_002", "1556_003", "1843_003", "1843_004",  
                                    "231_002", "336_002"))

point.comp = ggplot(art.comp) +
  geom_line(aes(x = new_Id, y = value, group = new_Id, color = Weathering_class)) +
  geom_point(aes(x = new_Id, y = value, group = new_Id, color = Weathering_class, shape = surface_class), size = 2) +
  facet_wrap( ~ fct_rev(measurement), scales = "free", ncol = 1, strip.position = "right") +
  # geom_bracket(xmin = "83_002", xmax = "survey_003", y.position = 0, label = "strongly", 
  #              tip.length = 0, label.size = 3, vjust = 1.5) +
  # geom_bracket(xmin = "791_002", xmax = "1843_004", y.position = 0, label = "mildly", 
  #              tip.length = 0, label.size = 3, vjust = 1.5) +
  # geom_bracket(xmin = "231_002", xmax = "336_002", y.position = 0, label = "weakly", 
  #              tip.length = 0, label.size = 3, vjust = 1.5) +
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
  filename = "figures/mw-lw-comparisons_weathering-class_V2.tiff", 
  dpi = 300, width = 5, height = 9
)

art.comp$new_Id = factor(art.comp$new_Id, 
                         levels = c("83_002", "231_002", "336_002","495_002", 
                                    "791_002",
                                    "1556_002", "1556_003", "1777_003" , "1843_003", "1843_004","1962_001", "1962_002", 
                                    "survey_002", "survey_003"))

point.comp2 = ggplot(art.comp) +
  geom_line(aes(x = new_Id, y = value, group = new_Id, color = location)) +
  geom_point(aes(x = new_Id, y = value, group = new_Id, color = location, shape = surface_class), size = 2) +
  facet_wrap( ~ measurement, scales = "free", ncol = 1, strip.position = "right") +
  geom_bracket(xmin = "83_002", xmax = "495_002", y.position = 0, label = "P1", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  geom_bracket(xmin = "791_002", xmax = "791_002", y.position = 0, label = "P2", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  geom_bracket(xmin = "1556_002", xmax = "1962_002", y.position = 0, label = "P5", 
               tip.length = 0, label.size = 3, vjust = 1.5) +
  geom_bracket(xmin = "survey_002", xmax = "survey_003", y.position = 0, label = "survey", 
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
  filename = "figures/mw-lw-comparisons_location_V2.tiff", 
  dpi = 300, width = 5, height = 9
)
