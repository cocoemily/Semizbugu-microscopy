#EDX analysis 2024
library(tidyverse)
library(ggpubr)
library(ggthemes)

theme_set(theme_bw())

artifacts = read_csv("data/microscopy_artifacts.csv")

artifacts = artifacts %>% mutate(location = ifelse(Site_name %in% c("Square 1", "Square 2", "Square 3"), "Semizbugu P1", 
                                                   ifelse(Site_name %in% c("Square 4", "Square 5"), "Semizbugu P2", "Semizbugu P5")))

cols = c("Id_number", "location", "Weathering_class")
artifacts = artifacts %>% select_at(cols)
artifacts$Id_number = as.character(artifacts$Id_number)

artifacts = artifacts %>% add_row(Id_number = "survey", location = "survey", Weathering_class = "strongly_weathered")

dirs = list.dirs("data/SEM-data_2024/artifact_surfaces")[-1]

edx.data = data.frame(
  Id_number = character(0),
  measurement_number = double(0),
  location = character(0), 
  Weathering_class = character(0), 
  surface_class = character(0), 
  O = double(0), 
  Na = double(0),
  Mg = double(0),
  Al = double(0), 
  Si = double(0), 
  K = double(0), 
  Ca = double(0), 
  Ti = double(0),
  Mn = double(0), 
  Fe = double(0)
)

#write_csv(edx.data, file = "data/SEM-data_2024/artifact-surfaces_weight-conc.csv", )

elements = c("O", "Na", "Mg", "Al", "Si", "K", "Ca", "Ti", "Mn", "Fe")

for(d in dirs) {
  edx = read.csv(list.files(path = d, pattern = "\\.csv$", full.names = T))
  file.split = str_split(d, pattern = "_")
  artifact.id = str_split(file.split[[1]][3], pattern = "-")[[1]][2] 
  if(artifact.id == "1566") { artifact.id = "1556"} #fixing typo 
  loc = (artifacts %>% filter(Id_number == artifact.id))$location
  wc = (artifacts %>% filter(Id_number == artifact.id))$Weathering_class
  surface.class = ifelse(str_detect(d, pattern = "lw"), "lw", "mw")
  measurement.id = str_extract_all(d, "(?<=_)[0-9]+")[[1]][2]
  
  element.data = unlist((edx %>% filter(Element.symbol %in% elements))[,5]) 
 
  edx.data[nrow(edx.data) + 1, ] = 
    c(artifact.id, measurement.id, loc, wc, surface.class, element.data)
}

edx.long = edx.data %>% pivot_longer(
  cols = all_of(elements), names_to = "element", values_to = "weight_percent"
) %>%
  mutate(weight_percent = as.numeric(weight_percent))

ggplot(edx.long) +
  geom_boxplot(aes(x = surface_class, y = weight_percent)) +
  stat_compare_means(aes(x = surface_class, y = weight_percent)) +
  facet_wrap(~element, scales = "free")
#significant differences between surface classes for Iron, Oxygen, Titanium

art.comp = edx.long %>% filter(element %in% c("Fe", "Mn", "Si", "Al"))
art.comp$Id_number = factor(art.comp$Id_number,
                            levels = c("231", "336",
                                       "791","1556","1843",
                                       "83","495", "1777", "1962", "survey"))
art.comp$element = factor(art.comp$element, levels = c("Fe", "Mn", "Si", "Al"))
art.comp$Weathering_class = factor(art.comp$Weathering_class, 
                                   levels = c("strongly_weathered", "mildly_weathered", "weakly_weathered"))

all.stats = compare_means(weight_percent ~ surface_class, group.by = c("Id_number", "Weathering_class", "element"), 
                          data = art.comp, paired = T) 

point.comp = ggplot(art.comp) +
  geom_col(aes(x = Id_number, y = weight_percent, group = interaction(Id_number, surface_class), fill = interaction(surface_class, Weathering_class)), position = "dodge2") +
  # geom_text(data = all.stats %>% filter(measurement %in% c("Sq", "Spc", "Smr2", "Ssk", "Vvv")) %>% filter(p.signif != "ns"), 
  #           mapping = aes(x = Id_number, y = max.y, label = p.signif), size = 6) +
  facet_wrap( ~ element, scales = "free", ncol = 1, strip.position = "right") +
  guides(color = "none", fill = "none") +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom", 
        #panel.spacing.y = unit(1.5, "lines")
  ) +
  labs(y = "Weight Concentration %") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired")
plot(point.comp)

ggsave(
  point.comp, 
  filename = "figures/mw-lw-EDX_comparisons.tiff", 
  dpi = 300, width = 6.5, height = 5
)

art.comp2 = edx.long %>% filter(element %in% c("Fe", "Mn", "Si", "Al", "K", "Ca"))
art.comp2$Id_number = factor(art.comp2$Id_number,
                            levels = c("231", "336", #weakly weathered
                                       "791","1556","1843", #mildly weathered
                                       "83","495", "1777", "1962", "survey")) #strongly weathered
art.comp2$element = factor(art.comp2$element, levels = c("Fe", "Mn", "Si", "Al", "K", "Ca"))
art.comp2$Weathering_class = factor(art.comp2$Weathering_class, 
                                   levels = c("weakly_weathered", "mildly_weathered", "strongly_weathered"))

all.stats2 = compare_means(weight_percent ~ surface_class, group.by = c("Id_number", "Weathering_class", "element"), 
                          data = art.comp2, paired = T) 

point.comp2 = ggplot(art.comp2) +
  geom_col(aes(x = Id_number, y = weight_percent, group = interaction(Id_number, surface_class), fill = interaction(surface_class, Weathering_class)), position = "dodge2") +
  facet_wrap( ~ element, scales = "free", ncol = 1, strip.position = "right") +
  guides(color = "none", fill = "none") +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom", 
        #panel.spacing.y = unit(1.5, "lines")
  ) +
  labs(y = "Weight Concentration %") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired")
plot(point.comp2)

ggsave(
  point.comp2, 
  filename = "figures/mw-lw-EDX_comparisons_Ca-K.tiff", 
  dpi = 300, width = 6.5, height = 7
)
