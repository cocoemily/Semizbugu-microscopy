library(tidyverse)
library(scales)

june = read.csv("data/cleaned_june_artifacts.csv")
july = read.csv("data/cleaned_july_artifacts.csv")

allartifacts = rbind(june, july)

micro.sample = allartifacts %>% 
  filter(Id_number %in% c(83, 231, 336, 495, 791, 1556, 1777, 1843, 1962))

# write.csv(micro.sample, file = "data/microscopy_artifacts.csv")


allartifacts = allartifacts %>% mutate(location = ifelse(Site_name %in% c("Square 1", "Square 2", "Square 3"), "Semizbugu P1", 
                                                   ifelse(Site_name %in% c("Square 4", "Square 5"), "Semizbugu P2", "Semizbugu P5")))
allartifacts$recycled = !is.na(allartifacts$Recycling_description)
allartifacts$double_patina = str_detect(allartifacts$Recycling_indications, "double_patina")
allartifacts$Raw_material_description = tolower(allartifacts$Raw_material_description)

allartifacts = allartifacts %>% filter(is.na(Problem_notes))

cols = c("Id_number", "location", "recycled", "double_patina", "Raw_material_description", 
         "Weathering_class", "Artifact_type", "Bordian_type", "Tool_type", "Flake_type", "Blank_form",
         "Dorsal_flake_scar_count", "Cortex_percentage", "Flake_fragment", "Flake_termination",
         "Retouch", "Retouch_side", "Edge_damage",
         "Platform_thickness", "Platform_width",
         "Flake_thickness", "Flake_length", "Flake_width", 
         "Maximum_core_length", "Maximum_core_width", "Maximum_core_thickness", 
         "Weight")

all_artifacts = allartifacts %>% select_at(cols)

#### DATA CLEANING ####
all_artifacts$Weathering_class = factor(all_artifacts$Weathering_class, 
                                        levels = c("strongly_weathered", "mildly_weathered", "weakly_weathered", "not_weathered", "other"))
all_artifacts$Artifact_type = factor(all_artifacts$Artifact_type, 
                                     levels = c("complete_flake", "broken_flake", "tool", "tool_fragment", "core", "core_fragment", "shatter"))
all_artifacts$Flake_termination = factor(all_artifacts$Flake_termination, 
                                         levels = c("feather", "hinge", "plunge", "step", "other"))
all_artifacts$Flake_fragment = factor(all_artifacts$Flake_fragment , 
                                      levels = c("proximal", "medial", "distal", "other"))

all_artifacts$retouch.side = ifelse(str_detect(all_artifacts$Retouch_side, pattern = " "), "bifacial", all_artifacts$Retouch_side)
all_artifacts$retouch.side = factor(all_artifacts$retouch.side, levels = c("dorsal", "ventral", "bifacial"))
#all_artifacts$Retouch_side = factor(all_artifacts$Retouch_side, levels = c("dorsal", "ventral", "bifacial"))

all_artifacts$tool.type = ifelse(str_detect(all_artifacts$Tool_type, "notch denticulate"), "notch/denticulate", 
                                 ifelse(str_detect(all_artifacts$Tool_type, " "), "multiple", 
                                        all_artifacts$Tool_type))
all_artifacts$tool.type = factor(all_artifacts$tool.type, levels = c("notch", "denticulate", "notch/denticulate", "scraper", "point", "biface", "multiple", "other"))

all_artifacts = all_artifacts %>%
  mutate(Thickness = ifelse(is.na(Flake_thickness), Maximum_core_thickness, Flake_thickness),
         Length = ifelse(is.na(Flake_length), Maximum_core_length, Flake_length),
         Width = ifelse(is.na(Flake_width), Maximum_core_width, Flake_width)) %>%
  filter(is.na(Length) | Length <= 200) %>%
  filter(is.na(Width) | Width <= 200) %>% 
  filter(is.na(Thickness) |Thickness <= 200) #size of calipers

all_artifacts = all_artifacts %>%
  mutate(flake.type = ifelse(is.na(Blank_form), Flake_type, Blank_form))

all_artifacts = all_artifacts %>%
  mutate(
    Tool_type = ifelse(str_detect(Tool_type, "notch denticulate"), "notch/denticulate", 
                       ifelse(str_detect(Tool_type, " "), "multiple", 
                              Tool_type)), 
    Flake_type = ifelse(str_detect(Flake_type, pattern = "flake"), "flake", Flake_type)
  )
all_artifacts$Tool_type = factor(all_artifacts$Tool_type, levels = c("notch", "denticulate", "notch/denticulate", "scraper", "point", "biface", "multiple", "other"))
all_artifacts$Flake_type = factor(all_artifacts$Flake_type, levels = c("flake", "blade", "bladelet", "other"))

#possible artifact rolling
all_artifacts$poss_roll = ifelse(all_artifacts$Bordian_name == "alternate scraper", TRUE, FALSE)
all_artifacts$poss_roll = ifelse(str_detect(all_artifacts$Retouch_side, pattern = " "), TRUE, all_artifacts$poss_roll)

rolled.rcycl = all_artifacts %>% filter(poss_roll == T & recycled == T)
all_artifacts = subset(all_artifacts, !(Id_number %in% rolled.rcycl$Id_number))

rm(list = c("allartifacts", "july", "june", "rolled.rcycl"))


#### Distribution of weathering stages ####
weathered_artifacts = all_artifacts %>% filter(Weathering_class %in% c("strongly_weathered", "mildly_weathered", "weakly_weathered", "not_weathered"))

wacounts = weathered_artifacts %>%
  group_by(Weathering_class, location) %>%
  summarize(count = n()) %>% 
  group_by(location) %>%
  mutate(artifact_count = sum(count), 
         prop = count/artifact_count)

ggplot(weathered_artifacts, aes(x = factor(Weathering_class))) +
  geom_bar(aes( y = after_stat(count/sum(count)))) + 
  scale_y_continuous(labels = scales::percent) +
  facet_grid(~location) +
  theme_bw()

ggplot(wacounts) +
  geom_col(aes(x = Weathering_class, y = prop, fill = Weathering_class)) + 
  facet_grid(~location) +
  theme_bw() +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly", "unweathered")) +
  labs(x = "weathering degree", y = "proportion of all artifacts") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "none")
