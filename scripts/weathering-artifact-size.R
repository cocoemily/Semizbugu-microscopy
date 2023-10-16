##weathering and artifact size
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(plyr)

theme_set(theme_bw())

june = read.csv("data/cleaned_june_artifacts.csv")
july = read.csv("data/cleaned_july_artifacts.csv")

allartifacts = rbind(june, july)

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


#### Weathering classes and artifact sizes ####
wartifacts = all_artifacts %>% filter(Weathering_class %in% c("strongly_weathered", "mildly_weathered", "weakly_weathered"))

mu.length = ddply(wartifacts, "Weathering_class", summarize, grp.median = median(Length))
ggplot(wartifacts) +
  geom_density(aes(Length, color = Weathering_class)) +
  geom_vline(data = mu.length, aes(xintercept = grp.median, color = Weathering_class), linetype = "dashed") +
  scale_color_colorblind()

mu.thick = ddply(wartifacts, "Weathering_class", summarize, grp.median = median(Thickness))
ggplot(wartifacts) +
  geom_density(aes(Thickness, color = Weathering_class)) +
  geom_vline(data = mu.thick, aes(xintercept = grp.median, color = Weathering_class), linetype = "dashed") +
  scale_color_colorblind()

mu.lt = ddply(wartifacts, "Weathering_class", summarize, grp.median = median(Length/Thickness))
ggplot(wartifacts) +
  geom_density(aes(Length/Thickness, color = Weathering_class)) +
  geom_vline(data = mu.lt, aes(xintercept = grp.median, color = Weathering_class), linetype = "dashed") +
  scale_color_colorblind()
