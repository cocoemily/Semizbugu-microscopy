library(tidyverse)
library(ggthemes)
theme_set(theme_minimal())

data = read_csv("data/microscopy_artifacts.csv")

data$Weathering_class = factor(data$Weathering_class, 
                               levels = c("strongly_weathered", "mildly_weathered", "weakly_weathered"))

ggplot(data) +
  geom_bar(aes(x = Weathering_class, fill = Weathering_class)) +
  labs(x = "Weathering categories") +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  scale_fill_colorblind()


ggplot(data) +
  geom_bar(aes(x = Weathering_class, fill = Weathering_class)) +
  labs(x = "Weathering categories") +
  facet_wrap(~Site_name) +
  scale_x_discrete(labels = c("strongly", "mildly", "weakly")) +
  scale_fill_colorblind()
