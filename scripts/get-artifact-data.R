library(tidyverse)

june = read.csv("data/cleaned_june_artifacts.csv")
july = read.csv("data/cleaned_july_artifacts.csv")

allartifacts = rbind(june, july)

micro.sample = allartifacts %>% 
  filter(Id_number %in% c(83, 231, 336, 495, 791, 1556, 1777, 1843, 1962))

write.csv(micro.sample, file = "data/microscopy_artifacts.csv")
