library(tidyverse)

artifacts = read_csv("data/microscopy_artifacts.csv")

artifacts = artifacts %>% mutate(location = ifelse(Site_name %in% c("Square 1", "Square 2", "Square 3"), "Semizbugu P1", 
                                                   ifelse(Site_name %in% c("Square 4", "Square 5"), "Semizbugu P2", "Semizbugu P5")))

cols = c("Id_number", "location", "Weathering_class")
artifacts = artifacts %>% select_at(cols)
artifacts$Id_number = as.character(artifacts$Id_number)

artifacts = artifacts %>% add_row(Id_number = "survey", location = "survey", Weathering_class = "strongly_weathered")

files = list.files("data/Sneox-CSV")

sensofar.data = data.frame(
  Id_number = character(0),
  sample_number = double(0),
  location = character(0), 
  Weathering_class = character(0), 
  surface_class = character(0), 
  Spc = double(0), 
  Spd = double(0),
  S5v = double(0),
  Sq = double(0), 
  Sp = double(0), 
  Sv = double(0), 
  Sa = double(0), 
  Ssk = double(0),
  Sal = double(0), 
  Sk = double(0), 
  Spk = double(0), 
  Smr1 = double(0), 
  Smr2 = double(0)
)

for(f in files) {
  filename = str_split(f, "_")[[1]]
  if(!str_detect(f, "survey")) {
    artifact.name = str_split(filename[1], "-")[[1]][2]
    artifact.id = str_split(filename[3], "-")[[1]][1]
    surface.class = ifelse(str_detect(str_split(filename[3],"-")[[1]][2], "1"), "lw", "mw")
  } else {
    artifact.name = filename[1]
    artifact.id = str_split(filename[3], "-")[[1]][1]
    surface.class = ifelse(str_detect(str_split(filename[3],"-")[[1]][2], "1"), "lw", "mw")
  }
  
  loc = (artifacts %>% filter(Id_number == artifact.name))$location
  wc = (artifacts %>% filter(Id_number == artifact.name))$Weathering_class
  
  sneox.data = read.csv(paste0("data/Sneox-CSV/", f), skip = 4, header = F, 
                        colClasses =  c("character", "character", "NULL", "NULL"))[,1:2]
  sneox.clean = unlist(sneox.data[,2])[-1]
  
  sensofar.data[nrow(sensofar.data) + 1, ] = 
    c(artifact.name, artifact.id, loc, wc, surface.class, sneox.clean)
}

sensofar.data[,6:18] = sapply(sensofar.data[,6:18], as.numeric)

