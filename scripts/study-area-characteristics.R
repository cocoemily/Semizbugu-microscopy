library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(ggthemes)
library(ggpubr)

p1 = readOGR("data/square-extents/", layer = "p1-sqs")
p1_window = st_buffer(st_as_sf(p1), 1)
p2 = readOGR("data/square-extents/", layer = "p2-sqs")
p2_window = st_buffer(st_as_sf(p2), 1)
p5 = readOGR("data/square-extents/", layer = "p5-sqs")
p5_window = st_buffer(st_as_sf(p5), 1)

####DEM####
dem = raster("/Users/emilycoco/Desktop/NYU/Dissertation-Research/sat-imagery/Kazakhstan/terrain/WorldDEMNeo_DSM_015_N47_09_E077_75/DEM/WorldDEMNeo_DSM_015_N47_09_E077_75_DEM.tif")
dem_rpj = projectRaster(dem, crs = 4326)
plot(dem_rpj)

dem_p1 = crop(dem_rpj, st_transform(st_as_sf(p1_window), 4326))
plot(dem_p1)

dem_p2 = crop(dem_rpj, st_transform(st_as_sf(p2_window), 4326))
plot(dem_p2)

dem_p5 = crop(dem_rpj, st_transform(st_as_sf(p5_window), 4326))
plot(dem_p5)


####slope percentage####
slope.perc = raster("/Users/emilycoco/Desktop/NYU/Dissertation-Research/sat-imagery/Kazakhstan/derived/slope-percentage.tif")
sp_rpj = projectRaster(slope.perc, crs = 4326)

sp_p1 = crop(sp_rpj, st_transform(st_as_sf(p1_window), 4326))
plot(sp_p1)

sp_p2 = crop(sp_rpj, st_transform(st_as_sf(p2_window), 4326))
plot(sp_p2)

sp_p5 = crop(sp_rpj, st_transform(st_as_sf(p5_window), 4326))
plot(sp_p5)


####slope degree####
slope.deg = raster("/Users/emilycoco/Desktop/NYU/Dissertation-Research/sat-imagery/Kazakhstan/derived/slope-degrees.tif")
sd_rpj = projectRaster(slope.deg, crs = 4326)

sd_p1 = crop(sd_rpj, st_transform(st_as_sf(p1_window), 4326))
plot(sd_p1)

sd_p2 = crop(sd_rpj, st_transform(st_as_sf(p2_window), 4326))
plot(sd_p2)

sd_p5 = crop(sd_rpj, st_transform(st_as_sf(p5_window), 4326))
plot(sd_p5)


####erosion risk####
erosion.risk = raster("/Users/emilycoco/Desktop/NYU/Dissertation-Research/Survey/erosion-analysis/RUSLE_A.tif")
er_rpj = projectRaster(erosion.risk, crs = 4326)

er_p1 = crop(er_rpj, st_transform(st_as_sf(p1_window), 4326))
plot(er_p1)

er_p2 = crop(er_rpj, st_transform(st_as_sf(p2_window), 4326))
plot(er_p2)

er_p5 = crop(er_rpj, st_transform(st_as_sf(p5_window), 4326))
plot(er_p5)


####study area characteristics####
#####Semizbugu P1######
summary(dem_p1)
summary(sp_p1)
summary(sd_p1)
summary(er_p1)

p1.char = data.frame(dem = getValues(dem_p1))
p1.char$location = "Semizbugu P1"
p1.char$slope.p = getValues(sp_p1)
p1.char$slope.d = getValues(sd_p1)
p1.char$er = getValues(er_p1)

#####Semizbugu P2######
summary(dem_p2)
summary(sp_p2)
summary(sd_p2)
summary(er_p2)

p2.char = data.frame(dem = getValues(dem_p2))
p2.char$location = "Semizbugu P2"
p2.char$slope.p = getValues(sp_p2)
p2.char$slope.d = getValues(sd_p2)
p2.char$er = getValues(er_p2)

#####Semizbugu P2######
summary(dem_p5)
summary(sp_p5)
summary(sd_p5)
summary(er_p5)

p5.char = data.frame(dem = getValues(dem_p5))
p5.char$location = "Semizbugu P5"
p5.char$slope.p = getValues(sp_p5)
p5.char$slope.d = getValues(sd_p5)
p5.char$er = getValues(er_p5)


theme_set(theme_bw())

sites = rbind(p1.char, p2.char, p5.char) %>%
  pivot_longer(cols = c(1, 3:5), names_to = "measure", values_to = "value")
measure.labs = c("elevation", "erosion risk", "slope (degrees)", "slope (percentage)")
names(measure.labs) = c("dem", "er", "slope.d", "slope.p")

site_comparisons <- list( c("Semizbugu P1", "Semizbugu P2"), 
                        c("Semizbugu P2", "Semizbugu P5"), 
                        c("Semizbugu P1", "Semizbugu P5") )

p = ggplot(sites, aes(x = location, y = value, color = location)) +
  geom_boxplot() +
  facet_wrap(~measure, scales = "free", labeller = labeller(measure = measure.labs)) +
  stat_compare_means(comparisons = site_comparisons, 
                     label = "p.signif",
                     ref.group = "0.5", vjust = 0.6) +
  scale_color_colorblind() +
  theme(legend.position = "none")
plot(p)

ggsave(filename = "figures/comparison-site-topography.tiff", 
       plot = p, 
       dpi = 300, width = 8, height = 6)
  

