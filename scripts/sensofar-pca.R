library(tidyverse)
library(ggthemes)
library(ggpubr)
library(paletteer)
library(factoextra)
library(nlme)
library(lme4)
library(broom)
library(broom.mixed)

library(FactoMineR)
library(ggcorrplot)
library(corrr)
library(ggfortify)
library(ggrepel)

theme_set(theme_bw())

source("scripts/get-sneox-2024-data.R")
rm(list = setdiff(ls(), "sensofar.data"))

sensofar.data = sensofar.data %>% filter(location != "survey")

mw_measures = sensofar.data %>% filter(surface_class == "mw")
lw_measures = sensofar.data %>% filter(surface_class == "lw")


norm.mw = scale(mw_measures[,6:21])
corr_mat = cor(norm.mw)
p.mat = cor_pmat(norm.mw)
ggcorrplot(corr_mat, type = "upper", 
           p.mat = p.mat, insig = "blank")

norm.lw = scale(lw_measures[,6:21])
lw.corr_mat = cor(norm.lw)
p.mat.lw = cor_pmat(norm.lw)
ggcorrplot(lw.corr_mat, type = "upper", 
           p.mat = p.mat.lw, insig = "blank")

# mw.pca2 = prcomp(mw_measures %>% select(
#   Sq, Spc, Smr2, Ssk
# ), scale = T)
# summary(mw.pca2)
# get_pca_var(mw.pca2)$contrib[,1:2]
# fviz_pca_ind(mw.pca2, label = "var", habillage = mw_measures$Weathering_class, 
#              addEllipses = T)
# fviz_pca_var(mw.pca2, col.var = "cos2")

####cluster analysis with three weathering class groups####
z = mw_measures %>% select(Sq, Smr2, Ssk)
means = apply(z,2,mean)
sds = apply(z,2,sd)
nor = scale(z, center = means, scale = sds)
rownames(nor) = paste0(mw_measures$Id_number, "_", mw_measures$measurement_number)

distance = dist(nor)

fviz_nbclust(nor, kmeans, method = "wss")

mw.kmeans = kmeans(nor, 3)
fviz_cluster(mw.kmeans, data = nor, ggtheme = theme_bw())

mw_measures$cluster_num1 = mw.kmeans$cluster
mw_measures$clean_wc = str_split_fixed(mw_measures$Weathering_class, "_", 2)

three.clust = fviz_pca_biplot(prcomp(z, scale = T), habillage = mw_measures$cluster_num1, 
                addEllipses = T, ellipse.type = "convex", label = "var", 
                repel = T, title = "") +
  geom_text_repel(aes(label = mw_measures$clean_wc[,1]), size = 2) +
  #guides(color = "none", fill = "none", shape = "none") +
  theme_bw() 
plot(three.clust)

table(mw_measures$Weathering_class, mw_measures$cluster_num1)

mw_measures %>% filter(cluster_num1 == 1) %>% select(Id_number, measurement_number, Weathering_class)
mw_measures %>% filter(cluster_num1 == 2) %>% select(Id_number, measurement_number, Weathering_class)
mw_measures %>% filter(cluster_num1 == 3) %>% select(Id_number, measurement_number, Weathering_class)

####cluster analysis with ideal group number####
mw.kmeans = kmeans(nor, 5)
fviz_cluster(mw.kmeans, data = nor, ggtheme = theme_bw())

mw_measures$cluster_num2 = mw.kmeans$cluster

table(mw_measures$Weathering_class, mw_measures$cluster_num2)

mw_measures %>% filter(cluster_num2 == 1) %>% select(Id_number, measurement_number, Weathering_class)
mw_measures %>% filter(cluster_num2 == 2) %>% select(Id_number, measurement_number, Weathering_class)
mw_measures %>% filter(cluster_num2 == 3) %>% select(Id_number, measurement_number, Weathering_class)
mw_measures %>% filter(cluster_num2 == 4) %>% select(Id_number, measurement_number, Weathering_class)
mw_measures %>% filter(cluster_num2 == 5) %>% select(Id_number, measurement_number, Weathering_class)

five.clust = fviz_pca_biplot(prcomp(z, scale = T), habillage = mw_measures$cluster_num2, 
                addEllipses = T, ellipse.type = "convex", label = "var", 
                repel = T, title = "")  +
  geom_text_repel(aes(label = mw_measures$clean_wc[,1]), size = 2) +
  #geom_text_repel(aes(label = mw_measures$Id_number), size = 2) +
  #guides(color = "none", fill = "none", shape = "none") +
  theme_bw() 
plot(five.clust)

biplots = ggarrange(three.clust, NULL, five.clust, 
                    nrow = 1, labels = c("A", "", "B"), 
                    widths = c(1, 0.25, 1))
plot(biplots)

ggsave("figures/pca-biplots-clusters.tiff", biplots, 
       dpi = 300, width = 10, height = 4)

get_pca_var(prcomp(z, scale = T))$contrib[,1:2]


# plot_by_param_value_CLUSTER = function(param) {
#   new = mw_measures  %>% 
#     mutate(new_Id = paste0(Id_number, "_", measurement_number)) %>% 
#     filter(surface_class == "mw") %>%
#     select_at(c("new_Id", "cluster_num2", param))
#   
#   new$new_Id = reorder(new$new_Id, new[,param])
#   
#   return(
#     ggplot(new) +
#       geom_point(aes(x = new_Id, y = !!sym(param), color = as.factor(cluster_num2))) +
#       scale_color_colorblind()
#   )
#   
# }
# 
# plot(plot_by_param_value_CLUSTER(parameters[1]))
# plot(plot_by_param_value_CLUSTER(parameters[2]))
# plot(plot_by_param_value_CLUSTER(parameters[3]))
# plot(plot_by_param_value_CLUSTER(parameters[4]))
# plot(plot_by_param_value_CLUSTER(parameters[5]))
# plot(plot_by_param_value_CLUSTER(parameters[6]))
# plot(plot_by_param_value_CLUSTER(parameters[7]))
# plot(plot_by_param_value_CLUSTER(parameters[8]))
# plot(plot_by_param_value_CLUSTER(parameters[9]))
# plot(plot_by_param_value_CLUSTER(parameters[10]))
# plot(plot_by_param_value_CLUSTER(parameters[11]))
# plot(plot_by_param_value_CLUSTER(parameters[12]))
# plot(plot_by_param_value_CLUSTER(parameters[13]))


####clustering of lw surfaces?####
lw.z =  lw_measures %>% select(
    Sq, Ssk, Smr2
  )
means = apply(lw.z,2,mean)
sds = apply(lw.z,2,sd)
nor.lw = scale(lw.z, center = means, scale = sds)

distance = dist(nor.lw)

fviz_nbclust(nor.lw, kmeans, method = "wss")

lw.kmeans = kmeans(nor.lw, 3)
fviz_cluster(lw.kmeans, data = nor.lw, ggtheme = theme_bw())

lw_measures$cluster_num1 = lw.kmeans$cluster
lw_measures$clean_wc = str_split_fixed(lw_measures$Weathering_class, "_", 2)
table(lw_measures$Weathering_class, lw_measures$cluster_num1)

three.clust2 = fviz_pca_biplot(prcomp(lw.z, scale = T), habillage = lw_measures$cluster_num1, 
                              addEllipses = T, ellipse.type = "convex", label = "var", 
                              repel = T, title = "") +
  geom_text_repel(aes(label = lw_measures$clean_wc[,1]), size = 2) +
  #guides(color = "none", fill = "none", shape = "none") +
  theme_bw() 
plot(three.clust2)

lw_measures %>% filter(cluster_num1 == 1) %>% select(Id_number, Weathering_class)
lw_measures %>% filter(cluster_num1 == 2) %>% select(Id_number, Weathering_class)
lw_measures %>% filter(cluster_num1 == 3) %>% select(Id_number, Weathering_class)
