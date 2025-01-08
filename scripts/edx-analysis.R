## EDX Analysis

library(tidyverse)
library(ggpubr)
library(ggthemes)
library(rvest)
library(IDPmisc)

theme_set(theme_bw())

sample = read_csv("data/microscopy_artifacts.csv")

read_spectra_table = function(file){
  return(
    read_tsv(file, skip = 25, trim_ws = T) %>%
      separate("Energy Counts", into = c("Energy", "count"), sep = "\\s", extra = "merge") %>%
      mutate(Energy = as.numeric(Energy), 
             count = as.numeric(count))
  )
}

#### get element table ####
url = "https://pages.uoregon.edu/epmalab/UCB_EPMA/emissionkev.htm"
text = url %>%
  read_html() %>%
  html_elements("pre") %>%
  html_text()
df = text[[2]] %>%
  read_fwf()
colnames(df) = df[1,]
elements = df[-1,]
elements[,c(2:7)] = lapply(elements[,c(2:7)], as.numeric)

iron = elements %>% filter(Element == "Fe") %>% 
  pivot_longer(c(Ka, Kb, La, Lb, Ma, Mb), names_to = "spectral_line", values_to = "energy") %>%
  filter(!is.na(energy))
manganese = elements %>% filter(Element == "Mn") %>% 
  pivot_longer(c(Ka, Kb, La, Lb, Ma, Mb), names_to = "spectral_line", values_to = "energy") %>%
  filter(!is.na(energy))


##can set minPH to define minimum height of peak to be reported
identify_element_peaks = function(data) {
  ##can set minPH to define minimum height of peak to be reported
  ##can set minPW 
  elem = peaks(data$Energy, data$count, minPH = 100) %>%
    mutate(lower = x-w, 
           upper = x+w) %>%
    ####Ka join####
  left_join(elements, 
            by = join_by(lower <= Ka, 
                         upper >= Ka)) %>%
    mutate(element_Ka = Element) %>%
    select(x,y,w,lower,upper,element_Ka) %>%
    ####Kb join####
  left_join(elements, 
            by = join_by(lower <= Kb, 
                         upper >= Kb)) %>%
    mutate(element_Kb = Element) %>%
    select(x,y,w,lower,upper,element_Ka, element_Kb) %>%
    ####La join####
  left_join(elements, 
            by = join_by(lower <= La, 
                         upper >= La)) %>%
    mutate(element_La = Element) %>%
    select(x,y,w,lower,upper,element_Ka, element_Kb, element_La) %>%
    ####Lb join####
  left_join(elements, 
            by = join_by(lower <= Lb, 
                         upper >= Lb)) %>%
    mutate(element_Lb = Element) %>%
    select(x,y,w,lower,upper,element_Ka, element_Kb, element_La, element_Lb)
  
  return(elem)
}


p1.83.less = read_spectra_table("data/initial-microscope-data/EDX-results/block/p1_83_less_spect_box.txt")
p1.83.less$sample_num = "p1-83"
p1.83.less$surface_condition = "less"
p1.83.more = read_spectra_table("data/initial-microscope-data/EDX-results/block/p1_83_more_spect_box.txt")
p1.83.more$sample_num = "p1-83"
p1.83.more$surface_condition = "more"

p1.231.less = read_spectra_table("data/initial-microscope-data/EDX-results/block/p1_231_less_spect_box.txt")
p1.231.less$sample_num = "p1-231"
p1.231.less$surface_condition = "less"
p1.231.more = read_spectra_table("data/initial-microscope-data/EDX-results/block/p1_231_more_spect_box.txt")
p1.231.more$sample_num = "p1-231"
p1.231.more$surface_condition = "more"

p5.1777.less = read_spectra_table("data/initial-microscope-data/EDX-results/block/p5_1777_less_spect_box.txt")
p5.1777.less$sample_num = "p5-1777"
p5.1777.less$surface_condition = "less"
p5.1777.more = read_spectra_table("data/initial-microscope-data/EDX-results/block/p5_1777_more_spect_box.txt")
p5.1777.more$sample_num = "p5-1777"
p5.1777.more$surface_condition = "more"

p5.1843.less = read_spectra_table("data/initial-microscope-data/EDX-results/block/p5_1843_less_spect_box.txt")
p5.1843.less$sample_num = "p5-1843"
p5.1843.less$surface_condition = "less"
p5.1843.more = read_spectra_table("data/initial-microscope-data/EDX-results/block/p5_1843_more_spect_box.txt")
p5.1843.more$sample_num = "p5-1843"
p5.1843.more$surface_condition = "more"

p5.1962.less = read_spectra_table("data/initial-microscope-data/EDX-results/block/p5_1962_less_spect_box.txt")
p5.1962.less$sample_num = "p5-1962"
p5.1962.less$surface_condition = "less"
p5.1962.more = read_spectra_table("data/initial-microscope-data/EDX-results/block/p5_1962_more_spect_box.txt")
p5.1962.more$sample_num = "p5-1962"
p5.1962.more$surface_condition = "more"

survey.less = read_spectra_table("data/initial-microscope-data/EDX-results/block/survey_less_spect_box.txt")
survey.less$sample_num = "s-survey"
survey.less$surface_condition = "less"
survey.more = read_spectra_table("data/initial-microscope-data/EDX-results/block/survey_more_spect.txt")
survey.more$sample_num = "s-survey"
survey.more$surface_condition = "more"

alldata = rbind(p1.83.less, p1.83.more, p1.231.less, p1.231.more, 
                p5.1777.less, p5.1777.more, p5.1843.less, p5.1843.more, 
                p5.1962.less, p5.1962.more, survey.less, survey.more)

plot_spectra = function(data) {
  pal1 <- RColorBrewer::brewer.pal(3, "Set2")
  names(pal1) = c("iron", "manganese", "other")
  
  return(
    ggplot(p1.83.less, aes(x = Energy, y = count)) +
      geom_vline(data = iron, aes(xintercept = energy, color = "iron")) +
      geom_vline(data = manganese, aes(xintercept = energy, color = "manganese")) +
      geom_area(alpha = 0.5) +
      scale_color_manual(values = pal1) +
      theme(legend.position = "bottom", legend.title = element_blank())
  )
}

compare_spectra = function(ldata, mdata) {
  pal1 <- RColorBrewer::brewer.pal(3, "Accent")
  names(pal1) = c("iron", "manganese", "other")
  
  pal2 = c("#E69F00", "#56B4E9")
  names(pal2) = c("more weathered", "less weathered")
  
  plot = ggplot(mapping = aes(x = Energy, y = count)) +
    geom_vline(data = iron, aes(xintercept = energy, color = "iron"), size = 0.25) +
    geom_vline(data = manganese, aes(xintercept = energy, color = "manganese"), size = 0.25) +
    geom_area(data = mdata, alpha = 0.75, mapping = aes(fill = "more weathered")) +
    geom_area(data = ldata, alpha = 0.75, mapping = aes(fill = "less weathered")) +
    scale_fill_manual(values = pal2) +
    scale_color_manual(values = pal1) +
    labs(fill = "surface condition", color = "element", x = "Energy (keV)") +
    theme(axis.title = element_text(size = 6), axis.text = element_text(size = 5))
  return(plot)
  
}

# compare_spectra(p1.83.less, p1.83.more)
# compare_spectra(p1.231.less, p1.231.more)
# compare_spectra(p5.1777.less, p5.1777.more)
# compare_spectra(p5.1843.less, p5.1843.more)
# compare_spectra(p5.1962.less, p5.1962.more)
# compare_spectra(survey.less, survey.more)

p1 = ggarrange(
  compare_spectra(p1.83.less, p1.83.more),
  compare_spectra(p1.231.less, p1.231.more),
  compare_spectra(p5.1777.less, p5.1777.more),
  compare_spectra(p5.1843.less, p5.1843.more),
  compare_spectra(p5.1962.less, p5.1962.more),
  compare_spectra(survey.less, survey.more), 
  labels = "AUTO", common.legend = T, ncol = 2, nrow = 3
)
plot(p1)

ggsave(filename = "figures/SM_spot-scan-comparison.tiff", p1, 
       dpi = 300, height = 12, width = 12)


####compare counts####
Fe.data = alldata %>% filter(Energy == iron$energy[3])
