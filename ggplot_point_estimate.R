rm(list=ls())
path <- "/Users/likangli/Google Drive wisc/covid19 and household spending/visualization/"

library(rvest)
library(splashr)
library('data.table')
library('readxl')
library(tidyverse)
library(beepr)
library(cowplot)
library(ggalt)
library(RColorBrewer)
library(stringr)

data<-read.csv(paste0(path,"reg_results.csv"))
data$level =ifelse(data$subcategory=="", 'cat1','cat2')

data$label[data$label=="finance and employment"] <- factor("fin. and empl.")

Depvar = c("perch", "diff")

## plots' loop
for (d in Depvar){
  ylabel = ifelse(d=="perch", "%", "$")
  f = ifelse(d=="perch", "percentage", "spending")
  
  # data for plot
  data_plot <- data %>%
    filter(sample== paste0(d, "_All"))

  p <- ggplot(data_plot, aes(x=label, y=beta, group=category, color=factor(category))) +
    geom_errorbar(aes(ymin=CI_low, ymax=CI_high), width=.2,color="darkgrey", position=position_dodge(.9)) +
    geom_point(size=1.6) +
    geom_hline(yintercept = 0, color='black') +
    scale_x_discrete(name="") +
    scale_y_continuous(name=paste0("policy effect (", ylabel, ")")) +
    theme(legend.position = "bottom", legend.title=element_blank()) + 
    coord_flip() +
    facet_wrap(vars(treatment)) 

  file = paste0(path, f, ".pdf")
  ggsave(file,height=8, width=10, units='in')
}


# color<-c("#a50026",
#          "#d73027",
#          "#f46d43",
#          "#fdae61",
#          "#fee090",
#          "#e0f3f8",
#          "#abd9e9",
#          "#74add1",
#          "#4575b4",
#          "#313695")

#   theme(legend.position="top", legend.title.align=-80,legend.key.size = unit(5, "mm"), axis.text.x = element_text(size=12), 
#         axis.title.x=element_text(vjust=-2)) +
#   guides(linetype=guide_legend(nrow=4)) +
#   guides(color = guide_legend(title.position = "top", 
#                               # hjust = 0.5 centres the title horizontally
#                               title.hjust = 0.5,
#                               label.position = "bottom")) + 

# scale_color_manual(name="Category", 
# values=color,
# labels=c("Entertain", "Endocrine", "Nose, mouth,\n and pharynx",
#          "Cardiovascular", "Hemic and \n lymphatic", "Urinary",
#          "Female genital \n organs", "Obstetrical", "Musculoskeletal",
#          "Miscellaneous")) 


