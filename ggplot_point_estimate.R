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
