library(ggplot2)
library(data.table)
library(stringr)
library(dplyr)
library(readxl)
library(lubridate)
library(tidyr)
library(tidyverse)
library(ggpubr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#code to assign higher level categories to ko pathways and aggregate the values at higher levels. 

ko_levels<- read_tsv("ko_pathway_mapping_levels.tsv") 

out_dir <- './koLevelAggregates/'

filter_out<-c('UNMAPPED','UNINTEGRATED')

ko_pathways <- read_tsv("./humann38/KeggPathwaysAbundance.byTaxa.txt") %>%
                rename('pathway'=`# Pathway`) %>%
                filter(!str_detect(pathway, paste(filter_out, collapse = "|"))) %>%
                separate(pathway, into = c("pathway", "species"), sep = "\\|") %>% 
                left_join(ko_levels, by=join_by("pathway" == "KO"))

level1_aggregate <- ko_pathways %>%
                    select(-c("taxonomy", "pathway", "Level2", "Level3")) %>%
                     group_by(species, Level1) %>%
                     summarize(across(everything(), sum, .names = "{col}"))

write.table(level1_aggregate, file=paste(out_dir, 'ko_level1_aggregates', '.tsv', sep=''), 
            quote=FALSE, sep='\t', row.names = FALSE)                      

level2_aggregate <- ko_pathways %>%
                      select(-c("taxonomy", "pathway", "Level1", "Level3")) %>%
                      group_by(species, Level2) %>%
                      summarize(across(everything(), sum, .names = "{col}"))

write.table(level2_aggregate, file=paste(out_dir, 'ko_level2_aggregates', '.tsv', sep=''), 
            quote=FALSE, sep='\t', row.names = FALSE)                      
