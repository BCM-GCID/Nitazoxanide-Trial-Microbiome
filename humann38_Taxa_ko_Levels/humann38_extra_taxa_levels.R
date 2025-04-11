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

#Code to split byTaxa Humann 3.8 tables by taxonomic level.

byTaxa_files <- list.files("./humann38/", pattern = "*byTaxa.txt", full.names = TRUE)
out_dir <- './extraTaxaLevels/'

taxonomy <- read_tsv("mpa_v31_CHOCOPhlAn_201901_taxonomy.txt",
                     col_names = c('taxonomy', 'taxids', 'length')) %>%
            select(-c('taxids', 'length')) %>%
            separate(taxonomy, into = c("Kingdom", "Phylum", "Class", "Order",
                              "Family", "Genus", "Species"), sep = "\\|") %>%
            distinct()

for (file_path in byTaxa_files) {
  file<- tools::file_path_sans_ext(basename(file_path))
  
  print(file)
  
  byTaxa <- read_tsv(file_path, show_col_types = FALSE) %>%
             rename_with(~ gsub("#", "", .x)) %>%
             rename_with(~ gsub(" ", "", .x)) %>%
             select(-taxonomy)
  
  colOne<-names(byTaxa)[1]
  byTaxa <- byTaxa %>%
            separate(col = 1, into = c("Feature", "Species"), sep = "\\|") 

  unclassified <- byTaxa %>%
                  filter(select(., 1) == 'UNMAPPED' | select(., 2) == 'unclassified')
  write.table(unclassified, file=paste(out_dir, paste(file,"unclassified",'txt', sep='.'),sep=''), 
              quote=FALSE, sep='\t', row.names = FALSE)
  
  byTaxa <- byTaxa %>%
             inner_join(taxonomy, by=join_by(Species)) 

  byGenus <- byTaxa %>%
              select(-c("Kingdom", "Phylum", "Class", "Order",
                        "Family", "Species")) %>%
              group_by(Feature, Genus) %>%
              summarize(across(everything(), sum, .names = "{col}"))%>%
              rename(!!colOne := Feature)

  write.table( byGenus, file=paste(out_dir, paste(file,"byGenus",'txt', sep='.'),sep=''),
              quote=FALSE, sep='\t', row.names = FALSE)
  
  byFamily <- byTaxa %>%
    select(-c("Kingdom", "Phylum", "Class", "Order",
              "Genus", "Species")) %>%
    group_by(Feature, Family) %>%
    summarize(across(everything(), sum, .names = "{col}"))%>%
    rename(!!colOne := Feature)
  
  write.table( byFamily, file=paste(out_dir, paste(file,"byFamily",'txt', sep='.'),sep=''),
              quote=FALSE, sep='\t', row.names = FALSE)
  
  byOrder <- byTaxa %>%
    select(-c("Kingdom", "Phylum", "Class", "Family",
              "Genus", "Species")) %>%
    group_by(Feature, Order) %>%
    summarize(across(everything(), sum, .names = "{col}"))%>%
    rename(!!colOne := Feature)
  
  write.table( byOrder, file=paste(out_dir, paste(file,"byOrder",'txt', sep='.'),sep=''),
              quote=FALSE, sep='\t', row.names = FALSE)
  
  byClass <- byTaxa %>%
    select(-c("Kingdom", "Phylum", "Order", "Family",
              "Genus", "Species")) %>%
    group_by(Feature, Class) %>%
    summarize(across(everything(), sum, .names = "{col}"))%>%
    rename(!!colOne := Feature)
  
  write.table( byClass, file=paste(out_dir, paste(file,"byClass",'txt', sep='.'),sep=''),
              quote=FALSE, sep='\t', row.names = FALSE)

  byPhylum <- byTaxa %>%
    select(-c("Kingdom", "Class", "Order", "Family",
              "Genus", "Species")) %>%
    group_by(Feature, Phylum) %>%
    summarize(across(everything(), sum, .names = "{col}"))%>%
    rename(!!colOne := Feature)
  
  write.table( byPhylum, file=paste(out_dir, paste(file,"byPhylum",'txt', sep='.'),sep=''),
              quote=FALSE, sep='\t', row.names = FALSE)
  
  byKingdom <- byTaxa %>%
    select(-c("Phylum", "Class", "Order", "Family",
              "Genus", "Species")) %>%
    group_by(Feature, Kingdom) %>%
    summarize(across(everything(), sum, .names = "{col}"))%>%
    rename(!!colOne := Feature)
  
  write.table( byKingdom, file=paste(out_dir, paste(file,"byKingdom",'txt', sep='.'),sep=''),
              quote=FALSE, sep='\t', row.names = FALSE)
  
  # check1<-bind_rows(unclassified %>% select(contains("p1709-"))) %>%
  #   summarize(across(everything(), sum))
  # 
  # check2<-bind_rows(byKingdom %>% select(contains("p1709-"))) %>%
  #   summarize(across(everything(), sum)) %>%
  #   select(-c(1)) %>%
  #   summarize(across(everything(), sum))
  # 
  # check4<-bind_rows(check1, check2) %>% summarize(across(everything(), sum))
  # 
  # print(check3[1:5])
  # print(check4[1:5])
}