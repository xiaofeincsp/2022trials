
rm(list = ls()) 
library(tidyverse)
library(readxl)
library(dplyr)
library(knitr)
library(rmarkdown)
library(stringr)

folder = "D:\\OneDrive - CGIAR\\01_2023_2023\\trial_trial\\Vietnam_2022-2023\\2022F1C_Dong Nai\\"
dat2loc = read.csv(paste(folder,"2022DMF1C__mean_2locs_2023-02-02.csv", sep=""),
                   stringsAsFactors = FALSE)

traitlist = c( "plot_name",  "accession_name" ,          
               "parents_num" , "plot_number",              
               "row_number" , "col_number",               
               "is_a_control" ,   "rep_number",               
               "Germination"  ,  "Vigor" ,                   
               "CMD_M1", "CMD_M3" , "CMD_M6",               
               "CMD_M9",  "CWBD_M9", "CBB_M9" ,
               "Red_Spider_Mites" ,
               "No_Main_Stem"  ,           
               "Shooting" ,  "Branching_Levels",          
               "Height_Of_Main_Stem" ,   "Height_To_1st_Branch",
               "Plant Type" , "Root Type" ,               
               "Peduncle" , "External Color" ,
               # "Root Shape" ,
               "Number Of Harvested Plant",
               "Number Of Roots" , "Root Weight" ,
               "Root Constriction" , "Starch Content",
               "Number_Of_Stems_Collected")

traitlist = gsub(" ", "_", traitlist )
traitlist = gsub("\\.", "_", traitlist )
pheno_trait = traitlist[9:length(traitlist)]
pheno_trait = pheno_trait [!pheno_trait =="Red_Spider_Mites"]

str(dat2loc)
setdiff(pheno_trait, names(dat2loc) )
dat2locSel = dat2loc[, traitlist] %>%
  mutate(across(all_of(pheno_trait), as.numeric) )



write.csv(dat2locSel, paste(folder,"2022DMF1C__mean_2locs_sel_", Sys.Date(), ".csv", sep=""),
          row.names=FALSE)





## ****************** hertability FUNCTION START ************************************* ##
library(lme4)
#install.packages("lme4")

dat2locSel$trial_name = str_sub(dat2locSel$plot_name, 1,14)

plot_dat = dat2locSel
h_col = pheno_trait
h2_file = "01_2022F1C_2loc_Vietnam_heritability.csv"
BLUP_file = "01_2022F1C_2loc_Vietnam_BLUP.csv"

#env_list = unique(plot_dat$environment)
h_wthin_trial = data.frame(matrix(nrow = 1,
                                  ncol = length(h_col)+1))
colnames(h_wthin_trial) = c("environment", h_col)
h_wthin_trial$environment = "2022F1C_2loc"

BLUP = data.frame(matrix(nrow = length (unique(plot_dat$accession_name)),
                         ncol = 1) )
names(BLUP) = "accession_name"
BLUP$accession_name = unique(plot_dat$accession_name)

plot_dat$accession_name = as.factor(plot_dat$accession_name)
plot_dat$trial_name = as.factor(plot_dat$trial_name)

for(i in 1:length(h_col)){
  g.ran <- lmer(plot_dat[, h_col[i]] ~ trial_name + (1|accession_name), data=plot_dat)
  vc.g <- summary(g.ran)$varcor$accession_name[1] 
  vc.e <- summary(g.ran)$sigma^2
  H2.s <- vc.g / (vc.g + (vc.e/2))
  h_wthin_trial[,i+1] = H2.s
  
  
  BLUP_i= data.frame(ranef(g.ran)$accession_name )
  names(BLUP_i) = paste("BLUP_", h_col[[i]], sep="")
  BLUP = BLUP %>%
    left_join(rownames_to_column(BLUP_i), by=c( "accession_name"= "rowname" )) 
  
} 
write.csv(h_wthin_trial, paste(folder, h2_file, sep=""), row.names = FALSE)
BLUP = BLUP %>%
  arrange(desc (BLUP_Root_Weight))
write.csv(BLUP,paste(folder, BLUP_file, sep=""), row.names = FALSE)

## ****************** FUNCTION END *************************************** ##

?lmer

BLUP_i= data.frame(ranef(g.ran)$accession_name )
names(BLUP_i) = paste("BLUP_", h_col[[i]], sep="")

BLUP = BLUP %>%
  left_join(rownames_to_column(BLUP_i), by=c( "accession_name"= "rowname" ))


  BLUP %>%
    arrange(desc (BLUP))

