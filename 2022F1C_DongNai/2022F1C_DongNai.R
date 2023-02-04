rm(list = ls()) 
library(tidyverse)
library(readxl)
library(dplyr)
library(knitr)
library(rmarkdown)
library(stringr)
# install.packages("statgenSTA")
#library(statgenSTA)
#library(statgenGxE)
#library(openxlsx)


folder = "D:\\OneDrive - CGIAR\\01_2023_2023\\trial_trial\\Vietnam_2022-2023\\2022F1C_Dong Nai\\"
file = "01_F1C1 dona data_2022Feb02.xlsx"

data = read_excel(paste(folder, file, sep=""),
                  sheet = "dona",
                  skip=0)
data = as.data.frame(data)
str(data)

chg_col = c("CWBD_M9", "CBB_M9", 	"Shooting", "No_Main_Stem",
          "Plant_Type"  )

for(i in 1:length(chg_col)){
  for(j in 1:nrow(data)){
    
    missing = 4- 4*data[j, ]$Germination 
    if (missing <4){ 
      data[j, chg_col[i] ] = 
        sum( as.numeric( strsplit(as.character(data[j, chg_col[i] ] ), "\\.") [[1]] )  ) / (4-missing) 
    }
    
    if(missing ==4) {
      data[j, chg_col[i] ] = 0
    }
    
    
  }
  
}

dongNai = data
View(dongNai)

 write.csv(dongNai, paste(folder,"2022DMF1C_DongNai_mean_", Sys.Date(), ".csv", sep=""),
          row.names=FALSE)



# selected traits
# names(tayNinh)
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
               "Root Shape" , "Number Of Harvested Plant",
               "Number Of Roots" , "Root Weight" ,
               "Root Constriction" , "Starch Content"  ,
               "Number_Of_Stems_Collected")
traitlist = gsub(" ", "_", traitlist )
traitlist = gsub("\\.", "_", traitlist )


pheno_trait = traitlist[9:length(traitlist)]

# convert into numeric
dongNai$Red_Spider_Mites = NA
dongnaiSel = dongNai[, traitlist] %>%
  mutate(across(all_of(pheno_trait), as.numeric) )



locs2 = rbind(tayNinh[, traitlist], dongNai[, traitlist])
names(locs2)
write.csv(locs2, paste(folder,"2022DMF1C__mean_2locs_", Sys.Date(), ".csv", sep=""),
          row.names=FALSE)



# not selected traits
setdiff(names(dongNai), names(dongnaiSel))

# View(tayninhSel)
str(dongnaiSel)

trial_data = dongnaiSel
#### function visualize the layout -

myplot <- ggplot(trial_data, aes(x=col_number, y= row_number, fill=rep_number)) +
  geom_tile(color="black", size=0.5) +           # Black border on tiles
  labs(x="col_number", y="row_number", fill = "rep") +
  coord_fixed() +                                # Square tiles
  theme_minimal() +                              # Minimal theme, no grey background
  theme(panel.grid=element_blank(),              # No underlying grid lines
        axis.text.x=element_text(                # Vertical text on x axis
          angle=0,vjust=0.5,hjust=0))
print(myplot)


####
names(trial_data)
library(plotly)
p1 <- trial_data %>%
  ggplot(aes(x = CMD_M1, y= CMD_M6))+geom_point() +
  geom_jitter(width = 0.15, height = 0.15)
ggplotly(p1)


p2 <- trial_data %>%
  ggplot(aes(x = CMD_M3, y= CMD_M6))+geom_point() +
  geom_jitter(width = 0.15, height = 0.15)
ggplotly(p2)

p3 <- trial_data %>%
  ggplot(aes(x = CMD_M9, y= CMD_M6))+geom_point() +
  geom_jitter(width = 0.15, height = 0.15)
ggplotly(p3)



#****************** plot correlation plot **********************

COR_PLOT = function(plot_data = plot_data, 
                    plot_title = plot_title,
                    ylabel = ylabel,
                    xlabel = xlabel){
  
  lm=lm(plot_data$Yvalue ~ plot_data$Xvalue, na.action=na.exclude)
  print(summary(lm))   
  
  # Remove NA rows in the pair
  # plot_data = data.frame(Yvalue = as.numeric(Yvalue), Xvalue = as.numeric(Xvalue))
  # str(plot_data)
  plot_data = plot_data[complete.cases(plot_data), ]
  str(plot_data)
  dim(plot_data)
  # max of the paired data w/t NA
  max.y = max(plot_data$Yvalue, na.rm=TRUE ) * 1.2
  max.x = max(plot_data$Xvalue, na.rm=TRUE ) * 1.2
  min.y = min(plot_data$Yvalue, na.rm=TRUE ) - max(plot_data$Yvalue, na.rm=TRUE ) * 0.2
  min.x = min(plot_data$Xvalue, na.rm=TRUE ) - max(plot_data$Xvalue, na.rm=TRUE ) * 0.2
  myplot = ggplot(plot_data, aes( Xvalue, Yvalue)) +  
    geom_point(size = 3) +
    # geom_jitter(width = 2, height = 2) +
    coord_cartesian(ylim = c(min.y, max.y), xlim = c(min.x, max.x)) +
    geom_abline(intercept= coef(lm)[1] , slope=coef(lm)[2], colour="red", size=1)	+
    theme(axis.text.x = element_text(face="bold", colour="black", size=12),	
          axis.text.y = element_text(face="bold", colour="black", size=12),	
          axis.title.y = element_text(size = rel(1.4), face="bold", angle = 90, vjust = 3),	
          axis.title.x = element_text(size = rel(1.4), face="bold" , vjust = -0.5),
          plot.title = element_text(face="bold", colour="black", size=16, hjust = 0.5, vjust=0.1) ,
          plot.margin = unit(c(1,1,1,1), "cm"))     +  
    ggtitle(plot_title)	+
    labs(x = xlabel, y = ylabel)
  print(myplot)
}



plot_data = trial_data [, c("CMD_M1", "CMD_M6")]
names(plot_data) = c("Xvalue", "Yvalue")
plot_title = "Cor of CMD between time"
ylabel = "CMD severity of 6MAP"
xlabel = "CMD severity of 1MAP"

COR_PLOT (plot_data = plot_data, 
          plot_title = plot_title,
          ylabel = ylabel,
          xlabel = xlabel)


plot_data = trial_data [, c("CMD_M3", "CMD_M6")]
names(plot_data) = c("Xvalue", "Yvalue")
plot_title = "Cor of CMD between time"
ylabel = "CMD severity of 6MAP"
xlabel = "CMD severity of 3MAP"

COR_PLOT (plot_data = plot_data, 
          plot_title = plot_title,
          ylabel = ylabel,
          xlabel = xlabel)

names(trial_data)
plot_data = trial_data [, c("CMD_M9", "CMD_M6")]
names(plot_data) = c("Xvalue", "Yvalue")
plot_title = "Cor of CMD between time"
ylabel = "CMD severity of 6MAP"
xlabel = "CMD severity of 9MAP"

COR_PLOT (plot_data = plot_data, 
          plot_title = plot_title,
          ylabel = ylabel,
          xlabel = xlabel)


names(trial_data)
plot_data = trial_data [, c("Root_Type", "Root_Weight")]
names(plot_data) = c("Xvalue", "Yvalue")
plot_title = "Root yield and type"
ylabel = "Root weight per plot (kg)"
xlabel = "Root type (1-5); 1, best"

COR_PLOT (plot_data = plot_data, 
          plot_title = plot_title,
          ylabel = ylabel,
          xlabel = xlabel)



library(Hmisc)
library(corrplot)
pheno_trait = pheno_trait [!pheno_trait =="Red_Spider_Mites"]
pheno_only = dongnaiSel [, pheno_trait]
#View(pheno_only)



# Compute the correlation -------------------------------------------------

M <- cor(as.matrix(pheno_only), use = "complete.obs")
testRes <- cor.mtest(as.matrix(pheno_only), conf.level = 0.95)


# Save the file -----------------------------------------------------------

pdf(paste(folder, "01_2022DMF1C_dongnai_trait_cor_",
          Sys.Date(),".pdf", sep=""), width = 18, height = 18)

corrplot(M,
         p.mat = testRes$p,
         method = "color",
         sig.level = c(0.001, 0.01, 0.05),
         insig = "label_sig",
         pch.cex = 0.9,
         # addCoef.col = 'black',
         type = "upper",
         diag = FALSE,
         col = colorRampPalette(c("red", "white", "blue"))(40),
         tl.cex = 1.5,
         tl.col = "black",
         addgrid.col = "black"
)$corrPos -> p1
text(p1$x, p1$y, pos = 3, round(p1$corr, 2), col = "black", cex = 1)

dev.off()












