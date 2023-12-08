# This script creates a model for TP from sample data collected for the L8 FEBOG study. 

library(dplyr)
library(ggplot2)
library(readr)
library(tidymodels)
library(ggrepel)
library(ggpmisc)


# Import data -------------------------------------------------------------
Ion_Ratios <- read_csv("Data/Ion Ratios.csv")


# Tidy Data ---------------------------------------------------------------

#tidy monitoring data from study
TP_Model_Data <- Ion_Ratios %>%
filter(COLLECT_METHOD=="G",Position =="Top" ,!is.na(`PHOSPHATE, TOTAL AS P`))   #filter to sample data collected from 0.5m depth. Grab samples only


# Create Model ------------------------------------------------------------

lm_OPO4_TURBIDITY_fit_top <- lm(`PHOSPHATE, TOTAL AS P` ~ `PHOSPHATE, ORTHO AS P`+TURBIDITY,data = TP_Model_Data) 

saveRDS(lm_OPO4_TURBIDITY_fit_top, "Data/lm_OPO4_TURBIDITY_fit_top.rds")

# Model Predictions --------------------------------------------------------------

#OPO4 Only All positions 
lm_OPO4_TURBIDITY_fit_top_predictions <- TP_Model_Data %>%
bind_cols(predict(lm_OPO4_TURBIDITY_fit_top ,newdata = TP_Model_Data)) %>%
rename(`Predicted TP`="...53")

#visualize predictions from top. 
ggplot(lm_OPO4_TURBIDITY_fit_top_predictions ,aes(x =`PHOSPHATE, TOTAL AS P`*1000, y = `Predicted TP`*1000,label=paste(DATE," ",CELL," ",Position)))+geom_point()+geom_smooth(method="lm",se=FALSE)+geom_text_repel()+
stat_poly_eq(formula = y~x, aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"),color="red"),parse = TRUE)+theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+scale_x_continuous(breaks = pretty_breaks(n=5))+
labs(y= expression(Predicted~mu~L^-1),x=expression(Measured~mu~L^-1), title = "Using OPO4 to predict TP in L-8 FEB\nSamples from all Depths used in Model ")

