#Predict TP using sensor data
#Steps
#1.) Import Data
#2.) Tidy Sensor Data and Compliance data
#3.) Predict TP



library(dplyr)
library(ggplot2)
library(readr)
library(tidymodels)
library(ggrepel)
library(ggpmisc)
library(lubridate)
library(dbhydroR)


# Import Data -------------------------------------------------------------


lm_OPO4_TURBIDITY_fit_top <- readRDS("Data/lm_OPO4_TURBIDITY_fit_top.rds")     #Load model
L8_Sensor_data <- read_csv("Data/fts-data_2023-11-01T08_13_00.000Z_2023-12-06T16_13_00.000Z.csv")  #Imports sensor data

Compliance_data <- get_wq(station_id= c("G539","G538"), date_min = "2001-03-01",date_max=as.character(today()),test_name =c("PHOSPHATE, TOTAL AS P",	"TOTAL NITROGEN",	"TEMP",	"SP CONDUCTIVITY, FIELD",	"DISSOLVED OXYGEN",	"PH, FIELD","NITRATE-N","KJELDAHL NITROGEN, TOTAL",	"PHOSPHATE, ORTHO AS P",	"NITRATE+NITRITE-N",	"NITRITE-N",	"CHLORIDE",	"TURBIDITY"	,"TOTAL DISSOLVED SOLIDS", "IRON, TOTAL","SULFATE"),raw=TRUE) #DBHYDRO WQ at compliance site


# Tidy Data ---------------------------------------------------------------

L8_Sensor_data_tidy <- L8_Sensor_data %>%
mutate(Date=ymd_hms(Date)) %>%
mutate(date=as.Date(Date),Hour=hour(Date)) %>%
rename(`PHOSPHATE, ORTHO AS P`="mPO4",TURBIDITY="Turb",DATE="Date") %>%
mutate(`QC Flag`=case_when(TURBIDITY>100~"Turbidity out of range",
                           `PHOSPHATE, ORTHO AS P`<0~"OPO4 Sensor out of range",
                           `mQC`>1~"OPO4 Sensor Failed QC")) 

#Predict TP
L8_Sensor_data_tidy_predictions <- L8_Sensor_data_tidy %>%
bind_cols(predict(lm_OPO4_TURBIDITY_fit_top ,newdata = L8_Sensor_data_tidy)) %>%
rename(`Predicted TP`="...17") 

Compliance_data_tidy <- Compliance_data %>%
filter(Sample.Type.New=="SAMP",Station.ID=="G539",Test.Name %in% c("PHOSPHATE, TOTAL AS P","PHOSPHATE, ORTHO AS P")) %>%
mutate(Hour=hour(dmy_hm(Collection_Date))) %>%
mutate(date=as.Date(dmy_hm(Collection_Date))) %>%
pivot_wider(names_from=Test.Name,values_from=Value) %>%
select(date,Hour,`PHOSPHATE, TOTAL AS P`,`PHOSPHATE, ORTHO AS P`)  %>%
rename(`PHOSPHATE, ORTHO AS P (Compliance)`="PHOSPHATE, ORTHO AS P",`PHOSPHATE, Total AS P (Compliance)`="PHOSPHATE, TOTAL AS P")

Sensor_Compliance_Data_Tidy <- L8_Sensor_data_tidy_predictions  %>%
full_join(Compliance_data_tidy,by=c("date","Hour")) %>%
mutate(`Date Time`=ISOdatetime(year=year(date),month=month(date),day=day(date),hour=Hour,min=0,sec = 0))

# Visualize -------------------------------------------------------------


#visualize predictions from all data
ggplot(Sensor_Compliance_Data_Tidy  ,aes(x =DATE, y = `Predicted TP`*1000))+geom_point()+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+facet_wrap(~`QC Flag`,scales = "free")+
labs(y= expression(Predicted~mu~L^-1),x="Date", title = "Using sensor data to predict TP in L-8 FEB")

#visualize predictions from data that passed QC
ggplot(filter(Sensor_Compliance_Data_Tidy,is.na(`QC Flag`),DATE> "2023-11-22 00:00:00" ) ,aes(x =DATE, y = `Predicted TP`*1000))+geom_point()+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+
labs(y= expression(Predicted~mu~L^-1),x="Date", title = "Using sensor data to predict TP in L-8 FEB")

#Long Data to display all parameters
OPO4_Turbidity_TP <- Sensor_Compliance_Data_Tidy%>%
select(DATE,`PHOSPHATE, ORTHO AS P`,`Predicted TP`,TURBIDITY,`QC Flag`) %>%
pivot_longer(names_to = "Parameter",values_to = "Value",2:4) %>%
filter(is.na(`QC Flag`),DATE> "2023-11-22 00:00:00")

#display predicted TP, Turbidity, and OPO4
ggplot(OPO4_Turbidity_TP  ,aes(x =DATE, y = Value,color=Parameter))+geom_point()+geom_line()+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+facet_wrap(~`Parameter`,scales = "free",nrow=3)+
labs(x="Date", title = "Using sensor data to predict TP in L-8 FEB")

#compliance and sensor data
ggplot(filter(Sensor_Compliance_Data_Tidy,is.na(`QC Flag`),`Date Time`> "2023-08-01 00:00:00" ) ,aes(x =`Date Time`, y = `Predicted TP`*1000))+geom_point()+
geom_point(aes(x =`Date Time`, y = `PHOSPHATE, Total AS P (Compliance)`*1000),color="red")+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+
labs(y= expression(Predicted~mu~L^-1),x="Date", title = "Using sensor data to predict TP in L-8 FEB")
