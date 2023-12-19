#Predict TP using sensor data
#Steps
#1.) Import Data
#2.) Tidy Sensor Data and Compliance data
#3.) Predict TP



library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidymodels)
library(ggrepel)
library(ggpmisc)
library(lubridate)
library(dbhydroR)


# Import Data -------------------------------------------------------------


lm_OPO4_TURBIDITY_fit_top <- readRDS("Data/lm_OPO4_TURBIDITY_fit_top.rds")     #Load model
L8_Sensor_data <-read_csv("Data/fts-data_2023-11-01T05_44_00.000Z_2023-12-19T13_44_00.000Z.csv")  #Imports sensor data
Provisional_Data <- read_excel("Data/Provisional Data.xlsx") #import provisional data
Compliance_data <- get_wq(station_id= c("G539","G538"), date_min = "2001-03-01",date_max=as.character(today()),test_name =c("PHOSPHATE, TOTAL AS P",	"TOTAL NITROGEN",	"TEMP",	"SP CONDUCTIVITY, FIELD",	"DISSOLVED OXYGEN",	"PH, FIELD","NITRATE-N","KJELDAHL NITROGEN, TOTAL",	"PHOSPHATE, ORTHO AS P",	"NITRATE+NITRITE-N",	"NITRITE-N",	"CHLORIDE",	"TURBIDITY"	,"TOTAL DISSOLVED SOLIDS", "IRON, TOTAL","SULFATE"),raw=TRUE) #DBHYDRO WQ at compliance site


# Tidy Data ---------------------------------------------------------------

#tidy sensor data
L8_Sensor_data_tidy <- L8_Sensor_data %>%
mutate(Date=ymd_hms(Date)) %>%
mutate(date=as.Date(Date),Hour=hour(Date)) %>%
rename(`PHOSPHATE, ORTHO AS P`="mPO4",TURBIDITY="Turb",DATE="Date") %>%
mutate(`QC Flag`=case_when(TURBIDITY>100~"Turbidity out of range",
                           `PHOSPHATE, ORTHO AS P`<0~"OPO4 Sensor out of range",
                           `mQC`>1~"OPO4 Sensor Failed QC")) 

#Predict TP from sensor
L8_Sensor_data_tidy_predictions <- L8_Sensor_data_tidy %>%
bind_cols(predict(lm_OPO4_TURBIDITY_fit_top ,newdata = L8_Sensor_data_tidy, interval = "prediction")) %>%
rename(`Predicted TP`="fit") 

#tidy compliance data- Passed QC
Compliance_data_tidy <- Compliance_data %>%
filter(Sample.Type.New=="SAMP",Station.ID %in% c("G539","G538") ,Test.Name %in% c("PHOSPHATE, TOTAL AS P","PHOSPHATE, ORTHO AS P")) %>%
mutate(Hour=hour(dmy_hm(Collection_Date))) %>%
mutate(date=as.Date(dmy_hm(Collection_Date))) %>%
pivot_wider(names_from=c(Test.Name,Station.ID),values_from=Value) %>%
select(date,Hour,`PHOSPHATE, TOTAL AS P_G538`,`PHOSPHATE, TOTAL AS P_G539`,`PHOSPHATE, ORTHO AS P_G539`) 
#%>%
rename(`PHOSPHATE, ORTHO AS P (Compliance)`="PHOSPHATE, ORTHO AS P",`PHOSPHATE, Total AS P (Compliance)`="PHOSPHATE, TOTAL AS P")

#Tidy Provisional Data
Provisional_Data_tidy <- Provisional_Data  %>%
filter(TEST_NAME %in% c("PHOSPHATE, ORTHO AS P","PHOSPHATE, TOTAL AS P"),SAMPLE_TYPE=="SAMP",STATION_ID=="G539")  %>%
mutate(date=as.Date(ymd_hms(DATE_COLLECTED)), Hour=hour(ymd_hms(DATE_COLLECTED))) %>%
select(date,Hour,TEST_NAME,VALUE)  %>%
pivot_wider(names_from = TEST_NAME,values_from = VALUE) %>%
rename(`PHOSPHATE, ORTHO AS P (Provisional)`="PHOSPHATE, ORTHO AS P",`PHOSPHATE, Total AS P (Provisional)`="PHOSPHATE, TOTAL AS P")

#Join all data streams
Sensor_Compliance_Data_Tidy <- L8_Sensor_data_tidy_predictions  %>%
full_join(Provisional_Data_tidy ,by=c("date","Hour")) %>%  
left_join(Compliance_data_tidy ,by=c("date","Hour")) %>%    
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
labs(x="Date", title = "Sensor Data and estimated TP")

#compliance and sensor data entire time series
ggplot(filter(Sensor_Compliance_Data_Tidy,is.na(`QC Flag`),`Date Time`> "2023-08-01 00:00:00" ) ,aes(x =`Date Time`, y = `Predicted TP`*1000))+geom_point()+
geom_ribbon(aes(ymin=lwr*1000,ymax=upr*1000),alpha=.5)+  
geom_point(aes(x =`Date Time`, y = `PHOSPHATE, Total AS P (Provisional)`*1000),color="red")+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=5))+
labs(y= expression(Predicted~mu~L^-1),x="Date", title = "Using sensor data to predict TP in L-8 FEB")

#compliance and sensor TP data last two weeks
ggplot(filter(Sensor_Compliance_Data_Tidy,is.na(`QC Flag`),`Date Time`> "2023-08-01 00:00:00" ) ,aes(x =`Date Time`, y = `Predicted TP`*1000))+geom_point(shape=21,fill="blue",size=2.5)+
geom_ribbon(aes(ymin=lwr*1000,ymax=upr*1000),alpha=.4,fill="lightblue")+  geom_line(linetype="dashed",color="blue",alpha=.5)+
geom_point(aes(x =`Date Time`, y = `PHOSPHATE, Total AS P (Provisional)`*1000),color="red",size=3)+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=10))+coord_cartesian(xlim=c(Sys.time()-weeks(2),Sys.time()))+scale_x_datetime(breaks = pretty_breaks(n=7))+
labs(y= expression(Predicted~TP~mu~L^-1),x="Date", title = "Predicted TP using OPO4 and turbidity sensor data at L-8 FEB discharge",caption = "Shaded area is prediction interval for TP")

#compliance and sensor OPO4 data entire last two weeks
ggplot(filter(Sensor_Compliance_Data_Tidy,is.na(`QC Flag`),`Date Time`> "2023-08-01 00:00:00" ) ,aes(x =`Date Time`, y = `PHOSPHATE, ORTHO AS P`*1000))+geom_point(shape=21,fill="blue",size=2.5)+
 geom_line(linetype="dashed",color="blue",alpha=.5)+
geom_point(aes(x =`Date Time`, y = `PHOSPHATE, ORTHO AS P (Provisional)`*1000),fill="red",size=3,shape=21)+
theme_bw()+scale_y_continuous(breaks = pretty_breaks(n=10))+coord_cartesian(xlim=c(Sys.time()-weeks(2),Sys.time()))+scale_x_datetime(breaks = pretty_breaks(n=7))+
labs(y= expression(Predicted~TP~mu~L^-1),x="Date", title = "OPO4 sensor data and provisional compliance data at L-8 FEB discharge",caption = "OPO4 compliance in red. Sensor data in blue.")

