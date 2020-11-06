library(stringr)
library(tidyverse)
library(lubridate)
library(pythonic)


setwd("C:/Users/alex-/Downloads/")
#Einlesen des Datensatzes, erste Zeile überspringen, da ansonsten die falsche Spaltenanzahl angenommen wird
soil_data=read.csv("00_data.csv",sep="",header=F,skip=1,stringsAsFactors = F)

names(soil_data)<-c('Index','Datum','Time', 'Box', 'Date.Time', 'SM1', 'SM1_Flag', 'Temp1', 'Temp1_Flag',
'SM2', 'SM2_Flag', 'Temp2', 'Temp2_Flag', 'SM3', 'SM3_Flag', 'Temp3',
'Temp3_Flag', 'SM4', 'SM4_Flag', 'Temp4', 'Temp4_Flag', 'SM5',
'SM5_Flag', 'Temp5', 'Temp5_Flag', 'SM6', 'SM6_Flag', 'Temp6',
'Temp6_Flag', 'BattV', 'SM1_Depth', 'SM2_Depth', 'SM3_Depth',
'SM4_Depth', 'SM5_Depth', 'SM6_Depth')

soil_data<-as_tibble(soil_data)
soil_data$datetime<-ymd_hms(soil_data$Date.Time)

#Entfernen von unwichtigen Variablen:
soil_data$Datum<-NULL
soil_data$Time<-NULL
soil_data$Date.Time<-NULL

soil_data_2018<-soil_data[soil_data$datetime %within% interval(ymd("20180101"),ymd("20181231")),]
soil_data_2018$datetime

#Erstellen eines Quartal Vektors:
quarter_vector<-map(1:35040,~ymd_hms("20180101010000")+.x*900)%>%reduce(c)
quarter_vector_2018<-quarter_vector[quarter_vector<ymd("20190101")]

quarter_intervals<-interval(quarter_vector_2018, quarter_vector_2018+900)

#Einschränken auf Werte größer erste Stunde des Jahres, da ab da erst der quarter Vektor beginnt
soil_data_2018<-soil_data_2018%>%
  filter(datetime>min(quarter_vector_2018))

soil_data_2018$quarter_interval<-cut(x = soil_data_2018$datetime,breaks = quarter_vector_2018)
soil_data_2018_aggregated<-soil_data_2018%>%
  group_by(Box,quarter_interval)%>%
  summarise_if(is.numeric,function(x){mean(x,na.rm = T)})

#Bei den Kategorialen Variablen immer die erste
soil_data_2018_aggregated_cat<-soil_data_2018%>%
  group_by(Box,quarter_interval)%>%
  summarise_if(is.character,function(x){x[1]})
soil_data_2018_aggregated<-soil_data_2018_aggregated%>%left_join(soil_data_2018_aggregated_cat)


#Fragen: Wie hoch sind die Sensoren miteinander korreliert
map(split(soil_data_2018_aggregated,
      soil_data_2018_aggregated$Box),function(box){
        box<-split(soil_data_2018_aggregated,
                   soil_data_2018_aggregated$Box)[1]
        box$`4`%>%
          select_if(str_detect(names(.),"^SM[:digit:](?!_)"))%>%
          cor(use = "pairwise.complete.obs")%>%
          ggplot()
      })


#SM1 Flag sollte okay sein falls SM 1 relevant
x<-2
y<-4
soil_data_2018
get_cor<-function(var="Temp",x,y){
  soil_data_2018%>%
    ungroup()%>%
    filter(!!parse_expr(paste0(var,x,"_Flag"))=="OK" & !!parse_expr(paste0(var,y,"_Flag"))=="OK")%>%
    select_if(str_detect(names(.),paste0("^",var,"[:digit:](?!_)")))%>%
    cor(use = "pairwise.complete.obs")%>%
    .[x,y]
}

map_dbl(1:6,~get_cor(var = "SM",x=.x,y=3))

#Step 1) Aggregate all SM Values per Box and Time

var<-"SM"

dat_values<-soil_data_2018_aggregated%>%
  select_if(str_detect(names(.),paste0("Box|quarter_interval|^",var,"[:digit:](?!_)")))%>%
  gather(key="SM",value="value",-Box,-quarter_interval)

dat_flag_values<-soil_data_2018_aggregated%>%
  select_if(str_detect(names(.),paste0("Box|quarter_interval|^",var,"[:digit:]_Flag")))%>%
  gather(key="SM",value="flag",-Box,-quarter_interval)%>%
  mutate(SM=str_sub(SM,1,3))

dat<-dat_values%>%
  left_join(dat_flag_values)

#Filtering!
#dat<-dat%>%filter(SM=="SM1"|SM=="SM2")

dat_filtered<-dat%>%ungroup()%>%
  filter(flag=="OK")%>%
  group_by(Box,quarter_interval)%>%
  summarise(value=mean(value))

dat_filtered%>%
  spread(.,key = "Box",value = "value")%>%
  select(-quarter_interval)%>%
  cor(use = "pairwise.complete.obs")%>%
  reshape2::melt()%>%
  #filter(value>0.5)%>%
  ggplot(.,aes(x=factor(Var1), y=factor(Var2),fill=value))+
  #geom_label(aes(x=Var1, y=Var2,fill=value,label=value))+
  geom_tile()+
  theme_bw(base_size = 22)+
  ylab("Box")+
  xlab("Box")

ggplot(soil_data_2018_aggregated, aes(y=BattV,x=Temp1_Flag))+
  geom_boxplot()



dat

glm(data = soil_data_2018_aggregated)


#Try to work with long format:
unique(dat$flag)
dat$battery_error<-ifelse(dat$flag=="Auto:BattV",1,0)
dat$range_error<-ifelse(dat$flag=="Auto:Range",1,0)
dat$spike_error<-ifelse(dat$flag=="Auto:Spike",1,0)
dat$outcome<-ifelse(dat$flag=="OK",1,0)


ggplot(dat%>%filter(Box==4  & quarter_interval<ymd("20180401") & SM %in% c("SM2","SM6")),aes(x=quarter_interval,y=value,color=flag))+
  geom_point()+
  facet_wrap(~SM)

soil_data_2018_aggregated
dat$flag

with(dat,table(flag,Box,SM))

soil_data_2018_aggregated%>%
  filter(Box %in% c(4,15))%>%
  select_if(str_detect(names(.),"SM2(?!_)|Box|quarter_interval"))%>%
  ungroup()%>%
  spread(.,key = "Box",value = "SM2")%>%
  select_if(is.numeric)%>%
  cor(use = "pairwise.complete.obs")

#Credible neighbor approach:

#Step 1) Find one or multiple Boxes that are highly correlated with current Box 
#after auto range, spike and battery values were excluded.
#This or these are the credible neighbors
#could be done for closest boxes if known


dat_values<-soil_data_2018_aggregated%>%
  select_if(str_detect(names(.),paste0("Box|quarter_interval|^",var,"[:digit:](?!_)")))%>%
  gather(key="SM",value="value",-Box,-quarter_interval)

dat_flag_values<-soil_data_2018_aggregated%>%
  select_if(str_detect(names(.),paste0("Box|quarter_interval|^",var,"[:digit:]_Flag")))%>%
  gather(key="SM",value="flag",-Box,-quarter_interval)%>%
  mutate(SM=str_sub(SM,1,3))

dat<-dat_values%>%
  left_join(dat_flag_values)

#Filtering!
#dat<-dat%>%filter(SM=="SM1"|SM=="SM2")
unique(dat$flag)
dat_filtered<-dat%>%ungroup()%>%
  filter(!flag %in% c("Auto:Spike","Auto:Range", "Auto:BattV"))%>%
  group_by(Box,quarter_interval)%>%
  summarise(value=mean(value))

#Plot correlation between boxes:
dat_filtered%>%
  spread(.,key = "Box",value = "value")%>%
  select(-quarter_interval)%>%
  cor(use = "pairwise.complete.obs")%>%
  reshape2::melt()%>%
  #filter(value>0.5)%>%
  ggplot(.,aes(x=factor(Var1), y=factor(Var2),fill=value))+
  #geom_label(aes(x=Var1, y=Var2,fill=value,label=value))+
  geom_tile()

R<-dat_filtered%>%
  spread(.,key = "Box",value = "value")%>%
  select(-quarter_interval)%>%
  cor(use = "pairwise.complete.obs")

#Identify closest neighbors:
k=3 #k=3 --> i.e. get three closest neighbors

credible_neighbors<-map(1:dim(R)[1],function(x){
  sorted_vector<-sort(R[x,])
  data.frame(Box=rownames(R)[x],
             credible_neighbors=str_c(names(sorted_vector[(length(sorted_vector)-1):(length(sorted_vector)-(k))]),collapse = "_"))
})%>%plyr::ldply()

#Step 2) Breaks the entire period into chunks (start with months or quarters, weeks if possible)
#Use average correlation of one, two, three neighbors to predict error in the sensors

l=3  
main_result<-map(1:dim(credible_neighbors)[1],function(box_row){
  #box_row<-13

  current_box<-as.character(credible_neighbors$Box[box_row])
  print(current_box)
  train<-dat%>%
    mutate(error=ifelse(flag %in% c("Auto:Spike","Auto:Range", "Auto:BattV"),1,0),outcome=ifelse(flag=="OK",1,0))
  
  current_boxes<-c(as.character(credible_neighbors$Box[box_row]),str_split(credible_neighbors$credible_neighbors[box_row],"_")%>%unlist())
  
  credible_sensors<-train%>%
    filter(Box %in% current_boxes)%>%
    mutate(Box_SM=str_c("BOX_",Box,"_",SM))%>%
    ungroup()%>%
    filter(error==0)%>%
    select(Box_SM,value,quarter_interval)%>%
    spread(key = "Box_SM",value = "value")
  
  credible_sensors<-credible_sensors%>%
    mutate(day=as.Date(credible_sensors$quarter_interval))
  
  

  day_list<-split(credible_sensors,
                  credible_sensors$day)
  
  credibility_corr<-map2(day_list,names(day_list),
                         function(x,day){
                           #s<-split(credible_sensors,
                            #    credible_sensors$day)
                           #x<-s[[59]]
                           #day<-names(day_list)[1]
                           
                           rcorr<-x%>%
                             select_if(is.numeric)%>%
                             cor(use = "pairwise.complete.obs")
                           
                           rows_with_current_box<-which(str_detect(rownames(rcorr),paste0("BOX_",current_box)))
                           
                           result<-map2(rows_with_current_box,rownames(rcorr)[rows_with_current_box],function(row,name){
                             #row<-rows_with_current_box[3]
                             #name<-rownames(rcorr)[rows_with_current_box][3]
                             if(all(is.na(rcorr[row,]))){
                               data.frame(average_corr=NA,
                                          name)
                             } else {
                               sorted_vector<-sort(rcorr[row,])
                               if(length(sorted_vector)>k){
                                 data.frame(average_corr=max(sorted_vector[(length(sorted_vector)-1):(length(sorted_vector)-(l))]),
                                            name)
                               } else {
                                 data.frame(average_corr=max(sorted_vector[1:(length(sorted_vector)-1)]),
                                            name)
                               }
                             }
                           })%>%plyr::ldply()
                           result$day<-day
                           #print(day)
                           #print(x)
                           return(result)
                           
                         })%>%plyr::ldply()
  return(credibility_corr)
  
})


credibility<-main_result%>%
  plyr::ldply()


credible_neighbors


new_dat<-dat%>%
  ungroup()%>%
  mutate(day=as.Date(as.character(x=quarter_interval)))%>%
  left_join(credibility%>%
      mutate(day=as.Date(as.character(x=day)))%>%
      mutate(Box=as.numeric(str_sub(name,5,5)),
             SM=str_sub(name,7,9))%>%
      select(Box,SM,average_corr,day))

new_dat%>%
  filter(!flag %in% c("Auto:Spike","Auto:Range", "Auto:BattV"))%>%
  ggplot(.,aes(x=average_corr,fill=flag))+
  geom_density(alpha=0.5)+
  theme_bw(base_size = 22)





map(which(str_detect(rownames(rcorr),paste0("BOX_",current_box))))
rcorr[]


credible_neighbors<-map(1:dim(R)[1],function(x){
  sorted_vector<-sort(R[x,])
  data.frame(Box=rownames(R)[x],
             credible_neighbors=str_c(names(sorted_vector[(length(sorted_vector)-1):(length(sorted_vector)-(k))]),collapse = "_"))
})%>%plyr::ldply()















