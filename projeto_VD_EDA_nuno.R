#Funcoes -----------------------------

libraries <- function() {
  require(data.table) #fread / apply
  library(rstudioapi)
  library(tidyverse) #readr / dplyr / ggplot2
  #library(ChicagoHelper) #custom library
  library(shiny) #interactive pages
  library(leaflet) #interactive maps
  library(rgdal) #Spatial line maps
  library(RColorBrewer) #Palletes de cores
  library(htmlwidgets)
  library(htmltools) #Para usar elementos html
  library(stringi)
  library(lubridate)
  library(readxl)
}
read_files <- function() {
  
  path = toString(rstudioapi::getSourceEditorContext()$path)
  parts = strsplit(path, "/")
  parts = parts[[1]][-length(parts[[1]])]
  final_path = paste(parts, collapse = "/")
  setwd(final_path)
  types = "iic?cccccllinnncnni?nn?"
  header = c("ID", "Case_Number", "Date", "Block", "IUCR", "Primary_Type", "Description", "Location_Description", "Arrest", "Domestic", "Beat", "District", "Ward", "Community_Area", "FBI_Code", "X_Coordinate", "Y_Coordinate", "Year", "Updated_On", "Latitude", "Longitude", "Location")
  
  crimes_2001_2004 = read_csv(file = "datasets/Chicago_Crimes_2001_to_2004.csv", col_types = types)
  crimes_2005_2007 = read_csv(file = "datasets/Chicago_Crimes_2005_to_2007.csv", col_types = types)
  crimes_2008_2011 = read_csv(file = "datasets/Chicago_Crimes_2008_to_2011.csv", col_types = types)
  crimes_2012_2017 = read_csv(file = "datasets/Chicago_Crimes_2012_to_2017.csv", col_types = types)
  
  #retirar coluna a mais
  crimes_2001_2004 = crimes_2001_2004[, !(names(crimes_2001_2004) %in% "X1")]
  crimes_2005_2007 = crimes_2005_2007[, !(names(crimes_2005_2007) %in% "X1")]
  crimes_2008_2011 = crimes_2008_2011[, !(names(crimes_2008_2011) %in% "X1")]
  crimes_2012_2017 = crimes_2012_2017[, !(names(crimes_2012_2017) %in% "X1")]
  #30692 NA na latitude, longitude, location, x coordinate e y coordinate
  #700248 NA na ward e Community area (para 2001_2004)
  
  colnames(crimes_2001_2004) = header
  colnames(crimes_2005_2007) = header
  colnames(crimes_2008_2011) = header
  colnames(crimes_2012_2017) = header
  
  return(list("2001_2004" = crimes_2001_2004, "2005_2007" = crimes_2005_2007, "2008_2011" = crimes_2008_2011, "2012_2017" = crimes_2012_2017));
}
remover_duplicados <- function(dataset) {
  dataset = dataset[!duplicated(dataset$ID) & !duplicated(dataset$Case_Number), ]
  flag = length(unique(dataset$ID)) == nrow(dataset) #TRUE, deixou de haver IDs duplicados
  
  return(list("df" = dataset, "no_dups" = flag))
}
remove_NA <- function(dataset, cols_to_apply = c()) {
  #message(sprintf("Removing NAs...\nlength before NA removal: %i", nrow(dataset)))
  if(length(cols_to_apply) == 0) {
    #Remove todos os NAs
    dataset = dataset[complete.cases(dataset), ]
  } else {
    #Remove linhas com certas colunas com NAs
    dataset = dataset[complete.cases(dataset[ , cols_to_apply]), ]
  }
  #message(sprintf("length after NA removal: %i", nrow(dataset)))
  
  return(dataset)
}
get_crimes_freq <- function(dataset) {
  dataset_table = lapply(dataset["Primary_Type"], table)
  
  crime_types = as.data.frame(dataset_table$Primary_Type)
  colnames(crime_types)[1] = "Type"
  
  #tabela de frequencia de crimes
  crime_types = crime_types %>%
    mutate(Type, percent = round((Freq / nrow(dataset)) * 100, 2))
  
  return(crime_types)
}
remover_crimes_irrelevantes <- function(dataset, percentage) {
  crime_types = get_crimes_freq(dataset)
  
  #Tipos de crime com percentagem 0
  crime_types_clean = crime_types %>%
    filter(percent <= percentage) %>%
    select(Type)
  
  #remover crimes com ocorrencia 0
  dataset = filter(dataset, !Primary_Type %in% crime_types_clean$Type)
  
  #repetir tabela de freqs para o dataset limpo
  crime_types = get_crimes_freq(dataset)
  
  return(list("freq_table" = crime_types, "dataset" = dataset))
}
sample_data <- function(dataset, cols.rm, irrelevant.perc = 0.00, na.rm = FALSE, na.relevant = c()) {
  #remover duplicados
  rows_before = nrow(dataset)
  #message("Removing duplicates (ID and Case Number)...")
  dups = remover_duplicados(dataset)
  if(dups$no_dups) {
    dataset = dups$df
  } else {
    warning("Strange behaviour, no duplicates found")
  }
  #message(sprintf("duplicates removed: %i", (rows_before - nrow(dataset))))
  
  #Remover NAs
  if(na.rm == TRUE) {
    rows_before = nrow(dataset)
    dataset = remove_NA(dataset, cols_to_apply = na.relevant)
    if(abs(rows_before - nrow(dataset)) == 0) {
      warning("The dataset had 0 NA")
    }
  } else {
    warning("There were no missing values")
  }
  
  #remover colunas especificadas
  #message("Removing unecessary columns...")
  if(length(cols.rm) != 0){
    dataset = dataset[-cols.rm]
  } else {
    warning("No columns were chosen to remove")
  }
  
  
  #remover crimes irrelevantes (de acordo com o parametro)
  rows_before = nrow(dataset)
  #message("Removing irrelevant crimes before sampling...")
  return_list = remover_crimes_irrelevantes(dataset, irrelevant.perc)
  if(nrow(dataset) == nrow(return_list$dataset)) {
    warning("There were no crimes with percentage 0 before sampling")
  }
  #tabela de frequencias da populacao inteira
  crime_types = return_list$freq_table
  dataset = return_list$dataset
  #message(sprintf("irrelevant crimes removed before sampling: %i", (rows_before - nrow(dataset))))
  
  #Sampling de 25%
  #message("Sampling...")
  #message(sprintf("Lines before sampling: %i", nrow(dataset)))
  
  sampled_dataset = dataset[seq(1, nrow(dataset), 4), ]
  rows_before = nrow(sampled_dataset)
  return_list = remover_crimes_irrelevantes(sampled_dataset, irrelevant.perc)
  if(nrow(dataset) == nrow(return_list$dataset)) {
    warning("There were no crimes with percentage 0 after sampling")
  }
  sampled_dataset = return_list$dataset
  
  #message(sprintf("irrelevant crimes removed after sampling: %i", (rows_before - nrow(sampled_dataset))))
  #message(sprintf("Lines after sampling: %i", nrow(sampled_dataset)))
  
  sampled_crime_types = return_list$freq_table
  
  return(list("dataset" = dataset, "sampled_dataset" = sampled_dataset, "pop_crime_freq" = crime_types, "sample_crime_freq" = sampled_crime_types))
}
distribution.test <- function(x, y, mean.threshold = 1.0, median.threshold = 1, sd.threshold = 0.3) {
  mean_flag = abs(mean(x) - mean(y)) < mean.threshold
  median_flag = abs(median(x) - median(y)) <= median.threshold
  sd_flag = abs(sd(x) - sd(y)) < sd.threshold
  
  if(!mean_flag) {
    warning(sprintf("mean threshold exceeded: %f", abs(mean(x) - mean(y))))
  }
  if(!median_flag) {
    warning(sprintf("median threshold exceeded: %f", abs(median(x) - median(y))))
  }
  if(!sd_flag) {
    warning(sprintf("sd threshold exceeded: %f", abs(sd(x) - sd(y))))
  }
  
  return(mean_flag & median_flag & sd_flag)
}
get_complete_sample <- function(files.read = FALSE, files.list = c(), cols.rm, irrelevant.perc = 0.00, na.rm = FALSE, na.relevant = c()) {
  libraries()
  if(files.read) {
    message("Reading files...")
    files_list = read_files()
  } else {
    warning("Files not read.\nUsing list from argument.")
  }
  
  crimes_2001_2004 = files_list$`2001_2004`
  crimes_2005_2007 = files_list$`2005_2007`
  crimes_2008_2011 = files_list$`2008_2011`
  crimes_2012_2017 = files_list$`2012_2017`
  
  comp_2001_2004 = sample_data(crimes_2001_2004, cols.rm, irrelevant.perc, na.rm, na.relevant)
  comp_2005_2007 = sample_data(crimes_2005_2007, cols.rm, irrelevant.perc, na.rm, na.relevant)
  comp_2008_2011 = sample_data(crimes_2008_2011, cols.rm, irrelevant.perc, na.rm, na.relevant)
  comp_2012_2017 = sample_data(crimes_2012_2017, cols.rm, irrelevant.perc, na.rm, na.relevant)
  
  merged_data = rbind(comp_2001_2004$sampled_dataset, comp_2005_2007$sampled_dataset)
  merged_data = rbind(merged_data, comp_2008_2011$sampled_dataset)
  complete_sample = rbind(merged_data, comp_2012_2017$sampled_dataset)
  
  return(list("complete_sample" = complete_sample, "files_read" = files_list))
}

#Leitura de dados------
cols_to_rm = c(1:2, 4:5, 12:13, 15:17, 19, 22)
return_list = get_complete_sample(files.read = TRUE, cols.rm = cols_to_rm, na.rm = TRUE)

complete_sample = return_list$complete_sample

rm(return_list)
complete_sample$Date = format(strptime(complete_sample$Date, format = "%m/%d/%Y %I:%M:%S %p"), format = "%d/%m/%Y %H:%M:%S %p")

complete_sample$Date = (dmy_hms(complete_sample$Date))

#indice socioeconomico de chicago
#Hardship index = juncao de todos os outros parametros do dataset
chicago_se_index = read_csv("datasets/indice_socioeconomico_chicago_2008_2012.csv")
colnames(chicago_se_index) = c("CA.number", "CA.name", "Percent.Housing.Crowded", "Percent.Households.Below.Poverty", "Percent.16+.Unemployed", "Percent.25+.Without.High.School.Diploma", "Percent.Indepency.18-.64+", "Per.Capita.Income", "Hardship.Index")
View(chicago_se_index)

#populacao por community area
chicago_pop <- read.table("datasets/chicago_pop.txt", quote="\"", comment.char="")
chicago_pop = chicago_pop[-c(2, 4:6)]
colnames(chicago_pop) = c("CA.num", "Population")
View(chicago_pop)


chicago_se_index = merge(x = chicago_se_index, y = chicago_pop, by.x = "CA.number", by.y = "CA.num")
View(chicago_se_index)

#Juncao dos datasets
sample_pop = merge(x = complete_sample, y = chicago_se_index, by.x = "Community_Area", by.y = "CA.number", all.y = TRUE)
sample_pop = sample_pop[complete.cases(sample_pop[, "Community_Area"]), ]
View(head(sample_pop))


#Mapa de community_areas--------
c.areas = readOGR("mapas/CA/geo_export_980e6813-e98b-47fa-9ed8-b8eb75ae851d.shp")

#para mostrar todas as palletes
display.brewer.all()

#criar uma pallete de cores para as community areas
bins = c(0, 400, 800, 1200, 1600, 2000, 2400, 2800, 3200, 3600)
pal <- colorBin(
  palette = "YlOrBr",
  domain = safe$n,
  bins = bins)

#latitude e longitude de cada beat em 2016
beat_locations = sample_pop[-c(1:7, 12:20)]
beat_locations = beat_locations %>%
  group_by(Beat) %>%
  filter(Year == 2016) %>%
  add_tally(name = "N.crimes") %>%
  filter(row_number(Beat) == 1)
View(beat_locations)


#Correção de nomes
chicago_se_index$CA.name[18] = tolower(c.areas$community[18])
chicago_se_index$CA.name[73] = tolower(c.areas$community[72])
chicago_se_index$CA.name[76] = tolower(c.areas$community[75])

#organizar populacao conforme shapefile
pop_list = vector(mode = "list", length = length(c.areas$community))
for(i in 1:length(c.areas$community)) {
  pop_list[[i]] = chicago_se_index[tolower(chicago_se_index$CA.name) == tolower(c.areas$community[i]), ]$Population
}

pop_list = unlist(pop_list)
pop_list

c.areas$population = pop_list

labels <- sprintf(
  "<strong>%s</strong><br/>total population: %g",
  c.areas$community, c.areas$population
) %>% lapply(htmltools::HTML)

#O titulo é opcional, provavelmente nao vai ser usado. Pode ser adicionado no shiny ou mesmo no notebook
leaflet(c.areas) %>% 
  
  addControl("<h3>População por community area em 2016 (estimativa)</h3><p><em>Crimes por beat em 2016</em></p>", position = "topleft") %>%
  
  addPolygons(color = ~pal(population), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              label = labels,
              highlightOptions = highlightOptions(color = "white", weight = 1)) %>%
  
  addLegend(pal = pal, values = ~population, opacity = 0.7, title = "Total da população",
            position = "bottomright") %>%
  
  addCircles(data = beat_locations, lng = ~Longitude, lat = ~Latitude, weight = 1, radius = ~N.crimes, popup = ~factor(Beat),
             highlightOptions = highlightOptions(color = "blue", weight = 2,
                                                 bringToFront = TRUE))



#Mapa das beats-------
#Densidade de crimes por beat (para 2016 ou 2009), fazer escala com a area do shapefile (mapa)
#Estudar uma beat em especifico (beat com maior número de crimes ou pior em termos de densidade)
#Em que CA se encontra a beat?
#Estudar ano dessa beat (ex: 2009 ano da crise financeira nos USA) atráves da safety
#Ver dias mais importantes (feriados)
#Ver esses dias por hora (alturas mais caoticas)
#Estudar evolução temporal destes estudos (os feriados sao sempre maus? as coisas pioraram com a crise?) Esta evolução pode ser demonstrada num mapa com glyphs, por exemplo setas para cima que indiquem aumento de algo.
#Comparação entre esta beat e uma beat representativa da média

b.areas = readOGR("mapas/Beats/geo_export_e80401d7-e6f8-4d20-8da4-b699365275e5.shp")
length(b.areas$beat_num)


#----------------------------SAFETY FUNCTION PRIMARY-----------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#---------------------------------------------------YES--------------------------------------------------------

#CHI stands for Crime Harm Index

#----------doodling around - please ignore ---------
descript<-complete_sample %>% 
   group_by(Primary_Type, Description)  %>% 
        summarise(n = n()) #%>% mutate(freq = prop.table(n))

nuno<-complete_sample %>%
        filter(Primary_Type=="HOMICIDE") 

#----------CRIME HARM INDEX ---------
        
complete_sample<-complete_sample %>%
                       mutate(CHI=case_when(Description == "BY EXPLOSIVE" ~ 730,
                                           Primary_Type == "ARSON" & Description != "AGGRAVATED" ~ 17.5,
                                           str_detect(Description,"^AGGRAV") ~ 1825,
                                           Primary_Type == "ARSON" & Description == "AGGRAVATED" ~ 2190,
                                           Primary_Type == "ASSAULT" & str_detect(Description,"^AGGRAV",negate=TRUE) ~ 1,
                                   Primary_Type == "ASSAULT" & str_detect(Description,"^AGGRAV") ~ 365,
                                   Primary_Type == "BATTERY" & str_detect(Description,"^AGGRAV", negate=TRUE) ~ 0.2,
                                   Primary_Type == "BATTERY" & str_detect(Description,"^AGGRAV") ~ 365,
                                   Primary_Type == "BURGLARY" ~ 20,
                                   Primary_Type == "CONCEALED CARRY LICENSE VIOLATION" ~ 1,
                                   Primary_Type == "CRIM SEXUAL ASSAULT" ~ 800,
                                   Primary_Type == "CRIMINAL DAMAGE" ~ 5,
                                   Primary_Type == "CRIMINAL TRESPASS" ~ 1,
                                   Primary_Type == "DECEPTIVE PRACTICE" ~ 3,
                                   Primary_Type == "GAMBLING" ~ 0.2,
                                   Primary_Type == "HOMICIDE" ~ 5475,
                                   Primary_Type == "INTERFERENCE WITH PUBLIC OFFICER" ~ 8,
                                   Primary_Type == "INTIMIDATION" ~ 8,
                                   Primary_Type == "KIDNAPPING" ~ 548,
                                   Primary_Type == "LIQUOR LAW VIOLATION" ~ 0.2,
                                   Primary_Type == "MOTOR VEHICLE THEFT" ~ 5,
                                   Primary_Type == "NARCOTICS" & str_detect(Description,"^MANU",negate=TRUE) ~ 2,
                                   Primary_Type == "NARCOTICS" & str_detect(Description,"^MANU") ~ 365,
                                   Primary_Type == "OBSCENITY" ~ 1,
                                   Description == "HARBOR RUNAWAY" ~ 1,
                                   Description == "SALE TOBACCO PRODUCTS TO MINOR" ~ 1,
                                   Description == "AGG CRIM SEX ABUSE FAM MEMBER" ~ 2300,
                                   Description == "AGG SEX ASSLT OF CHILD FAM MBR" ~ 2300,
                                   Description == "CHILD ABANDONMENT" ~700,
                                   Description == "CHILD ABDUCTION" ~ 265,
                                   Description == "CHILD ABUSE" ~ 365,
                                   Description == "CHILD PORNOGRAPHY" ~ 365,
                                   Description == "CRIM SEX ABUSE BY FAM MEMBER" ~ 700,
                                   Description == "ENDANGERING LIFE/HEALTH CHILD" ~ 365,
                                   Primary_Type == "OFFENSE INVOLVING CHILDREN" ~ 100,
                                   str_detect(Description,"^HARASSMENT") ~ 10,
                                   Primary_Type == "OTHER OFFENSE" ~ 1,
                                   Primary_Type == "PROSTITUTION" ~ 0.5,
                                   Primary_Type == "PUBLIC PEACE VIOLATION" ~ 1,
                                   Primary_Type == "ROBBERY" ~ 365,
                                   Description == "AGG CRIMINAL SEXUAL ABUSE" ~ 700,
                                   Description == "ATT AGG CRIMINAL SEXUAL ABUSE" ~ 700,
                                   Description == "ATT CRIM SEXUAL ABUSE" ~ 365,
                                   Description == "CRIMINAL SEXUAL ABUSE" ~ 365,
                                   Description == "SEXUAL EXPLOITATION OF A CHILD" ~ 365,
                                   Primary_Type == "SEX OFFENSE" ~ 30,
                                   Primary_Type == "STALKING" ~ 5,
                                   Primary_Type == "THEFT" ~ 10,
                                   Primary_Type == "WEAPONS VIOLATION" ~ 50,
                             )
                          
                         )





#------------------------------------------------SAFETY FUNCTION-----------------------------------------------


safe<-complete_sample %>%
       filter(Year==2016) %>%
              group_by(Community_Area) %>%
                 tally(CHI)  

sum(beats_2016$CHI)
sum(safe$n)
# for CHI/population  -----------
#safe$population<-chicago_pop$Population

#safe<-safe %>%
#      mutate(n=100*n/population)

lambda=1/median(safe$n)
  
safe<-safe %>%
      mutate( safety1=(-n*mean(n))/(10.75*sum(n))+1000)
      #mutate( safety=1000*exp(-n*lambda))
      #mutate(  safety2=sum(n)/(3.7*n))
annot<-data.frame(text=c("Exponential Decay","Hyperbolic Function","Linear Function"),x=c(400,800,120),y=c(0.002,0.005,0.01))     
ggplot(data=safe) + 
  geom_density(aes(safety),size=0.001,kernel="gaussian", alpha=0.8,fill="#943126") + xlim(0,1000) + 
  geom_density(aes(safety1),kernel="gaussian", alpha=0.3,fill="#6C3483") + 
  geom_density(aes(safety2),kernel="gaussian", alpha=0.3,fill="#0E6655") +
  theme(panel.grid.major = element_blank(), panel.grid.minor=element_blank(),panel.border = element_blank()) +
  geom_text(data=annot,aes(x=x,y=y,label=text),size=7) +
  ggtitle("Comparison of CHI Transformations - Data Spreading")



#here we can have a notion of dispersion

ggplot(data=safe,aes(n,fill="red")) + geom_density(kernel="gaussian", alpha=0.5) + xlim(-500,1500)
#-----------
safe.d<-melt(safe)[-c(1:77),]


#distribution of CHI vs Safety, if Safety is too narrow it will pose a problem in the color gradient

ggplot(data=safe.d) + geom_density(aes(x=value,fill=variable, alpha=0.5)) + xlim(-500,1500)



#criar uma pallete de cores para as community areas
#bins = c(cri2/2, 1/10*cri,2/10*cri, 3/10*cri, 4/10*cri, 5/10*cri, 6/10*cri, 7/10*cri, 8/10*cri, 9/10*cri, cri)
bins = c(-300, 0, 10, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
pal <- colorBin(
  palette = "RdYlGn",
  domain = safe$n,
  bins = bins
)

pop_list_s = vector(mode = "list", length = length(c.areas$community))
for(i in 1:length(c.areas$community)) {
  pop_list_s[[i]] = safe[(safe$Community_Area) == (c.areas$area_numbe[i]), ]$safety
}

pop_list_s <- unlist(pop_list_s)

c.areas$safety<-pop_list_s 

c.areas$safety

labels <- sprintf(
  "<strong>%s</strong><br/>total population: %g",
  c.areas$area_numbe, c.areas$safety
) %>% lapply(htmltools::HTML)

#---mapa

leaflet(c.areas) %>% 

  
  addPolygons(color = ~pal(safety), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              label = labels,
              highlightOptions = highlightOptions(color = "white", weight = 1)) 
  



#---------------------------------------------------------------------------------------
#-----------------------DAYS OF 2016----------------------------------------------------------------


beats_2016$Date1 <- as.Date(beats_2016$Date)
daily<-aggregate(beats_2016$CHI, by=list(beats_2016$Date1), sum)

daily$ID <- seq.int(nrow(daily))
ggplot(daily,aes(x=ID,y=safety))+geom_linerange()+coord_polar()+ylim(0,1000)

filter(daily,x>mean(daily$x))



ggplot(data=daily,aes(x,fill="red")) + geom_density(kernel="gaussian", alpha=0.5) + xlim(-2000,21000)

lambda=1/median(daily$x)

daily<-daily %>%
  #mutate( n=-n/CHIT+1)
  mutate( safety=1000*exp(-x*lambda))

ggplot(data=daily,aes(safety,fill="red")) + geom_density(kernel="gaussian", alpha=0.5) + xlim(0,1000)

#-------------------------------------POLAR PLOT---------------------------------------------------------
#not relevant anymore

radar2016<-beats_2016 %>%
   group_by(month(Date)) %>%
       tally(CHI)

sum(radar2016$n)

lambda=1/median(radar2016$n)

radar2016<-radar2016 %>%
    mutate(safety=1000*exp(-n*lambda))



ggplot(data=radar2016,aes(n)) + geom_density(kernel="gaussian")



radar2016$month<-seq(as.Date("2016-01-1"), as.Date("2016-12-01"), by="months")


p = ggplot(radar2016, aes(x=month, y=safety, fill=safety)) +
  theme_gray() +
  scale_fill_gradient(low="red", high="dark green", limits=c(min(radar2016$safety),max(radar2016$safety)),guide=FALSE) + 
  theme(panel.background = element_blank(),panel.border = element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text.y = element_blank(),axis.ticks.y = element_blank()) 
p + geom_bar(stat="identity") + 
  geom_hline(yintercept=mean(radar2016$safety),size=0.6,alpha=0.5,linetype="dashed") + 
  coord_polar() + 
  theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1)) + 
  labs(title = "Safety in Chicago (2016)") +
  annotate("text", x=as.Date("2016-06-01"), y=1.1*mean(radar2016$safety), label=("monthly average"),alpha=0.3,fontface =1)

#---------------------------------SAFETY CIRCLE-------------------------------------------------------------------------


beats_2013 <-complete_sample %>%
  filter(Year==2013 )
beats_2014 <-complete_sample %>%
  filter(Year==2014 )
beats_2015 <-complete_sample %>%
  filter(Year==2015 )

m1<-beats_2013 %>%
      group_by(month=floor_date(Date,"1 day")) %>%
             tally(CHI)

m2<-beats_2014 %>%
      group_by(month=floor_date(Date,"1 day")) %>%
            tally(CHI)

m3<-beats_2015 %>%
  group_by(month=floor_date(Date,"1 day")) %>%
  tally(CHI)


lambdam1=1/median(m1$n)
lambdam2=1/median(m2$n)
lambdam3=1/median(m3$n)

m1<-m1 %>%
  mutate(safety=1000+1000*exp(-n*lambdam1))
m2<-m2 %>%
  mutate(safety=1000+1000*exp(-n*lambdam2))
m3<-m3 %>%
  mutate(safety=1000+1000*exp(-n*lambdam3))



m1$s2014<-m2$safety
m1$s2015<-m3$safety

m1 <- m1 %>%
           mutate(min1=pmin(safety,s2014,s2015))

m1 <- m1 %>%
          mutate(max1=pmax(safety,s2014,s2015))

m1 <- m1 %>%
          mutate(Safety_level=(safety+s2014+s2015)/3)

day_first<-c(as.numeric(m1$month[1]), as.numeric(m1$month[32]), as.numeric(m1$month[60]),
             as.numeric(m1$month[91]), as.numeric(m1$month[121]), as.numeric(m1$month[152]),
             as.numeric(m1$month[182]), as.numeric(m1$month[213]), as.numeric(m1$month[244]),
             as.numeric(m1$month[274]), as.numeric(m1$month[305]), as.numeric(m1$month[335]))

m1$Safety_level

m1$months<-c("jan","feb","march","apr","may","jun","jul","aug","sep","oct","nov","dec")

p <- ggplot(m1, aes(month, Safety_level))
p + geom_pointrange(aes(ymin = min1, ymax = max1,color=Safety_level),size=0.72,fatten=1)+
  scale_color_gradient2(low = "red", mid="yellow", high = "dark green", guide=guide_colourbar(),midpoint=1400, aesthetics = "colour") + 
  expand_limits( y = 0)+
  coord_polar() +
  #theme_dark() +
  guides(fill = guide_colourbar(title="Safety",label=FALSE, ticks=FALSE)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    axis.text.y = element_blank(),axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = 'white'),
    panel.border = element_blank(),
  ) +
  labs(title = "Safety in Chicago (2013-2015)")+
  geom_hline(yintercept=mean(m1$Safety_level),size=0.3,alpha=0.6,color="dark grey") +
  geom_hline(yintercept=1000,size=0.3,alpha=0.6,color="dark grey",linetype="dashed") +
  annotate("text", x=m1$month[135], y=1.3*mean(m1$Safety_level), label=("daily average"),alpha=0.5,fontface =1) +
  geom_vline(xintercept=day_first,alpha=0.1) +
  annotate("text", x=m1$month[325], y=800, label=("zero line"),alpha=0.5,fontface =1) 















#------------------------------SAFETY RING FOR ONE YEAR------------------------


circle2012 <-small_sample %>%
                filter(Year==2012 ) 

lambda_c=1/median(circle2012$n)

circle2012<-circle2012 %>%
                mutate(safety=1000*exp(-n*lambda_c))

ggplot(data=circle2012) + 
  geom_density(aes(safety),kernel="gaussian",fill="red",alpha=0.4) +
  geom_density(aes(safety1),kernel="gaussian",colour="blue")

p <- ggplot(circle2012, aes(day, safety))
p + geom_point(aes(color=safety),size=2,fatten=1)+
  scale_color_gradient2(low = "red", mid="yellow", high = "dark green", midpoint=1400,guide = "colourbar", aesthetics = "colour") + 
  expand_limits( y = 0)+
  coord_polar() +
  #theme_dark() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    axis.text.y = element_blank(),axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = 'grey')
  ) +
  geom_hline(yintercept=mean(circle2012$safety),size=2,alpha=0.7,color="white") #+


#---------------------------------------------------RAINY DAYS IN CHICAGO------

draw40 <- data.frame(
                 monthz=character(), 
                 dayz=character(), 
                 stringsAsFactors=FALSE) 


for (i in 1:30)
{
  draw40[i,2]<-sample(1:30, 1)
  draw40[i,1]<-sample(1:12, 1)
}

library(readxl)
weather <-read_excel("datasets/Cambridge_Crime_Harm_Index_2.xlsx",sheet="Sheet3")

weather<-weather[-c(1),]

rainy<-round_sample[-c(32:365),]




rainy$r2007<-weather$Precipi0a0ion2007
rainy$r2008<-weather$Precipi0a0ion2008
rainy$r2009<-weather$Precipi0a0ion2009
rainy$r2010<-weather$Precipi0a0ion2010
rainy$r2011<-weather$Precipi0a0ion2011
rainy$r2012<-weather$Precipi0a0ion2012
rainy$r2013<-weather$Precipi0a0ion2013
rainy$r2014<-weather$Precipi0a0ion2014
rainy$r2015<-weather$Precipi0a0ion2015
rainy$r2016<-weather$Precipi0a0ion2016

#rainy$r2013[rainy$r2013=="T"]<-0.01
#rainy$r2013<-as.double(rainy$r2013)
#rainy$r2014[rainy$r2014=="T"]<-0.01
#rainy$r2014<-as.double(rainy$r2014)
#rainy$r2015[rainy$r2015=="T"]<-0.01
#rainy$r2015<-as.double(rainy$r2015)


rainy$r2007[rainy$r2007>0]=1
rainy$r2008[rainy$r2008>0]=1
rainy$r2009[rainy$r2009>0]=1
rainy$r2010[rainy$r2010>0]=1
rainy$r2011[rainy$r2011>0]=1
rainy$r2012[rainy$r2012>0]=1
rainy$r2013[rainy$r2013>0]=1
rainy$r2014[rainy$r2014>0]=1
rainy$r2015[rainy$r2015>0]=1
rainy$r2016[rainy$r2016>0]=1


ggplot(rainy)+
             geom_jitter(aes(x=r2007,y=`2007`,color=r2007))+geom_jitter(aes(x=r2008,y=`2008`))+geom_jitter(aes(x=r2009,y=`2008`))+
             geom_jitter(aes(x=r2007,y=`2010`))+geom_jitter(aes(x=r2011,y=`2011`))+geom_jitter(aes(x=r2012,y=`2012`))+
             geom_jitter(aes(x=r2007,y=`2013`))+geom_jitter(aes(x=r2014,y=`2014`))+geom_jitter(aes(x=r2015,y=`2015`))+
             geom_jitter(aes(x=r2007,y=`2016`))

ggplot(data=rainy,aes(n)) + geom_density(kernel="gaussian")


#-------------------------------WEATHER----------------------------------------------------

#------------------------------------------------------------------------------------------

small_sample <- complete_sample %>%
  group_by(Year, day=floor_date(Date,"1 day"))  %>%
  tally(CHI)

#remove 2001,2002,2003
small_sample <- small_sample[-c(1:1027),]
#remove leap days
small_sample <- small_sample[-c(60,1521,2982,4443),]
#remove 2017
small_sample <- small_sample[-c(4746:4750),]
#2014 is also bad, remove
small_sample <- small_sample[-c(1:365),]

lambda_ss=1/median(small_sample$n)

small_sample<-small_sample %>%
  mutate(safety=1000*exp(-n*lambda_ss))


weather <-read_excel("datasets/Cambridge_Crime_Harm_Index_2.xlsx",sheet="Weather")



small_sample$precipitation<-weather$Precipitation
small_sample$Snow_Depth<-weather$`Snow Depth`
small_sample$Snow<-weather$`New Snow`
small_sample$Temp<-weather$`Maximum`


ggplot(small_sample)+
  geom_jitter(aes(x=Snow_Depth,y=safety),colour="white")+
  #geom_jitter(aes(x=precipitation*5,y=Snow_Depth,size=safety,colour=safety,alpha=0.8)) +
  #scale_color_gradient2(low = "red", mid="yellow", high = "dark green", midpoint=500,guide = "colourbar", aesthetics = "colour") +
  #geom_point(aes(x=Snow,y=safety,colour="green")) +
  xlim(0,20) +
  geom_smooth(aes(x=Snow_Depth,y=safety),method=lm,color="white") +
  geom_smooth(aes(x=precipitation,y=safety),method=lm,color="red") +
  geom_smooth(aes(x=Snow,y=safety),method=lm,color="green") 

                 #RAINY SAFETY - MONTHLY

small_sample1 <- small_sample %>% 
                    filter(month(day)==3) 

small_sample1$precipitation[small_sample1$precipitation>0]=1

ggplot(small_sample1) +
       geom_point(aes(x=precipitation,y=safety)) +
       geom_smooth(aes(x=precipitation,y=safety))


                 #DAILY SAFETY RAINY

#IT WOULD BE NICE TO DISPLAY THE RATIO OF RAINY/NON RAINY DAYS
#ALL MONTHS HAVE HIGHER SAFETy WITH RAIN, EXCEPT DECEMBER. THO THIS IS WITHIN ERROR

small_sample1 <- small_sample %>% 
  filter(month(day)==12) 

small_sample1$precipitation[small_sample1$precipitation>0]=1

ggplot(small_sample1) +
  geom_point(aes(x=precipitation,y=safety)) +
  geom_smooth(aes(x=precipitation,y=safety))



#-----------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------

#--------------------------------SAFETY RING FOR ALL YEARS----------------------------------------------------------------------------


small_sample <- complete_sample %>%
                    group_by(Year, day=floor_date(Date,"1 day"))  %>%
                        tally(CHI)

#remove 2001,2002,2003
small_sample <- small_sample[-c(1:1027),]
#remove leap days
small_sample <- small_sample[-c(60,1521,2982,4443),]
#remove 2017
small_sample <- small_sample[-c(4746:4750),]
#2014 is also bad, remove
small_sample <- small_sample[-c(1:365),]

lambda_ss=1/median(small_sample$n)

small_sample<-small_sample %>%
  mutate(safety3=1000*exp(-n*lambda_ss))

small_sample<-small_sample %>%
  mutate(safety1=sum(n)/(2*n))

small_sample<-small_sample %>%
  mutate(safety2=(-n*mean(n))/sum(n)+1000)

ggplot(data=small_sample) + 
  geom_density(aes(safety2),kernel="gaussian", alpha=0.3,fill="blue") + xlim(0,1000) + 
  geom_density(aes(safety1),kernel="gaussian", alpha=0.3,fill="red") + 
  geom_density(aes(safety3),kernel="gaussian", alpha=0.3,fill="yellow")

max(small_sample$safety1)

small_sample$dayz<-small_sample$day[1:365]

round_sample <- dcast(small_sample, dayz ~ Year, value.var="safety")

round_sample <- round_sample %>%
  mutate(stDev=apply(.[(2:13)],1,sd))

round_sample <- round_sample %>%
  mutate(min1=pmin(`2005`,`2006`,`2007`,`2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`))

round_sample <- round_sample %>%
  mutate(max1=pmax(`2005`,`2006`,`2007`,`2008`,`2009`,`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`))

round_sample <- round_sample %>%
  mutate(mean1=(`2005`+`2006`+`2007`+`2008`+`2009`+`2010`+`2011`+`2012`+`2013`+`2014`+`2015`+`2016`)/12)



#ggplot(data=small_sample,aes(n,fill="red")) + geom_density(kernel="gaussian", alpha=0.5) + xlim(0,80000)

day_first<-c(as.numeric(round_sample$dayz[1]), as.numeric(round_sample$dayz[32]), as.numeric(round_sample$dayz[60]),
             as.numeric(round_sample$dayz[91]), as.numeric(round_sample$dayz[121]), as.numeric(round_sample$dayz[152]),
             as.numeric(round_sample$dayz[182]), as.numeric(round_sample$dayz[213]), as.numeric(round_sample$dayz[244]),
             as.numeric(round_sample$dayz[274]), as.numeric(round_sample$dayz[305]), as.numeric(round_sample$dayz[335]))


p <- ggplot(round_sample, aes(dayz, mean1))
p + geom_pointrange(aes(ymin = min1, ymax = max1,color=mean1),size=0.72,fatten=1)+
  scale_color_gradient2(low = "red", mid="yellow", high = "dark green", guide=guide_colourbar(),midpoint=2375, aesthetics = "colour") + 
  expand_limits( y = 0)+
  coord_polar() +
  #theme_dark() +
  guides(fill = guide_colourbar(title="Safety",label=FALSE, ticks=FALSE)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),axis.title.y=element_blank(),
    axis.text.y = element_blank(),axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = 'white')
  ) +
  labs(title = "Safety in Chicago (2005-2016)")+
  geom_hline(yintercept=mean(round_sample$mean1),size=0.3,alpha=0.6,color="dark grey") +
  geom_hline(yintercept=2000,size=0.3,alpha=0.6,color="dark grey",linetype="dashed") +
  annotate("text", x=round_sample$dayz[135], y=1.3*mean(round_sample$mean1), label=("daily average"),alpha=0.3,fontface =1) +
  geom_vline(xintercept=day_first,alpha=0.1) 




#---------------------------SAFETY ALONG TIME/HOLIDAYS/SPECIFIC DAY----------------------------------------------------

#new years day security has been improving / christmas getting worse
#huge blizzard feb 1 to feb 2, piled snow until feb 15
#recession
#9/11
#globally, safety has increased, but took a plunge in 2016



oneday <- small_sample %>% 
  filter(month(day)==1 & day(day)==1) 



ggplot(oneday) +
  geom_point(aes(x=day,y=safety)) +
  geom_smooth(aes(x=day,y=safety))

#WEAHTER VERY HOT HEAT WAVE###################################
###############################################
#########################################

onemonth3 <- small_sample %>% 
  filter(Year==2012 & ( month(day)==7))

onemonthm <- small_sample %>% 
  filter(Year!=2012 & ( month(day)==7))  %>%  
  group_by(day(day)) %>%
  summarise(media=mean(safety))
onemonthm$day<-onemonth3$day

ggplot(onemonth3) +
  geom_point(aes(x=day,y=safety,colour=Temp),size=2) +
  geom_smooth(aes(x=day,y=safety),colour="#57C685") +
  geom_smooth(data=onemonthm,aes(x=day,y=media),colour="black",
              se=FALSE,linetype="dashed",alpha=0.2) +
  theme(
    panel.border=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'white')
  ) +
  scale_colour_gradient(low = "#FFC300", high = "#C70039",
                        space = "Lab", na.value = "grey50", guide = "colourbar",
                        aesthetics = "colour") +
  ggtitle("Safety during the 2012 Heat Wave")+xlab("Day")+ylab("Safety") #+
  #annotate("text", x=onemonth3$day[], 
         #  y=1.3*mean(round_sample$mean1), label=("daily average"),alpha=0.3,fontface =1)




#SNOWMAGGEDON###############################

onemonth<-small_sample %>% 
  filter(Year==2011 & ( month(day)==2))


onemonth1 <- small_sample %>% 
       filter(Year!=2011 & ( month(day)==2))  %>%  
          group_by(day(day)) %>%
            summarise(media=mean(safety))
onemonth1$day<-onemonth$day
            
            


ggplot(onemonth) +
  geom_point(aes(x=day,y=safety,size=Snow),alpha=0.5) +
  geom_smooth(aes(x=day,y=safety),colour="purple") +
  geom_col(aes(x=day,y=Snow_Depth*40),fill="black",alpha=0.15) +
  geom_smooth(data=onemonth1,aes(x=day,y=media),colour="black",se=FALSE,linetype="dashed") +
  theme(
    panel.border=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'white')
  ) +
  scale_colour_gradient(low = "#FFC300", high = "#C70039",
                        space = "Lab", na.value = "grey50", guide = "colourbar",
                        aesthetics = "colour") +
  ggtitle("Safety during the 2011 Snowmaggedon") 
  #geom_text(aes(Snow_Depth, label = Snow_Depth), color = "white", fontface = "bold", hjust = 1) 


ggplot(onemonth,aes(Snow_Depth)) +
  geom_histogram()


ggplot(small_sample) +
  geom_point(aes(x=day,y=safety)) +
  geom_smooth(aes(x=day,y=safety),colour="#C70039") + xlab("Time")


ggplot(onemonth) +
  geom_point(aes(x=day,y=safety,colour=Temp),size=2) +
  geom_smooth(aes(x=day,y=safety),colour="#57C685") +
  geom_smooth(data=onemonth1,aes(x=day,y=media),colour="black",
              se=FALSE,linetype="dashed",alpha=0.2) +
  theme(
    panel.border=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = 'white')
  ) +
  scale_colour_gradient(low = "#FFC300", high = "#C70039",
                        space = "Lab", na.value = "grey50", guide = "colourbar",
                        aesthetics = "colour") +
  ggtitle("Safety during the 2012 Heat Wave")+xlab("Safety")


#--------------------------COMMUNITY AREAS EVOLUTION------------------------

community<-complete_sample %>%
       filter(Year %in% c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)) %>%
           group_by(Year, Community_Area) %>%
                summarise(sum=sum(CHI))

community<-community[!(community$Community_Area==0),]

lambda_ca=1/median(community$sum)

community<-community %>%
  mutate(safety=1000*exp(-sum*lambda_ca))

ggplot(community)+geom_smooth(aes(x=Year,y=safety,color=factor(Community_Area)),se=FALSE)



communityplot <- community %>%
                filter(Community_Area %in% (1:77))

x<-lm(formula=safety~Year,data=communityplot)
      x$coefficients[2]       

ggplot(communityplot)+geom_smooth(aes(x=Year,y=safety,color=factor(Community_Area)),se=FALSE)


regression<-matrix(ncol=2,nrow=77)
#regression<-as.data.frame(c(27:34))
for (i in 1:77) {
  communityplot1 <- community %>%
    filter(Community_Area == i)
      
      
  regression[i,2]<-lm(formula=safety~Year,data=communityplot1)$coefficients[2]
  regression[i,1]<-i
                 }
regression<-as.data.frame(regression)


#a few community areas' safety has decreased, mostly has increased.
#some had the largest safety during the post crisis
#


names(regression)<-c("Community_Area","Safety_Change")




aux<-community %>%
  filter(Year==2016) 

regression$safety2016<-aux$safety

annot<-data.frame(text=c("Low safety regions/Neglected","High safety regions/Neglected","High safety regions/Growing safety","Low safety regions/Growing safety"),
                  x=c(-40,-40,40,40),y=c(100,850,850,100))

ggplot()+
  geom_point(data=regression,aes(x=Safety_Change,y=safety2016),size=3,alpha=0.6,colour="#0E6655")+
  geom_point(data=regression_beat,aes(x=Safety_Change,y=safety2016),size=1.5,colour="#CD5C5C")+
  geom_vline(xintercept=mean(regression$Safety_Change),colour="#0E6655",linetype="dashed")+
  geom_hline(yintercept=mean(regression$safety2016),colour="#0E6655",linetype="dashed") +
  geom_vline(xintercept=mean(regression_beat$Safety_Change),colour="#CD5C5C",linetype="dashed")+
  geom_hline(yintercept=mean(regression_beat$safety2016),colour="#CD5C5C",linetype="dashed") +
  theme_grey() + xlab("Safety Points Variation per Year")+
  ylab("Safety in 2016") +ggtitle("Safety over Time vs. Current Panorama")#+
  geom_text(data=annot,aes(x=x,y=y,label=text),size=3)
    




######BEAT EVOLUTIN#################################

#check sparse beats
beaterzzz<-beater%>%
        group_by(Beat) %>%
           summarise(n=n()) %>%
             filter(n<12)

#remove sparse beats and select years
beater<-complete_sample %>%
  filter(Year %in% c(2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)) %>%
   filter(!(Beat %in% beaterzzz$Beat)) %>%
         group_by(Year, Beat) %>%
           summarise(sum=sum(CHI))

#safety

lambda_beat=1/median(beater$sum)

beater<-beater %>%
  mutate(safety=1000*exp(-sum*lambda_beat))



#check evolution for a few beats

ggplot(beatershow)+
  geom_smooth(aes(x=Year,y=safety,color=factor(Beat)),se=FALSE)


#regression values for beats
regression_beat<-matrix(ncol=2,nrow=253)

for (i in 1:253) {
  beaterchange <- beater %>%
    filter(Beat == beater$Beat[i])
  regression_beat[i,2]<-lm(formula=safety~Year,data=beaterchange)$coefficients[2]
  regression_beat[i,1]<-i
}
regression_beat<-as.data.frame(regression_beat)

names(regression_beat)<-c("Beat","Safety_Change")

aux<-beater%>% 
    filter(Year==2016)

regression_beat$safety2016<-aux$safety















bitz<- complete_sample %>%
  filter(Year==2014) %>%
     group_by(Beat) %>%
        tally(CHI)


lambda_b=1/median(bitz$n)

bitz<-bitz %>%
  mutate(safety=1000*exp(-n*lambda_b))


ggplot(data=bitz,aes(safety,fill="red")) + geom_density(kernel="gaussian", alpha=0.5) + xlim(0,1000)

ggplot(data=bitz,aes(x=reorder(Beat,safety),y=safety,color=safety))+geom_col(aes(width=2),alpha=0.1)+
  scale_x_discrete(labels = NULL) +
  scale_color_gradient2(low = "#C70039", mid="#FFC300", high = "dark green", 
                        midpoint=500,guide = "colourbar", aesthetics = "colour") +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.ticks.x = element_blank())+
  geom_hline(yintercept=mean(bitz$safety), linetype="dashed", color = "black",size=0.4) +
  annotate(geom="text", x=50, y=380, label="Average Safety",color="black",alpha=0.6, fontface="italic") +
  labs(title = "Safety for beats", y = "Safety", x = "Beat") 






































































#---------------------------------------------------GET AREA------------------------------------------------------

area<-data.frame(area=numeric(0))
for (i in 1:277){ 
  area[i,]<-b.areas@polygons[[i]]@area
}

beats_t<-beats_2016 %>%
            group_by(Beat) %>%
                   tally()

beats_t$check<-levels(b.areas@data[["beat_num"]])[1:274]

beats_t$area<-area$area[1:274]*100000

ggplot(data=beats_t,aes(x=n/area))+geom_area(stat="bin")

#----------------------------------------CRIME COUNT PER BEAT-----------------------------------------------------

ggplot(data=beats_t,aes(x=reorder(Beat,n),y=n))+geom_col(color="grey")+
  scale_x_discrete(labels = NULL) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.ticks.x = element_blank())+
  geom_hline(yintercept=365, linetype="dashed", color = "red",size=0.4) +
  annotate(geom="text", x=50, y=380, label="1 crime per day",color="red",alpha=0.6, fontface="italic") +
  labs(title = "Crimes per beat", y = "number of crimes / year", x = "beat")#+coord_flip()



#--------------------------------------------------------------------
pal <- colorBin(
  palette = "YlOrBr",
  bins = bins)


cm1<-beats_2016 %>%
     group_by(Community_Area) %>%
            tally()




leaflet(c.areas) %>% 
  
  #("<h3>População por community area em 2016 (estimativa)</h3><p><em>Crimes por beat em 2016</em></p>", position = "topleft") %>%
  
  addPolygons(color = ~pal(cm1), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              #label = labels,
              highlightOptions = highlightOptions(color = "white", weight = 1)) #%>%
  
    #addLegend(pal = pal, values = ~cm1$n, opacity = 0.7, title = "Total da população",
     #       position = "bottomright") %>%
  
        #addCircles(data = cm1$Community_Area, lng = ~Longitude, lat = ~Latitude, weight = 1,
         #    highlightOptions = highlightOptions(color = "blue", weight = 2,
          #                                       bringToFront = TRUE))



#-----------------------------------------------------------------------

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map



#----------------------------------------------SHINY------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------


#Lista de qauntidades crimes ordenados por beat para o shapefile, para 2016
n_crimes_list = list()
n_crimes_list
for(i in 1:length(b.areas$beat_num)) {
  n_crimes_list[[i]] <- beat_locations[beat_locations$Beat == 
                                         as.integer(stri_replace_first_regex(b.areas$beat_num[i], "0*(\\d+)", "$1")), ]$N.crimes
}
#Introduzir Nas por coação
n_crimes_list = as.numeric(as.character(n_crimes_list))
b.areas$crime_count = n_crimes_list

length(b.areas[is.na(b.areas$crime_count), ])

#CORRER A PARTIR DAQUI SEMPRE QUE FOR A PRIMEIRA VEZ QUE SE CORRE O MAPA
#Display para quando se da hover nos beats
labels <- sprintf(
  "<strong>Beat num: %s</strong><br/>Quantidade de crimes: %g",
  b.areas$beat_num, b.areas$crime_count
) %>% lapply(htmltools::HTML)

#Pallete para o total de crimes
bins = c(0, 100, 200, 300, 400, 500, 600)
pal <- colorBin(
  palette = "YlOrRd",
  domain = b.areas$crime_count,
  bins = bins)


#Mapa
beats.map = leaflet(b.areas) %>%
  
  addControl("<h3>Crimes por beat em 2016</h3>", position = "topleft") %>%
  
  addPolygons(color = ~pal(crime_count), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, label = labels,
              highlightOptions = highlightOptions(color = "red", weight = 2)) %>%
  
  addLegend(pal = pal, values = ~crime_count, opacity = 0.7, title = "Total de crimes",
            position = "bottomright")

beats.map



#Shiny app-------------
#UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("beatsmap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("year", "Crime year",
                            c(2016, 2009)
                )
  )
)


#Server
server <- function(input, output, session) {
  input_year = reactive({
    input$year
  })
  
  beat_loc <- reactive({
    year = input_year()
    beat_locations = sample_pop[-c(1:7, 12:20)]
    
    beat_locations %>%
      group_by(Beat) %>%
      filter(Year == year) %>%
      add_tally(name = "N.crimes") %>%
      filter(row_number(Beat) == 1)
  })
  
  reactive({print(beat_loc())})
  
  b.shape = reactive({
    beat_locations = beat_loc()
    n_crimes_list = list()
    for(i in 1:length(b.areas$beat_num)) {
      n_crimes_list[[i]] <- beat_locations[beat_locations$Beat == 
                                             as.integer(stri_replace_first_regex(b.areas$beat_num[i], "0*(\\d+)", "$1")), ]$N.crimes
    }
    #Introduzir Nas por coação
    n_crimes_list = as.numeric(as.character(n_crimes_list))
    b.areas$crime_count = n_crimes_list
    b.areas
  })
  
  
  labs = reactive({
    b.areas = b.shape()
    sprintf(
      "<strong>Beat num: %s</strong><br/>Quantidade de crimes: %g",
      b.areas$beat_num, b.areas$crime_count
    ) %>% lapply(htmltools::HTML)
  })
  
  #Pallete para o total de crimes
  pallete = reactive({
    b.areas = b.shape()
    bins = c(0, 100, 200, 300, 400, 500, 600, 1000)
    colorBin(
      palette = "YlOrRd",
      domain = b.areas$crime_count,
      bins = bins)
  })
  
  
  output$beatsmap <- renderLeaflet({
    year = input_year()
    pal = pallete()
    labels = labs()
    b.areas = b.shape()
    leaflet(b.areas) %>%
      
      addControl(sprintf("<h3>Crimes por beat em %s</h3>", year) %>% lapply(htmltools::HTML), position = "topleft") %>%
      
      addPolygons(color = ~pal(crime_count), weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, label = labels,
                  highlightOptions = highlightOptions(color = "red", weight = 2)) %>%
      
      addLegend(pal = pal, values = ~crime_count, opacity = 0.7, title = "Total de crimes",
                position = "bottomright")
  })
}

shinyApp(ui, server)