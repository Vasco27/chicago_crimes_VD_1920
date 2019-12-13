#Funcoes -----------------------------

libraries <- function() {
  require(data.table) #fread / apply
  library(rstudioapi) #get current path
  library(tidyverse) #readr / dplyr / ggplot2
  #library(ChicagoHelper) #custom library
  library(shiny) #interactive pages
  library(leaflet) #interactive maps
  library(rgdal) #Spatial line maps (shapefiles)
  library(RColorBrewer) #Palletes de cores
  library(htmlwidgets)
  library(htmltools) #Para usar elementos html
  library(stringi) #Regex
  library(lubridate) #datetime manipulation
  library(plotly) #graficos
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
View(head(complete_sample))
nrow(complete_sample)
rm(return_list)

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



#Safety function---------------------
#CHI stands for Crime Harm Index

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


#Conversão de datas
complete_sample$Date_converted = parse_date_time(complete_sample$Date, orders = "mdy HMS p")
complete_sample$Date = trimws(format(strptime(complete_sample$Date, format = "%m/%d/%Y %I:%M:%S %p"), format = "%d-%m-%Y %H:%M:%S %p"))

View(head(complete_sample))

#Exploração de uma só beat------------------

#Filtrar por feriados em Chicago
important_dates = complete_sample %>%
  select(Primary_Type, Beat, Community_Area, Year, CHI, Date_converted) %>%
  filter( 
           ((day(Date_converted) == 1 | day(Date_converted) == 18) & month(Date_converted) == 1) | 
           ((day(Date_converted) == 12 | day(Date_converted) == 15) & month(Date_converted) == 2) |
           (day(Date_converted) == 7 & month(Date_converted) == 3) |
           (day(Date_converted) == 30 & month(Date_converted) == 5) |
           (day(Date_converted) == 4 & month(Date_converted) == 7) |
           (day(Date_converted) == 5 & month(Date_converted) == 9) |
           (day(Date_converted) == 10 & month(Date_converted) == 10) |
           ((day(Date_converted) == 8 | day(Date_converted) == 11 | day(Date_converted) == 24) & month(Date_converted) == 11) |
           ((day(Date_converted) == 24 | day(Date_converted) == 25) & month(Date_converted) == 12)
         )

View(important_dates)

#Safety e nº de crimes de cada CA
CHI_per_CA = important_dates %>%
  filter(Year == 2016) %>%
  group_by(Community_Area) %>%
  summarise(
    CHI = sum(CHI),
    n = n()
    )

View(CHI_per_CA)

#Filtrar por community areas mais perigosas
CHI_per_CA[CHI_per_CA$CHI == max(CHI_per_CA$CHI), ] #CA com maior CHI (menos safety) (43)
CHI_per_CA[CHI_per_CA$n == max(CHI_per_CA$n), ] #CA com maior nº de crimes (25)


#Nomes dos feriados de acordo com os dias e os meses
holidays = c()
i = 1
for (date in important_dates$Date_converted) {
  date = as.POSIXct(date, origin="1970-01-01")
  month = month(date)
  day = day(date)

  if(month == 1) {
    if(day == 1) {
      holidays[i] = "New Year's Day"
    } else {
      holidays[i] = "Martin Luther King Jr. Day"
    }
  } else if(month == 2) {
    if(day == 12) {
      holidays[i] = "Lincoln's Birthday"
    } else {
      holidays[i] = "Washington's Birthday"
    }
  } else if(month == 3) {
    holidays[i] = "Casimir Pulaski Day"
  } else if(month == 5) {
    holidays[i] = "Memorial Day"
  } else if(month == 7) {
    holidays[i] = "Independence Day"
  } else if(month == 9) {
    holidays[i] = "Labor Day"
  } else if(month == 10) {
    holidays[i] = "Columbus Day"
  } else if(month == 11) {
    if(day == 8) {
      holidays[i] = "Election Day (US)"
    } else if(day == 11) {
      holidays[i] = "Veterans Day"
    } else {
      holidays[i] = "Thanksgiving"
    }
  } else if(month == 12) {
    if(day == 24) {
      holidays[i] = "Christmas Eve"
    } else {
      holidays[i] = "Christmas Day"
    }
  } else {
    holidays[i] = "unnamed holiday"
  }
  i = i+1
}

important_dates$Holiday = holidays
View(important_dates)

#Ver so para 2016
important_dates_2016 = important_dates %>%
  filter(Year == 2016) %>%
  filter(Community_Area == 25 | Community_Area == 43) 

View(important_dates)


#Pior CA em termos de safety
worst_safety = important_dates_2016 %>%
  filter(Community_Area == CHI_per_CA[CHI_per_CA$CHI == max(CHI_per_CA$CHI), ]$Community_Area)
View(worst_safety)


#Plot sobre os feriados
#configuração dos eixos
x_axis = list(
  title = "Hour (24h format)",
  showgrid = FALSE
)

y_axis = list(
  title = "Crime Harm Index (CHI)",
  #range = c(0,500),
  showgrid = FALSE
)

plot_ly(worst_safety, x = ~hour(worst_safety$Date_converted[Holiday == "Memorial Day"]), y = ~worst_safety$CHI[Holiday == "Memorial Day"], name = "Memorial Day", type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = "percent", fillcolor = '#F5FF8D') %>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Martin Luther King Jr. Day"]),y = ~worst_safety$CHI[Holiday == "Martin Luther King Jr. Day"], name = "Martin Luther King Jr. Day", fillcolor = "#4C74C9")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Lincoln's Birthday"]),y = ~worst_safety$CHI[Holiday == "Lincoln's Birthday"], name = "Lincoln's Birthday", fillcolor = "#700961")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Washington's Birthday"]),y = ~worst_safety$CHI[Holiday == "Washington's Birthday"], name = "Washington's Birthday", fillcolor = "#312F44")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Casimir Pulaski Day"]),y = ~worst_safety$CHI[Holiday == "Casimir Pulaski Day"], name = "Casimir Pulaski Day", fillcolor = "#151531")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Independence Day"]),y = ~worst_safety$CHI[Holiday == "Independence Day"], name = "Independence Day", fillcolor = "#421a92")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Labor Day"]),y = ~worst_safety$CHI[Holiday == "Labor Day"], name = "Labor Day", fillcolor = "#404be3")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Columbus Day"]),y = ~worst_safety$CHI[Holiday == "Columbus Day"], name = "Columbus Day", fillcolor = "#50CB86")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Election Day (US)"]),y = ~worst_safety$CHI[Holiday == "Election Day (US)"], name = "Election Day (US)", fillcolor = "#39beff")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Veterans Day"]),y = ~worst_safety$CHI[Holiday == "Veterans Day"], name = "Veterans Day", fillcolor = "#c051ff")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Thanksgiving"]),y = ~worst_safety$CHI[Holiday == "Thanksgiving"], name = "Thanksgiving", fillcolor = "#ec6b1a")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Christmas Eve"]),y = ~worst_safety$CHI[Holiday == "Christmas Eve"], name = "Christmas Eve", fillcolor = "#f6cd4d")%>%
  add_trace(x = ~hour(worst_safety$Date_converted[Holiday == "Christmas Day"]),y = ~worst_safety$CHI[Holiday == "Christmas Day"], name = "Christmas Day", fillcolor = "#51395e") %>%
  
  layout(xaxis = x_axis, yaxis = y_axis, title = "CHI per hour in different holidays in 2016 for the South Shore Community Area")




#5 CA/BEATS com Pior/Melhor Safety média entre 2005 e 2016---------------------------------------------------------------------------
#Safety média / CHI / Número de crimes


base.sample = complete_sample %>%
  select(Primary_Type, Description, Community_Area, Beat, Year, CHI, Date_converted) %>%
  filter(Year != 2001 & Year != 2002 & Year != 2003 & Year != 2004)
  
View(base.sample)

#Community Areas
CA.worse.safety = base.sample %>%
  group_by(Community_Area) %>%
  filter(Community_Area != 0) %>% #Erro?
  tally(CHI, name = "sum.CHI")

lambda=1/median(CA.worse.safety$sum.CHI)

CA.worse.safety<-CA.worse.safety %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))

View(CA.worse.safety)

#ggplot(CA.worse.safety, aes(safety)) + geom_density(kernel = "gaussian")

#5 maiores
best.CA = tail(CA.worse.safety[order(CA.worse.safety$safety), ], 5)
worst.CA = head(CA.worse.safety[order(CA.worse.safety$safety), ], 5)

names = c()
i = 1
for(num in best.CA$Community_Area) {
  names[i] = chicago_se_index[chicago_se_index$CA.number == num, ]$CA.name
  i = i+1
}
best.CA$CA.name = names
best.CA$CA.name = factor(best.CA$CA.name, levels = unique(best.CA$CA.name)[order(best.CA$safety)])
View(best.CA)

names = c()
i = 1
for(num in worst.CA$Community_Area) {
  names[i] = chicago_se_index[chicago_se_index$CA.number == num, ]$CA.name
  i = i+1
}
worst.CA$CA.name = names
worst.CA$CA.name = factor(worst.CA$CA.name, levels = unique(worst.CA$CA.name)[order(worst.CA$safety)])
View(worst.CA)


#PLOT
x_axis = list(
  title = "Safety"
)

y_axis = list(
  title = "Community Area"
  #type = "linear"
)

plot_ly(worst.CA, x = ~safety, y = ~CA.name, name = "Worst", color = I("#ff0027"), type = "bar", orientation = "h") %>%
  add_trace(x = best.CA$safety, y = best.CA$CA.name, name = "Best", color = I("#00babf")) %>%
  layout(yaxis=y_axis, xaxis = x_axis, title = "Safety in community areas (2005-2016)")




#Beats
Beats.worst.safety = base.sample %>%
  group_by(Beat) %>%
  filter(Beat != 430) %>%
  add_tally(CHI, name = "sum.CHI") %>%
  filter(row_number(Community_Area) == 1) %>%
  select(Community_Area, Beat, sum.CHI)

lambda=1/median(Beats.worst.safety$sum.CHI)

Beats.worst.safety<-Beats.worst.safety %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(Beats.worst.safety)

best.beats = tail(Beats.worst.safety[order(Beats.worst.safety$safety), ], 5)
worst.beats = head(Beats.worst.safety[order(Beats.worst.safety$safety), ], 5)

best.beats$Beat = factor(best.beats$Beat, levels = unique(best.beats$Beat)[order(best.beats$safety)])
worst.beats$Beat = factor(worst.beats$Beat, levels = unique(worst.beats$Beat)[order(worst.beats$safety)])


names = c()
i = 1
for(num in best.beats$Community_Area) {
  names[i] = chicago_se_index[chicago_se_index$CA.number == num, ]$CA.name
  i = i+1
}
best.beats$CA.name = names

names = c()
i = 1
for(num in worst.beats$Community_Area) {
  names[i] = chicago_se_index[chicago_se_index$CA.number == num, ]$CA.name
  i = i+1
}
worst.beats$CA.name = names

View(best.beats)
View(worst.beats)



#PLOT
x_axis = list(
  title = "Safety",
  type = "linear"
)

y_axis = list(
  title = "Beat"
  #type = "linear"
)

plot_ly(worst.beats, x = ~safety, y = ~Beat, name = "Worst", color = I("#ff0027"), text = ~CA.name, textposition = "outside", type = "bar", orientation = "h") %>%
  add_trace(x = best.beats$safety, y = best.beats$Beat, name = "best", color = I("#00babf"), text = best.beats$CA.name, textposition = "outside") %>%
  layout(yaxis=y_axis, xaxis = x_axis, title = "Most and less safe beats between 2005 and 2016")


#Pior/Melhor Beat/CA para o intervalo de anos 2005/2016 em termos de safety (evolução por hora)---------------------------------
#Melhor Beat -> 1652
#Pior Beat -> 421
#Melhor CA -> Edison Park - 9
#Pior CA -> Austin - 25

best = best.CA[best.CA$safety == max(best.CA$safety), ]

best.CA.perhour = base.sample %>%
  select(Community_Area, CHI, Date_converted, Year) %>%
  filter(Community_Area == best$Community_Area) %>%
  #filter(Year == 2016) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(best.CA.perhour)[1] = "hour"

lambda=1/median(best.CA.perhour$sum.CHI)

best.CA.perhour<-best.CA.perhour %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(best.CA.perhour)


worst = worst.CA[worst.CA$safety == min(worst.CA$safety), ]
worst.CA.perhour = base.sample %>%
  select(Community_Area, CHI, Date_converted, Year) %>%
  filter(Community_Area == worst$Community_Area) %>%
  #filter(Year == 2016) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(worst.CA.perhour)[1] = "hour"


lambda=1/median(worst.CA.perhour$sum.CHI)

worst.CA.perhour<-worst.CA.perhour %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(worst.CA.perhour)


#Melhor e pior CA para o intervalo 2005-2016
plot_ly(best.CA.perhour, x = ~hour, y = ~safety, name = "Edison Park (best)", type = "scatter", mode = "lines", fill = "tozeroy") %>%
  add_trace(x = worst.CA.perhour$hour, y = worst.CA.perhour$safety, name = "Austin (worst)", fill = "tozeroy") %>%
  layout(xaxis = list(title = "Daily hours (24h format)"), yaxis = list(title = "Safety"), title = "Mean safety per hour between 2005 and 2016")


#Melhor/pior ano da melhor/pior CA em termos de safety--------------------------------------------------------------------
best = best.CA[best.CA$safety == max(best.CA$safety), ]
safety.year = base.sample %>%
  filter(Year != 2017 & Community_Area == best$Community_Area) %>%
  group_by(Year) %>%
  tally(CHI, name = "sum.CHI")

lambda=1/median(safety.year$sum.CHI)

safety.year<-safety.year %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(safety.year)


best.year = safety.year[safety.year$safety == max(safety.year$safety), ]
worst.year = safety.year[safety.year$safety == min(safety.year$safety), ]

#Best CA Best Year
best.CA.best.year = base.sample %>%
  filter(Year == best.year$Year) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(best.CA.best.year)[1] = "hour"

lambda=1/median(best.CA.best.year$sum.CHI)

best.CA.best.year<-best.CA.best.year %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(best.CA.best.year)

#Best CA Worst Year
best.CA.worst.year = base.sample %>%
  filter(Year == worst.year$Year) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(best.CA.worst.year)[1] = "hour"

lambda=1/median(best.CA.worst.year$sum.CHI)

best.CA.worst.year<-best.CA.worst.year %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(best.CA.worst.year)


#Worst CA
worst = worst.CA[worst.CA$safety == max(worst.CA$safety), ]
safety.year = base.sample %>%
  filter(Year != 2017 & Community_Area == worst$Community_Area) %>%
  group_by(Year) %>%
  tally(CHI, name = "sum.CHI")

lambda=1/median(safety.year$sum.CHI)

safety.year<-safety.year %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(safety.year)


best.year = safety.year[safety.year$safety == max(safety.year$safety), ]
worst.year = safety.year[safety.year$safety == min(safety.year$safety), ]

#Worst CA Best Year
worst.CA.best.year = base.sample %>%
  filter(Year == best.year$Year) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(worst.CA.best.year)[1] = "hour"

lambda=1/median(worst.CA.best.year$sum.CHI)

worst.CA.best.year<-worst.CA.best.year %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(worst.CA.best.year)

#Worst CA Worst Year
worst.CA.worst.year = base.sample %>%
  filter(Year == worst.year$Year) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(worst.CA.worst.year)[1] = "hour"

lambda=1/median(worst.CA.worst.year$sum.CHI)

worst.CA.worst.year<-worst.CA.worst.year %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(worst.CA.worst.year)


#plot comparing Best CA
plot_ly(best.CA.perhour, x = ~hour, y = ~safety, name = "2005-2016 Interval", type = "scatter", mode = "lines", fill = "tozeroy") %>%
  add_trace(x = best.CA.best.year$hour, y = best.CA.best.year$safety, name = "Safest Year", fill = "tozeroy") %>%
  add_trace(x = best.CA.worst.year$hour, y = best.CA.worst.year$safety, name = "Worst Year", fill = "tozeroy") %>%
  layout(xaxis = list(title = "Daily hours (24h format)"), yaxis = list(title = "Safety"), title = "Edison park safety in depth")




#Comparação da pior Beat com a média das beats em termos de safety para datas importantes para o intervalo 2005/2016--------------
#Podemos comparar isto com a média geral de todos os dias também

#DATA IMPORTANTES -> NEW YEAR'S DAY e INDEPENDENCE DAY
View(important_dates)
important_dates.base = important_dates %>%
  filter(Year != 2001 & Year != 2002 & Year != 2003 & Year != 2004) %>%
  filter(Holiday == "New Year's Day" | Holiday == "Independence Day")
View(important_dates.base)

#Média total da safety para estes feriados
important_dates.safety.perhour = important_dates.base %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(important_dates.safety.perhour)[1] = "hour"

lambda=1/median(important_dates.safety.perhour$sum.CHI)

important_dates.safety.perhour<-important_dates.safety.perhour %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(important_dates.safety.perhour)



important_dates.safety = important_dates.base %>%
  group_by(Beat, Holiday) %>%
  tally(CHI, name = "sum.CHI")

lambda=1/median(important_dates.safety$sum.CHI)

important_dates.safety<-important_dates.safety %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(important_dates.safety)


worst.beat.newyear = important_dates.safety %>%
  filter(Holiday == "New Year's Day") %>%
  filter(safety == min(worst.beat.newyear$safety))
View(worst.beat.newyear)

worst.beat.independence = important_dates.safety %>%
  filter(Holiday == "Independence Day") %>%
  filter(safety == min(worst.beat.independence$safety))
View(worst.beat.independence)


#Worst beat per your in the new year day (2005-2016)
worst.newyear.perhour = important_dates.base %>%
  filter(Beat == worst.beat.newyear$Beat & Holiday == worst.beat.newyear$Holiday) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(worst.newyear.perhour)[1] = "hour"

lambda=1/median(worst.newyear.perhour$sum.CHI)

worst.newyear.perhour<-worst.newyear.perhour %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(worst.newyear.perhour)

#Worst beat per hour in the independence day (2005-2016)
worst.independence.perhour = important_dates.base %>%
  filter(Beat == worst.beat.independence$Beat & Holiday == worst.beat.independence$Holiday) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(worst.independence.perhour)[1] = "hour"

lambda=1/median(worst.independence.perhour$sum.CHI)

worst.independence.perhour<-worst.independence.perhour %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(worst.independence.perhour)


#Pior Beat em dois feriados com a média das beats
plot_ly(important_dates.safety.perhour, x = ~hour, y = ~safety, type = 'scatter', mode = 'lines', name = 'Mean for both holidays (all beats)', fill = 'tozeroy') %>%
  add_trace(x = worst.newyear.perhour$hour, y = worst.newyear.perhour$safety, name = paste(c("New Year's Eve (beat", worst.beat.newyear$Beat, ")"), collapse = " "), fill = "tozeroy") %>%
  add_trace(x = worst.independence.perhour$hour, y = worst.independence.perhour$safety, name = paste(c("Independence Day (beat", worst.beat.independence$Beat, ")"), collapse = " "), fill = "tozeroy") %>%
  layout(xaxis = list(title = "Daily hour (24h format)"), yaxis = list(title = "Safety"), title = "Safety in the worst beats for important holidays (2005-2016)")



#Safety por hora de todos os dias (nao so os feriados)
safety.perhour = complete_sample %>%
  filter(Year != 2001 & Year != 2002 & Year != 2003 & Year != 2004) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI") %>%
colnames(safety.perhour)[1] = "hour"

lambda=1/median(safety.perhour$sum.CHI)

safety.perhour<-safety.perhour %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(safety.perhour)


#plot comparacao ano novo e 4th of july com média de safety por hora de todos os dias do intervalo 2005-2016
plot_ly(safety.perhour, x = ~hour, y = ~safety, type = 'scatter', mode = 'lines', name = 'Mean daily safety (all beats)', fill = 'tozeroy') %>%
  add_trace(x = worst.newyear.perhour$hour, y = worst.newyear.perhour$safety, name = paste(c("New Year's Eve (beat", worst.beat.newyear$Beat, ")"), collapse = " "), fill = "tozeroy") %>%
  add_trace(x = worst.independence.perhour$hour, y = worst.independence.perhour$safety, name = paste(c("Independence Day (beat", worst.beat.independence$Beat, ")"), collapse = " "), fill = "tozeroy") %>%
  layout(xaxis = list(title = "Daily hour (24h format)"), yaxis = list(title = "Safety"), title = "Safety in the worst beats for important holidays (2005-2016)")





#comparar a safety media de todos os beats para os feriados com a média normal
plot_ly(safety.perhour, x = ~hour, y = ~safety, type = 'scatter', mode = 'lines', name = 'Every day', fill = 'tozeroy') %>%
  add_trace(x = important_dates.safety.perhour$hour, y = important_dates.safety.perhour$safety, name = 'Holidays (04/07 and 01/01)', fill = 'tozeroy') %>%
  layout(xaxis = list(title = "Daily hour (24h format)"), yaxis = list(title = "Safety"), title = "Mean safety for every beat")







#worst and best beats (safety)
worst.beat = worst.beats[worst.beats$safety == min(worst.beats$safety), ]
best.beat = best.beats[best.beats$safety == max(best.beats$safety), ]

best.beat.perhour = base.sample %>%
  filter(Beat == best.beat$Beat) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(best.beat.perhour)[1] = "hour"

lambda=1/median(best.beat.perhour$sum.CHI)

best.beat.perhour<-best.beat.perhour %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(best.beat.perhour)


worst.beat.perhour = base.sample %>%
  filter(Beat == worst.beat$Beat) %>%
  group_by(hour(Date_converted)) %>%
  tally(CHI, name = "sum.CHI")
colnames(worst.beat.perhour)[1] = "hour"

lambda=1/median(worst.beat.perhour$sum.CHI)

worst.beat.perhour<-worst.beat.perhour %>%
  #mutate( n=-n/CHIT+1)
  mutate(safety=1000*exp(-sum.CHI*lambda))
View(worst.beat.perhour)


n_beats = length(unique(base.sample$Beat)) #303 beats

chi.average = safety.perhour %>%
  mutate(chi.avg = sum.CHI / n_beats)

View(chi.average)



plot_ly(safety.perhour, x = ~hour, y = ~chi.avg, type = 'scatter', mode = 'lines', name = 'daily average (all beats)', fill = 'tozeroy') %>%
  add_trace(x = best.beat.perhour$hour, y = best.beat.perhour$sum.CHI, name = "best beat (1652)", fill = "tozeroy") %>%
  add_trace(x = worst.beat.perhour$hour, y = worst.beat.perhour$sum.CHI, name = "worst beat (421)", fill = "tozeroy") %>%
  layout(xaxis = list(title = "Daily hours (24h format)"), yaxis = list(title = "Safety"), title = "Average CHI for the best and worst beats")


b




#Mapa de community_areas (Area de testes)--------------
#ESTUDO DE COMMUNITY AREAS
#Ver as várias variáveis, por exemplo, o harship index,etc.
#Densidade populacional
#Densidade de crimes por população (crimes por 10000 habitantes p.e.)
#Safety de cada CA

c.areas = readOGR("mapas/CA/geo_export_980e6813-e98b-47fa-9ed8-b8eb75ae851d.shp")

#para mostrar todas as palletes
display.brewer.all()

#criar uma pallete de cores para as community areas
bins = c(0, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 100000)
pal <- colorBin(
  palette = "YlOrBr",
  domain = chicago_se_index$Population,
  bins = bins)

#latitude e longitude de cada beat em 2016
beat_locations = sample_pop[-c(1:7, 12:20)]
beat_locations = beat_locations %>%
  group_by(Beat) %>%
  filter(Year == 2009) %>%
  add_tally(name = "N.crimes") %>%
  filter(row_number(Beat) == 1)
View(beat_locations)
nrow(beat_locations)


#organizar populacao conforme shapefile
pop_list = c()
for(i in 1:length(c.areas$community)) {
  pop_list[i] = chicago_se_index[chicago_se_index$CA.number == c.areas$area_numbe[i], ]$Population
}
c.areas$population = pop_list

ca.density = c()
for(i in 1:length(c.areas$community)) {
  ca.density[i] = pop_list[i] / (c.areas@polygons[[i]]@area *10000)
}
ca.density


#Hover action para os poligonos
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
              highlightOptions = highlightOptions(color = "red", weight = 3)) %>%
  
  addLegend(pal = pal, values = ~population, opacity = 0.7, title = "Total da população",
            position = "bottomright") %>%
  
  addCircles(data = beat_locations, lng = ~Longitude, lat = ~Latitude, weight = 1, radius = ~N.crimes, popup = ~factor(Beat),
             highlightOptions = highlightOptions(color = "blue", weight = 2,
                                                 bringToFront = TRUE))



#Manipulação de dados para o mapa das beats-------
#Densidade de crimes por beat (para 2016 ou 2009), fazer escala com a area do shapefile (mapa)
#Estudar uma beat em especifico (beat com maior número de crimes ou pior em termos de densidade)
#Em que CA se encontra a beat?
#Estudar ano dessa beat (ex: 2009 ano da crise financeira nos USA) atráves da safety
#Ver dias mais importantes (feriados)
#Ver esses dias por hora (alturas mais caoticas)
#Estudar evolução temporal destes estudos (os feriados sao sempre maus? as coisas pioraram com a crise?) Esta evolução pode ser demonstrada num mapa com glyphs, por exemplo setas para cima que indiquem aumento de algo.
#Comparação entre esta beat e uma beat representativa da média

#Podemos fazer o mapa apenas da beat mais relevante e estudar os locais onde ha mais crimes (fazer uma divisão da beat para prevenir a sobrecarga)


display.brewer.all()

b.areas = readOGR("mapas/Beats/geo_export_e80401d7-e6f8-4d20-8da4-b699365275e5.shp")


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

n_crimes_list
#Densidade de crimes por beat
beat.density = list()
for(i in 1:length(b.areas$beat_num)) {
  beat.density[[i]] = n_crimes_list[i] / (b.areas@polygons[[i]]@area * 10000)
}
beat.density = as.numeric(as.character(beat.density))
beat.density
max(beat.density[!is.na(beat.density)])

#Mapa beats (Area de testes)-----------------

#CORRER A PARTIR DAQUI SEMPRE QUE FOR A PRIMEIRA VEZ QUE SE CORRE O MAPA
#Display para quando se da hover nos beats
labels <- sprintf(
  "<strong>Beat num: %s</strong><br/>Densidade de crimes: %g",
  b.areas$beat_num, beat.density
) %>% lapply(htmltools::HTML)

#Pallete para o total de crimes
bins = c(0, 100, 200, 300, 500, 1000, 2000, 5000)
pal <- colorBin(
  palette = "YlOrRd",
  domain = beat.density,
  bins = bins)


#Mapa
beats.map = leaflet(b.areas) %>%
  
  addControl("<h3>Densidade de crimes por beat em 2016</h3>", position = "topleft") %>%
  
  addPolygons(color = pal(beat.density), weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5, label = labels,
              highlightOptions = highlightOptions(color = "red", weight = 2)) %>%
  
  addLegend(pal = pal, values = beat.density, opacity = 0.7, title = "Densidade de crimes em crimes/unidade de area",
            position = "bottomright")

beats.map



#Shiny app para o mapa das beats-------------
#UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("beatsmap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
  selectInput("year", "Ano",
              c(2016, 2009)
  ),
  selectInput("type", "Tipo de dados", list("Total de crimes" = "crime", "Densidade de crimes" = "density"))
  )
)


#Server
server <- function(input, output, session) {
  input_year = reactive({
    input$year
  })
  input_type = reactive({
    input$type
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
  
  n.crimes = reactive({
    beat_locations = beat_loc()
    n_crimes_list = list()
    for(i in 1:length(b.areas$beat_num)) {
      n_crimes_list[[i]] <- beat_locations[beat_locations$Beat == 
                    as.integer(stri_replace_first_regex(b.areas$beat_num[i], "0*(\\d+)", "$1")), ]$N.crimes
    }
    #Introduzir Nas por coação
    n_crimes_list = as.numeric(as.character(n_crimes_list))
    n_crimes_list
  })
  
  
  beat_density = reactive({
    n_crimes_list = n.crimes()
    beat.density = list()
    for(i in 1:length(b.areas$beat_num)) {
      beat.density[[i]] = n_crimes_list[i] / (b.areas@polygons[[i]]@area * 10000)
    }
    beat.density = as.numeric(as.character(beat.density))
    beat.density
  })
  
  
  b.shape = reactive({
    type = input_type()
    if(type == "crime") {
      b.areas$var = n.crimes()
      b.areas
    } else if(type == "density") {
      b.areas$var = beat_density()
      b.areas
    }
  })
  
  
  labs = reactive({
    b.areas = b.shape()
    type = input_type()
    if(type == "crime") {
      sprintf(
        "<strong>Beat num: %s</strong><br/>Quantidade de crimes: %g",
        b.areas$beat_num, b.areas$var
      ) %>% lapply(htmltools::HTML) 
    } else if(type == "density") {
      sprintf(
        "<strong>Beat num: %s</strong><br/>Densidade de crimes: %g",
        b.areas$beat_num, b.areas$var
      ) %>% lapply(htmltools::HTML) 
    }
  })
  
  #Pallete para o total de crimes
  pallete = reactive({
    type = input_type()
    if(type == "crime") {
      b.areas = b.shape()
      bins = c(0, 100, 200, 300, 400, 500, 600, 1000)
      colorBin(
        palette = "YlOrRd",
        domain = b.areas$crime_count,
        bins = bins)  
    } else if(type == "density") {
      bins = c(0, 100, 200, 300, 500, 1000, 2000, 5000)
      pal <- colorBin(
        palette = "YlOrRd",
        domain = beat_density(),
        bins = bins)
    }
  })
  
  
  title = reactive({
    year = input_year()
    type = input_type()
    if(type == "crime") {
      sprintf("<h3>Crimes por beat em %s</h3>", year) %>% lapply(htmltools::HTML)
    }else if(type == "density") {
      sprintf("<h3>Densidade de crimes por beat em %s</h3>", year) %>% lapply(htmltools::HTML)
    }
  })
  
  
  legend_title = reactive({
    type = input_type()
    if(type == "crime") {
      sprintf("Total de crimes")
    } else if(type == "density") {
      sprintf("Densidade de crimes por área")
    }
  })
  
  
  output$beatsmap <- renderLeaflet({
    pal = pallete()
    labels = labs()
    b.areas = b.shape()
    leaflet(b.areas) %>%
      
      addControl(title(), position = "topleft") %>%
      
      addPolygons(color = ~pal(var), weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5, label = labels,
                  highlightOptions = highlightOptions(color = "red", weight = 2)) %>%
      
      addLegend(pal = pal, values = ~var, opacity = 0.7, title = legend_title(),
                position = "bottomright")
  })
}

shinyApp(ui, server)




#Shiny app para o mapa das CA-----------------


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("ca.map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
  selectInput("year", "Ano",
              c(2016, 2009)
  ),
  selectInput("type", "Tipo de dados", 
              list("População" = "pop", "Densidade populacional" = "pop.density")
  ),
  checkboxInput("show.beats", "Show beats", TRUE)
  )
)


server <- function(input, output, session) {
  
  input.year <- reactive({
    input$year
  })
  
  input.type <- reactive({
    input$type
  })
  
  input.showbeats <- reactive({
    input$show.beats
  })
  
  
  #Beats and each location (the location of a crime in that beat) in some year
  beat.loc <- reactive({
    year = input.year()
    beat_locations = sample_pop[-c(1:7, 12:20)]
    beat_locations %>%
      group_by(Beat) %>%
      filter(Year == year) %>%
      add_tally(name = "N.crimes") %>%
      filter(row_number(Beat) == 1)
  })
  
  
  #organizar populacao conforme shapefile
  c.shape <- reactive({
    #População
    pop_list = c()
    for(i in 1:length(c.areas$community)) {
      pop_list[i] = chicago_se_index[chicago_se_index$CA.number == c.areas$area_numbe[i], ]$Population
    }
    c.areas$population = pop_list
    
    #Densidade
    ca.density = c()
    for(i in 1:length(c.areas$community)) {
      ca.density[i] = c.areas$population[i] / (c.areas@polygons[[i]]@area * 10000)
    }
    c.areas$density = ca.density
    c.areas
  })
  
  
  #Hover action para os poligonos
  ca.labels <- reactive({
    c.areas = c.shape()
    type = input.type()
    
    if(type == "pop") {
      sprintf(
        "<strong>%s</strong><br/>População total: %g",
        c.areas$community, c.areas$population
      ) %>% lapply(htmltools::HTML)
    } else if(type == "pop.density") {
      sprintf(
        "<strong>%s</strong><br/>Densidade populacional: %g",
        c.areas$community, c.areas$density
      ) %>% lapply(htmltools::HTML)
    }
  })
  
  
  #criar uma pallete de cores para as community areas
  map.pallete <- reactive({
    c.areas = c.shape()
    type = input.type()
    
    if(type == "pop") {
      bins = c(0, 10000, 30000, 50000, 70000, 90000, Inf)
      colorBin(
        palette = "YlOrBr",
        domain = c.areas$population,
        bins = bins) 
    } else if(type == "pop.density") {
      bins = c(0, 1000, 3000, 5000, 7000, 9000, Inf)
      colorBin(
        palette = "YlOrBr",
        domain = c.areas$density,
        bins = bins) 
    }
  })
  
  
  output$ca.map <- renderLeaflet({
    c.areas = c.shape()
    beat_locations = beat.loc()
    pal = map.pallete()
    type = input.type()
    labels = ca.labels()
    
    if(type == "pop") {
      var = c.areas$population
    } else if(type == "pop.density") {
      var = c.areas$density
    }
    
    leaflet(c.areas) %>% 
      
      setView(lat = 41.82795, lng = -87.71007, zoom = 11) %>%
      
      addControl("<h3>População por community area em 2016 (estimativa)</h3><p><em>Crimes por beat em 2016</em></p>", position = "topleft") %>%
      
      addPolygons(color = pal(var), weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  label = labels,
                  highlightOptions = highlightOptions(color = "red", weight = 3)) %>%
      
      addLegend(pal = pal, values = var, opacity = 0.7, title = "Total da população",
                position = "bottomright")
  })
  
  
  observe({
    show.beats = input.showbeats()
    proxy = leafletProxy("ca.map", data = c.areas)
    
    proxy %>%
    addCircles(layerId = as.character(c(1:277)), data = beat_locations, lng = ~Longitude, lat = ~Latitude, weight = 1, radius = ~N.crimes, popup = ~factor(Beat), highlightOptions = highlightOptions(color = "blue", weight = 2, bringToFront = TRUE))
    
    if(show.beats == F) {
      proxy %>% removeShape(as.character(c(1:277)))
    }
  })
}

shinyApp(ui, server)

#PARA FAZER A DASHBOARD---------------------
#https://rstudio.github.io/shinydashboard/get_started.html
#https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
#https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example