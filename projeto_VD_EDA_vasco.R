#Funcoes -----------------------------

libraries <- function() {
  require(data.table) #fread / apply
  library(rstudioapi)
  library(tidyverse) #readr / dplyr / ggplot2
  library(ChicagoHelper) #custom library
  library(shiny) #interactive pages
  library(leaflet) #interactive maps
  library(rgdal) #Spatial line maps
  library(RColorBrewer) #Palletes de cores
  library(htmlwidgets)
  library(htmltools) #Para usar elementos html
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


#Mapa de community_areas--------
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
