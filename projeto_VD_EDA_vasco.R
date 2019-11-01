

# Testes de tempo de leitura de ficheiros ---------------------
system.time(crimes_2001_2004 <- read.csv("C:/Users/HP/Desktop/mestrado/VD/projeto/datasets/Chicago_Crimes_2001_to_2004.csv")) #129.72 segundos, rows: 1923516

system.time(crimes_2001_2004 <- fread("C:/Users/HP/Desktop/mestrado/VD/projeto/datasets/Chicago_Crimes_2001_to_2004.csv")) #muito mais rapido (2.81 seg) mas por alguma razão vem com menos rows, 1513589

system.time(crimes_2001_2004 <- read_csv("C:/Users/HP/Desktop/mestrado/VD/projeto/datasets/Chicago_Crimes_2001_to_2004.csv")) #12.95 segundos, rows: 1923516

View(crimes_2001_2004)
nrow(crimes_2001_2004)


#Funções -----------------------------

libraries <- function() {
  require(data.table) #fread / apply
  library(rstudioapi)
  library(tidyverse) #readr / dplyr / ggplot2
}

read_files <- function() {
  path = dirname(rstudioapi::getSourceEditorContext()$path)
  setwd(path) 
  
  types = "iic?cccccllinnncnni?nn?"
  
  crimes_2001_2004 = read_csv(file = "datasets/Chicago_Crimes_2001_to_2004.csv", col_types = types)
  crimes_2005_2007 = read_csv(file = "datasets/Chicago_Crimes_2005_to_2007.csv", col_types = types)
  crimes_2008_2011 = read_csv(file = "datasets/Chicago_Crimes_2008_to_2011.csv", col_types = types)
  crimes_2012_2017 = read_csv(file = "datasets/Chicago_Crimes_2012_to_2017.csv", col_types = types)
  
  return(list("2001_2004" = crimes_2001_2004, "2005_2007" = crimes_2005_2007, "2008_2011" = crimes_2008_2011, "2012_2017" = crimes_2012_2017));
}
libraries()
#atribuicao de variaveis
files_list = read_files()
crimes_2001_2004 = files_list$`2001_2004`
crimes_2005_2007 = files_list$`2005_2007`
crimes_2008_2011 = files_list$`2008_2011`
crimes_2012_2017 = files_list$`2012_2017`




#PRÉ-PROCESSAMENTO -------------------

#retirar coluna a mais
crimes_2001_2004 = crimes_2001_2004[, !(names(crimes_2001_2004) %in% "X1")]
crimes_2005_2007 = crimes_2005_2007[, !(names(crimes_2005_2007) %in% "X1")]
crimes_2008_2011 = crimes_2008_2011[, !(names(crimes_2008_2011) %in% "X1")]
crimes_2012_2017 = crimes_2012_2017[, !(names(crimes_2012_2017) %in% "X1")]
View(crimes_2008_2011)
View(crimes_2001_2004)
#30692 NA na latitude, longitude, location, x coordinate e y coordinate
#700248 NA na ward e Community area



#paralelismo (caso seja necessário usar foreach)
library(doParallel)
registerDoParallel(cores = 4)

?sample

#1. ver a percentagem da ocorrencia de cada primary type no dataset completo 
#2. fazer uma sample de 25% do dataset (df.new = df[seq(1, nrow(df), 4), ])
#3. ver a percentagem de ocorrencia de cada primary type no dataset partido
#4. comparar as percentagens de cada tipo de modo a perceber se a distribuição não se alterou muito


#Verificar se os thefts englobam os restantes tipos de roubo:
#Resposta: não, cada roubo é especifico, o theft é usado mais para roubo de dinheiro (ou roubo em geral)
#Robbery: Ameaça de uso de força com intenção de roubar
#Assault: Uso de força ou ofensa à integridade fisica de uma pessoa com intenção de roubar
#Burglary: Entrada forçada ou ilegal em propriedade privada ou alheia com intenção de roubar
crimes_sample = crimes_2008_2011[-c(1:2, 5, 9:22)]
colnames(crimes_sample)[3] = "Primary_Type"
View(crimes_sample)

sample_thefts = filter(crimes_sample, Primary_Type %in% c("THEFT", "ROBBERY", "ASSAULT", "BURGLARY"))
View(sample_thefts)

sample_thefts = filter(crimes_sample, Primary_Type %in% "OFFENSE INVOLVING CHILDREN")
View(sample_thefts)







#SAMPLING DO DATASET-------------
#Eliminar colunas não necessárias
non_sampled_2001_2004 = crimes_2001_2004[-c(1:2, 4:5, 11, 13:17, 19:22)]

colnames(non_sampled_2001_2004)[2] = "Primary_Type"; colnames(non_sampled_2001_2004)[4] = "Location_Description"
View(non_sampled_2001_2004)

#Tipos de crime com percentagem 0:
#DOMESTIC VIOLENCE / IUCR (erro no df) / NON-CRIMINAL / OBSCENITY / OTHER NARCOTIC VIOLATION / PUBLIC INDECENCY / RITUALISM
non_sampled_2001_2004 = filter(non_sampled_2001_2004, !Primary_Type %in% c("DOMESTIC VIOLENCE", "IUCR", "NON-CRIMINAL", "OBSCENITY", "OTHER NARCOTIC VIOLATION", "PUBLIC INDECENCY", "RITUALISM"))

#Tabela de frequência do Primary_Type e Description
sample_2001_2004_table = lapply(non_sampled_2001_2004["Primary_Type"], table)

crime_types = as.data.frame(sample_2001_2004_table$Primary_Type)
colnames(crime_types)[1] = "Type"

#options("scipen"=100, "digits"=4) #não mostrar potencias de 10
#percentagem de ocorrencia de cada crime
crime_types = crime_types %>% 
  mutate(Type, percent = round((Freq / nrow(non_sampled_2001_2004)) * 100, 2))

View(crime_types)



#sampling de 25%
nrow(non_sampled_2001_2004)
sample_2001_2004 = non_sampled_2001_2004[seq(1, nrow(non_sampled_2001_2004), 4), ]
nrow(sample_2001_2004)


#Verificação de percentagens
sample_2001_2004_table = lapply(sample_2001_2004["Primary_Type"], table)

sampled_crime_types = as.data.frame(sample_2001_2004_table)
colnames(sampled_crime_types) = c("Type", "Freq")

sampled_crime_types = sampled_crime_types %>% 
  mutate(Type, percent = round((Freq / nrow(sample_2001_2004)) * 100, 2))

View(sampled_crime_types)




#Transformar valores categóricos de Primary_Type em valores numericos
numeric_primary_type_pop1 = unclass(as.factor(non_sampled_2001_2004$Primary_Type))
numeric_primary_type_pop2 = unclass(as.factor(sample_2001_2004$Primary_Type))

mean(numeric_primary_type_pop1)
mean(numeric_primary_type_pop2)

median(numeric_primary_type_pop1)
median(numeric_primary_type_pop2)

sd(numeric_primary_type_pop1)
sd(numeric_primary_type_pop2)
