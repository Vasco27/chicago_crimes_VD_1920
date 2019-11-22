

# Testes de tempo de leitura de ficheiros ---------------------
system.time(crimes_2001_2004 <- read.csv("C:/Users/HP/Desktop/mestrado/VD/projeto/datasets/Chicago_Crimes_2001_to_2004.csv")) #129.72 segundos, rows: 1923516

system.time(crimes_2001_2004 <- fread("C:/Users/HP/Desktop/mestrado/VD/projeto/datasets/Chicago_Crimes_2001_to_2004.csv")) #muito mais rapido (2.81 seg) mas por alguma raz?o vem com menos rows, 1513589

system.time(crimes_2001_2004 <- read_csv("C:/Users/HP/Desktop/mestrado/VD/projeto/datasets/Chicago_Crimes_2001_to_2004.csv")) #12.95 segundos, rows: 1923516

View(crimes_2001_2004)
nrow(crimes_2001_2004)

#Funcoes -----------------------------

libraries <- function() {
  require(data.table) #fread / apply
  library(rstudioapi)
  library(tidyverse) #readr / dplyr / ggplot2
  library(ChicagoHelper) #custom library
}


#PRE-PROCESSAMENTO-----------------

libraries()
#atribuicao de variaveis
files_list = read_files()
crimes_2001_2004 = files_list$`2001_2004`
crimes_2005_2007 = files_list$`2005_2007`
crimes_2008_2011 = files_list$`2008_2011`
crimes_2012_2017 = files_list$`2012_2017`

sum(duplicated(crimes_2001_2004$ID))
sum(duplicated(crimes_2001_2004$Case_Number))
View(crimes_2001_2004)


#remover variavel pesada (1.8Gb)
rm(files_list)

View(head(crimes_2001_2004))

#paralelismo (caso seja necessario usar foreach)------
library(doParallel)
registerDoParallel(cores = 4)


#1. ver a percentagem da ocorrencia de cada primary type no dataset completo
#2. fazer uma sample de 25% do dataset (df.new = df[seq(1, nrow(df), 4), ])
#3. ver a percentagem de ocorrencia de cada primary type no dataset partido
#4. comparar as percentagens de cada tipo de modo a perceber se a distribui??o n?o se alterou muito



#PR?-PROCESSAMENTO

#Verificar se os thefts englobam os restantes tipos de roubo----------
#Resposta: n?o, cada roubo ? especifico, o theft ? usado mais para roubo de dinheiro (ou roubo em geral)
#Robbery: Amea?a de uso de for?a com inten??o de roubar
#Assault: Uso de for?a ou ofensa ? integridade fisica de uma pessoa com inten??o de roubar
#Burglary: Entrada for?ada ou ilegal em propriedade privada ou alheia com inten??o de roubar
crimes_sample = crimes_2008_2011[-c(1:2, 5, 9:22)]
colnames(crimes_sample)[3] = "Primary_Type"
View(crimes_sample)

sample_thefts = filter(crimes_sample, Primary_Type %in% c("THEFT", "ROBBERY", "ASSAULT", "BURGLARY"))
View(sample_thefts)

sample_thefts = filter(crimes_sample, Primary_Type %in% "OFFENSE INVOLVING CHILDREN")
View(sample_thefts)






#SAMPLING DOS CHUNKS DO DATASET-----------------

cols_to_rm = c(1:2, 4:5, 11, 13:17, 19:22)
return_list = sample_data(crimes_2001_2004, cols_to_rm, 0)
dataset_2001_2004 = return_list$dataset
sample_2001_2004 = return_list$sampled_dataset

return_list = sample_data(crimes_2005_2007, cols_to_rm, 0)
dataset_2005_2007 = return_list$dataset
sample_2005_2007 = return_list$sampled_dataset

return_list = sample_data(crimes_2008_2011, cols_to_rm, 0)
dataset_2008_2011 = return_list$dataset
sample_2008_2011 = return_list$sampled_dataset

return_list = sample_data(crimes_2012_2017, cols_to_rm, 0)
dataset_2012_2017 = return_list$dataset
sample_2012_2017 = return_list$sampled_dataset


View(return_list$pop_crime_freq)
View(return_list$sample_crime_freq)


#TESTES DE DISTRIBUICAO DAS AMOSTRAS (em termos de Primary type)--------------------------------
#Transformar valores categoricos de Primary_Type em valores numericos
#Teste para 2001_2004
numeric_primary_type_pop1 = unclass(as.factor(dataset_2001_2004$Primary_Type))
numeric_primary_type_pop2 = unclass(as.factor(sample_2001_2004$Primary_Type))

distribution.test(x = numeric_primary_type_pop1, y = numeric_primary_type_pop2, mean.threshold =  0.5, median.threshold = 1, sd.threshold =  0.5) #TRUE significa que o teste passou


#Teste para 2005_2007
numeric_primary_type_pop1 = unclass(as.factor(dataset_2005_2007$Primary_Type))
numeric_primary_type_pop2 = unclass(as.factor(sample_2005_2007$Primary_Type))

distribution.test(numeric_primary_type_pop1, numeric_primary_type_pop2, 0.5, 1, 0.5)


#Teste para 2008_2011
numeric_primary_type_pop1 = unclass(as.factor(dataset_2008_2011$Primary_Type))
numeric_primary_type_pop2 = unclass(as.factor(sample_2008_2011$Primary_Type))

distribution.test(numeric_primary_type_pop1, numeric_primary_type_pop2, 0.5, 1, 0.5)


#Teste para 2012_2017
numeric_primary_type_pop1 = unclass(as.factor(dataset_2012_2017$Primary_Type))
numeric_primary_type_pop2 = unclass(as.factor(sample_2012_2017$Primary_Type))

distribution.test(numeric_primary_type_pop1, numeric_primary_type_pop2, 0.5, 2, 0.5)



#JUNCAO DOS 4 DATASETS SAMPLED---------------
merged_data = rbind(sample_2001_2004, sample_2005_2007)
merged_data = rbind(merged_data, sample_2008_2011)
complete_sample = rbind(merged_data, sample_2012_2017)
View(complete_sample)



#Experiencia com a biblioteca------
cols_to_rm = c(1:2, 4:5, 11, 13:17, 19:22)
complete_sample = get_complete_sample(files.read = TRUE, cols_to_rm = cols_to_rm, irrelevant.perc = 0)
View(complete_sample)

length(unique(crimes_2001_2004$Beat))
length(unique(crimes_2005_2007$Beat))
length(unique(crimes_2008_2011$Beat))
length(unique(crimes_2012_2017$Beat))
length(unique(crimes_2012_2017$District))


nrow(complete_sample)
