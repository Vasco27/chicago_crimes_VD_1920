

# Testes de tempo de leitura de ficheiros ---------------------
system.time(crimes_2001_2004 <- read.csv("C:/Users/HP/Desktop/mestrado/VD/projeto/datasets/Chicago_Crimes_2001_to_2004.csv")) #129.72 segundos, rows: 1923516

system.time(crimes_2001_2004 <- fread("C:/Users/HP/Desktop/mestrado/VD/projeto/datasets/Chicago_Crimes_2001_to_2004.csv")) #muito mais rapido (2.81 seg) mas por alguma razão vem com menos rows, 1513589

system.time(crimes_2001_2004 <- read_csv("C:/Users/HP/Desktop/mestrado/VD/projeto/datasets/Chicago_Crimes_2001_to_2004.csv")) #12.95 segundos, rows: 1923516

View(crimes_2001_2004)
nrow(crimes_2001_2004)


#Funções -----------------------------

libraries <- function() {
  require(data.table) #fread
  library(readr) #read_csv
  library(rstudioapi)
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




#pré processamento -------------------

#retirar coluna a mais
crimes_2001_2004 = crimes_2001_2004[, !(names(crimes_2001_2004) %in% "X1")]
crimes_2005_2007 = crimes_2005_2007[, !(names(crimes_2005_2007) %in% "X1")]
crimes_2008_2011 = crimes_2008_2011[, !(names(crimes_2008_2011) %in% "X1")]
crimes_2012_2017 = crimes_2012_2017[, !(names(crimes_2012_2017) %in% "X1")]
View(crimes_2008_2011)
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



#https://www.researchgate.net/post/How_to_sample_a_smaller_data_set_from_a_very_large_data_set
#https://analyticsindiamag.com/data-preprocessing-with-r-hands-on-tutorial/