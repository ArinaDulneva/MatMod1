install.packages("xlsx", dep = T)
install.packages("readr")
library (xlsx)
library (readr)
install.packages("ggplot2")
install.packages("tidyverse")
library (ggplot2)
library (tidyverse)

plot(Trees$`Ht (m)`, Trees$`Crown Diameter (m)`)   
ggplot(Trees, aes(x = 'Ht (m)', y = 'Crown Diameter (m)', color = Species)) + geom_point()

plot(Trees$`Ht (m)`, Trees$`Crown Diameter (m)`)
ggplot(Trees, aes(x = `Ht (m)`, y = `Crown Diameter (m)`, color = Species)) + geom_point()      

library(readr)
Trees <- read_delim("C:/Users/Арина Валерьевна/Desktop/Math/MatMod1/Trees.csv", 
                    ";", escape_double = FALSE, col_types = cols(Tno = col_double(), 
                                                                 HR = col_character(), `% Variation` = col_double()), 
                    locale = locale(decimal_mark = ","), 
                    trim_ws = TRUE)
View(Trees)

ggplot(Trees, aes(x = `Ht (m)`, y = `Crown Diameter (m)`, color = Species)) + geom_point()

# #################################
# Все переменные имеют корректный тип данных
# Повторяющиеся переменные убраны
# Из имен переменных убраны размерности
# Всем переменам заданы их реальные размерности
# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
# Категориальные переменные должны быть факторами
# Категории переменной из имени должны быть убраны
# Коды категориальных переменных заменены их категориями
# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
# Виды должны быть переименованы на латыне
# #########################################

trees
Trees

# Все переменные имеют корректный тип данных 
да

# Повторяющиеся переменные убраны

dbh (mm)
HR

Trees = Trees %>% select(-`dbh (mm)`, HR)
Trees = Trees %>% select(-HR)

# Из имен переменных убраны размерности
Trees = Trees %>% rename(dbh = `dbh (m)`)
Trees = Trees %>% rename(Ht = 'Ht (m)')
Trees = Trees %>% rename(ClearenceHt = `Clearance Ht (m)`)
Trees = Trees %>% rename(CrownDepth = `Crown Depth (m)`)
Trees = Trees %>% rename(CrownDiameter = `Crown Diameter (m)`)
Trees = Trees %>% rename(`Total N,S,E,W Radial Crown Spread` = `Total N,S,E,W Radial Crown Spread (m)`)
Trees = Trees %>% rename(`Average Radial Crown spread` = `Average Radial Crown spread (m)`)
Trees = Trees %>% rename(`Total Mean Radial Crown Spread` = `Total Mean Radial Crown Spread (m)`)
Trees = Trees %>% rename(`2yr dia gain` = `2yr dia gain (mm)`)
Trees = Trees %>% rename(`Stem diameter Jan 2017` = `Stem diameter Jan 2017 (mm)`)
Trees = Trees %>% rename(`Annual Girth Increment` = `Annual Girth Increment (mm)`)

# Всем переменам заданы их реальные размерности

install.packages("units")
library(units)
units(Trees$Ht) = as_units("m")
Trees$Ht
Trees %>% as.data.frame()

units(Trees$dbh) = as_units("m")
Trees$dbh
Trees %>% as.data.frame()

units(Trees$ClearenceHt) = as_units("m")
units(Trees$CrownDepth) = as_units("m")
units(Trees$`Total N,S,E,W Radial Crown Spread`) = as_units("m")
units(Trees$`Average Radial Crown spread`) = as_units("m")
units(Trees$`Total Mean Radial Crown Spread`) = as_units("m")
units(Trees$CrownDiameter) = as_units("m")
units(Trees$`Predicted Crown Diameter`) = as_units("m")
Trees %>% as.data.frame()

# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
Trees = Trees %>% mutate(error = `Predicted crown diamet using combined formulla` - CrownDiameter)
Trees = Trees %>% select(-Difference)


Trees = Trees %>% mutate(error2 = `Predicted Crown Diameter` - CrownDiameter)
Trees = Trees %>% select(-Diference)

# Категориальные переменные должны быть факторами
library(forcats)
install.packages ('sf')
library(sf)
Trees$`Data Set      1=Norwich                0= Peterborough`
Trees = Trees %>%
  mutate(Data_Set = as_factor(`Data Set      1=Norwich                0= Peterborough`)) %>%
  mutate(Data_Set = fct_recode(Data_Set, Peterborough = "0", Norwich = "1"))

Trees = Trees %>%
  mutate(AgeIndex = as_factor(`Age Index 1=Y 2=SM 3=EM 4=M`)) %>%
  mutate(AgeIndex = fct_recode(AgeIndex, Y = "1", SM = "2", EM = "3", M = "4"))

Trees = Trees %>% select(-Data_Set)

Trees = Trees %>%
  mutate(PruningIndex = as_factor(`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`)) %>%
  mutate(PruningIndex = fct_recode(PruningIndex, 'pruned within 5yrs' = "5", 'pruned between 5 and 10yrs' = "10"))

Trees = Trees %>%
  mutate(TypeofPruning = as_factor(`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`)) %>%
  mutate(TypeofPruning = fct_recode(TypeofPruning, None = "0", CR = "1", Other = "2", Both = "3"))

Trees = Trees %>%
  mutate(SoilCode = as_factor(`Soil Code 1=sand and gravel 2= Clay 3=silt`)) %>%
  mutate(SoilCode = fct_recode(SoilCode, 'Sand and gravel' = "1", Clay = "2", Silt = "3"))

Trees = Trees %>% select(-`Data Set      1=Norwich                0= Peterborough`)
Trees = Trees %>% select(-`Age Index 1=Y 2=SM 3=EM 4=M`)
Trees = Trees %>% select(-`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`)
Trees = Trees %>% select(-`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`)
Trees = Trees %>% select(-`Soil Code 1=sand and gravel 2= Clay 3=silt`)


# Виды должны быть переименованы на латыне
# Transform all to latin 
# maple - Acer platanoides, 
# Oak - Quercus robur,
# Silver birch - Betula pendula, 
# Sycamore - Platanus occidentalis
Trees$Species
Trees$Species[Trees$Species == "Oak"] = "Quercus robur"
Trees$Species[Trees$Species == "Silver birch"] = "Betula pendula"
Trees$Species[Trees$Species == "Silver Birch"] = "Betula pendula"
Trees$Species[Trees$Species == "Norway Maple"] = "Acer platanoides"
Trees$Species[Trees$Species == "Norway maple"] = "Acer platanoides"
Trees$Species[Trees$Species == "Sycamore"] = "Platanus occidentalis"


# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
library(stringr)
Trees$`Grid Reference`
coord1 = str_replace_all(Trees$`Grid Reference`, ' ', '')
coord_n = str_trunc(coord1, 12, "right", ellipsis = "") %>% str_trunc(5, "left", ellipsis = "")
coord_e = str_trunc(coord1, 7, "right", ellipsis = "") %>% str_trunc(5, "left", ellipsis = "")
quadr = str_trunc(coord1, 2, "right", ellipsis = "")
table_c = data.frame(as.integer(coord_e), as.integer(coord_n), quadr)

names(table_c)=c("E", "N", "quadr")
head(table_c)
table_c = na.exclude(table_c)




table_c = table_c %>% mutate("Easting_BC" = case_when(
  quadr == "TF" ~ E +600000,
  quadr == "TG" ~ E +700000,
  quadr == "TL" ~ E +600000,
))
table_c = table_c %>% mutate("Northing_BC" = case_when(
  quadr == "TF" ~ N +300000,
  quadr == "TG" ~ N +300000,
  quadr == "TL" ~ N +200000,
))

table_c = na.exclude(table_c)

library(sf)
table_WGS = 
  table_c %>% 
  st_as_sf(coords = c("Easting_BC", "Northing_BC"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>% as.data.frame()


table_WGS = data.frame(Lat = table_WGS$Y, Lon = table_WGS$X)
table_WGS %>% head


Trees$`Grid Reference`[1]
table_c[1,]
table_WGS[1,]

coord = cbind(table_c,table_WGS)
head(coord)
