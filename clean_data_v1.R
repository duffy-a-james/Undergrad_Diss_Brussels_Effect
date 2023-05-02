library(tidyverse)
library(readxl)
library(dplyr)
library(tidyselect)

#  Loading CO2 emission data
data_CO2 <- read_excel("/Users/jamesduffy/Library/CloudStorage/OneDrive-TheUniversityofManchester/BASS/Modules/Year\ 3/Sem\ 2/Diss/Data/EDGAR/IEA_EDGAR_CO2_1970-2021.xlsx")
attach(data_CO2)
# Put CO2 emission data into long format
data_CO2_long <- pivot_longer(data_CO2,
                              cols = starts_with("Y_"),
                              names_to = "Year",
                              names_prefix = "Y_",
                              values_to='CO2')

#  Doing the same transformation for SO2 emission data

data_SO2 <- read_excel("/Users/jamesduffy/Library/CloudStorage/OneDrive-TheUniversityofManchester/BASS/Modules/Year\ 3/Sem\ 2/Diss/Data/EDGAR/SO2_1970_2018.xlsx")
data_SO2_long <- pivot_longer(data_SO2,
                              cols = starts_with("Y_"),
                              names_to = "Year",
                              names_prefix = "Y_",
                              values_to='SO2')
#Delete column in SO2 long
data_SO2_long <- within(data_SO2_long, rm("C_group_IM24_sh"))

#Merge CO2 and SO2 datasets
CO2_SO2_long <- merge(data_CO2_long, data_SO2_long ,by=c("IPCC_annex","Country_code_A3","Name","Year"))
CO2_SO2_long <- within(CO2_SO2_long, rm("Substance.x","Substance.y"))

#  Doing the same transformation for PM10 emission data

data_PM10 <- read_excel("/Users/jamesduffy/Library/CloudStorage/OneDrive-TheUniversityofManchester/BASS/Modules/Year\ 3/Sem\ 2/Diss/Data/EDGAR/PM10_1970_2018.xlsx")
data_PM10_long <- pivot_longer(data_PM10,
                              cols = starts_with("Y_"),
                              names_to = "Year",
                              names_prefix = "Y_",
                              values_to='PM10')

#Merge PM10 dataset

all_pollutants_long <- merge(CO2_SO2_long, data_PM10_long ,by=c("IPCC_annex","Country_code_A3","Name","Year"))
all_pollutants_long <- within(all_pollutants_long, rm("C_group_IM24_sh","Substance"))

#Add unit of measure for CO2 and SO2 (Kt)

colnames(all_pollutants_long)[5] ="CO2(kt)"
colnames(all_pollutants_long)[6] ="SO2(kt)"
colnames(all_pollutants_long)[7] ="PM10(kt)"

#Load % population living in urban areas

urbanpop <- read_excel("/Users/jamesduffy/Library/CloudStorage/OneDrive-TheUniversityofManchester/BASS/Modules/Year\ 3/Sem\ 2/Diss/Data/WORLD\ BANK/wb_urban_population_percent.xlsx")



#transform year column names of urbanpop

names(urbanpop) <- gsub("\\[YR\\d+\\]", "", names(urbanpop)) #format year column names
# view original column names
names(urbanpop)
# specify range of year columns to modify
start_year <- 1960
end_year <- 2021
year_cols <- which(as.numeric(names(urbanpop)) >= start_year & as.numeric(names(urbanpop)) <= end_year)
# modify year column names by adding "YR_" prefix
names(urbanpop)[year_cols] <- paste0("Y_", names(urbanpop)[year_cols])
# view modified column names
names(urbanpop)

#Put urbanpop into long fromat

urbanpop_long <- pivot_longer(urbanpop,
                              cols = starts_with("Y_"),
                              names_to = "Year",
                              names_prefix = "Y_",
                              values_to='Urban Pop (%)')

# before merge, rename relevant columns of urbapop

colnames(urbanpop_long)[1] ="Name"
colnames(urbanpop_long)[2] ="Country_code_A3"


#filter for only relevant year in urban pop

attach(urbanpop_long)

urbanpop_long$Year <- as.numeric(urbanpop_long$Year)

urbanpop_long <- urbanpop_long[!(urbanpop_long$Year >= 1960 & urbanpop_long$Year <= 1969), ]
urbanpop_long <- urbanpop_long[!(urbanpop_long$Year >= 2019 & urbanpop_long$Year <= 2021), ]
summary(urbanpop_long)

#filter for only relevant countries in urban pop

urbanpop_long <- urbanpop_long[urbanpop_long$Country_code_A3 %in% all_pollutants_long$Country_code_A3, ]

#merge two datasets

chemcials_pop  <- merge(all_pollutants_long, urbanpop_long ,by=c("Country_code_A3","Name","Year"))

#chemcials_pop  <- merge(all_pollutants_long, urbanpop_long ,by=c("Country_code_A3","Name","Year"),all.x = ) trail with this extra argument

# plm tryout--

random <- plm('CO2(kt)' ~ 'Urban Pop (%)', data=chemcials_pop, model="random")
summary(random)




 
