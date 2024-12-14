library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 0 Common Requirements

# path to the directory of all EWL query excels
path = "D:\\emission constraint 2\\data repo\\GCAM-model\\output\\gcam_diagnostics\\gcam_data\\EWL\\main_figures"
# all the files in the directory
setwd(path)
xls_list<-list.files()
dir <- paste("./", xls_list, sep = "")
n_scen <- length(dir)
# reset scenario names
scenarios = list("Reference","Reference_CI_GFDL","Reference_CI_HadGEM","Reference_CI_IPSL",
                 "Reference_CI_MIROC","Reference_CI_NorESM","Policy","Policy_CI_GFDL",
                 "Policy_CI_HadGEM","Policy_CI_IPSL","Policy_CI_MIROC","Policy_CI_NorESM",
                 "Policy_NoDAC","Policy_NoDAC_CI_GFDL","Policy_NoDAC_CI_HadGEM",
                 "Policy_NoDAC_CI_IPSL","Policy_NoDAC_CI_MIROC","Policy_NoDAC_CI_NorESM")
# years of modeling
years = c("1990","2005","2010","2015","2020","2025","2030","2035","2040","2045","2050",
          "2055","2060","2065","2070","2075","2080","2085","2090","2095","2100")



# 1 LandUse
# 1.1 create a blink dataframe to store data
LU <- as.data.frame(matrix(nrow = 0,ncol = 5))
LU_detail <- as.data.frame(matrix(nrow = 0,ncol = 5))
for (i in 1:n_scen){
  # 1.2 set path and read in
  LU_scen = read_excel(dir[i], sheet='Sheet1', skip=1)
  # 1.3 drop the redundant title row and change variable names
  LU_scen <- LU_scen%>%filter(title != "title")%>%
    rename(landuse=LandLeaf)
  LU_scen_detail <- LU_scen
  # 1.4 merge landuse types of the same categories
  LU_scen$landuse[which((LU_scen$landuse=="forest (managed)")|
                          (LU_scen$landuse=="forest (unmanaged)"))]<-"forest"
  LU_scen$landuse[which((LU_scen$landuse=="pasture (grazed)")|
                          (LU_scen$landuse=="pasture (other)"))]<-"pasture"
  LU_scen$landuse[which((LU_scen$landuse=="otherarable")|
                          (LU_scen$landuse=="rock and desert")|
                          (LU_scen$landuse=="tundra"))]<-"naturalOther"
  # 1.5 sum up the LU_frac of the same category
  LU_scen <- LU_scen%>%gather(key="year",value = "value",years) # gather years into long format
  LU_scen$value <- sapply(LU_scen$value, as.numeric) # set values into numeric(doubles)
  LU_scen <- LU_scen%>%
    mutate(scenario=scenarios[i])%>%
    group_by(region,scenario,landuse,year)%>%
    summarize(LU_frac=sum(value))
  # 1.5.5 detailed LU
  LU_scen_detail <- LU_scen_detail%>%gather(key="year",value = "value",years) # gather years into long format
  LU_scen_detail$value <- sapply(LU_scen_detail$value, as.numeric) # set values into numeric(doubles)
  LU_scen_detail <- LU_scen_detail%>%
    mutate(scenario=scenarios[i],LU_frac=value)%>%
    select(region,scenario,landuse,year,LU_frac)
  # 1.6 rbind the current LU_scen into summarized LU
  LU <- rbind(LU, LU_scen)
  LU_detail <- rbind(LU_detail, LU_scen_detail)
}
# 1.7 write the summarized file into a new csv
LU$scenario<-unlist(LU$scenario)
LU_detail$scenario<-unlist(LU_detail$scenario)
write.csv(LU, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\LU.csv", row.names = FALSE)
write.csv(LU_detail, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\LU_detail.csv", row.names = FALSE)


# 2 Agricultural Production
# 2.1 create a blink dataframe to store data
Ag <- as.data.frame(matrix(nrow = 0,ncol = 5))
for (i in 1:n_scen){
  # 2.2 set path and read in
  Ag_scen = read_excel(dir[i], sheet='Sheet2', skip=1)
  # 2.3 drop the redundant title row and change variable names
  Ag_scen <- Ag_scen%>%filter(title != "title")%>%
    rename(crop=output)
  # 2.4 drop some crop types
  Ag_scen <- Ag_scen%>%filter(crop != "Forest")%>%
    filter(crop != "biomass")%>%
    select(-sector)
  # 2.5 sum up the Ag_frac of the same category
  Ag_scen <- Ag_scen%>%gather(key="year",value = "value",years) # gather years into long format
  Ag_scen$value <- sapply(Ag_scen$value, as.numeric) # set values into numeric(doubles)
  Ag_scen <- Ag_scen%>%
    mutate(scenario=scenarios[i])%>%
    group_by(region,scenario,crop,year)%>%
    summarize(Ag_prod=sum(value))
  # 2.6 rbind the current Ag_scen into summarized Ag
  Ag <- rbind(Ag, Ag_scen)
}
# 2.7 write the summarized file into a new csv
Ag$scenario<-unlist(Ag$scenario)
write.csv(Ag, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\Ag.csv", row.names = FALSE)


# 3 Water Withdrawals
# 3.1 create a blink dataframe to store data
WW <- as.data.frame(matrix(nrow = 0,ncol = 5))
WW_detail <- as.data.frame(matrix(nrow = 0,ncol = 5))
for (i in 1:n_scen){
  # 3.2 set path and read in
  WW_scen = read_excel(dir[i], sheet='Sheet3', skip=1)
  # 3.3 drop the redundant title row
  WW_scen <- WW_scen%>%filter(title != "title")
  WW_scen_detail <- WW_scen
  # 3.4.1 merge sectors of the same categories
  Livestock <- list("Beef","Dairy","Pork","Poultry","SheepGoat",
                    "FodderGrass","FodderHerb")
  Agriculture <- list("Corn","FiberCrop","MiscCrop","OilCrop","OtherGrain","Rice",
                       "RootTuber","SugarCrop","Wheat","PalmFruit")
  Electricity <- list("elec_biomass (IGCC CCS)","elec_biomass (conv CCS)",
                      "elec_coal (IGCC CCS)","elec_coal (conv pul CCS)",
                      "elec_gas (CC CCS)","elec_refined liquids (CC CCS)","electricity",
                      "nuclearFuelGenII","nuclearFuelGenIII")
  Primary <- list("biomass","regional coal",
                  "regional natural gas","regional oil")
  Industry <- list("industry")
  Municipal <- list("municipal water")
  # 3.4.2 merge sectors of the same categories
  WW_scen$sector[which(WW_scen$sector %in% Livestock)]<-"livestock"
  WW_scen$sector[which(WW_scen$sector %in% Agriculture)]<-"agriculture"
  WW_scen$sector[which(WW_scen$sector %in% Electricity)]<-"electricity"
  WW_scen$sector[which(WW_scen$sector %in% Primary)]<-"primaryEnergy"
  WW_scen$sector[which(WW_scen$sector %in% Industry)]<-"industry"
  WW_scen$sector[which(WW_scen$sector %in% Municipal)]<-"municipal"
  # 3.5 sum up the WW of the same category
  WW_scen <- WW_scen%>%gather(key="year",value = "value",years) # gather years into long format
  WW_scen$value <- sapply(WW_scen$value, as.numeric) # set values into numeric(doubles)
  WW_scen <- WW_scen%>%
    mutate(scenario=scenarios[i])%>%
    group_by(region,scenario,sector,year)%>%
    summarize(Water_Withdrawal=sum(value))
  # 3.5.5 detailed WW
  WW_scen_detail <- WW_scen_detail%>%gather(key="year",value = "value",years) # gather years into long format
  WW_scen_detail$value <- sapply(WW_scen_detail$value, as.numeric) # set values into numeric(doubles)
  WW_scen_detail <- WW_scen_detail%>%
    mutate(scenario=scenarios[i],Water_Withdrawal=value)%>%
    select(region,scenario,sector,year,Water_Withdrawal)
  # 3.6 rbind the current WW_scen into summarized WW
  WW <- rbind(WW, WW_scen)
  WW_detail <- rbind(WW_detail, WW_scen_detail)
}
# 3.7 write the summarized file into a new csv
WW$scenario<-unlist(WW$scenario)
WW_detail$scenario<-unlist(WW_detail$scenario)
write.csv(WW, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\WW.csv", row.names = FALSE)
write.csv(WW_detail, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\WW_detail.csv", row.names = FALSE)


# 4 Primary Energy
# 4.1 create a blink dataframe to store data
PrEn <- as.data.frame(matrix(nrow = 0,ncol = 5))
for (i in 1:n_scen){
  # 4.2 set path and read in
  PrEn_scen = read_excel(dir[i], sheet='Sheet4', skip=1)
  # 4.3 drop the redundant title row and change variable names
  PrEn_scen <- PrEn_scen%>%filter(title != "title")
  # 4.4 reset fuel names
  PrEn_scen$fuel[which(PrEn_scen$fuel=="a oil")]<-"oil"
  PrEn_scen$fuel[which(PrEn_scen$fuel=="b natural gas")]<-"natural gas"
  PrEn_scen$fuel[which(PrEn_scen$fuel=="c coal")]<-"coal"
  PrEn_scen$fuel[which(PrEn_scen$fuel=="d biomass")]<-"biomass"
  PrEn_scen$fuel[which(PrEn_scen$fuel=="e nuclear")]<-"nuclear"
  PrEn_scen$fuel[which(PrEn_scen$fuel=="f hydro")]<-"hydro"
  PrEn_scen$fuel[which(PrEn_scen$fuel=="g wind")]<-"wind"
  PrEn_scen$fuel[which(PrEn_scen$fuel=="h solar")]<-"solar"
  PrEn_scen$fuel[which(PrEn_scen$fuel=="i geothermal")]<-"geothermal"
  PrEn_scen$fuel[which(PrEn_scen$fuel=="j traditional biomass")]<-"biomass_traditional"
  # 4.5 sum up the primary energy of the same category
  PrEn_scen <- PrEn_scen%>%gather(key="year",value = "value",years) # gather years into long format
  PrEn_scen$value <- sapply(PrEn_scen$value, as.numeric) # set values into numeric(doubles)
  PrEn_scen <- PrEn_scen%>%
    mutate(scenario=scenarios[i])%>%
    group_by(region,scenario,fuel,year)%>%
    summarize(Primary_Energy=sum(value))
  # 4.6 rbind the current PrEn_scen into summarized PrEn
  PrEn <- rbind(PrEn, PrEn_scen)
}
# 4.7 write the summarized file into a new csv
PrEn$scenario<-unlist(PrEn$scenario)
write.csv(PrEn, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\PrEn.csv", row.names = FALSE)



# 5 Electricity Generation
# 5.1 create a blink dataframe to store data
ElecGen <- as.data.frame(matrix(nrow = 0,ncol = 5))
for (i in 1:n_scen){
  # 5.2 set path and read in
  ElecGen_scen = read_excel(dir[i], sheet='Sheet5', skip=1)
  # 5.3 drop the redundant title row and unused output sectors, change variable names
  ElecGen_scen <- ElecGen_scen%>%filter(title != "title")%>%
    filter(output == "electricity")%>%
    rename(fuel = subsector)%>%
    select(-output)
  # 5.4 reset fuel names
  ElecGen_scen$fuel[which(ElecGen_scen$fuel=="refined liquids")]<-"oil"
  # 5.5 sum up the primary energy of the same category
  ElecGen_scen <- ElecGen_scen%>%gather(key="year",value = "value",years) # gather years into long format
  ElecGen_scen$value <- sapply(ElecGen_scen$value, as.numeric) # set values into numeric(doubles)
  ElecGen_scen <- ElecGen_scen%>%
    mutate(scenario=scenarios[i])%>%
    group_by(region,scenario,fuel,year)%>%
    summarize(Elec_Generation=sum(value))
  # 5.6 rbind the current ElecGen_scen into summarized ElecGen
  ElecGen <- rbind(ElecGen, ElecGen_scen)
}
# 5.7 write the summarized file into a new csv
ElecGen$scenario<-unlist(ElecGen$scenario)
write.csv(ElecGen, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\ElecGen.csv", row.names = FALSE)
