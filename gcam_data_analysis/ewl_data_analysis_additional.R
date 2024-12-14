library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 0 Common Requirements

# path to the directory of all EWL query excels
path = "D:\\emission constraint 2\\data repo\\GCAM-model\\output\\gcam_diagnostics\\gcam_data\\EWL\\intermediate_factors"
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



# 6 Basin level available runoff
# 6.1 create a blink dataframe to store data
basin_runoff <- as.data.frame(matrix(nrow = 0,ncol = 6))
for (i in 1:n_scen){
  # 6.2 set path and read in
  basin_runoff_scen = read_excel(dir[i], sheet='Sheet6', skip=1)
  # 6.3 drop the redundant title row and change variable names
  basin_runoff_scen <- basin_runoff_scen%>%filter(title != "title")
  # 6.4 change the scenario names, and convert to long format
  basin_runoff_scen <- basin_runoff_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  basin_runoff_scen$value <- sapply(basin_runoff_scen$value, as.numeric) # set values into numeric(doubles)
  basin_runoff_scen <- basin_runoff_scen%>%mutate(basin_runoff=value)%>%
    select(region,scenario,subresource,Basin,year,basin_runoff)
  # 6.5 rbind the current basin_runoff_scen into summarized basin_runoff
  basin_runoff <- rbind(basin_runoff, basin_runoff_scen)
}
# 6.6 write the summarized file into a new csv
basin_runoff$scenario<-unlist(basin_runoff$scenario)
write.csv(basin_runoff, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\basin_runoff.csv", row.names = FALSE)





# 7 Basin level runoff & groundwater WW
# 7.1 create a blink dataframe to store data
basin_WW <- as.data.frame(matrix(nrow = 0,ncol = 6))
for (i in 1:n_scen){
  # 7.2 set path and read in
  basin_WW_scen = read_excel(dir[i], sheet='Sheet7', skip=1)
  # 7.3 drop the redundant title row and change variable names
  basin_WW_scen <- basin_WW_scen%>%filter(title != "title")
  # 7.4 change the scenario names, and convert to long format
  basin_WW_scen <- basin_WW_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  basin_WW_scen$value <- sapply(basin_WW_scen$value, as.numeric) # set values into numeric(doubles)
  basin_WW_scen <- basin_WW_scen%>%mutate(WW=value,Basin=resource)%>%
    select(region,scenario,Basin,subresource,year,WW)
  # 7.5 rbind the current basin_WW_scen into summarized basin_WW
  basin_WW <- rbind(basin_WW, basin_WW_scen)
}
# 7.6 write the summarized file into a new csv
basin_WW$scenario<-unlist(basin_WW$scenario)
write.csv(basin_WW, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\basin_WW.csv", row.names = FALSE)






# 8 ag tech yield
yield <- as.data.frame(matrix(nrow = 0,ncol = 7))
for (i in 1:n_scen){
  # 8.2 set path and read in
  yield_scen = read_excel(dir[i], sheet='Sheet8', skip=1)
  # 8.3 drop the redundant title row and change variable names
  yield_scen <- yield_scen%>%filter(title != "title")
  # 8.4 change the scenario names, and convert to long format
  yield_scen <- yield_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  yield_scen$value <- sapply(yield_scen$value, as.numeric) # set values into numeric(doubles)
  yield_scen <- yield_scen%>%mutate(Yield=value)%>%
    select(region,scenario,sector,subsector,technology,year,Yield)
  # 8.5 rbind the current yield_scen into summarized yield
  yield <- rbind(yield, yield_scen)
}
# 8.6 write the summarized file into a new csv
yield$scenario<-unlist(yield$scenario)
write.csv(yield, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\yield.csv", row.names = FALSE)






# 9 ag commodity price
ag_price <- as.data.frame(matrix(nrow = 0,ncol = 5))
for (i in 1:n_scen){
  # 9.2 set path and read in
  ag_price_scen = read_excel(dir[i], sheet='Sheet8', skip=1)
  # 9.3 drop the redundant title row and change variable names
  ag_price_scen <- ag_price_scen%>%filter(title != "title")
  # 9.4 change the scenario names, and convert to long format
  ag_price_scen <- ag_price_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  ag_price_scen$value <- sapply(ag_price_scen$value, as.numeric) # set values into numeric(doubles)
  ag_price_scen <- ag_price_scen%>%mutate(Price=value)%>%
    select(region,scenario,sector,year,Price)
  # 9.5 rbind the current ag_price_scen into summarized ag_price
  ag_price <- rbind(ag_price, ag_price_scen)
}
# 9.6 write the summarized file into a new csv
ag_price$scenario<-unlist(ag_price$scenario)
write.csv(ag_price, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\ag_price.csv", row.names = FALSE)





# 10 detailed Land Allocation
setwd("D:\\emission constraint 2\\data repo\\GCAM-model\\output\\gcam_diagnostics\\gcam_data\\EWL\\specific\\detailedLU")
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

# pre process detailed LU data
detaile_LA <- as.data.frame(matrix(nrow = 0,ncol = 5))
for (i in 1:n_scen){
  # 9.2 set path and read in
  detaile_LA_scen = read_excel(dir[i], sheet='Sheet1', skip=1)
  # 9.3 drop the redundant title row and change variable names
  detaile_LA_scen <- detaile_LA_scen%>%filter(title != "title")
  # 9.4 change the scenario names, and convert to long format
  detaile_LA_scen <- detaile_LA_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  detaile_LA_scen$value <- sapply(detaile_LA_scen$value, as.numeric) # set values into numeric(doubles)
  detaile_LA_scen <- detaile_LA_scen%>%mutate(Area=value)%>%
    select(region,scenario,LandLeaf,year,Area)
  # 9.5 rbind the current detaile_LA_scen into summarized detaile_LA
  detaile_LA <- rbind(detaile_LA, detaile_LA_scen)
}
# 9.6 write the summarized file into a new csv
detaile_LA$scenario<-unlist(detaile_LA$scenario)
write.csv(detaile_LA, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\detaile_LA.csv", row.names = FALSE)








