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



# 1 CO2 emission by tech
# 1.1 create a blink dataframe to store data
emission <- as.data.frame(matrix(nrow = 0,ncol = 7))
for (i in 1:n_scen){
  # 1.2 set path and read in
  emission_scen = read_excel(dir[i], sheet='Sheet1', skip=1)
  # 1.3 drop the redundant title row and change variable names
  emission_scen <- emission_scen%>%filter(title != "title")
  # 1.4 change the scenario names, and convert to long format
  emission_scen <- emission_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  emission_scen$value <- sapply(emission_scen$value, as.numeric) # set values into numeric(doubles)
  emission_scen <- emission_scen%>%mutate(Emission=value)%>%
    select(region,scenario,sector,subsector,technology,year,Emission)
  # 1.5 rbind the current emission_scen into summarized emission
  emission <- rbind(emission, emission_scen)
}
# 1.6 write the summarized file into a new csv
emission$scenario<-unlist(emission$scenario)
write.csv(emission, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\emission_by_tech.csv", row.names = FALSE)




# 2 elec gen costs by tech
# 2.1 create a blink dataframe to store data
elec_cost <- as.data.frame(matrix(nrow = 0,ncol = 7))
for (i in 1:n_scen){
  # 2.2 set path and read in
  elec_cost_scen = read_excel(dir[i], sheet='Sheet2', skip=1)
  # 2.3 drop the redundant title row and change variable names
  elec_cost_scen <- elec_cost_scen%>%filter(title != "title")
  # 2.4 change the scenario names, and convert to long format
  elec_cost_scen <- elec_cost_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  elec_cost_scen$value <- sapply(elec_cost_scen$value, as.numeric) # set values into numeric(doubles)
  elec_cost_scen <- elec_cost_scen%>%mutate(cost=value)%>%
    select(region,scenario,sector,subsector,technology,year,cost)
  # 2.5 rbind the current elec_cost_scen into summarized elec_cost
  elec_cost <- rbind(elec_cost, elec_cost_scen)
}
# 2.6 write the summarized file into a new csv
elec_cost$scenario<-unlist(elec_cost$scenario)
write.csv(elec_cost, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\elec_cost_by_tech.csv", row.names = FALSE)




# 3 elec WW by gen tech
# 3.1 create a blink dataframe to store data
elec_WW <- as.data.frame(matrix(nrow = 0,ncol = 7))
for (i in 1:n_scen){
  # 3.2 set path and read in
  elec_WW_scen = read_excel(dir[i], sheet='Sheet4', skip=1)
  # 3.3 drop the redundant title row and change variable names
  elec_WW_scen <- elec_WW_scen%>%filter(title != "title")
  # 3.4 change the scenario names, and convert to long format
  elec_WW_scen <- elec_WW_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  elec_WW_scen$value <- sapply(elec_WW_scen$value, as.numeric) # set values into numeric(doubles)
  elec_WW_scen <- elec_WW_scen%>%mutate(WW=value)%>%
    select(region,scenario,sector,subsector,technology,year,WW)
  # 3.5 rbind the current elec_WW_scen into summarized elec_WW
  elec_WW <- rbind(elec_WW, elec_WW_scen)
}
# 3.6 write the summarized file into a new csv
elec_WW$scenario<-unlist(elec_WW$scenario)
write.csv(elec_WW, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\elec_WW_by_tech.csv", row.names = FALSE)





# 4 elec gen by gen tech
# 4.1 create a blink dataframe to store data
elec_gen <- as.data.frame(matrix(nrow = 0,ncol = 7))
for (i in 1:n_scen){
  # 4.2 set path and read in
  elec_gen_scen = read_excel(dir[i], sheet='Sheet3', skip=1)
  # 4.3 drop the redundant title row and change variable names
  elec_gen_scen <- elec_gen_scen%>%filter(title != "title")
  # 4.4 change the scenario names, and convert to long format
  elec_gen_scen <- elec_gen_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  elec_gen_scen$value <- sapply(elec_gen_scen$value, as.numeric) # set values into numeric(doubles)
  elec_gen_scen <- elec_gen_scen%>%mutate(gen=value)%>%
    select(region,scenario,output,subsector,technology,year,gen)
  # 4.5 rbind the current elec_gen_scen into summarized elec_gen
  elec_gen <- rbind(elec_gen, elec_gen_scen)
}
# 4.6 write the summarized file into a new csv
elec_gen$scenario<-unlist(elec_gen$scenario)
write.csv(elec_gen, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\elec_gen_by_tech.csv", row.names = FALSE)





# 5 final energy consumption by tech
# 5.1 create a blink dataframe to store data
end_use <- as.data.frame(matrix(nrow = 0,ncol = 6))
for (i in 1:n_scen){
  # 5.2 set path and read in
  end_use_scen = read_excel(dir[i], sheet='Sheet5', skip=1)
  # 5.3 drop the redundant title row and change variable names
  end_use_scen <- end_use_scen%>%filter(title != "title")
  # 5.4 change the scenario names, and convert to long format
  end_use_scen <- end_use_scen%>%mutate(scenario=scenarios[i])%>%
    gather(key="year",value = "value",years) # gather years into long format
  end_use_scen$value <- sapply(end_use_scen$value, as.numeric) # set values into numeric(doubles)
  end_use_scen <- end_use_scen%>%mutate(Consumption=value)%>%
    select(region,scenario,sector,input,year,Consumption)
  # 5.5 rbind the current end_use_scen into summarized end_use
  end_use <- rbind(end_use, end_use_scen)
}
# 5.6 write the summarized file into a new csv
end_use$scenario<-unlist(end_use$scenario)
write.csv(end_use, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\end_use_by_energySource.csv", row.names = FALSE)





# 6 WW by tech
# only scenario Policy, for ces DAC
DAC_WW <- read_excel("D:\\emission constraint 2\\data repo\\GCAM-model\\output\\gcam_diagnostics\\gcam_data\\EWL\\scenario_specific\\WWbyTech\\Updated-Incr.xls"
                     , sheet='Sheet1', skip=1)
DAC_WW <- DAC_WW%>%filter(sector=="ces")%>%
  mutate(scenario="Policy")%>%
  gather(key="year",value = "WW",years)%>%
  select(region,scenario,sector,subsector,technology,year,WW)
DAC_WW$WW <- sapply(DAC_WW$WW, as.numeric)
write.csv(DAC_WW, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\DAC_WW.csv", row.names = FALSE)
