library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 0 Common Requirements

# path to the directory of all EWL query excels
setwd("D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new")
# reset scenario names
scenarios = list("Reference","Reference_CI_GFDL","Reference_CI_HadGEM","Reference_CI_IPSL",
                 "Reference_CI_MIROC","Reference_CI_NorESM","Policy","Policy_CI_GFDL",
                 "Policy_CI_HadGEM","Policy_CI_IPSL","Policy_CI_MIROC","Policy_CI_NorESM",
                 "Policy_NoDAC","Policy_NoDAC_CI_GFDL","Policy_NoDAC_CI_HadGEM",
                 "Policy_NoDAC_CI_IPSL","Policy_NoDAC_CI_MIROC","Policy_NoDAC_CI_NorESM")
scenarios_Reference <- list("Reference","Reference_CI_GFDL","Reference_CI_HadGEM",
                            "Reference_CI_IPSL","Reference_CI_MIROC","Reference_CI_NorESM")
scenarios_Policy <- list("Policy","Policy_CI_GFDL","Policy_CI_HadGEM",
                         "Policy_CI_IPSL","Policy_CI_MIROC","Policy_CI_NorESM")
scenarios_Policy_NoDAC <- list("Policy_NoDAC","Policy_NoDAC_CI_GFDL","Policy_NoDAC_CI_HadGEM",
                               "Policy_NoDAC_CI_IPSL","Policy_NoDAC_CI_MIROC","Policy_NoDAC_CI_NorESM")
scenarios_GFDL <- list("Reference_CI_GFDL","Policy_CI_GFDL","Policy_NoDAC_CI_GFDL")
scenarios_HadGEM <- list("Reference_CI_HadGEM","Policy_CI_HadGEM","Policy_NoDAC_CI_HadGEM")
scenarios_IPSL <- list("Reference_CI_IPSL","Policy_CI_IPSL","Policy_NoDAC_CI_IPSL")
scenarios_MIROC <- list("Reference_CI_MIROC","Policy_CI_MIROC","Policy_NoDAC_CI_MIROC")
scenarios_NorESM <- list("Reference_CI_NorESM","Policy_CI_NorESM","Policy_NoDAC_CI_NorESM")
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
# years of modeling
years = c("1990","2005","2010","2015","2020","2025","2030","2035","2040","2045","2050",
          "2055","2060","2065","2070","2075","2080","2085","2090","2095","2100")
# Labels
Labels_EC <- c("基准","碳中和(全技术)","碳中和(无DAC)")
Labels_CI <- c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM")
# CI
# 1 CI depiction

# 1.1 hydro
# read in the summarized ElecGen.csv
Elec <- read.csv("./ElecGen.csv",header = TRUE,check.names = FALSE)
# plot the Reference Elec_Generation as stacking bar plot 
# add EC and CI components
Elec <- Elec%>%
  mutate(EC=scenario,CI=scenario)
Elec$EC[which(Elec$EC%in%scenarios_Reference)] <- "基准"
Elec$EC[which(Elec$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
Elec$EC[which(Elec$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
Elec$CI[which(Elec$CI%in%scenarios_GFDL)] <- "GFDL"
Elec$CI[which(Elec$CI%in%scenarios_HadGEM)] <- "HadGEM"
Elec$CI[which(Elec$CI%in%scenarios_IPSL)] <- "IPSL"
Elec$CI[which(Elec$CI%in%scenarios_MIROC)] <- "MIROC"
Elec$CI[which(Elec$CI%in%scenarios_NorESM)] <- "NorESM"
Elec$CI[which(Elec$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
Elec_mean <- Elec%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,fuel,year)%>%
  summarise(Elec_Generation=mean(Elec_Generation))%>%
  mutate(CI="GCM平均",scenario="CI")
Elec_mean$scenario[which(Elec_mean$EC=="基准")] <- "Reference_CI"
Elec_mean$scenario[which(Elec_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
Elec_mean$scenario[which(Elec_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
Elec_mean <- Elec_mean%>%
  select(region,scenario,fuel,year,Elec_Generation,EC,CI)

Elec <- rbind(Elec,Elec_mean)
# reorder variables
Elec$CI <- factor(Elec$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
Elec$EC <- factor(Elec$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
#save file
write.csv(Elec, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\Elec_inclMean.csv", row.names = FALSE)

# China
ggplot(data = Elec%>%
         filter(region=="China",
                fuel=="hydro"), 
       aes(x = year, y=Elec_Generation, fill = factor(fuel)))+
  geom_bar(stat="identity")+
  facet_grid(EC~CI)+
  scale_y_continuous("发电量/EJ")+
  scale_x_continuous("年份")+
  labs(fill="能源种类")+
  scale_fill_brewer(labels=c("水电"),palette = "Dark2")+
  ggtitle("中国不同情景水电发电量")+
  theme(plot.title = element_text(size=50,hjust=0.5))+
  theme(axis.title = element_text(size=40))+
  theme(axis.text = element_text(size=20))+
  theme(legend.title = element_text(size=40))+
  theme(legend.text = element_text(size=40))+
  theme(strip.text = element_text(size=40))






# 1.2 China total runoff
runoff <- read.csv("./支线/basin_runoff.csv",header = TRUE,check.names = FALSE)
# plot stacking bar plot 
# add EC and CI components
runoff <- runoff%>%
  mutate(EC=scenario,CI=scenario)
runoff$EC[which(runoff$EC%in%scenarios_Reference)] <- "基准"
runoff$EC[which(runoff$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
runoff$EC[which(runoff$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
runoff$CI[which(runoff$CI%in%scenarios_GFDL)] <- "GFDL"
runoff$CI[which(runoff$CI%in%scenarios_HadGEM)] <- "HadGEM"
runoff$CI[which(runoff$CI%in%scenarios_IPSL)] <- "IPSL"
runoff$CI[which(runoff$CI%in%scenarios_MIROC)] <- "MIROC"
runoff$CI[which(runoff$CI%in%scenarios_NorESM)] <- "NorESM"
runoff$CI[which(runoff$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
runoff_mean <- runoff%>%
  filter(CI!="无CI")%>%
  group_by(region,Basin,EC,subresource,year)%>%
  summarise(basin_runoff=mean(basin_runoff))%>%
  mutate(CI="GCM平均",scenario="CI")
runoff_mean$scenario[which(runoff_mean$EC=="基准")] <- "Reference_CI"
runoff_mean$scenario[which(runoff_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
runoff_mean$scenario[which(runoff_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
runoff_mean <- runoff_mean%>%
  select(region,scenario,subresource,Basin,year,basin_runoff,EC,CI)

runoff <- rbind(runoff,runoff_mean)
# reorder variables
runoff$CI <- factor(runoff$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
runoff$EC <- factor(runoff$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
#save file
write.csv(runoff, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\basin_runoff_inclMean.csv", row.names = FALSE)

# China
runoff_sum <- runoff%>%
  group_by(region,EC,CI,scenario,subresource,year)%>%
  summarise(China_runoff=sum(basin_runoff))
ggplot(data = runoff_sum%>%
         filter(region=="China"), 
       aes(x = year, y=China_runoff, fill = factor(subresource)))+
  geom_bar(stat="identity")+
  facet_grid(EC~CI)+
  scale_y_continuous("径流量/km3")+
  scale_x_continuous("年份")+
  labs(fill="CI类型")+
  scale_fill_brewer(labels=c("地表径流"),palette = "Set2")+
  ggtitle("中国不同情景地表径流量")+
  theme(plot.title = element_text(size=50,hjust=0.5))+
  theme(axis.title = element_text(size=40))+
  theme(axis.text = element_text(size=20))+
  theme(legend.title = element_text(size=40))+
  theme(legend.text = element_text(size=40))+
  theme(strip.text = element_text(size=40))





# 2 Water System
# 2.1 WW
# 2.1.1 total WW
WW <- read.csv("./WW.csv",header = TRUE,check.names = FALSE)
# plot stacking bar plot 
# add EC and CI components
WW <- WW%>%
  mutate(EC=scenario,CI=scenario)
WW$EC[which(WW$EC%in%scenarios_Reference)] <- "基准"
WW$EC[which(WW$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
WW$EC[which(WW$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
WW$CI[which(WW$CI%in%scenarios_GFDL)] <- "GFDL"
WW$CI[which(WW$CI%in%scenarios_HadGEM)] <- "HadGEM"
WW$CI[which(WW$CI%in%scenarios_IPSL)] <- "IPSL"
WW$CI[which(WW$CI%in%scenarios_MIROC)] <- "MIROC"
WW$CI[which(WW$CI%in%scenarios_NorESM)] <- "NorESM"
WW$CI[which(WW$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
WW_mean <- WW%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,sector,year)%>%
  summarise(Water_Withdrawal=mean(Water_Withdrawal))%>%
  mutate(CI="GCM平均",scenario="CI")
WW_mean$scenario[which(WW_mean$EC=="基准")] <- "Reference_CI"
WW_mean$scenario[which(WW_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
WW_mean$scenario[which(WW_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
WW_mean <- WW_mean%>%
  select(region,scenario,sector,year,Water_Withdrawal,EC,CI)

WW <- rbind(WW,WW_mean)
# reorder variables
WW$CI <- factor(WW$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
WW$EC <- factor(WW$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
#save file
write.csv(WW, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\WW_inclMean.csv", row.names = FALSE)

# total WW China
WW_sum <- WW%>%
  group_by(region,EC,CI,scenario,year)%>%
  summarise(total_WW=sum(Water_Withdrawal),.groups = "drop")
ggplot(data = WW_sum%>%
         filter(region=="China",
                (CI=="无CI")|(CI=="GCM平均")), 
       aes(x = year, y=total_WW, color = factor(CI)))+
  geom_line(size=2)+
  facet_grid(~EC)+
  scale_y_continuous("取水量/km3")+
  scale_x_continuous("年份")+
  labs(color="有无CI")+
  scale_fill_brewer(palette = "Set2")+
  ggtitle("中国不同情景总取水")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# total WW differences
# prepare dif dataframe
WW_sum_dif <- WW_sum%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "total_WW")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,year,Difference,Rel_Dif)
WW_sum_dif <- WW_sum_dif%>%rbind(WW_sum%>%filter(EC=="碳中和(全技术)")%>%
                                   filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                   select(-scenario)%>%
                                   spread(key = "CI",value = "total_WW")%>%
                                   mutate(Difference=GCM平均-无CI,
                                          Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                   select(region,EC,year,Difference,Rel_Dif))
WW_sum_dif <- WW_sum_dif%>%rbind(WW_sum%>%filter(EC=="碳中和(无DAC)")%>%
                                   filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                   select(-scenario)%>%
                                   spread(key = "CI",value = "total_WW")%>%
                                   mutate(Difference=GCM平均-无CI,
                                          Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                   select(region,EC,year,Difference,Rel_Dif))
WW_sum_dif$EC <- factor(WW_sum_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot China
ggplot(data = WW_sum_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Rel_Dif, color = factor(region)))+
  geom_line(size=2)+
  facet_grid(~EC)+
  scale_y_continuous("相对差异/%")+
  scale_x_continuous("年份")+
  labs(color="")+
  scale_color_brewer(labels=c("总取水"),palette="Set2")+
  ggtitle("中国不同政策情景下CI造成的总取水量差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))




# 2.1.2 sectoral WW China
ggplot(data = WW%>%
         filter(region=="China",sector!="ces",
                (CI=="无CI")|(CI=="GCM平均")), 
       aes(x = year, y=Water_Withdrawal, fill = factor(sector)))+
  geom_bar(stat = "identity")+
  facet_grid(EC~CI)+
  scale_y_continuous("取水量/km3")+
  scale_x_continuous("年份")+
  labs(fill="部门")+
  scale_fill_discrete(labels=c("农业","电力","工业","畜牧业","市政","一次能源开发"))+
  ggtitle("中国不同情景各部门取水")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# sectoral WW differences
# prepare dif dataframe
WW_dif <- WW%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "Water_Withdrawal")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,sector,year,Difference,Rel_Dif)
WW_dif <- WW_dif%>%rbind(WW%>%filter(EC=="碳中和(全技术)")%>%
                                   filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                   select(-scenario)%>%
                                   spread(key = "CI",value = "Water_Withdrawal")%>%
                                   mutate(Difference=GCM平均-无CI,
                                          Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                   select(region,EC,sector,year,Difference,Rel_Dif))
WW_dif <- WW_dif%>%rbind(WW%>%filter(EC=="碳中和(无DAC)")%>%
                                   filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                   select(-scenario)%>%
                                   spread(key = "CI",value = "Water_Withdrawal")%>%
                                   mutate(Difference=GCM平均-无CI,
                                          Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                   select(region,EC,sector,year,Difference,Rel_Dif))
WW_dif$EC <- factor(WW_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot China
ggplot(data = WW_dif%>%
         filter(region=="China",sector!="ces"), 
       aes(x = year, y=Difference, fill = factor(sector)))+
  geom_bar(stat = "identity")+
  facet_grid(~EC)+
  scale_y_continuous("取水量差异/km3")+
  scale_x_continuous("年份")+
  labs(fill="部门")+
  scale_fill_discrete(labels=c("农业","电力","工业","畜牧业","市政","一次能源开发"))+
  ggtitle("中国不同政策情景下CI造成的分部门取水差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))





# 2.2 Elec WW
elec_WW <- read.csv("./intermediate/elec_WW_by_tech.csv",header = TRUE,check.names = FALSE)
# add EC and CI components
elec_WW <- elec_WW%>%
  mutate(EC=scenario,CI=scenario)
elec_WW$EC[which(elec_WW$EC%in%scenarios_Reference)] <- "基准"
elec_WW$EC[which(elec_WW$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
elec_WW$EC[which(elec_WW$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
elec_WW$CI[which(elec_WW$CI%in%scenarios_GFDL)] <- "GFDL"
elec_WW$CI[which(elec_WW$CI%in%scenarios_HadGEM)] <- "HadGEM"
elec_WW$CI[which(elec_WW$CI%in%scenarios_IPSL)] <- "IPSL"
elec_WW$CI[which(elec_WW$CI%in%scenarios_MIROC)] <- "MIROC"
elec_WW$CI[which(elec_WW$CI%in%scenarios_NorESM)] <- "NorESM"
elec_WW$CI[which(elec_WW$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
elec_WW_mean <- elec_WW%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,sector,subsector,technology,year)%>%
  summarise(WW=mean(WW),.groups = "drop")%>%
  mutate(CI="GCM平均",scenario="CI")
elec_WW_mean$scenario[which(elec_WW_mean$EC=="基准")] <- "Reference_CI"
elec_WW_mean$scenario[which(elec_WW_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
elec_WW_mean$scenario[which(elec_WW_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
elec_WW_mean <- elec_WW_mean%>%
  select(region,scenario,sector,subsector,technology,year,WW,EC,CI)

elec_WW <- rbind(elec_WW,elec_WW_mean)
# reorder variables
elec_WW$CI <- factor(elec_WW$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
elec_WW$EC <- factor(elec_WW$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
#save file
write.csv(elec_WW, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\elec_WW_inclMean.csv", row.names = FALSE)

# aggregate by subsector (fuel)
elec_WW_fuel <- elec_WW%>%
  group_by(region, EC, CI, subsector, year)%>%
  summarise(WW=sum(WW),.groups = "drop")
# plot ele subsector WW
ggplot(data = elec_WW_fuel%>%
         filter(region=="China",
                (CI=="无CI")|(CI=="GCM平均")), 
       aes(x = year, y=WW, fill = factor(subsector)))+
  geom_bar(stat = "identity")+
  facet_grid(CI~EC)+
  scale_y_continuous("取水量/km3")+
  scale_x_continuous("年份")+
  labs(fill="一次能源类型")+
  scale_fill_discrete(labels=c("生物质","煤","天然气","地热能","核能","石油","光"))+
  ggtitle("中国不同情景电力取水结构对比")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# elec_WW_fuel differences
# prepare dif dataframe
elec_WW_fuel_dif <- elec_WW_fuel%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  spread(key = "CI",value = "WW")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,subsector,year,Difference,Rel_Dif)
elec_WW_fuel_dif <- elec_WW_fuel_dif%>%rbind(elec_WW_fuel%>%filter(EC=="碳中和(全技术)")%>%
                           filter((CI=="无CI")|(CI=="GCM平均"))%>%
                           spread(key = "CI",value = "WW")%>%
                           mutate(Difference=GCM平均-无CI,
                                  Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                           select(region,EC,subsector,year,Difference,Rel_Dif))
elec_WW_fuel_dif <- elec_WW_fuel_dif%>%rbind(elec_WW_fuel%>%filter(EC=="碳中和(无DAC)")%>%
                           filter((CI=="无CI")|(CI=="GCM平均"))%>%
                           spread(key = "CI",value = "WW")%>%
                           mutate(Difference=GCM平均-无CI,
                                  Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                           select(region,EC,subsector,year,Difference,Rel_Dif))
elec_WW_fuel_dif$EC <- factor(elec_WW_fuel_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot China
ggplot(data = elec_WW_fuel_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(subsector)))+
  geom_bar(stat = "identity")+
  facet_grid(EC~.)+
  scale_y_continuous("取水量差异/km3")+
  scale_x_continuous("年份")+
  labs(fill="一次能源类型")+
  scale_fill_discrete(labels=c("生物质","煤","天然气","地热能","核能","石油","光"))+
  ggtitle("中国不同政策情景下CI造成的电力取水结构差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
#save file
write.csv(elec_WW_fuel, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\elec_WW_fuel.csv", row.names = FALSE)
write.csv(elec_WW_fuel_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\elec_WW_fuel_dif.csv", row.names = FALSE)





# 2.3 ag crops WW
WW_detail <- read.csv("./WW_detail.csv",header = TRUE,check.names = FALSE)
# get crop WW
Ag_crops <- list("Corn","FiberCrop","MiscCrop","OilCrop","OtherGrain","Rice",
                 "RootTuber","SugarCrop","Wheat")
Ag_WW <- WW_detail%>%
  filter(sector%in%Ag_crops)
# add EC and CI components
Ag_WW <- Ag_WW%>%
  mutate(EC=scenario,CI=scenario)
Ag_WW$EC[which(Ag_WW$EC%in%scenarios_Reference)] <- "基准"
Ag_WW$EC[which(Ag_WW$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
Ag_WW$EC[which(Ag_WW$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
Ag_WW$CI[which(Ag_WW$CI%in%scenarios_GFDL)] <- "GFDL"
Ag_WW$CI[which(Ag_WW$CI%in%scenarios_HadGEM)] <- "HadGEM"
Ag_WW$CI[which(Ag_WW$CI%in%scenarios_IPSL)] <- "IPSL"
Ag_WW$CI[which(Ag_WW$CI%in%scenarios_MIROC)] <- "MIROC"
Ag_WW$CI[which(Ag_WW$CI%in%scenarios_NorESM)] <- "NorESM"
Ag_WW$CI[which(Ag_WW$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
Ag_WW_mean <- Ag_WW%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,sector,year)%>%
  summarise(Water_Withdrawal=mean(Water_Withdrawal),.groups = "drop")%>%
  mutate(CI="GCM平均",scenario="CI")
Ag_WW_mean$scenario[which(Ag_WW_mean$EC=="基准")] <- "Reference_CI"
Ag_WW_mean$scenario[which(Ag_WW_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
Ag_WW_mean$scenario[which(Ag_WW_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
Ag_WW_mean <- Ag_WW_mean%>%
  select(region,scenario,sector,year,Water_Withdrawal,EC,CI)

Ag_WW <- rbind(Ag_WW,Ag_WW_mean)
# reorder variables
Ag_WW$CI <- factor(Ag_WW$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
Ag_WW$EC <- factor(Ag_WW$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
#save file
write.csv(Ag_WW, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\Ag_WW_inclMean.csv", row.names = FALSE)

# plot scens Ag WW
Ag_WW$sector <- factor(Ag_WW$sector, levels = c("Corn","Rice","Wheat","MiscCrop",
                                                "OtherGrain","FiberCrop","SugarCrop",
                                                "OilCrop","RootTuber"))
ggplot(data = Ag_WW%>%
         filter(region=="China",
                (CI=="无CI")|(CI=="GCM平均")), 
       aes(x = year, y=Water_Withdrawal, fill = factor(sector)))+
  geom_bar(stat = "identity")+
  facet_grid(EC~CI)+
  scale_y_continuous("取水量/km3")+
  scale_x_continuous("年份")+
  labs(fill="农作物种类")+
  scale_fill_discrete(labels=c("玉米","大米","小麦","杂粮作物",
                               "其他谷物","纤维作物","糖类作物",
                               "油类作物","块根作物"))+
  ggtitle("中国不同情景农业取水结构对比")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))


# Ag_WW differences
# prepare dif dataframe
Ag_WW_dif <- Ag_WW%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "Water_Withdrawal")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,sector,year,Difference,Rel_Dif)
Ag_WW_dif <- Ag_WW_dif%>%rbind(Ag_WW%>%filter(EC=="碳中和(全技术)")%>%
                                       filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                       select(-scenario)%>%
                                       spread(key = "CI",value = "Water_Withdrawal")%>%
                                       mutate(Difference=GCM平均-无CI,
                                              Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                       select(region,EC,sector,year,Difference,Rel_Dif))
Ag_WW_dif <- Ag_WW_dif%>%rbind(Ag_WW%>%filter(EC=="碳中和(无DAC)")%>%
                                       filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                       select(-scenario)%>%
                                       spread(key = "CI",value = "Water_Withdrawal")%>%
                                       mutate(Difference=GCM平均-无CI,
                                              Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                       select(region,EC,sector,year,Difference,Rel_Dif))
Ag_WW_dif$EC <- factor(Ag_WW_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
Ag_WW_dif$sector <- factor(Ag_WW_dif$sector, levels = c("Corn","Rice","Wheat","MiscCrop",
                                                "OtherGrain","FiberCrop","SugarCrop",
                                                "OilCrop","RootTuber"))
# plot China
ggplot(data = Ag_WW_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(sector)))+
  geom_bar(stat = "identity")+
  facet_grid(~EC)+
  scale_y_continuous("取水量差异/km3")+
  scale_x_continuous("年份")+
  labs(fill="农作物种类")+
  scale_fill_discrete(labels=c("玉米","大米","小麦","杂粮作物",
                               "其他谷物","纤维作物","糖类作物",
                               "油类作物","块根作物"))+
  ggtitle("中国不同政策情景下CI造成的农业取水结构差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
#save file
write.csv(Ag_WW_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\Ag_WW_dif.csv", row.names = FALSE)





# 3 Ag crops
# 3.1 ag production
ag <- read.csv("./Ag.csv",header = TRUE,check.names = FALSE)
# get crop prod
Ag_crops <- list("Corn","FiberCrop","MiscCrop","OilCrop","OtherGrain","Rice",
                 "RootTuber","SugarCrop","Wheat")
crop_prod <- ag%>%
  filter(crop%in%Ag_crops)
# add EC and CI components
crop_prod <- crop_prod%>%
  mutate(EC=scenario,CI=scenario)
crop_prod$EC[which(crop_prod$EC%in%scenarios_Reference)] <- "基准"
crop_prod$EC[which(crop_prod$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
crop_prod$EC[which(crop_prod$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
crop_prod$CI[which(crop_prod$CI%in%scenarios_GFDL)] <- "GFDL"
crop_prod$CI[which(crop_prod$CI%in%scenarios_HadGEM)] <- "HadGEM"
crop_prod$CI[which(crop_prod$CI%in%scenarios_IPSL)] <- "IPSL"
crop_prod$CI[which(crop_prod$CI%in%scenarios_MIROC)] <- "MIROC"
crop_prod$CI[which(crop_prod$CI%in%scenarios_NorESM)] <- "NorESM"
crop_prod$CI[which(crop_prod$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
crop_prod_mean <- crop_prod%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,crop,year)%>%
  summarise(Ag_prod=mean(Ag_prod),.groups = "drop")%>%
  mutate(CI="GCM平均",scenario="CI")
crop_prod_mean$scenario[which(crop_prod_mean$EC=="基准")] <- "Reference_CI"
crop_prod_mean$scenario[which(crop_prod_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
crop_prod_mean$scenario[which(crop_prod_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
crop_prod_mean <- crop_prod_mean%>%
  select(region,scenario,crop,year,Ag_prod,EC,CI)

crop_prod <- rbind(crop_prod,crop_prod_mean)
# reorder variables
crop_prod$CI <- factor(crop_prod$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
crop_prod$EC <- factor(crop_prod$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
#save file
write.csv(crop_prod, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crop_prod_inclMean.csv", row.names = FALSE)

# plot scens Ag prod
crop_prod$crop <- factor(crop_prod$crop, levels = c("Corn","Rice","Wheat","MiscCrop",
                                                "OtherGrain","FiberCrop","SugarCrop",
                                                "OilCrop","RootTuber"))
ggplot(data = crop_prod%>%
         filter(region=="China",
                (CI=="无CI")|(CI=="GCM平均")), 
       aes(x = year, y=Ag_prod, fill = factor(crop)))+
  geom_bar(stat = "identity")+
  facet_grid(EC~CI)+
  scale_y_continuous("产量/Mt")+
  scale_x_continuous("年份")+
  labs(fill="农作物种类")+
  scale_fill_discrete(labels=c("玉米","大米","小麦","杂粮作物",
                               "其他谷物","纤维作物","糖类作物",
                               "油类作物","块根作物"))+
  ggtitle("中国不同情景农作物产量对比")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# crop_prod differences
# prepare dif dataframe
crop_prod_dif <- crop_prod%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "Ag_prod")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,crop,year,Difference,Rel_Dif)
crop_prod_dif <- crop_prod_dif%>%rbind(crop_prod%>%filter(EC=="碳中和(全技术)")%>%
                                 filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                 select(-scenario)%>%
                                 spread(key = "CI",value = "Ag_prod")%>%
                                 mutate(Difference=GCM平均-无CI,
                                        Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                 select(region,EC,crop,year,Difference,Rel_Dif))
crop_prod_dif <- crop_prod_dif%>%rbind(crop_prod%>%filter(EC=="碳中和(无DAC)")%>%
                                 filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                 select(-scenario)%>%
                                 spread(key = "CI",value = "Ag_prod")%>%
                                 mutate(Difference=GCM平均-无CI,
                                        Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                 select(region,EC,crop,year,Difference,Rel_Dif))
crop_prod_dif$EC <- factor(crop_prod_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
crop_prod_dif$crop <- factor(crop_prod_dif$crop, levels = c("Corn","Rice","Wheat","MiscCrop",
                                                        "OtherGrain","FiberCrop","SugarCrop",
                                                        "OilCrop","RootTuber"))
# plot China
ggplot(data = crop_prod_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(crop)))+
  geom_bar(stat = "identity")+
  facet_grid(~EC)+
  scale_y_continuous("产量差异/Mt")+
  scale_x_continuous("年份")+
  labs(fill="农作物种类")+
  scale_fill_discrete(labels=c("玉米","大米","小麦","杂粮作物",
                               "其他谷物","纤维作物","糖类作物",
                               "油类作物","块根作物"))+
  ggtitle("中国不同政策情景下CI造成的农作物产量差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
#save file
write.csv(crop_prod_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crop_prod_dif.csv", row.names = FALSE)





# 3.2 crop land area
crop_LU <- read.csv("./LU.csv",header = TRUE,check.names = FALSE)
crop_LU <- crop_LU%>%filter(landuse=="crops")
# add EC and CI components
crop_LU <- crop_LU%>%
  mutate(EC=scenario,CI=scenario)
crop_LU$EC[which(crop_LU$EC%in%scenarios_Reference)] <- "基准"
crop_LU$EC[which(crop_LU$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
crop_LU$EC[which(crop_LU$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
crop_LU$CI[which(crop_LU$CI%in%scenarios_GFDL)] <- "GFDL"
crop_LU$CI[which(crop_LU$CI%in%scenarios_HadGEM)] <- "HadGEM"
crop_LU$CI[which(crop_LU$CI%in%scenarios_IPSL)] <- "IPSL"
crop_LU$CI[which(crop_LU$CI%in%scenarios_MIROC)] <- "MIROC"
crop_LU$CI[which(crop_LU$CI%in%scenarios_NorESM)] <- "NorESM"
crop_LU$CI[which(crop_LU$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
crop_LU_mean <- crop_LU%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,landuse,year)%>%
  summarise(LU_frac=mean(LU_frac),.groups = "drop")%>%
  mutate(CI="GCM平均",scenario="CI")
crop_LU_mean$scenario[which(crop_LU_mean$EC=="基准")] <- "Reference_CI"
crop_LU_mean$scenario[which(crop_LU_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
crop_LU_mean$scenario[which(crop_LU_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
crop_LU_mean <- crop_LU_mean%>%
  select(region,scenario,landuse,year,LU_frac,EC,CI)

crop_LU <- rbind(crop_LU,crop_LU_mean)
# reorder variables
crop_LU$CI <- factor(crop_LU$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
crop_LU$EC <- factor(crop_LU$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot scens crop LU
ggplot(data = crop_LU%>%
         filter(region=="China",
                (CI=="无CI")|(CI=="GCM平均")), 
       aes(x = year, y=LU_frac, fill = factor(landuse)))+
  geom_bar(stat = "identity")+
  facet_grid(EC~CI)+
  scale_y_continuous("面积/千km2")+
  scale_x_continuous("年份")+
  labs(fill="用地类型")+
  scale_fill_discrete(labels=c("农作物"))+
  ggtitle("中国不同情景农作物面积对比")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# crop_LU differences
# prepare dif dataframe
crop_LU_dif <- crop_LU%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "LU_frac")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,landuse,year,Difference,Rel_Dif)
crop_LU_dif <- crop_LU_dif%>%rbind(crop_LU%>%filter(EC=="碳中和(全技术)")%>%
                                         filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                         select(-scenario)%>%
                                         spread(key = "CI",value = "LU_frac")%>%
                                         mutate(Difference=GCM平均-无CI,
                                                Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                         select(region,EC,landuse,year,Difference,Rel_Dif))
crop_LU_dif <- crop_LU_dif%>%rbind(crop_LU%>%filter(EC=="碳中和(无DAC)")%>%
                                         filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                         select(-scenario)%>%
                                         spread(key = "CI",value = "LU_frac")%>%
                                         mutate(Difference=GCM平均-无CI,
                                                Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                         select(region,EC,landuse,year,Difference,Rel_Dif))
crop_LU_dif$EC <- factor(crop_LU_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot China
ggplot(data = crop_LU_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Rel_Dif, fill = factor(landuse)))+
  geom_bar(stat = "identity")+
  facet_grid(~EC)+
  scale_y_continuous("相对差异/%")+
  scale_x_continuous("年份")+
  labs(fill="用地类型")+
  scale_fill_discrete(labels=c("农作物"))+
  ggtitle("中国不同政策情景下CI造成的农作物种植面积相对差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))






# importanr AgRegion
OilCrop_ZiyaHe <- list("OilCrop_ZiyaHe_IRR_hi","OilCrop_ZiyaHe_IRR_lo",
                        "OilCrop_ZiyaHe_RFD_hi","OilCrop_ZiyaHe_RFD_lo")
Rice_ZiyaHe <- list("Rice_ZiyaHe_IRR_hi","Rice_ZiyaHe_IRR_lo",
                     "Rice_ZiyaHe_RFD_hi","Rice_ZiyaHe_RFD_lo")
Wheat_ZiyaHe <- list("Wheat_ZiyaHe_IRR_hi","Wheat_ZiyaHe_IRR_lo",
                      "Wheat_ZiyaHe_RFD_hi","Wheat_ZiyaHe_RFD_lo")
Corn_ZiyaHe <- list("Corn_ZiyaHe_IRR_hi","Corn_ZiyaHe_IRR_lo",
                     "Corn_ZiyaHe_RFD_hi","Corn_ZiyaHe_RFD_lo")
Misc_ZiyaHe <- list("Misc_ZiyaHe_IRR_hi","Misc_ZiyaHe_IRR_lo",
                     "Misc_ZiyaHe_RFD_hi","Misc_ZiyaHe_RFD_lo")
ZiyaHe_crops <- union(OilCrop_ZiyaHe,Rice_ZiyaHe)%>%
  union(Wheat_ZiyaHe)%>%
  union(Corn_ZiyaHe)%>%
  union(Misc_ZiyaHe)



# 3.3 certain AgRegion detailed land allocation
Ag_LA <- read.csv("./支线/detailed_LA.csv",header = TRUE,check.names = FALSE)
# filter data of certain region and mark crops
Ag_LA_reg <- Ag_LA%>%filter(LandLeaf%in%ZiyaHe_crops)%>%
  mutate(crop="crop")
Ag_LA_reg$crop[which(Ag_LA_reg$LandLeaf%in%OilCrop_ZiyaHe)] <- "OilCrop"
Ag_LA_reg$crop[which(Ag_LA_reg$LandLeaf%in%Rice_ZiyaHe)] <- "Rice"
Ag_LA_reg$crop[which(Ag_LA_reg$LandLeaf%in%Wheat_ZiyaHe)] <- "Wheat"
Ag_LA_reg$crop[which(Ag_LA_reg$LandLeaf%in%Corn_ZiyaHe)] <- "Corn"
Ag_LA_reg$crop[which(Ag_LA_reg$LandLeaf%in%Misc_ZiyaHe)] <- "MiscCrop"
# add EC and CI components
Ag_LA_reg <- Ag_LA_reg%>%
  mutate(EC=scenario,CI=scenario)
Ag_LA_reg$EC[which(Ag_LA_reg$EC%in%scenarios_Reference)] <- "基准"
Ag_LA_reg$EC[which(Ag_LA_reg$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
Ag_LA_reg$EC[which(Ag_LA_reg$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
Ag_LA_reg$CI[which(Ag_LA_reg$CI%in%scenarios_GFDL)] <- "GFDL"
Ag_LA_reg$CI[which(Ag_LA_reg$CI%in%scenarios_HadGEM)] <- "HadGEM"
Ag_LA_reg$CI[which(Ag_LA_reg$CI%in%scenarios_IPSL)] <- "IPSL"
Ag_LA_reg$CI[which(Ag_LA_reg$CI%in%scenarios_MIROC)] <- "MIROC"
Ag_LA_reg$CI[which(Ag_LA_reg$CI%in%scenarios_NorESM)] <- "NorESM"
Ag_LA_reg$CI[which(Ag_LA_reg$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
Ag_LA_reg_mean <- Ag_LA_reg%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,crop,LandLeaf,year)%>%
  summarise(Area=mean(Area),.groups = "drop")%>%
  mutate(CI="GCM平均",scenario="CI")
Ag_LA_reg_mean$scenario[which(Ag_LA_reg_mean$EC=="基准")] <- "Reference_CI"
Ag_LA_reg_mean$scenario[which(Ag_LA_reg_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
Ag_LA_reg_mean$scenario[which(Ag_LA_reg_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
Ag_LA_reg_mean <- Ag_LA_reg_mean%>%
  select(region,scenario,LandLeaf,year,Area,crop,EC,CI)

Ag_LA_reg <- rbind(Ag_LA_reg,Ag_LA_reg_mean)
# reorder variables
Ag_LA_reg$CI <- factor(Ag_LA_reg$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
Ag_LA_reg$EC <- factor(Ag_LA_reg$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot scens crop LU
ggplot(data = Ag_LA_reg%>%
         filter(region=="China",
                (CI=="无CI")|(CI=="GCM平均"),
                crop=="OilCrop"), 
       aes(x = year, y=Area, fill = factor(LandLeaf)))+
  geom_bar(stat = "identity")+
  facet_grid(EC~CI)+
  scale_y_continuous("面积/千km2")+
  scale_x_continuous("年份")+
  labs(fill="所用技术")+
  scale_fill_discrete()+
  ggtitle("子牙河流域不同情景油类作物种植面积对比")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# Ag_LA_reg differences
# prepare dif dataframe
Ag_LA_reg_dif <- Ag_LA_reg%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "Area")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,crop,LandLeaf,year,Difference,Rel_Dif)
Ag_LA_reg_dif <- Ag_LA_reg_dif%>%rbind(Ag_LA_reg%>%filter(EC=="碳中和(全技术)")%>%
                                     filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                     select(-scenario)%>%
                                     spread(key = "CI",value = "Area")%>%
                                     mutate(Difference=GCM平均-无CI,
                                            Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                     select(region,EC,crop,LandLeaf,year,Difference,Rel_Dif))
Ag_LA_reg_dif <- Ag_LA_reg_dif%>%rbind(Ag_LA_reg%>%filter(EC=="碳中和(无DAC)")%>%
                                     filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                     select(-scenario)%>%
                                     spread(key = "CI",value = "Area")%>%
                                     mutate(Difference=GCM平均-无CI,
                                            Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                     select(region,EC,crop,LandLeaf,year,Difference,Rel_Dif))
Ag_LA_reg_dif$EC <- factor(Ag_LA_reg_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot China
ggplot(data = Ag_LA_reg_dif%>%
         filter(region=="China",
                crop=="Wheat"), 
       aes(x = year, y=Difference, fill = factor(LandLeaf)))+
  geom_bar(stat = "identity")+
  facet_grid(~EC)+
  scale_y_continuous("面积差异/千km2")+
  scale_x_continuous("年份")+
  labs(fill="所用技术")+
  scale_fill_discrete()+
  ggtitle("子牙河流域不同政策情景下CI造成的小麦面积差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save file
write.csv(Ag_LA_reg, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\ZiyaHe_LA.csv", row.names = FALSE)
write.csv(Ag_LA_reg_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\ZiyaHe_LA_dif.csv", row.names = FALSE)





# 3.3 certain AgRegion detailed land allocation
Ag_yield <- read.csv("./支线/yield.csv",header = TRUE,check.names = FALSE)
# filter data of certain region and mark sectors
Ag_yield_reg <- Ag_yield%>%filter(technology%in%Yangtze_crops)
# add EC and CI components
Ag_yield_reg <- Ag_yield_reg%>%
  mutate(EC=scenario,CI=scenario)
Ag_yield_reg$EC[which(Ag_yield_reg$EC%in%scenarios_Reference)] <- "基准"
Ag_yield_reg$EC[which(Ag_yield_reg$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
Ag_yield_reg$EC[which(Ag_yield_reg$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
Ag_yield_reg$CI[which(Ag_yield_reg$CI%in%scenarios_GFDL)] <- "GFDL"
Ag_yield_reg$CI[which(Ag_yield_reg$CI%in%scenarios_HadGEM)] <- "HadGEM"
Ag_yield_reg$CI[which(Ag_yield_reg$CI%in%scenarios_IPSL)] <- "IPSL"
Ag_yield_reg$CI[which(Ag_yield_reg$CI%in%scenarios_MIROC)] <- "MIROC"
Ag_yield_reg$CI[which(Ag_yield_reg$CI%in%scenarios_NorESM)] <- "NorESM"
Ag_yield_reg$CI[which(Ag_yield_reg$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
Ag_yield_reg_mean <- Ag_yield_reg%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,sector,subsector,technology,year)%>%
  summarise(Yield=mean(Yield),.groups = "drop")%>%
  mutate(CI="GCM平均",scenario="CI")
Ag_yield_reg_mean$scenario[which(Ag_yield_reg_mean$EC=="基准")] <- "Reference_CI"
Ag_yield_reg_mean$scenario[which(Ag_yield_reg_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
Ag_yield_reg_mean$scenario[which(Ag_yield_reg_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
Ag_yield_reg_mean <- Ag_yield_reg_mean%>%
  select(region,scenario,subsector,technology,year,Yield,sector,EC,CI)

Ag_yield_reg <- rbind(Ag_yield_reg,Ag_yield_reg_mean)
# reorder variables
Ag_yield_reg$CI <- factor(Ag_yield_reg$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
Ag_yield_reg$EC <- factor(Ag_yield_reg$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))

# Ag_yield_reg differences
# prepare dif dataframe
Ag_yield_reg_dif <- Ag_yield_reg%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "Yield")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,sector,technology,year,Difference,Rel_Dif)
Ag_yield_reg_dif <- Ag_yield_reg_dif%>%rbind(Ag_yield_reg%>%filter(EC=="碳中和(全技术)")%>%
                                         filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                         select(-scenario)%>%
                                         spread(key = "CI",value = "Yield")%>%
                                         mutate(Difference=GCM平均-无CI,
                                                Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                         select(region,EC,sector,technology,year,Difference,Rel_Dif))
Ag_yield_reg_dif <- Ag_yield_reg_dif%>%rbind(Ag_yield_reg%>%filter(EC=="碳中和(无DAC)")%>%
                                         filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                         select(-scenario)%>%
                                         spread(key = "CI",value = "Yield")%>%
                                         mutate(Difference=GCM平均-无CI,
                                                Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                         select(region,EC,sector,technology,year,Difference,Rel_Dif))
Ag_yield_reg_dif$EC <- factor(Ag_yield_reg_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot China
ggplot(data = Ag_yield_reg_dif%>%
         filter(region=="China",
                sector=="Corn"), 
       aes(x = year, y=Difference, color = factor(technology)))+
  geom_line(size=2)+
  facet_grid(~EC)+
  scale_y_continuous("亩产差异 (Mt/千km2)")+
  scale_x_continuous("年份")+
  labs(color="所用技术")+
  scale_color_discrete()+
  ggtitle("长江流域不同政策情景下CI造成的玉米亩产差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save file
write.csv(Ag_yield_reg, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\Yangtze_yield.csv", row.names = FALSE)
write.csv(Ag_yield_reg_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\Yangtze_yield_dif.csv", row.names = FALSE)





# 3.4 Certain Crop Tech LA!
# 3.4.1 OilCrop
# list of 4 technologies
OilCrop_IRR_hi <- list("OilCrop_AmurR_IRR_hi","OilCrop_BoHai_IRR_hi","OilCrop_ChinaCst_IRR_hi",
                       "OilCrop_GangesR_IRR_hi","OilCrop_Gobi_IRR_hi","OilCrop_Hainan_IRR_hi",
                       "OilCrop_Hong_IRR_hi","OilCrop_HuangHeR_IRR_hi","OilCrop_IndusR_IRR_hi",
                       "OilCrop_IrrawaddyR_IRR_hi","OilCrop_LBalkash_IRR_hi","OilCrop_Mekong_IRR_hi",
                       "OilCrop_ObR_IRR_hi","OilCrop_RusCstSE_IRR_hi","OilCrop_SChinaSea_IRR_hi",
                       "OilCrop_Salween_IRR_hi","OilCrop_Tarim_IRR_hi","OilCrop_Xunjiang_IRR_hi",
                       "OilCrop_Yangtze_IRR_hi","OilCrop_ZiyaHe_IRR_hi")
OilCrop_IRR_lo <- list("OilCrop_AmurR_IRR_lo","OilCrop_BoHai_IRR_lo","OilCrop_ChinaCst_IRR_lo",
                       "OilCrop_GangesR_IRR_lo","OilCrop_Gobi_IRR_lo","OilCrop_Hainan_IRR_lo",
                       "OilCrop_Hong_IRR_lo","OilCrop_HuangHeR_IRR_lo","OilCrop_IndusR_IRR_lo",
                       "OilCrop_IrrawaddyR_IRR_lo","OilCrop_LBalkash_IRR_lo","OilCrop_Mekong_IRR_lo",
                       "OilCrop_ObR_IRR_lo","OilCrop_RusCstSE_IRR_lo","OilCrop_SChinaSea_IRR_lo",
                       "OilCrop_Salween_IRR_lo","OilCrop_Tarim_IRR_lo","OilCrop_Xunjiang_IRR_lo",
                       "OilCrop_Yangtze_IRR_lo","OilCrop_ZiyaHe_IRR_lo")
OilCrop_RFD_hi <- list("OilCrop_AmurR_RFD_hi","OilCrop_BoHai_RFD_hi","OilCrop_ChinaCst_RFD_hi",
                       "OilCrop_GangesR_RFD_hi","OilCrop_Gobi_RFD_hi","OilCrop_Hainan_RFD_hi",
                       "OilCrop_Hong_RFD_hi","OilCrop_HuangHeR_RFD_hi","OilCrop_IndusR_RFD_hi",
                       "OilCrop_IrrawaddyR_RFD_hi","OilCrop_LBalkash_RFD_hi","OilCrop_Mekong_RFD_hi",
                       "OilCrop_ObR_RFD_hi","OilCrop_RusCstSE_RFD_hi","OilCrop_SChinaSea_RFD_hi",
                       "OilCrop_Salween_RFD_hi","OilCrop_Tarim_RFD_hi","OilCrop_Xunjiang_RFD_hi",
                       "OilCrop_Yangtze_RFD_hi","OilCrop_ZiyaHe_RFD_hi")
OilCrop_RFD_lo <- list("OilCrop_AmurR_RFD_lo","OilCrop_BoHai_RFD_lo","OilCrop_ChinaCst_RFD_lo",
                       "OilCrop_GangesR_RFD_lo","OilCrop_Gobi_RFD_lo","OilCrop_Hainan_RFD_lo",
                       "OilCrop_Hong_RFD_lo","OilCrop_HuangHeR_RFD_lo","OilCrop_IndusR_RFD_lo",
                       "OilCrop_IrrawaddyR_RFD_lo","OilCrop_LBalkash_RFD_lo","OilCrop_Mekong_RFD_lo",
                       "OilCrop_ObR_RFD_lo","OilCrop_RusCstSE_RFD_lo","OilCrop_SChinaSea_RFD_lo",
                       "OilCrop_Salween_RFD_lo","OilCrop_Tarim_RFD_lo","OilCrop_Xunjiang_RFD_lo",
                       "OilCrop_Yangtze_RFD_lo","OilCrop_ZiyaHe_RFD_lo")
# filter OilCrop LA
OilCrop_All <- union(OilCrop_IRR_hi,OilCrop_IRR_lo)%>%
  union(OilCrop_RFD_hi)%>%
  union(OilCrop_RFD_lo)
LA_OilCrop <- Ag_LA%>%
  filter(LandLeaf%in%OilCrop_All)%>%
  mutate(technology="OilCrop")
# mark technologies and aggregate
LA_OilCrop$technology[which(LA_OilCrop$LandLeaf%in%OilCrop_IRR_hi)] <- "OilCrop_IRR_hi"
LA_OilCrop$technology[which(LA_OilCrop$LandLeaf%in%OilCrop_IRR_lo)] <- "OilCrop_IRR_lo"
LA_OilCrop$technology[which(LA_OilCrop$LandLeaf%in%OilCrop_RFD_hi)] <- "OilCrop_RFD_hi"
LA_OilCrop$technology[which(LA_OilCrop$LandLeaf%in%OilCrop_RFD_lo)] <- "OilCrop_RFD_lo"

LA_OilCrop <- LA_OilCrop%>%
  group_by(region,scenario,technology,year)%>%
  summarise(Area=sum(Area),.groups = "drop")
# add EC and CI components
LA_OilCrop <- LA_OilCrop%>%
  mutate(EC=scenario,CI=scenario)
LA_OilCrop$EC[which(LA_OilCrop$EC%in%scenarios_Reference)] <- "基准"
LA_OilCrop$EC[which(LA_OilCrop$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
LA_OilCrop$EC[which(LA_OilCrop$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
LA_OilCrop$CI[which(LA_OilCrop$CI%in%scenarios_GFDL)] <- "GFDL"
LA_OilCrop$CI[which(LA_OilCrop$CI%in%scenarios_HadGEM)] <- "HadGEM"
LA_OilCrop$CI[which(LA_OilCrop$CI%in%scenarios_IPSL)] <- "IPSL"
LA_OilCrop$CI[which(LA_OilCrop$CI%in%scenarios_MIROC)] <- "MIROC"
LA_OilCrop$CI[which(LA_OilCrop$CI%in%scenarios_NorESM)] <- "NorESM"
LA_OilCrop$CI[which(LA_OilCrop$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
LA_OilCrop_mean <- LA_OilCrop%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,technology,year)%>%
  summarise(Area=mean(Area),.groups = "drop")%>%
  mutate(CI="GCM平均",scenario="CI")
LA_OilCrop_mean$scenario[which(LA_OilCrop_mean$EC=="基准")] <- "Reference_CI"
LA_OilCrop_mean$scenario[which(LA_OilCrop_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
LA_OilCrop_mean$scenario[which(LA_OilCrop_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
LA_OilCrop_mean <- LA_OilCrop_mean%>%
  select(region,scenario,technology,year,Area,EC,CI)

LA_OilCrop <- rbind(LA_OilCrop,LA_OilCrop_mean)
# reorder variables
LA_OilCrop$CI <- factor(LA_OilCrop$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
LA_OilCrop$EC <- factor(LA_OilCrop$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot scens crop LU
# no need, can add post hoc

# LA_OilCrop differences
# prepare dif dataframe
LA_OilCrop_dif <- LA_OilCrop%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "Area")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,technology,year,Difference,Rel_Dif)
LA_OilCrop_dif <- LA_OilCrop_dif%>%rbind(LA_OilCrop%>%filter(EC=="碳中和(全技术)")%>%
                                         filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                         select(-scenario)%>%
                                         spread(key = "CI",value = "Area")%>%
                                         mutate(Difference=GCM平均-无CI,
                                                Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                         select(region,EC,technology,year,Difference,Rel_Dif))
LA_OilCrop_dif <- LA_OilCrop_dif%>%rbind(LA_OilCrop%>%filter(EC=="碳中和(无DAC)")%>%
                                         filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                         select(-scenario)%>%
                                         spread(key = "CI",value = "Area")%>%
                                         mutate(Difference=GCM平均-无CI,
                                                Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                         select(region,EC,technology,year,Difference,Rel_Dif))
LA_OilCrop_dif$EC <- factor(LA_OilCrop_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot China
ggplot(data = LA_OilCrop_dif%>%
         filter(region=="China",
                technology=="OilCrop_IRR_hi"), 
       aes(x = year, y=Rel_Dif, fill = factor(technology)))+
  geom_bar(stat = "identity")+
  facet_grid(~EC)+
  scale_y_continuous("相对差异/%")+
  scale_x_continuous("年份")+
  labs(fill="所用技术")+
  scale_fill_discrete()+
  ggtitle("中国不同政策情景下CI造成的高灌溉油类作物相对差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save file
write.csv(LA_OilCrop, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\LA_OilCrop.csv", row.names = FALSE)
write.csv(LA_OilCrop_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\LA_OilCrop_dif.csv", row.names = FALSE)



# 3.4.2 Wheat
# list of 4 technologies
Wheat_IRR_hi <- list("Wheat_AmurR_IRR_hi","Wheat_BoHai_IRR_hi","Wheat_ChinaCst_IRR_hi",
                       "Wheat_GangesR_IRR_hi","Wheat_Gobi_IRR_hi","Wheat_Hainan_IRR_hi",
                       "Wheat_Hong_IRR_hi","Wheat_HuangHeR_IRR_hi","Wheat_IndusR_IRR_hi",
                       "Wheat_IrrawaddyR_IRR_hi","Wheat_LBalkash_IRR_hi","Wheat_Mekong_IRR_hi",
                       "Wheat_ObR_IRR_hi","Wheat_RusCstSE_IRR_hi","Wheat_SChinaSea_IRR_hi",
                       "Wheat_Salween_IRR_hi","Wheat_Tarim_IRR_hi","Wheat_Xunjiang_IRR_hi",
                       "Wheat_Yangtze_IRR_hi","Wheat_ZiyaHe_IRR_hi")
Wheat_IRR_lo <- list("Wheat_AmurR_IRR_lo","Wheat_BoHai_IRR_lo","Wheat_ChinaCst_IRR_lo",
                       "Wheat_GangesR_IRR_lo","Wheat_Gobi_IRR_lo","Wheat_Hainan_IRR_lo",
                       "Wheat_Hong_IRR_lo","Wheat_HuangHeR_IRR_lo","Wheat_IndusR_IRR_lo",
                       "Wheat_IrrawaddyR_IRR_lo","Wheat_LBalkash_IRR_lo","Wheat_Mekong_IRR_lo",
                       "Wheat_ObR_IRR_lo","Wheat_RusCstSE_IRR_lo","Wheat_SChinaSea_IRR_lo",
                       "Wheat_Salween_IRR_lo","Wheat_Tarim_IRR_lo","Wheat_Xunjiang_IRR_lo",
                       "Wheat_Yangtze_IRR_lo","Wheat_ZiyaHe_IRR_lo")
Wheat_RFD_hi <- list("Wheat_AmurR_RFD_hi","Wheat_BoHai_RFD_hi","Wheat_ChinaCst_RFD_hi",
                       "Wheat_GangesR_RFD_hi","Wheat_Gobi_RFD_hi","Wheat_Hainan_RFD_hi",
                       "Wheat_Hong_RFD_hi","Wheat_HuangHeR_RFD_hi","Wheat_IndusR_RFD_hi",
                       "Wheat_IrrawaddyR_RFD_hi","Wheat_LBalkash_RFD_hi","Wheat_Mekong_RFD_hi",
                       "Wheat_ObR_RFD_hi","Wheat_RusCstSE_RFD_hi","Wheat_SChinaSea_RFD_hi",
                       "Wheat_Salween_RFD_hi","Wheat_Tarim_RFD_hi","Wheat_Xunjiang_RFD_hi",
                       "Wheat_Yangtze_RFD_hi","Wheat_ZiyaHe_RFD_hi")
Wheat_RFD_lo <- list("Wheat_AmurR_RFD_lo","Wheat_BoHai_RFD_lo","Wheat_ChinaCst_RFD_lo",
                       "Wheat_GangesR_RFD_lo","Wheat_Gobi_RFD_lo","Wheat_Hainan_RFD_lo",
                       "Wheat_Hong_RFD_lo","Wheat_HuangHeR_RFD_lo","Wheat_IndusR_RFD_lo",
                       "Wheat_IrrawaddyR_RFD_lo","Wheat_LBalkash_RFD_lo","Wheat_Mekong_RFD_lo",
                       "Wheat_ObR_RFD_lo","Wheat_RusCstSE_RFD_lo","Wheat_SChinaSea_RFD_lo",
                       "Wheat_Salween_RFD_lo","Wheat_Tarim_RFD_lo","Wheat_Xunjiang_RFD_lo",
                       "Wheat_Yangtze_RFD_lo","Wheat_ZiyaHe_RFD_lo")
# filter Wheat LA
Wheat_All <- union(Wheat_IRR_hi,Wheat_IRR_lo)%>%
  union(Wheat_RFD_hi)%>%
  union(Wheat_RFD_lo)
LA_Wheat <- Ag_LA%>%
  filter(LandLeaf%in%Wheat_All)%>%
  mutate(technology="Wheat")
# mark technologies and aggregate
LA_Wheat$technology[which(LA_Wheat$LandLeaf%in%Wheat_IRR_hi)] <- "Wheat_IRR_hi"
LA_Wheat$technology[which(LA_Wheat$LandLeaf%in%Wheat_IRR_lo)] <- "Wheat_IRR_lo"
LA_Wheat$technology[which(LA_Wheat$LandLeaf%in%Wheat_RFD_hi)] <- "Wheat_RFD_hi"
LA_Wheat$technology[which(LA_Wheat$LandLeaf%in%Wheat_RFD_lo)] <- "Wheat_RFD_lo"

LA_Wheat <- LA_Wheat%>%
  group_by(region,scenario,technology,year)%>%
  summarise(Area=sum(Area),.groups = "drop")
# add EC and CI components
LA_Wheat <- LA_Wheat%>%
  mutate(EC=scenario,CI=scenario)
LA_Wheat$EC[which(LA_Wheat$EC%in%scenarios_Reference)] <- "基准"
LA_Wheat$EC[which(LA_Wheat$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
LA_Wheat$EC[which(LA_Wheat$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
LA_Wheat$CI[which(LA_Wheat$CI%in%scenarios_GFDL)] <- "GFDL"
LA_Wheat$CI[which(LA_Wheat$CI%in%scenarios_HadGEM)] <- "HadGEM"
LA_Wheat$CI[which(LA_Wheat$CI%in%scenarios_IPSL)] <- "IPSL"
LA_Wheat$CI[which(LA_Wheat$CI%in%scenarios_MIROC)] <- "MIROC"
LA_Wheat$CI[which(LA_Wheat$CI%in%scenarios_NorESM)] <- "NorESM"
LA_Wheat$CI[which(LA_Wheat$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
LA_Wheat_mean <- LA_Wheat%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,technology,year)%>%
  summarise(Area=mean(Area),.groups = "drop")%>%
  mutate(CI="GCM平均",scenario="CI")
LA_Wheat_mean$scenario[which(LA_Wheat_mean$EC=="基准")] <- "Reference_CI"
LA_Wheat_mean$scenario[which(LA_Wheat_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
LA_Wheat_mean$scenario[which(LA_Wheat_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
LA_Wheat_mean <- LA_Wheat_mean%>%
  select(region,scenario,technology,year,Area,EC,CI)

LA_Wheat <- rbind(LA_Wheat,LA_Wheat_mean)
# reorder variables
LA_Wheat$CI <- factor(LA_Wheat$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
LA_Wheat$EC <- factor(LA_Wheat$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot scens crop LU
# no need, can add post hoc

# LA_Wheat differences
# prepare dif dataframe
LA_Wheat_dif <- LA_Wheat%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "Area")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,technology,year,Difference,Rel_Dif)
LA_Wheat_dif <- LA_Wheat_dif%>%rbind(LA_Wheat%>%filter(EC=="碳中和(全技术)")%>%
                                           filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                           select(-scenario)%>%
                                           spread(key = "CI",value = "Area")%>%
                                           mutate(Difference=GCM平均-无CI,
                                                  Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                           select(region,EC,technology,year,Difference,Rel_Dif))
LA_Wheat_dif <- LA_Wheat_dif%>%rbind(LA_Wheat%>%filter(EC=="碳中和(无DAC)")%>%
                                           filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                           select(-scenario)%>%
                                           spread(key = "CI",value = "Area")%>%
                                           mutate(Difference=GCM平均-无CI,
                                                  Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                           select(region,EC,technology,year,Difference,Rel_Dif))
LA_Wheat_dif$EC <- factor(LA_Wheat_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot China
ggplot(data = LA_Wheat_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(technology)))+
  geom_bar(stat = "identity")+
  facet_grid(~EC)+
  scale_y_continuous("面积差异/千km2")+
  scale_x_continuous("年份")+
  labs(fill="所用技术")+
  scale_fill_discrete()+
  ggtitle("中国不同政策情景下CI造成的小麦物面积差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save file
write.csv(LA_Wheat, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\LA_Wheat.csv", row.names = FALSE)
write.csv(LA_Wheat_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\LA_Wheat_dif.csv", row.names = FALSE)



# 3.4.3 Corn
# list of 4 technologies
Corn_IRR_hi <- list("Corn_AmurR_IRR_hi","Corn_BoHai_IRR_hi","Corn_ChinaCst_IRR_hi",
                     "Corn_GangesR_IRR_hi","Corn_Gobi_IRR_hi","Corn_Hainan_IRR_hi",
                     "Corn_Hong_IRR_hi","Corn_HuangHeR_IRR_hi","Corn_IndusR_IRR_hi",
                     "Corn_IrrawaddyR_IRR_hi","Corn_LBalkash_IRR_hi","Corn_Mekong_IRR_hi",
                     "Corn_ObR_IRR_hi","Corn_RusCstSE_IRR_hi","Corn_SChinaSea_IRR_hi",
                     "Corn_Salween_IRR_hi","Corn_Tarim_IRR_hi","Corn_Xunjiang_IRR_hi",
                     "Corn_Yangtze_IRR_hi","Corn_ZiyaHe_IRR_hi")
Corn_IRR_lo <- list("Corn_AmurR_IRR_lo","Corn_BoHai_IRR_lo","Corn_ChinaCst_IRR_lo",
                     "Corn_GangesR_IRR_lo","Corn_Gobi_IRR_lo","Corn_Hainan_IRR_lo",
                     "Corn_Hong_IRR_lo","Corn_HuangHeR_IRR_lo","Corn_IndusR_IRR_lo",
                     "Corn_IrrawaddyR_IRR_lo","Corn_LBalkash_IRR_lo","Corn_Mekong_IRR_lo",
                     "Corn_ObR_IRR_lo","Corn_RusCstSE_IRR_lo","Corn_SChinaSea_IRR_lo",
                     "Corn_Salween_IRR_lo","Corn_Tarim_IRR_lo","Corn_Xunjiang_IRR_lo",
                     "Corn_Yangtze_IRR_lo","Corn_ZiyaHe_IRR_lo")
Corn_RFD_hi <- list("Corn_AmurR_RFD_hi","Corn_BoHai_RFD_hi","Corn_ChinaCst_RFD_hi",
                     "Corn_GangesR_RFD_hi","Corn_Gobi_RFD_hi","Corn_Hainan_RFD_hi",
                     "Corn_Hong_RFD_hi","Corn_HuangHeR_RFD_hi","Corn_IndusR_RFD_hi",
                     "Corn_IrrawaddyR_RFD_hi","Corn_LBalkash_RFD_hi","Corn_Mekong_RFD_hi",
                     "Corn_ObR_RFD_hi","Corn_RusCstSE_RFD_hi","Corn_SChinaSea_RFD_hi",
                     "Corn_Salween_RFD_hi","Corn_Tarim_RFD_hi","Corn_Xunjiang_RFD_hi",
                     "Corn_Yangtze_RFD_hi","Corn_ZiyaHe_RFD_hi")
Corn_RFD_lo <- list("Corn_AmurR_RFD_lo","Corn_BoHai_RFD_lo","Corn_ChinaCst_RFD_lo",
                     "Corn_GangesR_RFD_lo","Corn_Gobi_RFD_lo","Corn_Hainan_RFD_lo",
                     "Corn_Hong_RFD_lo","Corn_HuangHeR_RFD_lo","Corn_IndusR_RFD_lo",
                     "Corn_IrrawaddyR_RFD_lo","Corn_LBalkash_RFD_lo","Corn_Mekong_RFD_lo",
                     "Corn_ObR_RFD_lo","Corn_RusCstSE_RFD_lo","Corn_SChinaSea_RFD_lo",
                     "Corn_Salween_RFD_lo","Corn_Tarim_RFD_lo","Corn_Xunjiang_RFD_lo",
                     "Corn_Yangtze_RFD_lo","Corn_ZiyaHe_RFD_lo")
# filter Corn LA
Corn_All <- union(Corn_IRR_hi,Corn_IRR_lo)%>%
  union(Corn_RFD_hi)%>%
  union(Corn_RFD_lo)
LA_Corn <- Ag_LA%>%
  filter(LandLeaf%in%Corn_All)%>%
  mutate(technology="Corn")
# mark technologies and aggregate
LA_Corn$technology[which(LA_Corn$LandLeaf%in%Corn_IRR_hi)] <- "Corn_IRR_hi"
LA_Corn$technology[which(LA_Corn$LandLeaf%in%Corn_IRR_lo)] <- "Corn_IRR_lo"
LA_Corn$technology[which(LA_Corn$LandLeaf%in%Corn_RFD_hi)] <- "Corn_RFD_hi"
LA_Corn$technology[which(LA_Corn$LandLeaf%in%Corn_RFD_lo)] <- "Corn_RFD_lo"

LA_Corn <- LA_Corn%>%
  group_by(region,scenario,technology,year)%>%
  summarise(Area=sum(Area),.groups = "drop")
# add EC and CI components
LA_Corn <- LA_Corn%>%
  mutate(EC=scenario,CI=scenario)
LA_Corn$EC[which(LA_Corn$EC%in%scenarios_Reference)] <- "基准"
LA_Corn$EC[which(LA_Corn$EC%in%scenarios_Policy)] <- "碳中和(全技术)"
LA_Corn$EC[which(LA_Corn$EC%in%scenarios_Policy_NoDAC)] <- "碳中和(无DAC)"
LA_Corn$CI[which(LA_Corn$CI%in%scenarios_GFDL)] <- "GFDL"
LA_Corn$CI[which(LA_Corn$CI%in%scenarios_HadGEM)] <- "HadGEM"
LA_Corn$CI[which(LA_Corn$CI%in%scenarios_IPSL)] <- "IPSL"
LA_Corn$CI[which(LA_Corn$CI%in%scenarios_MIROC)] <- "MIROC"
LA_Corn$CI[which(LA_Corn$CI%in%scenarios_NorESM)] <- "NorESM"
LA_Corn$CI[which(LA_Corn$CI%in%scenarios_NCI)] <- "无CI"
# add mean values
LA_Corn_mean <- LA_Corn%>%
  filter(CI!="无CI")%>%
  group_by(region,EC,technology,year)%>%
  summarise(Area=mean(Area),.groups = "drop")%>%
  mutate(CI="GCM平均",scenario="CI")
LA_Corn_mean$scenario[which(LA_Corn_mean$EC=="基准")] <- "Reference_CI"
LA_Corn_mean$scenario[which(LA_Corn_mean$EC=="碳中和(全技术)")] <- "Policy_CI"
LA_Corn_mean$scenario[which(LA_Corn_mean$EC=="碳中和(无DAC)")] <- "Policy_NoDAC_CI"
LA_Corn_mean <- LA_Corn_mean%>%
  select(region,scenario,technology,year,Area,EC,CI)

LA_Corn <- rbind(LA_Corn,LA_Corn_mean)
# reorder variables
LA_Corn$CI <- factor(LA_Corn$CI,levels = c("无CI","GFDL","HadGEM","IPSL","MIROC","NorESM","GCM平均"))
LA_Corn$EC <- factor(LA_Corn$EC,levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot scens crop LU
# no need, can add post hoc

# LA_Corn differences
# prepare dif dataframe
LA_Corn_dif <- LA_Corn%>%filter(EC=="基准")%>%
  filter((CI=="无CI")|(CI=="GCM平均"))%>%
  select(-scenario)%>%
  spread(key = "CI",value = "Area")%>%
  mutate(Difference=GCM平均-无CI,
         Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
  select(region,EC,technology,year,Difference,Rel_Dif)
LA_Corn_dif <- LA_Corn_dif%>%rbind(LA_Corn%>%filter(EC=="碳中和(全技术)")%>%
                                       filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                       select(-scenario)%>%
                                       spread(key = "CI",value = "Area")%>%
                                       mutate(Difference=GCM平均-无CI,
                                              Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                       select(region,EC,technology,year,Difference,Rel_Dif))
LA_Corn_dif <- LA_Corn_dif%>%rbind(LA_Corn%>%filter(EC=="碳中和(无DAC)")%>%
                                       filter((CI=="无CI")|(CI=="GCM平均"))%>%
                                       select(-scenario)%>%
                                       spread(key = "CI",value = "Area")%>%
                                       mutate(Difference=GCM平均-无CI,
                                              Rel_Dif=100*(GCM平均-无CI)/无CI)%>%
                                       select(region,EC,technology,year,Difference,Rel_Dif))
LA_Corn_dif$EC <- factor(LA_Corn_dif$EC, levels = c("基准","碳中和(全技术)","碳中和(无DAC)"))
# plot China
ggplot(data = LA_Corn_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(technology)))+
  geom_bar(stat = "identity")+
  facet_grid(~EC)+
  scale_y_continuous("面积差异/千km2")+
  scale_x_continuous("年份")+
  labs(fill="所用技术")+
  scale_fill_discrete()+
  ggtitle("中国不同政策情景下CI造成的玉米面积差异")+
  theme(plot.title = element_text(size=30,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save file
write.csv(LA_Corn, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\LA_Corn.csv", row.names = FALSE)
write.csv(LA_Corn_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\支线\\crops\\LA_Corn_dif.csv", row.names = FALSE)



