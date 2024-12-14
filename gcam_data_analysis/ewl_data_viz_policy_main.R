library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 0 Common Preparation
setwd("D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new")
# future years
future_years <- list("2020","2025","2030","2035","2040","2045","2050",
          "2055","2060","2065","2070","2075","2080","2085","2090","2095","2100")
# scenarios
scenarios = list("Reference","Reference_CI_GFDL","Reference_CI_HadGEM","Reference_CI_IPSL",
                 "Reference_CI_MIROC","Reference_CI_NorESM","Policy","Policy_CI_GFDL",
                 "Policy_CI_HadGEM","Policy_CI_IPSL","Policy_CI_MIROC","Policy_CI_NorESM",
                 "Policy_NoDAC","Policy_NoDAC_CI_GFDL","Policy_NoDAC_CI_HadGEM",
                 "Policy_NoDAC_CI_IPSL","Policy_NoDAC_CI_MIROC","Policy_NoDAC_CI_NorESM")


# 1 Landuse
# 1.1 read in the summarized LU.csv
LU <- read.csv("./LU.csv",header = TRUE,check.names = FALSE)
# 1.2 plot the Reference LU_frac as stacking bar plot 
# prepare data components
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")
LU_scen <- LU%>%filter(scenario%in%scenarios_NCI)
LU_scen$scenario <- factor(LU_scen$scenario, levels = c("Reference","Policy","Policy_NoDAC"))
LU_scen$landuse <- factor(LU_scen$landuse, levels = c("biomass","crops","forest","grass","pasture",
                                                      "shrubs","naturalOther","urban"))
# China
ggplot(data = LU_scen%>%
         filter(region=="China"), 
       aes(x = year, y=LU_frac, fill = factor(landuse)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景土地利用方式")+
  scale_y_continuous("各土地类型所占面积/千km2")+
  scale_x_continuous("年份")+
  labs(fill="土地类型")+
  scale_fill_discrete(labels=c("生物质","农作物","森林","草地","牧场","灌木","其他自然","城镇"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# 1.3 plot the other scenarios comparison as stacking bar plot
# 1.3.1 process data
LU_dif <- LU%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "LU_frac")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,landuse,year,Difference,Rel_Dif)
LU_dif <- LU_dif%>%rbind(LU%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                           spread(key = "scenario",value = "LU_frac")%>%
                           mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                  ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                           select(region,Name,landuse,year,Difference,Rel_Dif))
LU_dif <- LU_dif%>%rbind(LU%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                           spread(key = "scenario",value = "LU_frac")%>%
                           mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                  ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                           select(region,Name,landuse,year,Difference,Rel_Dif))
LU_dif$Name <- factor(LU_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
LU_dif$landuse <- factor(LU_dif$landuse, levels = c("biomass","crops","forest","grass","pasture",
                                                      "shrubs","naturalOther","urban"))
# save LU_dif file
write.csv(LU_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\LU_dif_policy.csv", row.names = FALSE)
# prepare data components
Labels_Name <- c("Policy"="碳中和(全技术)-基准","Policy_NoDAC"="碳中和(无DAC)-基准",
                 "NoDAC"="碳中和(无DAC)-碳中和(全技术)")
# 1.3.2 plot China
# absolute difference
ggplot(data = LU_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(landuse)))+
  geom_bar(stat="identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  scale_y_continuous("变化量/千km2")+
  scale_x_continuous("年份")+
  labs(fill="土地类型")+
  scale_fill_discrete(labels=c("生物质","农作物","森林","草地","牧场","灌木","其他自然","城镇"))+
  ggtitle("中国不同政策情景各土地类型占地面积差异")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# relative difference
ggplot(data = LU_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Rel_Dif, fill = factor(landuse)))+
  geom_bar(stat="identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  scale_y_continuous("相对差异/%")+
  scale_x_continuous("年份")+
  labs(fill="土地类型")+
  scale_fill_discrete(labels=c("生物质","农作物","森林","草地","牧场","灌木","其他自然","城镇"))+
  ggtitle("中国不同政策情景各土地类型占地面积相对差异")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# 1.4 certain landuse types, NETs direct effects
LU_detail <- read.csv("./LU_detail.csv",header = TRUE,check.names = FALSE)
# 1.4.1 forest
list_forest <- list("forest (managed)","forest (unmanaged)")
LU_forest <- LU_detail%>%filter(landuse%in%list_forest,scenario%in%scenarios_NCI)
# scenarios
LU_forest$scenario <- factor(LU_forest$scenario, levels = c("Reference","Policy","Policy_NoDAC"))
ggplot(data = LU_forest%>%
         filter(region=="China"), 
       aes(x = year, y=LU_frac, fill = factor(landuse)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景森林覆盖情况")+
  scale_y_continuous("森林面积/千km2")+
  scale_x_continuous("年份")+
  labs(fill="森林类型")+
  scale_fill_discrete(labels=c("管理","未管理"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# difference processing
LU_forest_dif <- LU_forest%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "LU_frac")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,landuse,year,Difference,Rel_Dif)
LU_forest_dif <- LU_forest_dif%>%rbind(LU_forest%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                           spread(key = "scenario",value = "LU_frac")%>%
                           mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                  ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                           select(region,Name,landuse,year,Difference,Rel_Dif))
LU_forest_dif <- LU_forest_dif%>%rbind(LU_forest%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                           spread(key = "scenario",value = "LU_frac")%>%
                           mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                  ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                           select(region,Name,landuse,year,Difference,Rel_Dif))
LU_forest_dif$Name <- factor(LU_forest_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
write.csv(LU_forest_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\LU_forest_dif_Policy.csv", row.names = FALSE)
# absolute differences
ggplot(data = LU_forest_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(landuse)))+
  geom_bar(stat="identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  scale_y_continuous("变化量/千km2")+
  scale_x_continuous("年份")+
  labs(fill="森林类型")+
  scale_fill_discrete(labels=c("管理","未管理"))+
  ggtitle("中国不同政策情景森林情况差异")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))





# 2 Agriculture
# 2.1 read in the summarized Ag.csv
Ag <- read.csv("./Ag.csv",header = TRUE,check.names = FALSE)

# 2.2 plot the Reference Ag_prod as stacking bar plot 
Ag_ref <- Ag%>%filter(scenario=="Reference")
# China
ggplot(data = Ag_ref%>%
         filter(region=="China"), 
       aes(x = year, y=Ag_prod, fill = factor(crop)))+
  geom_bar(stat="identity")+
  ggtitle("中国农作物产量")+
  theme(plot.title = element_text(hjust = 0.5))  # title adjusted to middle
# 2.3 plot the other scenarios compared to Reference as stacking bar plot
Ag_scen <- Ag%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "Ag_prod")%>%
  mutate(Difference=Policy-Reference)
# China
ggplot(data = Ag_scen%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(crop)))+
  geom_bar(stat="identity")+
  ggtitle("中国政策情景下的农作物产量变化")+
  theme(plot.title = element_text(hjust = 0.5))  # title adjusted to middle
# 2.4 plot the scenarios of Ag Production as line chart
# 2.4.1 absolute value
# China
ggplot(data = Ag%>%filter(region=="China")%>%
         group_by(region,scenario,year)%>%
         summarize(tot_prod=sum(Ag_prod)),
       aes(x = year, y = tot_prod, group = factor(scenario), color = factor(scenario)))+
  geom_line()+
  scale_y_continuous("农作物总产量/Mt")+
  scale_x_continuous("年份")+
  labs(color="情景")+
  scale_color_discrete(labels=c("碳中和(全技术)","碳中和(全技术)_CI","碳中和(无DAC)","碳中和(无DAC)_CI","基准","基准_CI"))+
  ggtitle("中国各情景下农作物总产量")+
  theme(plot.title = element_text(hjust = 0.5))  # title adjusted to middle
# 2.4.2 relative difference from Reference
# China
Ag_dif <- Ag%>%filter(region=="China")%>%
  group_by(region,scenario,year)%>%
  summarize(tot_prod=sum(Ag_prod))%>%
  spread(key = "scenario", value = "tot_prod")%>%
  mutate(ref = Reference)
Ag_dif$ref<-sapply(Ag_dif$ref, as.numeric)
ggplot(data = Ag_dif%>%gather(key="scenario",value = "tot_prod",unlist(scenarios))%>%
         mutate(rel_dif_per = 100*(tot_prod-ref)/ref)%>%
         filter(scenario != "Reference"),
       aes(x = year, y = rel_dif_per, group = factor(scenario), color = factor(scenario)))+
  geom_line()+
  scale_y_continuous("差异/%")+
  scale_x_continuous("年份")+
  labs(color="情景")+
  scale_color_discrete(labels=c("碳中和(全技术)","碳中和(全技术)_CI","碳中和(无DAC)","碳中和(无DAC)_CI","基准_CI"))+
  ggtitle("中国各情景下农作物总产量与基准情景的差异")+
  theme(plot.title = element_text(hjust = 0.5))  # title adjusted to middle





# 3 Water Withdrawals
# 3.1 read in the summarized WW.csv
WW <- read.csv("./WW.csv",header = TRUE,check.names = FALSE)
# 3.2 plot the Reference Water_Withdrawal as stacking bar plot 
# prepare data components
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")
WW_scen <- WW%>%filter(scenario%in%scenarios_NCI)
WW_scen$scenario <- factor(WW_scen$scenario, levels = c("Reference","Policy","Policy_NoDAC"))
WW_scen$sector <- factor(WW_scen$sector, levels = c("agriculture","electricity","industry",
                                                    "livestock","municipal","primaryEnergy","ces"))
# China
ggplot(data = WW_scen%>%
         filter(region=="China"), 
       aes(x = year, y=Water_Withdrawal, fill = factor(sector)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景各部门取水量")+
  scale_y_continuous("取水量/km3")+
  scale_x_continuous("年份")+
  labs(fill="用水部门")+
  scale_fill_discrete(labels=c("农业","电力","工业","畜牧业","市政","一次能源开发","其他"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# 3.3 plot the other scenarios comparison as stacking bar plot
# 3.3.1 process data
ces <- WW%>%filter(scenario=="Policy",sector=="ces")%>%
  mutate(scenario="Policy_NoDAC",Water_Withdrawal=0)
ces <- ces%>%rbind(ces%>%mutate(scenario="Reference"))
WW <- rbind(WW,ces)
WW_dif <- WW%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "Water_Withdrawal")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,sector,year,Difference,Rel_Dif)
WW_dif <- WW_dif%>%rbind(WW%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                               spread(key = "scenario",value = "Water_Withdrawal")%>%
                               mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                      ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                               select(region,Name,sector,year,Difference,Rel_Dif))
WW_dif <- WW_dif%>%rbind(WW%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                               spread(key = "scenario",value = "Water_Withdrawal")%>%
                               mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                      ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                               select(region,Name,sector,year,Difference,Rel_Dif))
WW_dif$Name <- factor(WW_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
WW_dif$sector <- factor(WW_dif$sector, levels = c("agriculture","electricity","industry",
                                                    "livestock","municipal","primaryEnergy","ces"))
# save WW_dif file
write.csv(WW_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\WW_dif_policy.csv", row.names = FALSE)
# prepare data components
Labels_Name <- c("Policy"="碳中和(全技术)-基准","Policy_NoDAC"="碳中和(无DAC)-基准",
                 "NoDAC"="碳中和(无DAC)-碳中和(全技术)")
# 3.3.2 plot China
# absolute difference
ggplot(data = WW_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(sector)))+
  geom_bar(stat="identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  scale_y_continuous("变化量/km3")+
  scale_x_continuous("年份")+
  labs(fill="用水部门")+
  scale_fill_discrete(labels=c("农业","电力","工业","畜牧业","市政","一次能源开发","其他"))+
  ggtitle("中国不同政策情景各部门取水量差异")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# relative difference
ggplot(data = WW_dif%>%
         filter(region=="China",sector!="ces"), 
       aes(x = year, y=Rel_Dif, fill = factor(sector)))+
  geom_bar(stat="identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  scale_y_continuous("相对差异/%")+
  scale_x_continuous("年份")+
  labs(fill="用水部门")+
  scale_fill_discrete(labels=c("农业","电力","工业","畜牧业","市政","一次能源开发","其他"))+
  ggtitle("中国不同政策情景各部门取水量相对差异")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))





# 4 Primary Energy
# 4.1 read in the summarized PrEn.csv
PrEn <- read.csv("./PrEn.csv",header = TRUE,check.names = FALSE)
# 4.2 plot the Reference Primary_Energy as stacking bar plot 
# prepare data components
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")
PrEn_scen <- PrEn%>%filter(scenario%in%scenarios_NCI)
PrEn_scen$scenario <- factor(PrEn_scen$scenario, levels = c("Reference","Policy","Policy_NoDAC"))
# China
ggplot(data = PrEn_scen%>%
         filter(region=="China"), 
       aes(x = year, y=Primary_Energy, fill = factor(fuel)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  scale_y_continuous("一次能源消费量/EJ")+
  scale_x_continuous("年份")+
  labs(fill="能源种类")+
  scale_fill_discrete(labels=c("生物质","传统生物质","煤","地热","水","天然气","核能","石油","光","风"))+
  ggtitle("中国不同政策情景一次能源结构")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# 4.3 plot the other scenarios comparison as stacking bar plot
# 4.3.1 process data 
PrEn_dif <- PrEn%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "Primary_Energy")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,fuel,year,Difference,Rel_Dif)
PrEn_dif <- PrEn_dif%>%rbind(PrEn%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                               spread(key = "scenario",value = "Primary_Energy")%>%
                               mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                      ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                               select(region,Name,fuel,year,Difference,Rel_Dif))
PrEn_dif <- PrEn_dif%>%rbind(PrEn%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                               spread(key = "scenario",value = "Primary_Energy")%>%
                               mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                      ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                               select(region,Name,fuel,year,Difference,Rel_Dif))
PrEn_dif$Name <- factor(PrEn_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
# save PrEn_dif file
write.csv(PrEn_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\PrEn_dif_policy.csv", row.names = FALSE)
# prepare data components
Labels_Name <- c("Policy"="碳中和(全技术)-基准","Policy_NoDAC"="碳中和(无DAC)-基准",
                 "NoDAC"="碳中和(无DAC)-碳中和(全技术)")
# 4.3.2 plot China
ggplot(data = PrEn_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(fuel)))+
  geom_bar(stat="identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  scale_y_continuous("变化量/EJ")+
  scale_x_continuous("年份")+
  labs(fill="能源种类")+
  scale_fill_discrete(labels=c("生物质","传统生物质","煤","地热","水","天然气","核能","石油","光","风"))+
  ggtitle("中国不同政策情景一次能源结构差异")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# single out biomass
ggplot(data = PrEn_dif%>%
         filter(region=="China",fuel=="biomass"), 
       aes(x = year, y=Difference, fill = factor(fuel)))+
  geom_bar(stat="identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  scale_y_continuous("变化量/EJ")+
  scale_x_continuous("年份")+
  labs(fill="能源种类")+
  scale_fill_discrete(labels=c("生物质"))+
  ggtitle("中国不同政策情景一次能源结构差异")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))


# 4.5 PrEn percentage
PrEn_per<-PrEn%>%
  group_by(region,scenario,year)%>%
  mutate(Percentage = 100*Primary_Energy/sum(Primary_Energy))%>%
  ungroup()
# 4.5.1 Plot non-fossil percentage as line chart for each scenario 
# China
non_fossil <- list("biomass","nuclear","hydro","wind","solar","geothermal")
ggplot(data = PrEn_per%>%filter(region=="China")%>%
         group_by(region,scenario,year)%>%
         filter(fuel %in% non_fossil)%>%
         summarise(nonFossil_per = sum(Percentage)),
       aes(x = year, y = nonFossil_per, group = factor(scenario), color = factor(scenario)))+
  geom_line()+
  scale_y_continuous("非化石能源消费比例/%")+
  scale_x_continuous("年份")+
  labs(color="情景")+
  scale_color_discrete(labels=c("碳中和(全技术)","碳中和(全技术)_CI","碳中和(无DAC)","碳中和(无DAC)_CI","基准","基准_CI"))+
  ggtitle("中国各情景下非化石能源消费比例")+
  theme(plot.title = element_text(hjust = 0.5))  # title adjusted to middle



# 5 Electricity Generation
# 5.1 read in the summarized ElecGen.csv
Elec <- read.csv("./ElecGen.csv",header = TRUE,check.names = FALSE)
# 5.2 plot the Reference Elec_Generation as stacking bar plot 
# prepare data components
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")
Elec_scen <- Elec%>%filter(scenario%in%scenarios_NCI)
Elec_scen$scenario <- factor(Elec_scen$scenario, levels = c("Reference","Policy","Policy_NoDAC"))
Elec_scen$fuel <- factor(Elec_scen$fuel, levels = c("biomass","coal","gas",
                                                    "geothermal","hydro","nuclear",
                                                    "oil","solar","wind"))
# China
ggplot(data = Elec_scen%>%
         filter(region=="China"), 
       aes(x = year, y=Elec_Generation, fill = factor(fuel)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  scale_y_continuous("一次能源消费量/EJ")+
  scale_x_continuous("年份")+
  labs(fill="能源种类")+
  scale_fill_discrete(labels=c("生物质","煤","天然气","地热","水","核能","石油","光","风"))+
  ggtitle("中国不同政策情景电力结构")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))

# 5.3 plot the other scenarios comparison as stacking bar plot
# 5.3.1 process data 
Elec_dif <- Elec%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "Elec_Generation")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,fuel,year,Difference,Rel_Dif)
Elec_dif <- Elec_dif%>%rbind(Elec%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                               spread(key = "scenario",value = "Elec_Generation")%>%
                               mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                      ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                               select(region,Name,fuel,year,Difference,Rel_Dif))
Elec_dif <- Elec_dif%>%rbind(Elec%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                               spread(key = "scenario",value = "Elec_Generation")%>%
                               mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                      ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                               select(region,Name,fuel,year,Difference,Rel_Dif))
Elec_dif$Name <- factor(Elec_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
Elec_dif$fuel <- factor(Elec_dif$fuel, levels = c("biomass","coal","gas",
                                                    "geothermal","hydro","nuclear",
                                                    "oil","solar","wind"))
# save Elec_dif file
write.csv(Elec_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\Elec_dif_policy.csv", row.names = FALSE)
# prepare data components
Labels_Name <- c("Policy"="碳中和(全技术)-基准","Policy_NoDAC"="碳中和(无DAC)-基准",
                 "NoDAC"="碳中和(无DAC)-碳中和(全技术)")
# 5.3.2 plot China
ggplot(data = Elec_dif%>%
         filter(region=="China"), 
       aes(x = year, y=Difference, fill = factor(fuel)))+
  geom_bar(stat="identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  scale_y_continuous("变化量/EJ")+
  scale_x_continuous("年份")+
  labs(fill="能源种类")+
  scale_fill_discrete(labels=c("生物质","煤","天然气","地热","水","核能","石油","光","风"))+
  ggtitle("中国不同政策情景电力结构差异")+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
