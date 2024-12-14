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


# 1 CO2 emission by tech
# read in emission data
emission <- read.csv("./intermediate/emission_by_tech.csv",header = TRUE,check.names = FALSE)
# 1.1 plot stacked area chart for different sectors
# 1.1.1  prepare
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")
# merge sectors into general categories
subsector_DAC <- list("airCO2")
subsector_BECCS_AF <- list("regional biomass","regional biomassOil","regional corn for ethanol")
subsector_BE <- list("biomass","biomass (conv)","biomass (IGCC)",
                           "biomass gasification","biomass liquids",
                           "biomass (conv CCS)","biomass (IGCC CCS)")
subsector_coal <- list("coal","coal (conv pul CCS)","coal (conv pul)","coal (IGCC CCS)","coal (IGCC)",
                       "coal gasification","coal to liquids")
subsector_oil <- list("refined liquids","refined liquids (CC CCS)","refined liquids (CC)",
                      "refined liquids (steam/CT)","oil refining","2W and 3W")
subsector_gas <- list("gas","gas (CC CCS)","gas (CC)","gas (steam/CT)","gas CCS",
                      "gas pipeline","gas to liquids")
subsector_other <- list("cement")
tech_coal <- list("Coal")
tech_oil <- list("Liquids","Hybrid Liquids")
tech_gas <- list("NG")
emission_01 <- emission
emission_01$sector[which(emission$subsector%in%subsector_DAC)] <- "DAC"
emission_01$sector[which(emission$subsector%in%subsector_BECCS_AF)] <- "BECCS_AF"
emission_01$sector[which(emission$subsector%in%subsector_BE)] <- "BE"
emission_01$sector[which(emission$subsector%in%subsector_coal)] <- "coal"
emission_01$sector[which(emission$subsector%in%subsector_oil)] <- "oil"
emission_01$sector[which(emission$subsector%in%subsector_gas)] <- "gas"
emission_01$sector[which(emission$subsector%in%subsector_other)] <- "non_energy"
emission_01$sector[which(emission$technology%in%tech_coal)] <- "coal"
emission_01$sector[which(emission$technology%in%tech_oil)] <- "oil"
emission_01$sector[which(emission$technology%in%tech_gas)] <- "gas"
# aggregate emissions in the same category
emission_01 <- emission_01%>%group_by(region,scenario,sector,year)%>%
  summarise(sector_Emission=sum(Emission))
# reorder plot
emission_01$scenario <- factor(emission_01$scenario, levels = c("Reference","Policy","Policy_NoDAC"))
emission_01$sector <- factor(emission_01$sector, levels = c("coal","oil","gas","BE","non_energy",
                                                            "BECCS_AF","DAC"))
# 1.1.2 plot
ggplot(data = emission_01%>%
         filter(region=="China",scenario%in%scenarios_NCI),
       aes(x = year, y = sector_Emission, fill = factor(sector)))+
  geom_area()+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景各部门排放")+
  scale_y_continuous("排放量/MTC")+
  scale_x_continuous("年份")+
  labs(fill="排放/负排放部门")+
  scale_fill_discrete(labels=c("煤","石油","天然气","生物质能","非能源排放","BECCS与造林","DAC"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# only certain sector
ggplot(data = emission_01%>%
         filter(region=="China",scenario%in%scenarios_NCI,
                sector=="BECCS_AF"),
       aes(x = year, y = sector_Emission, fill = factor(sector)))+
  geom_area()+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景BECCS与造林负排放")+
  scale_y_continuous("排放量/MTC")+
  scale_x_continuous("年份")+
  labs(fill="排放/负排放部门")+
  scale_fill_discrete(labels=c("BECCS与造林"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save emission file
write.csv(emission_01, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\emission_01_fueltype.csv", row.names = FALSE)

# 1.2 plot scenario differences
DAC <- emission_01%>%filter(scenario=="Policy",sector=="DAC")%>%
  mutate(scenario="Policy_NoDAC",sector_Emission=0)
DAC <- DAC%>%rbind(DAC%>%mutate(scenario="Reference"))
emission_01 <- rbind(emission_01,DAC)
# 1.2.1 process dif dataframe
emission_01_dif <- emission_01%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "sector_Emission")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,sector,year,Difference,Rel_Dif)
emission_01_dif <- emission_01_dif%>%rbind(emission_01%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                           spread(key = "scenario",value = "sector_Emission")%>%
                           mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                  ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                           select(region,Name,sector,year,Difference,Rel_Dif))
emission_01_dif <- emission_01_dif%>%rbind(emission_01%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                           spread(key = "scenario",value = "sector_Emission")%>%
                           mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                  ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                           select(region,Name,sector,year,Difference,Rel_Dif))
emission_01_dif$Name <- factor(emission_01_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
emission_01_dif$sector <- factor(emission_01_dif$sector, levels = c("coal","oil","gas","BE","non_energy",
                                                            "BECCS_AF","DAC"))
# 1.2.2 plot
Labels_Name <- c("Policy"="碳中和(全技术)-基准","Policy_NoDAC"="碳中和(无DAC)-基准",
                 "NoDAC"="碳中和(无DAC)-碳中和(全技术)")
ggplot(data = emission_01_dif%>%
         filter(region=="China"),
       aes(x = year, y = Difference, fill = factor(sector)))+
  geom_area()+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  ggtitle("中国不同政策情景各部门排放差异")+
  scale_y_continuous("排放量差异/MTC")+
  scale_x_continuous("年份")+
  labs(fill="排放/负排放部门")+
  scale_fill_discrete(labels=c("煤","石油","天然气","生物质能","非能源排放","BECCS与造林","DAC"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# only certain sector
ggplot(data = emission_01_dif%>%
         filter(region=="China",
                sector=="BECCS_AF"),
       aes(x = year, y = Difference, fill = factor(sector)))+
  geom_area()+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  ggtitle("中国不同政策情景BECCS与造林负排放差异")+
  scale_y_continuous("排放量差异/MTC",limits=c(-1200,0),breaks=c(0,-300,-600,-900))+
  scale_x_continuous("年份")+
  labs(fill="排放/负排放部门")+
  scale_fill_discrete(labels=c("BECCS与造林"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save emission file
write.csv(emission_01_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\emission_01_fueltype_dif.csv", row.names = FALSE)

# 1.3 plot NET emissions
# 1.3.1 prepare NETs lists
list_sub_elec_CCS <- list("biomass (conv CCS)","biomass (IGCC CCS)","coal (conv pul CCS)",
                          "coal (IGCC CCS)","gas (CC CCS)","gas CCS","refined liquids (CC CCS)")
list_sub_BECCS_AF <- list("regional biomass","regional biomassOil","regional corn for ethanol")
list_sub_DAC <- list("airCO2")
list_tech_other_CCS <- list("biomass to H2 CCS","coal CCS","coal chemical CCS","gas CCS",
                            "FT biofuels CCS level 1","FT biofuels CCS level 2",
                            "cellulosic ethanol CCS level 1","cellulosic ethanol CCS level 2",
                            "coal to liquids CCS level 1","coal to liquids CCS level 2")
# 1.3.2 plot
ggplot(data = emission%>%
         filter(region=="China",scenario%in%scenarios_NCI)%>%
         filter((subsector%in%list_sub_elec_CCS)|(subsector%in%list_sub_BECCS_AF)|
                  (subsector%in%list_sub_DAC)|(technology%in%list_tech_other_CCS)),
       aes(x = year, y = Emission, fill = factor(subsector)))+
  geom_area()+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景各部门排放")+
  scale_y_continuous("排放量/MTC")+
  scale_x_continuous("年份")+
  labs(fill="负排放部门")+
  scale_fill_discrete()+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))






# 2 elec gen by gen tech
elec_gen <- read.csv("./intermediate/elec_gen_by_tech.csv",header = TRUE,check.names = FALSE)
# 2.1 plot gen of fuels
list_fossil <- list("coal","refined liquids","gas")
notable_tech <- list("biomass (conv CCS)","biomass (IGCC CCS)",
                     "coal (conv pul CCS)","coal (IGCC CCS)",
                     "coal (conv pul)","coal (IGCC)",
                     "geothermal","Gen_III",
                     "PV","CSP_storage","wind")
# preparation
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")
elec_gen_01 <- elec_gen%>%
  filter(technology%in%notable_tech)
elec_gen_01$scenario <- factor(elec_gen_01$scenario,levels = c("Reference","Policy","Policy_NoDAC"))
elec_gen_01$technology <- factor(elec_gen_01$technology,
                              levels = c("biomass (conv CCS)","biomass (IGCC CCS)",
                                         "coal (conv pul CCS)","coal (IGCC CCS)",
                                         "coal (conv pul)","coal (IGCC)",
                                         "geothermal","Gen_III",
                                         "PV","CSP_storage","wind"))
# plot
ggplot(data = elec_gen_01%>%
         filter(region=="China",scenario%in%scenarios_NCI),
       aes(x = year, y = gen, fill = factor(technology)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景部分主要技术发电量")+
  scale_y_continuous("发电量/EJ")+
  scale_x_continuous("年份")+
  labs(fill="发电技术")+
  scale_fill_discrete(labels=c("生物质(conv CCS)","生物质(IGCC CCS)",
                               "煤(conv pul CCS)","煤(IGCC CCS)",
                               "煤(conv pul)","煤(IGCC)",
                               "地热","核(Gen_III)",
                               "光(PV)","光(CSP_storage)","风"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# only plot certain fuel
ggplot(data = elec_gen_01%>%
         filter(region=="China",scenario%in%scenarios_NCI,
                (technology=="biomass (conv CCS)"|technology=="biomass (IGCC CCS)")),
       aes(x = year, y = gen, fill = factor(technology)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景BECCS技术发电量")+
  scale_y_continuous("发电量/EJ",limits=c(0,6))+
  scale_x_continuous("年份")+
  labs(fill="发电技术")+
  scale_fill_discrete(labels=c("生物质(conv CCS)","生物质(IGCC CCS)"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# 2.2 plot scenario diiferences
# 2.2.1 process dif dataframe
elec_gen_01_dif <- elec_gen_01%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "gen")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,output,subsector,technology,year,Difference,Rel_Dif)
elec_gen_01_dif <- elec_gen_01_dif%>%rbind(elec_gen_01%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                                             spread(key = "scenario",value = "gen")%>%
                                             mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                                    ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                                             select(region,Name,output,subsector,technology,year,Difference,Rel_Dif))
elec_gen_01_dif <- elec_gen_01_dif%>%rbind(elec_gen_01%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                                             spread(key = "scenario",value = "gen")%>%
                                             mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                                    ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                                             select(region,Name,output,subsector,technology,year,Difference,Rel_Dif))
elec_gen_01_dif$Name <- factor(elec_gen_01_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
elec_gen_01_dif$technology <- factor(elec_gen_01_dif$technology,
                                  levels = c("biomass (conv CCS)","biomass (IGCC CCS)",
                                             "coal (conv pul CCS)","coal (IGCC CCS)",
                                             "coal (conv pul)","coal (IGCC)",
                                             "geothermal","Gen_III",
                                             "PV","CSP_storage","wind"))
# 2.2.2 plot
notable_tech <- list("biomass (conv CCS)","biomass (IGCC CCS)",
                     "coal (conv pul CCS)","coal (IGCC CCS)",
                     "coal (conv pul)","coal (IGCC)",
                     "geothermal","Gen_III",
                     "PV","CSP_storage","wind")
Labels_Name <- c("Policy"="碳中和(全技术)-基准","Policy_NoDAC"="碳中和(无DAC)-基准",
                 "NoDAC"="碳中和(无DAC)-碳中和(全技术)")
ggplot(data = elec_gen_01_dif%>%
         filter(region=="China",technology%in%notable_tech),
       aes(x = year, y = Difference, fill = factor(technology)))+
  geom_bar(stat = "identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  ggtitle("中国不同政策情景各主要技术发电量差异")+
  scale_y_continuous("发电量差异/EJ")+
  scale_x_continuous("年份")+
  labs(fill="发电技术")+
  scale_fill_discrete(labels=c("生物质(conv CCS)","生物质(IGCC CCS)",
                               "煤(conv pul CCS)","煤(IGCC CCS)",
                               "煤(conv pul)","煤(IGCC)",
                               "地热","核(Gen_III)",
                               "光(PV)","光(CSP_storage)","风"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# plot certain fuel type
ggplot(data = elec_gen_01_dif%>%
         filter(region=="China",
                (technology=="biomass (conv CCS)"|technology=="biomass (IGCC CCS)")),
       aes(x = year, y = Difference, fill = factor(technology)))+
  geom_bar(stat = "identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  ggtitle("中国不同政策情景BECCS发电量差异")+
  scale_y_continuous("发电量差异/EJ",limits=c(0,6))+
  scale_x_continuous("年份")+
  labs(fill="发电技术")+
  scale_fill_discrete(labels=c("生物质(conv CCS)","生物质(IGCC CCS)"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save emission file
write.csv(elec_gen_01_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\elec_gen_01_dif.csv", row.names = FALSE)




# 3 elec costs by gen tech
elec_cost <- read.csv("./intermediate/elec_cost_by_tech.csv",header = TRUE,check.names = FALSE)
# 3.1 plot costs of nonFossil fuels
list_fossil <- list("coal","refined liquids","gas")
notable_tech <- list("biomass (conv CCS)","biomass (IGCC CCS)",
                     "coal (conv pul CCS)","coal (IGCC CCS)",
                     "coal (conv pul)","coal (IGCC)",
                     "geothermal","Gen_III",
                     "PV","CSP_storage","wind")
elec_cost_01 <- elec_cost%>%
  filter(technology%in%notable_tech)
elec_cost_01$scenario <- factor(elec_cost_01$scenario,levels = c("Reference","Policy","Policy_NoDAC"))
elec_cost_01$technology <- factor(elec_cost_01$technology,
                                  levels = c("biomass (conv CCS)","biomass (IGCC CCS)",
                                             "coal (conv pul CCS)","coal (IGCC CCS)",
                                             "coal (conv pul)","coal (IGCC)",
                                             "geothermal","Gen_III",
                                             "PV","CSP_storage","wind"))
# plot
ggplot(data = elec_cost_01%>%
         filter(region=="China",scenario%in%scenarios_NCI),
       aes(x = year, y = cost, color = factor(technology)))+
  geom_line(size=3)+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景主要发电技术成本")+
  scale_y_continuous("成本 (1975$/GJ)")+
  scale_x_continuous("年份")+
  labs(color="发电技术")+
  scale_color_discrete(labels=c("生物质(conv CCS)","生物质(IGCC CCS)",
                               "煤(conv pul CCS)","煤(IGCC CCS)",
                               "煤(conv pul)","煤(IGCC)",
                               "地热","核(Gen_III)",
                               "光(PV)","光(CSP_storage)","风"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))





# 4 elec WW by tech
elec_WW <- read.csv("./intermediate/elec_WW_by_tech.csv",header = TRUE,check.names = FALSE)
# 4.1 plot WW of fuels
list_fossil <- list("coal","refined liquids","gas")
notable_tech <- list("biomass (conv CCS)","biomass (IGCC CCS)",
                     "coal (conv pul CCS)","coal (IGCC CCS)",
                     "coal (conv pul)","coal (IGCC)",
                     "geothermal","Gen_III",
                     "PV","CSP_storage","wind")
# preparation
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")
elec_WW_01 <- elec_WW%>%
  filter(technology%in%notable_tech)
elec_WW_01$scenario <- factor(elec_WW_01$scenario,levels = c("Reference","Policy","Policy_NoDAC"))
elec_WW_01$technology <- factor(elec_WW_01$technology,
                                 levels = c("biomass (conv CCS)","biomass (IGCC CCS)",
                                            "coal (conv pul CCS)","coal (IGCC CCS)",
                                            "coal (conv pul)","coal (IGCC)",
                                            "geothermal","Gen_III",
                                            "PV","CSP_storage","wind"))
# plot
ggplot(data = elec_WW_01%>%
         filter(region=="China",scenario%in%scenarios_NCI),
       aes(x = year, y = WW, fill = factor(technology)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景主要发电技术取水量")+
  scale_y_continuous("取水量/km3")+
  scale_x_continuous("年份")+
  labs(fill="发电技术")+
  scale_fill_discrete(labels=c("生物质(conv CCS)","生物质(IGCC CCS)",
                               "煤(conv pul CCS)","煤(IGCC CCS)",
                               "煤(conv pul)","煤(IGCC)",
                               "地热","核(Gen_III)",
                               "光(PV)","光(CSP_storage)","风"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# 4.2 plot scenario diiferences
# 4.2.1 process dif dataframe
elec_WW_01_dif <- elec_WW_01%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "WW")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,sector,subsector,technology,year,Difference,Rel_Dif)
elec_WW_01_dif <- elec_WW_01_dif%>%rbind(elec_WW_01%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                                             spread(key = "scenario",value = "WW")%>%
                                             mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                                    ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                                             select(region,Name,sector,subsector,technology,year,Difference,Rel_Dif))
elec_WW_01_dif <- elec_WW_01_dif%>%rbind(elec_WW_01%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                                             spread(key = "scenario",value = "WW")%>%
                                             mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                                    ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                                             select(region,Name,sector,subsector,technology,year,Difference,Rel_Dif))
elec_WW_01_dif$Name <- factor(elec_WW_01_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
elec_WW_01_dif$technology <- factor(elec_WW_01_dif$technology,
                                     levels = c("biomass (conv CCS)","biomass (IGCC CCS)",
                                                "coal (conv pul CCS)","coal (IGCC CCS)",
                                                "coal (conv pul)","coal (IGCC)",
                                                "geothermal","Gen_III",
                                                "PV","CSP_storage","wind"))
# 4.2.2 plot
notable_tech <- list("biomass (conv CCS)","biomass (IGCC CCS)",
                     "coal (conv pul CCS)","coal (IGCC CCS)",
                     "coal (conv pul)","coal (IGCC)",
                     "geothermal","Gen_III",
                     "PV","CSP_storage","wind")
Labels_Name <- c("Policy"="碳中和(全技术)-基准","Policy_NoDAC"="碳中和(无DAC)-基准",
                 "NoDAC"="碳中和(无DAC)-碳中和(全技术)")
ggplot(data = elec_WW_01_dif%>%
         filter(region=="China",technology%in%notable_tech),
       aes(x = year, y = Difference, fill = factor(technology)))+
  geom_bar(stat = "identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  ggtitle("中国不同政策情景各主要技术取水量差异")+
  scale_y_continuous("取水量差异/km3")+
  scale_x_continuous("年份")+
  labs(fill="发电技术")+
  scale_fill_discrete(labels=c("生物质(conv CCS)","生物质(IGCC CCS)",
                               "煤(conv pul CCS)","煤(IGCC CCS)",
                               "煤(conv pul)","煤(IGCC)",
                               "地热","核(Gen_III)",
                               "光(PV)","光(CSP_storage)","风"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save emission file
write.csv(elec_WW_01_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\elec_WW_01_dif.csv", row.names = FALSE)





# 5 WW detail, different PrEn development
WW_detail <- read.csv("./WW_detail.csv",header = TRUE,check.names = FALSE)
# 5.1 plot WW of different PrEn
# preparation
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")
Primary <- list("biomass","nuclearFuelGenIII","regional coal",
                "regional natural gas","regional oil")
WW_PrEn <- WW_detail%>%
  filter(sector%in%Primary)
WW_PrEn$scenario <- factor(WW_PrEn$scenario,levels = c("Reference","Policy","Policy_NoDAC"))
# plot
ggplot(data = WW_PrEn%>%
         filter(region=="China",scenario%in%scenarios_NCI),
       aes(x = year, y = Water_Withdrawal, fill = factor(sector)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景一次能源开发取水量")+
  scale_y_continuous("取水量/km3")+
  scale_x_continuous("年份")+
  labs(fill="所开发的一次能源")+
  scale_fill_discrete(labels=c("生物质","核能","煤","天然气","石油"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# 5.2 plot scenario diiferences
# 5.2.1 process dif dataframe
WW_PrEn_dif <- WW_PrEn%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "Water_Withdrawal")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,sector,year,Difference,Rel_Dif)
WW_PrEn_dif <- WW_PrEn_dif%>%rbind(WW_PrEn%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                                           spread(key = "scenario",value = "Water_Withdrawal")%>%
                                           mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                                  ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                                           select(region,Name,sector,year,Difference,Rel_Dif))
WW_PrEn_dif <- WW_PrEn_dif%>%rbind(WW_PrEn%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                                           spread(key = "scenario",value = "Water_Withdrawal")%>%
                                           mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                                  ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                                           select(region,Name,sector,year,Difference,Rel_Dif))
WW_PrEn_dif$Name <- factor(WW_PrEn_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
# 5.2.2 plot
Labels_Name <- c("Policy"="碳中和(全技术)-基准","Policy_NoDAC"="碳中和(无DAC)-基准",
                 "NoDAC"="碳中和(无DAC)-碳中和(全技术)")
ggplot(data = WW_PrEn_dif%>%
         filter(region=="China"),
       aes(x = year, y = Difference, fill = factor(sector)))+
  geom_bar(stat = "identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  ggtitle("中国不同政策情景各一次能源开发取水量差异")+
  scale_y_continuous("取水量差异/km3")+
  scale_x_continuous("年份")+
  labs(fill="所开发的一次能源")+
  scale_fill_discrete(labels=c("生物质","核能","煤","天然气","石油"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save emission file
write.csv(WW_PrEn_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\WW_PrEn_dif.csv", row.names = FALSE)





# 6 WW by DAC
DAC_WW <- read.csv("./intermediate/DAC_WW.csv",header = TRUE,check.names = FALSE)
# 6.1 plot WW_DAC by tech
# preparation
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")

DAC_WW$scenario <- factor(DAC_WW$scenario,levels = c("Reference","Policy","Policy_NoDAC"))
# plot
ggplot(data = DAC_WW%>%
         filter(region=="China",scenario%in%scenarios_NCI),
       aes(x = year, y = WW, fill = factor(technology)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("碳中和(全技术)情景 DAC取水量")+
  scale_y_continuous("取水量/km3")+
  scale_x_continuous("年份")+
  labs(fill="技术")+
  scale_fill_discrete(labels=c("高温DAC-电","高温DAC-天然气"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))





# 7 end-use energy cnsumption by tech
end_use <- read.csv("./intermediate/end_use_by_energySource.csv",header = TRUE,check.names = FALSE)
# 7.1 plot end_use of different scenarios
# merge sectors of the same category
list_transportation <- list("trn_aviation_intl","trn_freight","trn_freight_road",
                            "trn_pass","trn_pass_road","trn_pass_road_LDV",
                            "trn_pass_road_LDV_4W","trn_shipping_intl")
list_industry <- list("resid cooling","resid heating","resid others")
list_residential <- list("cement","industrial energy use","industrial feedstocks",
                         "N fertilizer","process heat cement")
list_DAC <- list("ces","process heat dac")
list_other <- list("comm cooling","comm heating","comm others")

end_use <- end_use%>%
  mutate(category=sector)
end_use$category[which(end_use$category%in%list_transportation)] <- "transportation"
end_use$category[which(end_use$category%in%list_industry)] <- "industry"
end_use$category[which(end_use$category%in%list_residential)] <- "residential"
end_use$category[which(end_use$category%in%list_DAC)] <- "DAC"
end_use$category[which(end_use$category%in%list_other)] <- "other"

end_use_sector <- end_use%>%group_by(region,scenario,category,year)%>%
  summarise(total_Consumption=sum(Consumption))
# preparation for plotting
scenarios_NCI <- list("Reference","Policy","Policy_NoDAC")
Labels_scen <- c("Reference"="基准","Policy"="碳中和(全技术)","Policy_NoDAC"="碳中和(无DAC)")
end_use_sector$scenario <- factor(end_use_sector$scenario,levels = c("Reference","Policy","Policy_NoDAC"))
end_use_sector$category <- factor(end_use_sector$category,levels = c("DAC","industry","residential",
                                                                     "transportation","other"))
# plot
ggplot(data = end_use_sector%>%
         filter(region=="China",scenario%in%scenarios_NCI),
       aes(x = year, y = total_Consumption, fill = factor(category)))+
  geom_bar(stat="identity")+
  facet_wrap(~scenario,labeller = labeller(scenario=as_labeller(Labels_scen)))+
  ggtitle("中国不同政策情景终端用能")+
  scale_y_continuous("用能/EJ")+
  scale_x_continuous("年份")+
  labs(fill="部门")+
  scale_fill_discrete(labels=c("DAC","工业","居住","交通","其他"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# 7.2 plot scenario diiferences
end_DAC <- end_use_sector%>%filter(scenario=="Policy",category=="DAC")%>%
  mutate(scenario="Policy_NoDAC",total_Consumption=0)
end_DAC <- end_DAC%>%rbind(end_DAC%>%mutate(scenario="Reference"))
end_use_sector <- rbind(end_use_sector,end_DAC)
# 7.2.1 process dif dataframe
end_use_sector_dif <- end_use_sector%>%filter((scenario=="Policy")|(scenario=="Reference"))%>%
  spread(key = "scenario",value = "total_Consumption")%>%
  mutate(Name="Policy",Difference=Policy-Reference
         ,Rel_Dif=100*(Policy-Reference)/Reference)%>%
  select(region,Name,category,year,Difference,Rel_Dif)
end_use_sector_dif <- end_use_sector_dif%>%rbind(end_use_sector%>%filter((scenario=="Policy_NoDAC")|(scenario=="Reference"))%>%
                                     spread(key = "scenario",value = "total_Consumption")%>%
                                     mutate(Name="Policy_NoDAC",Difference=Policy_NoDAC-Reference
                                            ,Rel_Dif=100*(Policy_NoDAC-Reference)/Reference)%>%
                                     select(region,Name,category,year,Difference,Rel_Dif))
end_use_sector_dif <- end_use_sector_dif%>%rbind(end_use_sector%>%filter((scenario=="Policy_NoDAC")|(scenario=="Policy"))%>%
                                     spread(key = "scenario",value = "total_Consumption")%>%
                                     mutate(Name="NoDAC",Difference=Policy_NoDAC-Policy
                                            ,Rel_Dif=100*(Policy_NoDAC-Policy)/Policy)%>%
                                     select(region,Name,category,year,Difference,Rel_Dif))
end_use_sector_dif$Name <- factor(end_use_sector_dif$Name, levels = c("Policy","Policy_NoDAC","NoDAC"))
end_use_sector_dif$category <- factor(end_use_sector_dif$category,
                                      levels = c("DAC","industry","residential",
                                                "transportation","other"))
# 7.2.2 plot
Labels_Name <- c("Policy"="碳中和(全技术)-基准","Policy_NoDAC"="碳中和(无DAC)-基准",
                 "NoDAC"="碳中和(无DAC)-碳中和(全技术)")
ggplot(data = end_use_sector_dif%>%
         filter(region=="China"),
       aes(x = year, y = Difference, fill = factor(category)))+
  geom_bar(stat = "identity")+
  facet_wrap(~Name,labeller = labeller(Name=as_labeller(Labels_Name)))+
  ggtitle("中国不同政策情景终端用能差异")+
  scale_y_continuous("用能差异/EJ")+
  scale_x_continuous("年份")+
  labs(fill="部门")+
  scale_fill_discrete(labels=c("DAC","工业","居住","交通","其他"))+
  theme(plot.title = element_text(size=26,hjust=0.5))+
  theme(axis.title = element_text(size=20))+
  theme(axis.text = element_text(size=10))+
  theme(legend.title = element_text(size=20))+
  theme(legend.text = element_text(size=20))+
  theme(strip.text = element_text(size=20))
# save emission file
write.csv(end_use_sector, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\end_use_sector.csv", row.names = FALSE)
write.csv(end_use_sector_dif, "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\EWL\\new\\intermediate\\end_use_sector_dif.csv", row.names = FALSE)
