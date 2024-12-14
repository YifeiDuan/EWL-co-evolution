try:
  import xml.etree.cElementTree as ET
except ImportError:
  import xml.etree.ElementTree as ET

import os
from xml.dom import xmlbuilder
import numpy as np
import pandas as pd

path1 = "D:\emission constraint 2\data repo\GCAM-model\output\gcam_diagnostics\gcam_data\Core"
path2 = "D:\emission constraint 2\data repo\GCAM-model\input\policy\\add"
path3 = "D:\dyf毕设\毕设\碳中和路径讨论\Emission Constraints"

# Step1:
# 读入query输出的xls提取区域名
scenario = "Reference" #改文件名
xlsfile = path1+"\\ec_"+scenario+".xls"  
nonCO2_df = pd.read_excel(xlsfile, sheet_name="Sheet4",header=1) 
regions = nonCO2_df["region"].unique()
# 读入link.xml文件提取每个区域的GHG内容
xmlfile = "REF_2020_link.xml"  #改文件名
tree = ET.parse(path2+"\\"+xmlfile)
root = tree.getroot()

GHGs = []
for region in root.iter('region'):
    if region.attrib['name'] == "Africa_Eastern":
        AfE = region
        break
for ghg in AfE.iter('linked-ghg-policy'):
    GHGs.append(ghg.attrib['name'])


# Step2:
# 创建一个df，[(21种GHG*32区域),21年份]
CO2E_data = np.full([21*32,23], np.nan)
CO2E_df = pd.DataFrame(CO2E_data, columns=['region','GHG','1990','2005','2010','2015','2020','2025','2030',
'2035','2040','2045','2050','2055','2060','2065','2070','2075','2080','2085','2090','2095','2100'])
CO2E_df['region'] = CO2E_df['region'].astype(np.str)
CO2E_df['GHG'] = CO2E_df['GHG'].astype(np.str)

for i in CO2E_df.index:
    CO2E_df['region'].at[i] = regions[int(i//21)]
    CO2E_df['GHG'].at[i] = GHGs[int(i%21)]
# 从xml提取各区域各GHG在标签年份的MtCO2-e值存入CO2E_df中
for region in root.iter('region'):
    region_name = region.attrib['name']
    for ghg in region.iter('linked-ghg-policy'):
        ghg_name = ghg.attrib['name']
        for demand in ghg.iter('demand-adjust'):
            year = demand.attrib['year']
            if year == '1975':
                continue
            coeff = float(demand.text)
            CO2E_df[year].at[CO2E_df[(CO2E_df.region==region_name)&(CO2E_df.GHG==ghg_name)].index.tolist()[0]] =  coeff           


# Step3:
#填充其他年份的值
series = CO2E_df.isnull().all()
val_years = series[series.values==False].index.tolist()[2:]
years = ['1990','2005','2010','2015','2020','2025','2030','2035','2040','2045','2050','2055',
'2060','2065','2070','2075','2080','2085','2090','2095','2100']
#先把第一个有值年份之前的空值全部用对应行后面的首个非空值填充
for i in range(0,len(val_years)):
    for na_year in years:
        if int(na_year) <= int(val_years[0]):
            CO2E_df[na_year].fillna(CO2E_df[val_years[i]], inplace=True)
        if int(na_year) > int(val_years[0]):
            break
#此时val_years[0]那年已全部非空，再从此向后不断填充，每到一个新的val_years年份就换成这年的数据作为新的填充依据
i = 0
for na_year in years:
    if int(na_year) <= int(val_years[0]):
        continue
    if i < len(val_years)-1:
        if (int(na_year) > int(val_years[i])) & (int(na_year) <= int(val_years[i+1])):
            CO2E_df[na_year].fillna(CO2E_df[val_years[i]], inplace=True)
        if int(na_year) > int(val_years[i+1]):
            i = i+1
            CO2E_df[na_year].fillna(CO2E_df[val_years[i]], inplace=True)
    else:
        CO2E_df[na_year].fillna(CO2E_df[val_years[i]], inplace=True)
CO2E_df.to_csv(path3+"\\"+scenario+"\\CO2E_tmp.csv")


# Step4:
# 针对xls，将GHG排放和LUC-CO2合并存成1个df
LUC_df = pd.read_excel(xlsfile, sheet_name="Sheet10",header=1)
LUC_df.rename(columns={'title':'GHG'}, inplace=True)
LUC_df['GHG'] = LUC_df['GHG'].str.replace('LUC emissions by region','CO2_LUC')
GHG_df = pd.concat([nonCO2_df,LUC_df], axis=0, join='inner').reset_index(drop=True)
# drop掉非21种GHG的行
GHG_df = GHG_df[GHG_df['GHG'].isin(GHGs)].reset_index(drop=True)
GHG_df.to_csv(path3+"\\"+scenario+"\\GHG_tmp.csv")
# 处理GHG_df中输出未涵盖全部21种GHG的问题
check_df = pd.merge(GHG_df,CO2E_df, on=['region','GHG'], how='right')
check_df.fillna(0, inplace=True)
GHGnew_df = check_df.loc[:,'region':'2100_x']
GHGnew_df.rename(columns={'1990_x':'1990','2005_x':'2005','2010_x':'2010','2015_x':'2015',
'2020_x':'2020','2025_x':'2025','2030_x':'2030','2035_x':'2035','2040_x':'2040','2045_x':'2045',
'2050_x':'2050','2055_x':'2055','2060_x':'2060','2065_x':'2065','2070_x':'2070','2075_x':'2075',
'2080_x':'2080','2085_x':'2085','2090_x':'2090','2095_x':'2095','2100_x':'2100'},inplace=True)
GHGnew_df.to_csv(path3+"\\"+scenario+"\\GHG_new.csv")


# Step5:
# 确保两个df的行排序完全对应，相乘即可得到MtCO2当量表
regghg_df = GHGnew_df.loc[:,'region':'GHG']
result_df = GHGnew_df.loc[:,'1990':'2100'] * CO2E_df.loc[:,'1990':'2100']
result_df = pd.concat([regghg_df,result_df],axis=1)
result_df.to_csv(path3+"\\"+scenario+"\\result_raw.csv")
#然后按区域分组后加总（1.Gross：无LUC； 2.Net：含LUC）
result_NetGHG = result_df.groupby('region').agg('sum')
result_NetGHG.to_csv(path3+"\\"+scenario+"\\result_NetGHG.csv")

result_GrossGHG = result_df[~(result_df['GHG'] == "CO2_LUC")]
result_GrossGHG = result_GrossGHG.groupby('region').agg('sum')
result_GrossGHG.to_csv(path3+"\\"+scenario+"\\result_GrossGHG.csv")


# Step6:
# 用全取到实际值的CO2-e计算GHG的CO2当量
CO2E_v2 = pd.read_csv(path3+"\\CO2E_v2.csv") 
regghg_df = GHGnew_df.loc[:,'region':'GHG']
result_v2_df = GHGnew_df.loc[:,'1990':'2100'] * CO2E_v2.loc[:,'1990':'2100']
result_v2_df = pd.concat([regghg_df,result_v2_df],axis=1)
result_v2_df.to_csv(path3+"\\"+scenario+"\\result_v2_raw.csv")
#然后按区域分组后加总（1.Gross：无LUC； 2.Net：含LUC）
result_v2_NetGHG = result_v2_df.groupby('region').agg('sum')
result_v2_NetGHG.to_csv(path3+"\\"+scenario+"\\result_v2_NetGHG.csv")

result_v2_GrossGHG = result_v2_df[~(result_v2_df['GHG'] == "CO2_LUC")]
result_v2_GrossGHG = result_v2_GrossGHG.groupby('region').agg('sum')
result_v2_GrossGHG.to_csv(path3+"\\"+scenario+"\\result_v2_GrossGHG.csv")