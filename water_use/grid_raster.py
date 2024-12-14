import os
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

"""
# 0 a testing figure to get colorbar
path1 = "D:\\tethys\\example\\Output\\ec_Updated-Incr"
data_scen = np.loadtxt(open(path1+"\\wdtotal_km3peryr.csv","rb"),delimiter=",",skiprows=1)

path2 = "D:\\tethys\\example\\Output\\ec_Current-Con"
data_ref = np.loadtxt(open(path2+"\\wdtotal_km3peryr.csv","rb"),delimiter=",",skiprows=1)

path3 = "D:\\dyf毕设\\毕设\\论文写作\\可视化——自己作图\\tethys\\test-colorbar"

yr = 2060
map_ref = np.zeros((360,720))
map_dif = np.zeros((360,720))

for i in range(0,len(data_scen)):
    ilon = data_scen[i,3]
    ilat = data_scen[i,4]
    map_ref[360-int(ilat),int(ilon)-1] = data_ref[i,16]
    map_dif[360-int(ilat),int(ilon)-1] = data_scen[i,16] - data_ref[i,16]
    

map_ref = pd.DataFrame(map_ref)
map_dif = pd.DataFrame(map_dif)
    
plt.figure(dpi=150,figsize=(18,9))
plt.axis('off')

fig = sns.heatmap(data=map_ref,vmin=0,vmax=0.5,cmap="Reds")
getfig = fig.get_figure()
getfig.savefig(path3+"\\wdtotal_ref_"+str(yr)+".png")

fig = sns.heatmap(data=map_dif,vmin=-50,vmax=50,cmap="bwr")
getfig = fig.get_figure()
getfig.savefig(path3+"\\wdtotal_dif_policy_"+str(yr)+".png")

"""
"""
# 1 the reference scenario (Current-Con) total water demand
path = "D:\\tethys\\example\\Output\\ec_Current-Con"
data = np.loadtxt(open(path+"\\wdtotal_km3peryr.csv","rb"),delimiter=",",skiprows=1)


#for k in range(5,data.shape[1]):
for k in [7, 10, 16, 24]:

    yr = 2000 + 5*(k-4)
    map = np.zeros((360,720))

    for i in range(0,len(data)):
        ilon = data[i,3]
        ilat = data[i,4]
        value = data[i,k]
        map[360-int(ilat),int(ilon)-1] = value
    
    subpath = path+"\\raster\\new"
    if os.path.exists(subpath) is False:
        os.makedirs(subpath)

    map = pd.DataFrame(map)
    map = map.iloc[60:150,500:640]

    map.to_csv(subpath+"\\wdtotal_ref_"+str(yr)+".csv")
    
    plt.figure(dpi=150,figsize=(14,11))
    plt.axis('off')
    fig = sns.heatmap(data=map,vmin=0,vmax=0.5,cmap="Reds",cbar=False)
    getfig = fig.get_figure()
    getfig.savefig(subpath+"\\wdtotal_ref_"+str(yr)+".png")

"""

#"""
# 2 the scenarios' differences in total water demand
dif = "CI"
path1 = "D:\\tethys\\example\\Output\\ec_Current-Con_CI_HadGEM2-ES" # 改scen
data_scen = np.loadtxt(open(path1+"\\wdtotal_km3peryr.csv","rb"),delimiter=",",skiprows=1)

path2 = "D:\\tethys\\example\\Output\\ec_Current-Con" # 改ref
data_ref = np.loadtxt(open(path2+"\\wdtotal_km3peryr.csv","rb"),delimiter=",",skiprows=1)

for k in [7, 10, 16, 24]:

    yr = 2000 + 5*(k-4)
    map = np.zeros((360,720))

    for i in range(0,len(data_scen)):
        ilon = data_scen[i,3]
        ilat = data_scen[i,4]
        if(data_ref[i,k]>0):
            value = 100*(data_scen[i,k] - data_ref[i,k]) / data_ref[i,k]
        else:
            if(data_scen[i,k]==0):
                value = 0
            else:
                value = 100
        map[360-int(ilat),int(ilon)-1] = value
    
    subpath = path1+"\\raster\\new\\"+dif
    if os.path.exists(subpath) is False:
        os.makedirs(subpath)

    map = pd.DataFrame(map)
    map = map.iloc[60:150,500:640]

    map.to_csv(subpath+"\\wdtotal_dif_"+dif+"_"+str(yr)+".csv")
    
    plt.figure(dpi=150,figsize=(14,11))
    plt.axis('off')
    fig = sns.heatmap(data=map,vmin=-50,vmax=50,cmap="bwr",cbar=False)
    getfig = fig.get_figure()
    getfig.savefig(subpath+"\\wdtotal_dif_"+dif+"_"+str(yr)+".png")
#"""
