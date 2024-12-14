import os
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

"""
# 1. the reference scenario (Current-Con) total water demand
path = "D:\\demeter\\demeter\\example\\demeter_v1.2.0_data_supplement\\outputs\\Current-Con_2022-04-07_00h11m31s"  # change folder scenario
filedir = path+"\\spatial_landcover_tabular"
filelist = os.listdir(filedir)
for csv in filelist:
    if ".csv" not in csv:
        continue
    #split file name to get year
    landcover, yr, timestep = csv.split("_")
    #if yr not in ["2015", "2030", "2060", "2100"]:
        continue
    #read the csv
    data = np.loadtxt(open(filedir+"\\"+csv,"rb"),delimiter=",",skiprows=1)

    #create empty np as maps for landuses
    map_forest = np.zeros((180*12,360*12))
    map_crop = np.zeros((180*12,360*12))

    #process the csv data, convert into gridded map (2-D matrix)
    for i in range(0,len(data)):
        lat = data[i,0]
        lon = data[i,1]
        ilat = (90*12-1) - round((lat - 0.0417112)/0.0833333)  # 0.0417112
        ilon = (180*12-1) + round((lon + 0.0417386)/0.0833333)  # -0.0417386   
        map_forest[ilat,ilon] = data[i,5]
        map_crop[ilat,ilon] = data[i,11]

    #forest
    #check path
    subpath = path+"\\raster\\forest"
    if os.path.exists(subpath) is False:
        os.makedirs(subpath)
    #export map as figure
    map_forest = pd.DataFrame(map_forest)
    plt.figure(dpi=300,figsize=(18,9))
    plt.axis('off')
    fig = sns.heatmap(data=map_forest,cmap="Greens",cbar=False)
    getfig = fig.get_figure()
    getfig.savefig(path+"\\raster\\forest\\forest_ref_"+yr+".png")

    #crop
    #check path
    subpath = path+"\\raster\\crop"
    if os.path.exists(subpath) is False:
        os.makedirs(subpath)
    #export map as figure
    map_crop = pd.DataFrame(map_crop)
    plt.figure(dpi=300,figsize=(18,9))
    plt.axis('off')
    fig = sns.heatmap(data=map_crop,cmap="YlOrBr",cbar=False)
    getfig = fig.get_figure()
    getfig.savefig(path+"\\raster\\crop\\crop_ref_"+yr+".png")    
"""

# 2. difference between scenarios
# after checking reference scenario, narrow down outputing maps of interest
dif = "Policy"  #change what type of difference between scenario is of interest
path_main = "D:\\demeter\\demeter\\example\\demeter_v1.2.0_data_supplement\\outputs"
path_ref = path_main + "\\Current-Con_2022-04-07_00h11m31s"  #change ref scenario
path_scen = path_main + "\\Updated-Incr_2022-03-23_18h02m51s"  # change folder scenario
for yr in ["2030","2060","2100"]:
    #read the csvs
    data_ref = np.loadtxt(open(path_ref+"\\spatial_landcover_tabular\\landcover_"+yr+"_timestep.csv","rb"),delimiter=",",skiprows=1)
    data_scen = np.loadtxt(open(path_scen+"\\spatial_landcover_tabular\\landcover_"+yr+"_timestep.csv","rb"),delimiter=",",skiprows=1)

    #create empty np as maps for landuses
    map_forest = np.zeros((180*12,360*12))
    map_crop = np.zeros((180*12,360*12))
    #map_water = np.zeros((180*12,360*12))
    #map_grass = np.zeros((180*12,360*12))

    #process the csv data, convert into gridded map (2-D matrix)
    for i in range(0,len(data_ref)):
        lat = data_ref[i,0]
        lon = data_ref[i,1]
        ilat = (90*12-1) - round((lat - 0.0417112)/0.0833333)  # 0.0417112
        ilon = (180*12-1) + round((lon + 0.0417386)/0.0833333)  # -0.0417386   

        if(data_ref[i,5]>0):
            map_forest[ilat,ilon] = 100*(data_scen[i,5] - data_ref[i,5]) / data_ref[i,5]
        else:
            if(data_scen[i,5]==0):
                map_forest[ilat,ilon] = 0
            else:
                map_forest[ilat,ilon] = 100
        
        if(data_ref[i,11]>0):
            map_crop[ilat,ilon] = 100*(data_scen[i,11] - data_ref[i,11]) / data_ref[i,11]
        else:
            if(data_scen[i,11]==0):
                map_crop[ilat,ilon] = 0
            else:
                map_crop[ilat,ilon] = 100
        #map_water[ilat,ilon] = data_scen[i,4] - data_ref[i,4]
        #map_grass[ilat,ilon] = data_scen[i,7] - data_ref[i,7]

    #forest
    #check path
    subpath = path_scen+"\\raster\\new\\"+dif+"\\forest"
    if os.path.exists(subpath) is False:
        os.makedirs(subpath)
    #export map as figure
    map_forest = pd.DataFrame(map_forest)
    map_forest = map_forest.iloc[360:900,3000:3840]
    plt.figure(dpi=300,figsize=(14,11))
    plt.axis('off')
    fig = sns.heatmap(data=map_forest,vmin=-50,vmax=50,cmap="bwr",cbar=False)
    getfig = fig.get_figure()
    getfig.savefig(subpath+"\\forest_dif_"+dif+"_"+yr+".png")

    #crop
    #check path
    subpath = path_scen+"\\raster\\new\\"+dif+"\\crop"
    if os.path.exists(subpath) is False:
        os.makedirs(subpath)
    #export map as figure
    map_crop = pd.DataFrame(map_crop)
    map_crop = map_crop.iloc[360:900,3000:3840]
    plt.figure(dpi=300,figsize=(14,11))
    plt.axis('off')
    fig = sns.heatmap(data=map_crop,vmin=-50,vmax=50,cmap="bwr",cbar=False)
    getfig = fig.get_figure()
    getfig.savefig(subpath+"\\crop_dif_"+dif+"_"+yr+".png")
    
    """
    #grass
    #check path
    subpath = path_scen+"\\raster\\"+dif+"\\grass"
    if os.path.exists(subpath) is False:
        os.makedirs(subpath)
    #export map as figure
    map_grass = pd.DataFrame(map_grass)
    plt.figure(dpi=300,figsize=(18,9))
    plt.axis('off')
    fig = sns.heatmap(data=map_grass,vmin=-0.2,vmax=0.2,cmap="RdBu",cbar=False)
    getfig = fig.get_figure()
    getfig.savefig(subpath+"\\grass_dif_"+dif+"_"+yr+".png")

    #water
    #check path
    subpath = path_scen+"\\raster\\"+dif+"\\water"
    if os.path.exists(subpath) is False:
        os.makedirs(subpath)
    #export map as figure
    map_water = pd.DataFrame(map_water)
    plt.figure(dpi=300,figsize=(18,9))
    plt.axis('off')
    fig = sns.heatmap(data=map_water,vmin=-0.2,vmax=0.2,cmap="RdBu")
    getfig = fig.get_figure()
    getfig.savefig(subpath+"\\water_dif_"+dif+"_"+yr+".png")
    """