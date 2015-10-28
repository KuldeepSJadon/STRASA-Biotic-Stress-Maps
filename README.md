# STRASA
This repository hosts R scripts for generating maps of biotic stresses for the Stress-Tolerant Rice for Africa and South Asia (STRASA) project. These files are generated from outputs of the EPIRICE model (Savary et al. 2012) in conjunction with the FAO's Global Administrative Unit Layers (GAUL; 2015-2014) data set.

To use these data, you will need to download the GAUL dataset, GAUL - Second level - Reference year 2014, http://www.fao.org/geonetwork/srv/en/metadata.show?id=12691&currTab=simple.

The "Data" folder contains GeoTiff files from EPIRICE with AUDPC values to be extracted at district level using GAUL data.

The "csv files" folder contains csv files that can be used with the GAUL data set for making further maps in R or another GIS software like ArcGIS or QGIS. The BB_BS_LB_Maps.R script uses these for such a purpose.

See http://strasa.irri.org for more on the STRASA Project itself.
