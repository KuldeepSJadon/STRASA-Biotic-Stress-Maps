library(cartography)
library(rgdal)

# Load data
# GAUL Level 2 country layer (FAO)
gaul <- readOGR(dsn = "/Users/asparks/Google Drive/Data/gaul/g2015_2014_0/",
                layer = "g2015_2014_0")
# thin GAUL dataset for background plotting only. Speed up process. Not used for SA
gaul <- thinnedSpatialPoly(gaul, tolerance = 0.2, minarea = 0.01)

# This is the shapefile that is used for plotting the disease severity results
SA <- readOGR(dsn = "/Users/asparks/Google Drive/Data/gaul/g2015_2014_2/BGD_IND_NPL",
              layer = "BGD_IND_NPL")
# original GAUL unit layers are available from FAO:
#http://data.fao.org/map?entryId=f7e7adb0-88fd-11da-a88f-000d939bc5d8

Mod_IND_BS <- read.csv("csv files/Modified_IND_BS.csv")

# set margins
opar <- par(mar = c(0, 0, 1.2, 0))

# Layout plot
layoutLayer(title = "Bacterial Blight Severity as Predicted by EPIRICE for BGD, IND and NPL", # title of the map
            author = "Adam H. Sparks, IRRI, CESD",
            sources = "FAO GAUL, IRRI Interpolated GSOD Weather, EPIRICE Model",
            scale = NULL, # no scale
            col = NA, # no color for the title box
            coltitle = "black", # color of the title
            frame = TRUE,  # no frame around the map
            bg = "#A6CAE0", # background of the map
            extent = SA) # set the extent of the map

# Non-South Asia Space
plot(gaul, col  = "#E3DEBF", border = NA, add = TRUE)

choroLayer(spdf = SA, # SpatialPolygonsDataFrame of the regions
           df = Mod_IND_BS, # data frame with compound annual growth rate
           var = "BS_Modified", # compound annual growth rate field in df
           breaks = c("Low", "Moderately Low", "Moderate", "Moderately Severe", "Severe"), # list of breaks
           col = cols, # colors
           border = "grey40", # color of the polygons borders
           lwd = 0.5, # width of the borders
           legend.pos = "right", # position of the legend
           legend.title.txt = "Brown Spot Severity", # title of the legend
           add = TRUE) # add the layer to the current plot

# Add an explanation text
text(x = 5477360, y = 4177311, labels = "The 10 most populated countries of Europe
Total population 2008, in millions of inhabitants.", cex = 0.7, adj = 0)
