
library(sf)
library(here) #attention: here() argument caused by the same name for profiles  
st_layers(here("gadm41_AUS.gpkg")) #查看这个文件里有那些图层
Ausoutline <- st_read(here("gadm41_AUS.gpkg"), 
                      layer='ADM_ADM_0') #读取之后发现是Austrilia的边界
print(Ausoutline)
st_crs(Ausoutline)$proj4string #提取目标对象的坐标系统
# instructions below to transform the origion CRS to  Aus local..
AusoutlinePROJECTED <- Ausoutline %>%
  st_transform(.,3112)
print(AusoutlinePROJECTED)

#From sf to sp
AusoutlineSP <- Ausoutline %>%
  as(., "Spatial")

#From sp to sf
AusoutlineSF <- AusoutlineSP %>%
  st_as_sf()
# 处理raster数据（i.e.栅格化数据）
library(raster)
library(sp)
library(terra)
Jul<-terra::rast("D:\\study\\CASA_GISS\\week3\\new_project\\wc2.1_5m_prec\\wc2.1_5m_prec_07.tif")
# have a look at the raster layer Jul
Jul
plot(Jul) # maybe the default projection is Mercator
# set the proj 4 to a new object
#project the raster data into CRS using PROJ.4
pr1 <- terra::project(Jul, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(pr1)
# "+proj=moll" means using Mollweide projection, "+lon_0=0, x_0=0,etc.to define the Greenwich longitude is 0, setting the origion of coordinate sysytem is 0(0 is basically default)
pr1 <- pr1 %>%
  terra::project(., "+proj=longlat +datum=WGS84 +no_defs +type=crs")
plot(pr1) #back to Mercator
## new scheme
# look in our folder, find the files that end with .tif and 
install.packages("fs")
library(fs)
dir_info("wc2.1_5m_prec") #lookup must be a pathway on your computer,not be a doc.
dir_info("D:\\study\\CASA_GISS\\week3\\new_project") 
# both 2 above dir_info() gets the details of files
library(tidyverse)
listfiles<-dir_info("wc2.1_5m_prec/") %>%
  filter(str_detect(path, ".tif")) %>%
  dplyr::select(path)%>%
  pull()

#have a look at the file names 
listfiles
worldclimtemp <- listfiles %>%
  terra::rast()

#have a look at the raster stack
worldclimtemp
# access the July layer
worldclimtemp[[7]] ##_Python starting from 0, but R with 1!!_

month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

names(worldclimtemp) <- month
worldclimtemp$Jul # $ for extracting elements(or columns) already named in lists 
# make a new datasets
site <- c("Brisbane", "Melbourne", "Perth", "Sydney", "Broome", "Darwin", "Orange", 
              "Bunbury", "Cairns", "Adelaide", "Gold Coast", "Canberra", "Newcastle", 
              "Wollongong", "Logan City" )
lon <- c(153.03, 144.96, 115.86, 151.21, 122.23, 130.84, 149.10, 115.64, 145.77, 
         138.6, 153.43, 149.13, 151.78, 150.89, 153.12)
lat <- c(-27.47, -37.91, -31.95, -33.87, 17.96, -12.46, -33.28, -33.33, -16.92, 
         -34.93, -28, -35.28, -32.93, -34.42, -27.64)
#Put all of this inforamtion into one list 
samples <- data.frame(site, lon, lat, row.names="site")
# row.names="sites" is to make the site containing Austrilia city names column to a row 
# Extract the data from the Rasterstack for all points 
AUcitytemp<- terra::extract(worldclimtemp, samples)
Aucitytemp2 <- AUcitytemp %>% 
  as_tibble()%>%  #convert data.frame into a new type: tibble
  add_column(Site = site, .before = "Jan") # add a new column before the Jan cloumn
Perthtemp <- Aucitytemp2 %>%
  filter(site=="Perth")
Perthtemp <- Aucitytemp2[3,]
hist(as.numeric(Perthtemp)) #hist(): create a histogram
library(tidyverse)
#define where you want the breaks in the historgram
userbreak <- c(0, 25, 50, 75, 100, 125, 150, 175, 200) #将数据以25为单位分隔开
# remove the ID and site columns
Perthtemp <- Aucitytemp2 %>%
  filter(site == "Perth")

t <- Perthtemp %>%
  dplyr::select(Jan:Dec)
range(unlist(as.numeric(t))) #测定t的范围 unlist是把一个numeric的list合并到一个向量里，方便range()测算出范围

# 使用 unlist() 转换数据框为向量
hist(unlist(as.numeric(t)),  #hist() to plot a histogram
     breaks = userbreak, 
     col = "red", 
     main = "Histogram of Perth Temperature", 
     xlab = "Temperature", 
     ylab = "Frequency")
histinfo <- t %>% 
  as.numeric() %>%   # 仅转换一次即可
  hist(.)
histinfo
# plotting temp distribution for Austrilia
plot(Ausoutline$geom)
AusoutSIMPLE <- Ausoutline %>%
  st_simplify(., dTolerance = 1000) %>% # dTolerances how blunt it is
  st_geometry()%>%
  plot()
# instructions above to simplify the .shp
# # st_geometry() is to keep the arguments of boundries only
print(Ausoutline)
#this works nicely for rasters
crs(worldclimtemp) #review the CRS of this data
Austemp <- Ausoutline %>%
  # now crop our temp data to the extent
  terra::crop(worldclimtemp,.) #tailor the Austria part from worldclimtemp
# plot the output
plot(Austemp)
exactAus<-terra::mask(Austemp, Ausoutline)
#subset using the known location of the raster
hist(exactAus[[3]], col="red", main ="March temperature")
# histogram with ggplot
exactAusdf <- exactAus %>% # transfrom raster data into data.frame otherwise incompatible
  as.data.frame()
library(ggplot2)
# set up the basic histogram
gghist <- ggplot(exactAusdf, 
                 aes(x=Mar)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Australian March temperatures", 
       x="Temperature", 
       y="Frequency")+
  scale_x_continuous(breaks=seq(min(exactAusdf$Mar), max(exactAusdf$Mar), by=100))+
  xlim(0,400)

# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(Mar, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))
# put multiple data into one plot
squishdata<-exactAusdf%>%
  pivot_longer(
    cols = 1:12,
    names_to = "Month",
    values_to = "Temp"
  )
twomonths <- squishdata %>%
  # | = OR
filter(., Month=="Jan" | Month=="Jun")
meantwomonths <- twomonths %>%
  group_by(Month) %>%
  summarise(mean=mean(Temp, na.rm=TRUE))

meantwomonths
ggplot(twomonths, aes(x=Temp, color=Month, fill=Month)) +
  geom_histogram(position="identity", alpha=0.5)+
  geom_vline(data=meantwomonths, 
             aes(xintercept=mean, 
                 color=Month),
             linetype="dashed")+
  labs(title="Ggplot2 histogram of Australian Jan and Jun
       temperatures",
       x="Temperature",
       y="Frequency")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))
data_complete_cases <- squishdata %>%
  drop_na()%>% 
  mutate(Month = factor(Month, levels = c("Jan","Feb","Mar",
                                          "Apr","May","Jun",
                                          "Jul","Aug","Sep",
                                          "Oct","Nov","Dec")))

# Plot faceted histogram
ggplot(data_complete_cases, aes(x=Temp, na.rm=TRUE))+
  geom_histogram(color="black", binwidth = 5)+
  labs(title="Ggplot2 faceted histogram of Australian temperatures", 
       x="Temperature",
       y="Frequency")+
  facet_grid(Month ~ .)+
  theme(plot.title = element_text(hjust = 0.5))
library(plotly)
# split the data for plotly based on month

jan <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jan")

jun <- squishdata %>%
  drop_na() %>%
  filter(., Month=="Jun")

# give axis titles
x <- list (title = "Temperature")
y <- list (title = "Frequency")

# set the bin width
xbinsno<-list(start=0, end=40, size = 2.5)

# plot the histogram calling all the variables we just set
ihist<-plot_ly(alpha = 0.6) %>%
  add_histogram(x = jan$Temp,
                xbins=xbinsno, name="January") %>%
  add_histogram(x = jun$Temp,
                xbins=xbinsno, name="June") %>% 
  layout(barmode = "overlay", xaxis=x, yaxis=y)

ihist
