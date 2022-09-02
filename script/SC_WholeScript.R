
# GitHub repo with associated datasets: 
# https://github.com/BjnNowak/PhotosynteticActivity 

# You do not have to download the .csv files, which can be loaded directly with their URL 
# The shapefiles in the "map" folder must be downloaded to create Figure 2

# Clean memory
gc()
rm(list = ls())

# Load packages
###############
# Data manipulation:
library(tidyverse)
library(lubridate)
# Plot customization:
library(ggridges)
library(camcorder)
library(ggtext)
library(patchwork)
library(ggtext)
library(glue)
library(showtext)
# Maps:
library(sf)
library(ggspatial) # for North Arrow
library(raster)

# Load fonts
############
font_add_google("Oswald", "oswald")
font_add_google("Fira Sans", "fira")
font_add_google("Fira Sans Condensed", "fira con")
# Automatically use {showtext} for plots
showtext_auto()

################################################################################################

# Parameters
############
# Define 1/1/2018 as first_day
first_day <- round(as.numeric(as.POSIXct("01/01/2018", format="%m/%d/%Y"))/86400)-1
# Fraction of active photosynthetic radiation
f<-0.48
# Number of plots
nb_plots<-28124

# Functions
###########
# Function to convert NDVI data from "wide" to "long" format:
fun_long <- function(data){
  
  # Set column names for start/end
  n<-colnames(data)
  start<-n[4]
  end<-n[ncol(data)]
  
  # Convert using gather() 
  data_long <- tidyr::gather(
    data=data,            # data object
    key=plot,             # Name of new key column (made from names of data columns)
    value=ndvi,           # Name of new value column
    start:end,            # Names of source columns that contain values
    factor_key=TRUE       # Treat the new key column as a factor (instead of character vector)
  )%>%
    mutate(date = make_date(Year,Month,Day))%>%
    mutate(my_date = as.POSIXct(date, format="%m/%d/%Y"))%>%
    # Convert date object to number of seconds since 1/1/1970
    mutate(my_date_seconds = as.numeric(my_date))%>%
    # Convert date object to number of days since 1/1/2018
    mutate(day_num = round(as.numeric(my_date_seconds) / 86400)-first_day)%>%
    dplyr::select(date,day_num,plot,ndvi)
  
  return(data_long)
}

# Almost the same function to convert the radiation file from "wide" to "long" format:
fun_long_rad <- function(data){
  
  # Set column names for start/end
  n<-colnames(data)
  start<-n[4]
  end<-n[ncol(data)]
  
  # Convert using gather() 
  data_long <- tidyr::gather(
    data=data,            # Data object
    key=com,              # Name of new key column (made from names of data columns)
    value=radiation,      # Name of new value column
    start:end,            # Names of source columns that contain values
    factor_key=TRUE       # Treat the new key column as a factor (instead of character vector)
  )%>%
    mutate(date = make_date(Year,Month,Day))%>%
    mutate(my_date = as.POSIXct(date, format="%m/%d/%Y"))%>%
    # Convert date object to number of seconds since 1/1/1970
    mutate(my_date_seconds = as.numeric(my_date))%>%
    # Convert date object to number of days since 1/1/2018
    mutate(day_num = round(as.numeric(my_date_seconds) / 86400)-first_day)%>%
    dplyr::select(date,day_num,com,radiation)
  
  return(data_long)
}



##################################################################################

# Load data
###########

# All files here have been extracted from the Google Earth Engine, using various data sources

# NDVI evolution for the study area (Data source: Sentinel-2)
#############################################################
# Those files are quite wide (since each column is a plot) 
# so we will first increase the connection buffer to be able to use the URL of each file.
# You do not have to do this if you work locally.
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2) # Double default buffer size

# 2018 (two months of NDVI values for each plot in each file)
#NDVI_2018_63_0102 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2018_63_0102.csv')
#NDVI_2018_63_0304 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2018_63_0304.csv')
#NDVI_2018_63_0506 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2018_63_0506.csv')
#NDVI_2018_63_0708 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2018_63_0708.csv')
#NDVI_2018_63_0910 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2018_63_0910.csv')
#NDVI_2018_63_1112 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2018_63_1112.csv')

# 2019
#NDVI_2019_63_0102 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2019_63_0102.csv')
#NDVI_2019_63_0304 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2019_63_0304.csv')
#NDVI_2019_63_0506 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2019_63_0506.csv')
#NDVI_2019_63_0708 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2019_63_0708.csv')
#NDVI_2019_63_0910 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2019_63_0910.csv')
#NDVI_2019_63_1112 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2019_63_1112.csv')

# 2020
#NDVI_2020_63_0102 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2020_63_0102.csv')
#NDVI_2020_63_0304 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2020_63_0304.csv')
#NDVI_2020_63_0506 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2020_63_0506.csv')
#NDVI_2020_63_0708 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2020_63_0708.csv')
#NDVI_2020_63_0910 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2020_63_0910.csv')
#NDVI_2020_63_1112 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/NDVI_2020_63_1112.csv')

# Solar radiation at municipality scale (Data source: ERA5)
rad<-read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/StablePlots63_Radiation_Com_SurfSolarRad_hourly.csv')

# Reference table to assign each plot to a municipality (Data source: INSEE)
com<-read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/RPG_INSEE_63_StablePlots.csv')

# Plot features (surface in ha and crop for each year) (Data source: French RPG)
feat <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/RPG_63_StablePlots_18to20.csv')%>%
  mutate(plot=ID_PARCEL) # New column with different name for easier merging with other tables

# Soil organic content at various depths for each plot (Data source: SoilGrid)
SOC_SG<-read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/data/SOC_StablePlots_63_AllDepths.csv')%>%
  dplyr::select(ID_PARCEL,soc_0_5cm,soc_5_15cm,soc_15_30cm)

# Data for maps (Figure 2)
##########################
DEM63 <- read_csv('https://raw.githubusercontent.com/BjnNowak/PhotosynteticActivity/main/map/DEM63.csv')
 
##### Shapefile: to download in the 'Map' folder ###########################
# URL: https://github.com/BjnNowak/PhotosynteticActivity/tree/main/map #####
france <- read_sf(dsn = "Map", layer="france")
com63 <- read_sf(dsn = "Map", layer="COM63")%>%
  filter(as.numeric(INSEE_COM)>62999) %>%
  filter(as.numeric(INSEE_COM)<64000)

###########################################################################################""

# Data cleaning / preparation
#############################

# Convert NDVI files from wide to long
#NDVI_2018_63 <- fun_long(NDVI_2018_63_0102)%>%bind_rows(fun_long(NDVI_2018_63_0304))%>%bind_rows(fun_long(NDVI_2018_63_0506))%>%bind_rows(fun_long(NDVI_2018_63_0708))%>%bind_rows(fun_long(NDVI_2018_63_0910))%>%bind_rows(fun_long(NDVI_2018_63_1112))
#NDVI_2019_63 <- fun_long(NDVI_2019_63_0102)%>%bind_rows(fun_long(NDVI_2019_63_0304))%>%bind_rows(fun_long(NDVI_2019_63_0506))%>%bind_rows(fun_long(NDVI_2019_63_0708))%>%bind_rows(fun_long(NDVI_2019_63_0910))%>%bind_rows(fun_long(NDVI_2019_63_1112))
#NDVI_2020_63 <- fun_long(NDVI_2020_63_0102)%>%bind_rows(fun_long(NDVI_2020_63_0304))%>%bind_rows(fun_long(NDVI_2020_63_0506))%>%bind_rows(fun_long(NDVI_2020_63_0708))%>%bind_rows(fun_long(NDVI_2020_63_0910))%>%bind_rows(fun_long(NDVI_2020_63_1112))


# As you may have notice, the NDVI data cleaning take some time. If you plan to use this script 
# several times, you may spare some time by saving the files and loading them locally for next use:

#write_csv(NDVI_2019_63,"StablePlots/NDVI_2019_63.csv")
#write_csv(NDVI_2018_63,"StablePlots/NDVI_2018_63.csv")
#write_csv(NDVI_2020_63,"StablePlots/NDVI_2020_63.csv")

NDVI_2018_63<-read_csv("StablePlots/NDVI_2018_63.csv")
NDVI_2019_63<-read_csv("StablePlots/NDVI_2019_63.csv")
NDVI_2020_63<-read_csv("StablePlots/NDVI_2020_63.csv")

# Convert solar radiation file from wide to long format
rad_long <- fun_long_rad(rad)
# Set municipality ID as factor in reference plot table
com_fact <- com%>%
  mutate(com=as.factor(belongsTo))%>%
  dplyr::select(com,plot=pointID)
# Full join to get daily radiation for each plot
rad_full <- rad_long%>%
  full_join(com_fact)%>%
  dplyr::select(-day_num) # Remove day number

##########################################################################################"

# Photosynthesis indicators calculation
#######################################

# Getting one NDVI value per plot and per day
data_full <- NDVI_2018_63 %>%
  bind_rows(NDVI_2019_63)%>%
  bind_rows(NDVI_2020_63)%>%
  drop_na(ndvi)%>%
  # Complete with missing days (no NDVI observation)
  complete(date = seq.Date(as.Date('2018-01-01'), max(as.Date('2020-12-31')), by="day"),plot)%>%
  group_by(plot)%>%
  distinct(date, .keep_all = TRUE)%>%
  arrange(date)%>%
  # Fill missing days with closer NDVI observation in a "downup" fashion
  fill(ndvi, .direction = "downup")%>%
  ungroup()%>%
  mutate(my_date = as.POSIXct(date, format="%m/%d/%Y"))%>%
  # Convert date object to number of seconds since 1/1/1970
  mutate(my_date_seconds = as.numeric(my_date))%>%
  # Convert date object to number of days since 1/1/2018
  mutate(day_num = round(as.numeric(my_date_seconds) / 86400)-first_day)%>%
  dplyr::select(date,day_num,plot,ndvi)%>%
  mutate(plot=as.numeric(as.character(plot)))%>%
  # Add crop information for each year in a "tidy" format
  left_join(feat,by=c("plot"))%>%
  dplyr::select(-ID_PARCEL)%>%
  drop_na()%>%
  mutate(crop=case_when(
    year(date)==2018~CODE_CULTU_2018,
    year(date)==2019~CODE_CULTU_2019,
    year(date)==2020~CODE_CULTU_2020
  ))%>%
  mutate(year=as.factor(year(date)))

# Adding one radiation value par day and per plot
data_rad <- data_full%>%
  left_join(rad_full)

# Computing the indicators
data_frac <- data_rad %>%
  group_by(plot)%>%
  mutate(
    min_ndvi = min(ndvi),
    max_ndvi = max(ndvi)
  )%>%
  ungroup(plot)%>%
  mutate(
    min_ndvi = case_when(
      (min_ndvi<0.3)&(min_ndvi>0.05)~min_ndvi,
      TRUE~0.15
    ),
    max_ndvi = case_when(
      (max_ndvi<0.9)&(max_ndvi>0.65)~max_ndvi,
      TRUE~0.75
    ),
    ndvi = case_when(
      ndvi>max_ndvi~max_ndvi,
      TRUE~ndvi
    ),
    scaled_ndvi = (ndvi-min_ndvi)/(max_ndvi-min_ndvi),
    
    # Daily fractional soil cover for each plot
    frac = scaled_ndvi*scaled_ndvi,
    # Daily PAR for each plot 
    par = f*radiation,
    # Daily APAR for each plot
    apar = frac*par
  )
  
######################################################################

# Data analysis
###############

# Whole territory estimation
############################
territory_estimation <- data_frac %>%
  summarize(
    plot_surf=SURF_PARC[1],
    # Fractional vegetation cover
    frac_mean=mean(frac),
    # APAR (in MJ per m2 per year)
    apar_year=(sum(na.omit(apar))/(3*1000000))/nb_plots,
    # Ratio APAR/PAR
    rapar=sum(na.omit(apar))/sum(na.omit(par))
  )
territory_estimation

# Per municipality
##################
data_crop_com <- data_frac%>%
  group_by(com)%>%
  summarize(
    frac_mean=mean(na.omit(frac)),
    rapar=sum(na.omit(apar))/sum(na.omit(par))
  )%>%
  ungroup()%>%
  mutate(INSEE_COM=as.character(com))

# Per crop
##########
# Look for most cultivated crops
crop_rank <- data_frac%>%
  filter(year==2018)%>%
  filter(day_num==1)%>%
  group_by(crop)%>%
  summarize(sum_surf=sum(SURF_PARC))%>%
  arrange(-sum_surf)

# Select code id for 8 more cultivated crops 
crop_sub<-crop_rank%>%head(8)%>%pull(crop)
#crop_sub <- c('BTH','MIS','TTH','MIE','TRN','ORH','CZH','PTR')

data_crop <- data_frac%>%
  filter(crop %in% crop_sub)%>%
  mutate(
    crop_name = case_when(
      crop=='BTH'~'Soft winter wheat',
      crop=='CZH'~'Winter rapeseed',
      crop=='MIE'~'Silage maize',
      crop=='MIS'~'Grain maize',
      crop=='ORH'~'Winter barley',
      crop=='PTR'~'Temporary grassland',
      crop=='TRN'~'Sunflower',
      crop=='TTH'~'Winter triticale'
    )
  )%>%
  group_by(plot,year)%>%
  summarize(
    crop=crop_name[1],
    frac_mean=mean(frac),
    rapar=sum(na.omit(apar))/sum(na.omit(par))
  )%>%
  ungroup()%>%
  dplyr::select(crop,frac_mean,rapar)%>%
  pivot_longer(!crop,names_to='indicator',values_to='value')
 

# Per crop rotation
###################
data_rotation <- data_frac%>%
  mutate(rotation=paste(CODE_CULTU_2018,CODE_CULTU_2019,CODE_CULTU_2020,sep='_'))

# This is Table 1
table1 <- data_rotation %>%
  group_by(plot)%>%
  summarize(
    plot_surf=SURF_PARC[1],
    rotation=rotation[1],
    frac_mean=mean(frac),
    apar_year=sum(na.omit(apar))/(3*1000000),
    rapar=sum(na.omit(apar))/sum(na.omit(par))
  )%>%
  ungroup()%>%
  group_by(rotation)%>%
  summarize(
    rotation_surf=sum(plot_surf),
    frac_mean_whole=mean(frac_mean),
    frac_sd=sd(frac_mean),
    rapar_mean=mean(na.omit(rapar)),
    rapar_sd=sd(na.omit(rapar)),
    apar_mean=mean(na.omit(apar_year)),
    apar_sd=sd(na.omit(apar_year))
  )%>%
  ungroup()%>%
  arrange(-rotation_surf)%>%
  head(11)


# Comparison with COrg content
##############################
data_crop_corg<-data_frac%>%
  group_by(plot)%>%
  summarize(
    frac_mean=mean(frac),
    apar_year=sum(na.omit(apar))/(3*1000000), # in MJ.m-2.yr-1
    rapar=sum(na.omit(apar))/sum(na.omit(par))
  )%>%
  ungroup()%>%
  left_join(SOC_SG,by=c('plot'='ID_PARCEL'))%>%
  filter(frac_mean!=0&apar_year!=0)

test_fcv_5<-lm(soc_0_5cm~frac_mean,data=data_crop_corg)
test_fcv_15<-lm(soc_5_15cm~frac_mean,data=data_crop_corg)
test_fcv_30<-lm(soc_15_30cm~frac_mean,data=data_crop_corg)

test_apar_5<-lm(soc_0_5cm~apar_year,data=data_crop_corg)
test_apar_15<-lm(soc_5_15cm~apar_year,data=data_crop_corg)
test_apar_30<-lm(soc_15_30cm~apar_year,data=data_crop_corg)

summary(test_fcv_5)
summary(test_fcv_15)
summary(test_fcv_30)

summary(test_apar_5)
summary(test_apar_15)
summary(test_apar_30)



######################################################
# Figures
#########

# Figure 2: Maps
####################
# Merge shapefile of muncipalities with results of photosynthetic activities 
COM63_cover <- com63%>%
  left_join(data_crop_com)%>%
  mutate(frac_mean=case_when(
    frac_mean>0.8~0.8,
    TRUE~frac_mean
  ))
# Border of Puy-de-Dome department (merging municipalities)
dep63<-COM63_cover%>%
  group_by(CODE_DEPT)%>%
  summarize(mean_cov=mean(frac_mean))

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 20, 
  #width = 10*1.618, 
  #height = 10, 
  units = "cm", 
  dpi = 300 
)

# Location of Puy-de-Dôme in France
fr<-ggplot()+ 
  geom_sf(data = france, fill="#DCDDE5",colour="black",size=0.1)+
  geom_sf(data = dep63, fill="#0A0908",colour="black")+
  theme(
    axis.line = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    plot.background = element_rect(fill=NA,color=NA),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
fr

elev<-ggplot() +  
  geom_tile(data=DEM63, aes(x=x, y=y, fill=value))+
  geom_sf(data=dep63,fill=NA,color='black',size=0.8)+
  scale_fill_gradientn(
    colours = c("#006837", "#ffffcc", "#9E6240"),
    values = scales::rescale(c(250,500,1000,1500,1900)),
    lim=c(250,1900),
    breaks=c(500,1000,1500),
    labels=c("500","1000","1500"),
    na.value = "#f7fcb9",
    guide=guide_colorbar(
      nbin=1000,raster=F, 
      barwidth=16, barheight = 1.5,
      frame.colour=c('white'),frame.linewidth=0.1, 
      ticks.colour='white',  direction="horizontal",
      title.position = "top")
  )+
  labs(
    fill="Elevation (m)"
  )+
  theme(
    legend.position = c(0.5,0.1),
    legend.text=element_text(size=48,family="fira con"),
    legend.title=element_text(
      size=44,family="fira con",hjust=0.5,
      margin = margin(0,0,0,0, unit = 'cm'),lineheight=0.35
    ),
    legend.spacing.x = unit(0, "cm"),
    legend.spacing.y = unit(0.4, "cm"),
    legend.box.margin= margin(6,0,0,0, 'cm'),
    axis.line = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    plot.background = element_rect(fill=NA,color=NA),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
elev

fvc<-ggplot(data=COM63_cover)+
  geom_sf(aes(fill=frac_mean),color='#FBF5F3',size=0.25)+
  geom_sf(data=dep63,fill=NA,color='black',size=0.8)+
  scale_fill_gradientn(
    colours = c('#ECFEF9','#8BF9DA', '#16F3B4', '#0BD59C','#01130E'),
    lim=c(0.2,0.8),breaks=seq(0.2,0.8,0.15),na.value = "#A0A4B8",
    guide=guide_coloursteps(
      barwidth=16, barheight = 1.5,
      frame.colour=c('white'),frame.linewidth=0.1, 
      ticks.colour='white',  direction="horizontal",title.position = "top")
  )+
  labs(
    fill='Mean fractional vegetation cover\n(FVC)'
  )+
  annotation_scale(
    location = "tr", pad_x = unit(2, "cm"), pad_y = unit(1, "cm"),
    line_width = 1.5,width_hint = 0.15,style = c("bar"),text_family = "fira con",text_cex=4)+
  theme(
    legend.position = c(0.5,0.1),
    legend.text=element_text(size=44,family="fira con"),
    legend.title=element_text(
      size=48,family="fira con",hjust=0.5,
      margin = margin(0,0,0,0, unit = 'cm'),lineheight=0.35
    ),
    legend.spacing.x = unit(0, "cm"),
    legend.spacing.y = unit(0.4, "cm"),
    legend.box.margin= margin(6,0,0,0, 'cm'),
    axis.line = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    plot.background = element_rect(fill=NA,color=NA),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

rapar<-ggplot(data=COM63_cover)+
  geom_sf(aes(fill=rapar),color='#FBF5F3',size=0.25)+
  geom_sf(data=dep63,fill=NA,color='black',size=0.8)+
  scale_fill_gradientn(
    colours = c('#F5EBFE','#C58BF9', '#962AF4', '#6109AE','#160227'),
    lim=c(0.2,0.8),breaks=seq(0.2,0.8,0.15),na.value = "#A0A4B8",
    guide=guide_coloursteps(
      barwidth=16, barheight = 1.5,frame.colour=c('white'),frame.linewidth=0.1, 
      ticks.colour='white',  direction="horizontal",title.position = "top")
  )+
  labs(
    title="c.",
    fill='Ratio between absorbed and total\nphotosynthetic active radiation (RPAR)'
  )+
  annotation_north_arrow(
    location = "tr",which_north = TRUE, pad_x = unit(2, "cm"), 
    pad_y = unit(0.5, "cm"), 
    height = unit(2,"cm"),width=unit(2,"cm"),
    style = ggspatial::north_arrow_fancy_orienteering(text_family = "fira con",text_size = 40)
  )+
  theme(
    plot.title = element_blank(),
    legend.position = c(0.5,0.1),
    legend.text=element_text(size=44,family="fira con"),
    legend.title=element_text(
      size=48,family="fira con",
      hjust=0.5,margin = margin(0,0,0,0, unit = 'cm'),lineheight=0.35),
    legend.spacing.x = unit(0, "cm"),
    legend.spacing.y = unit(0.4, "cm"),
    legend.box.margin= margin(6,0,0,0, 'cm'),
    axis.line = element_blank(), 
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(),
    plot.background = element_rect(fill=NA,color=NA),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Set resolution
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 55, 
  units = "cm", 
  dpi = 300 
)

# Plot layout
layout <- c(
  patchwork::area(t = 5, l = 70, b = 15, r = 100),
  patchwork::area(t = 10, l = 10, b = 20, r = 100),
  patchwork::area(t = 25, l = 10, b = 35, r = 100),
  patchwork::area(t = 40, l = 10, b = 50, r = 100),
  patchwork::area(t=50,l=10,b=55,r=100)
)

# Assemble plots
fr+elev+fvc+rapar+guide_area()+
  plot_layout(design = layout)


# Figure 3: Distribution curve for indicators
#############################################

# Set resolution
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 16, 
  height = 20, 
  units = "cm", 
  dpi = 300 
)

col_grid <- "dimgrey"
col_line <- '#E1B07E'
col_line_darker<- '#C67A2F'
col_fv <- '#06D6A0'
col_dd <- '#42047e'

lab <- tibble(
  value=0.95,
  indicator='rapar',
  crop=factor(
    'Grain maize',
    levels=c(
      'Soft winter wheat',
      'Winter rapeseed',
      'Silage maize',
      'Grain maize',
      'Winter barley',
      'Temporary grassland',
      'Sunflower',
      'Winter triticale'
    )),
  label='Vertical lines show\nmedian value\namong plots\n'
)

ggplot(
  data_crop%>%filter(indicator=="frac_mean"|indicator=="rapar"),
  aes(x=value,y=indicator,fill=indicator))+
  stat_density_ridges(
    quantile_lines = TRUE, 
    quantiles = 2, # Two quantiles to show only median value
    scale=1.3,
    color=col_line,
    size=0.5, draw_baseline = FALSE)+
  geom_text(
    data=lab,aes(x=value,y=indicator,label=label),
    size=9,family = 'fira con',hjust=1,vjust=0,lineheight=0.25,
    color=col_line_darker,fontface='italic'
  )+
  facet_wrap(~crop,ncol=2)+
  scale_fill_manual(values=c(col_fv,col_dd,"grey"))+
  scale_y_discrete(label=c(
    "Mean fractional\nvegetation cover (FVC)",
    "Ratio between absorbed\nand total photosynthetic\nactive radiation (RPAR)"))+
  guides(fill='none')+
  labs(
    x="Indicator value for the period from 2018 to 2020",
    y=""
  )+
  theme_ridges(grid=TRUE,center_axis_labels = TRUE)+
  theme(
    plot.background = element_rect(fill='white'),
    axis.title.y = element_blank(),
    strip.background = element_rect(colour=NA, fill="white"),
    strip.text = element_text(
      size=35,margin=margin(t = 0, r = 0, b = 10, l = 0, unit = "pt"),
      family = 'oswald'),
    axis.text.x = element_text(size=28,family = 'fira con'),
    axis.text.y = element_text(size=30,family = 'fira con',lineheight=0.25),
    axis.title.x = element_text(size=30,family = 'fira con')
  )

# Figure 4: Evolution of FVC
############################

gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 8, 
  units = "cm", 
  dpi = 300 
)

high_fvc <- '#29BF12'
low_fvc <- '#F40752'
col_back <- '#FFD97D'
col_year <- "dimgrey"

# Plot for wheat rotation
BTH<-ggplot()+
  annotate(geom="rect",xmin=0,xmax=365,ymin=0,ymax=1,alpha=0.25,fill=col_back)+
  annotate(geom="rect",xmin=731,xmax=1083,ymin=0,ymax=1,alpha=0.25,fill=col_back)+
  annotate(geom="text",x=5,y=0.95,angle=270,label="2018",size=10,hjust=0,vjust=0,family="oswald",color=col_year)+
  annotate(geom="text",x=371,y=0.95,angle=270,label="2019",size=10,hjust=0,vjust=0,family="oswald",color=col_year)+
  annotate(geom="text",x=736,y=0.95,angle=270,label="2020",size=10,hjust=0,vjust=0,family="oswald",color=col_year)+
  annotate(
    geom="text",x=5,y=1.145,label="Plot with highest photosynthetic activity (mean FCV=0.53, RPAR=0.52)",
    size=8.5,hjust=0,vjust=1,family="fira con",color=high_fvc)+
  annotate(
    geom="text",x=5,y=1.075,label="Plot with average photosynthetic activity (mean FCV=0.31, RPAR=0.39)",
    size=8.5,hjust=0,vjust=1,family="fira con",color=low_fvc)+
  # Plot with highest photosynthetic activity
  geom_line(data_rotation%>%filter(plot==6885498),mapping=aes(x=day_num,y=frac),color=high_fvc)+
  # Plot with average photosynthetic activity
  geom_line(data_rotation%>%filter(plot==6810603),mapping=aes(x=day_num,y=frac),color=low_fvc,lty="dashed")+
  annotate(
    geom="text",x=80,y=0.1,label="Wheat 1",
    size=8.5,hjust=0,vjust=0,family="fira con",color=col_year,fontface='italic')+
  annotate(
    geom="text",x=460,y=0.1,label="Wheat 2",
    size=8.5,hjust=0,vjust=0,family="fira con",color=col_year,fontface='italic')+
  annotate(
    geom="text",x=800,y=0.1,label="Wheat 3",
    size=8.5,hjust=0,vjust=0,family="fira con",color=col_year,fontface='italic')+
  labs(
    x="Day number (since 1st January 2018)",
    y="Fractional Vegetation Cover (FVC)         ",
    title="a. Three-year crop rotation with soft winter wheat as main crop"
  )+
  scale_y_continuous(limits = c(0,1.145),breaks=seq(0,1,0.2))+
  scale_x_continuous(breaks=seq(0,1000,200))+
  theme_light()+
  theme(
    plot.background = element_rect(fill='white',color=NA),
    panel.grid.minor=element_blank(),
    panel.grid.major.x=element_blank(),
    plot.title = element_text(size=30,family = 'oswald'),
    axis.title = element_text(size=26,family = 'fira con'),
    axis.text = element_text(size=24,family = 'fira con'),
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank()
  )

# Plot for grain maize rotation
MIS<-ggplot()+
  annotate(geom="rect",xmin=0,xmax=365,ymin=0,ymax=1,alpha=0.25,fill=col_back)+
  annotate(geom="rect",xmin=731,xmax=1083,ymin=0,ymax=1,alpha=0.25,fill=col_back)+
  annotate(geom="text",x=5,y=0.95,angle=270,label="2018",size=10,hjust=0,vjust=0,family="oswald",color=col_year)+
  annotate(geom="text",x=371,y=0.95,angle=270,label="2019",size=10,hjust=0,vjust=0,family="oswald",color=col_year)+
  annotate(geom="text",x=736,y=0.95,angle=270,label="2020",size=10,hjust=0,vjust=0,family="oswald",color=col_year)+
  annotate(
    geom="text",x=5,y=1.145,label="Plot with highest photosynthetic activity (mean FCV=0.62, RPAR=0.64)",
    size=8.5,hjust=0,vjust=1,family="fira con",color=high_fvc)+
  annotate(
    geom="text",x=5,y=1.075,label="Plot with average photosynthetic activity (mean FCV=0.19, RPAR=0.30)",
    size=8.5,hjust=0,vjust=1,family="fira con",color=low_fvc)+
  annotate(
    geom="text",x=5,y=0.1,label="Cover crop",
    size=8.5,hjust=0,vjust=0,family="fira con",color=high_fvc,fontface='italic')+
  annotate(
    geom="text",x=385,y=0.1,label="Cover crop",
    size=8.5,hjust=0,vjust=0,family="fira con",color=high_fvc,fontface='italic')+
  annotate(
    geom="text",x=750,y=0.1,label="Cover crop",
    size=8.5,hjust=0,vjust=0,family="fira con",color=high_fvc,fontface='italic')+
  # Plot with highest photosynthetic activity
  geom_line(data_rotation%>%filter(plot==6819224),mapping=aes(x=day_num,y=frac),color=high_fvc)+
  # Plot with average photosynthetic activity
  geom_line(data_rotation%>%filter(plot==13277098),mapping=aes(x=day_num,y=frac),color=low_fvc,lty="dashed")+
  annotate(
    geom="text",x=190,y=0.1,label="Maize 1",
    size=8.5,hjust=0,vjust=0,family="fira con",color=col_year,fontface='italic')+
  annotate(
    geom="text",x=575,y=0.1,label="Maize 2",
    size=8.5,hjust=0,vjust=0,family="fira con",color=col_year,fontface='italic')+
  annotate(
    geom="text",x=930,y=0.1,label="Maize 3",
    size=8.5,hjust=0,vjust=0,family="fira con",color=col_year,fontface='italic')+
  labs(
    x="Day number (since 1st January 2018)",
    y="Fractional Vegetation Cover (FVC)         ",
    title="b. Three-year crop rotation with grain maize as main crop"
  )+
  scale_y_continuous(limits = c(0,1.145),breaks=seq(0,1,0.2))+
  scale_x_continuous(breaks=seq(0,1000,200))+
  theme_light()+
  theme(
    plot.background = element_rect(fill='white',color=NA),
    panel.grid.minor=element_blank(),
    panel.grid.major.x=element_blank(),
    plot.title = element_text(size=30,family = 'oswald'),
    axis.title = element_text(size=26,family = 'fira con'),
    axis.text = element_text(size=24,family = 'fira con')
  )

# Set resolution
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 15, 
  height = 16, 
  units = "cm", 
  dpi = 300 
)
# Assemble plots
BTH/MIS

# Comparison with soil COrg
###########################

pal<-c(
  rep('#0BD59C',3),
  rep(col_line,3)
)

size_tit=60
size_text=54
size_r2 = 20

lab<-tibble(
  x=c(
    rep(0.90,3),
    rep(2000,3)
  ),
  y=rep(900,6),
  mod=c(
    "fcv5","fcv15","fcv30",
    "apar5","apar15","apar30"
  ),
  r2=c(
    round(summary(test_fcv_5)$r.squared,2),round(summary(test_fcv_15)$r.squared,2),round(summary(test_fcv_30)$r.squared,2),
    round(summary(test_apar_5)$r.squared,2),round(summary(test_apar_15)$r.squared,2),round(summary(test_apar_30)$r.squared,2)
  ),
  pval=rep("< 2.2e<sup>-16</sup>",6),
  color=pal,
)%>% mutate(
  name = glue("<span style='color:{color}'>R^2 = {r2}<br>pval {pval}</span>")
)

fcv5<-ggplot(data_crop_corg,aes(x=frac_mean,y=soc_0_5cm))+
  geom_point(alpha=0.05,color=col_grid)+
  geom_smooth(method='lm',color=pal[1])+
  labs(
    y="Soil organic carbon content (dg per kg)",
    x="Mean fractional vegetation cover\n(FVC)")+
  geom_richtext(
    data=lab%>%filter(mod=="fcv5"),
    aes(x=x,y=y,label=name),
    family='fira con',size=size_r2,hjust=1,vjust=1,lineheight=0.35,
    fill = NA, label.color = NA, # remove background and outline
    label.padding = grid::unit(rep(0, 4), "pt") # remove padding
  )+ 
  annotate("text",x=0.1,y=900,hjust=0,vjust=1,label="0 to 5 cm",size=22,family="oswald")+
  scale_x_continuous(limits=c(0.1,0.9))+
  scale_y_continuous(limits=c(100,900))+
  theme_light()+
  theme(
    plot.background = element_rect(fill='white',color=NA),
    axis.title = element_text(size=size_tit,family = 'fira con',lineheight=0.30),
    axis.text = element_text(size=size_text,family = 'fira con'),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

fcv15<-ggplot(data_crop_corg,aes(x=frac_mean,y=soc_5_15cm))+
  geom_point(alpha=0.05,color=col_grid)+
  geom_smooth(method='lm',color=pal[2])+
  labs(
    y="Soil organic carbon content (dg per kg)",
    x="Mean fractional vegetation cover\n(FVC)")+
  geom_richtext(
    data=lab%>%filter(mod=="fcv15"),
    aes(x=x,y=y,label=name),
    family='fira con',size=size_r2,hjust=1,vjust=1,lineheight=0.35,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+ 
  annotate("text",x=0.1,y=900,hjust=0,vjust=1,label="5 to 15 cm",size=22,family="oswald")+
  scale_x_continuous(limits=c(0.1,0.9))+
  scale_y_continuous(limits=c(100,900))+
  theme_light()+
  theme(
    plot.background = element_rect(fill='white',color=NA),
    axis.title = element_text(size=size_tit,family = 'fira con',lineheight=0.30),
    axis.text = element_text(size=size_text,family = 'fira con'),
    axis.title.x = element_blank(),
    axis.title.y = element_text(margin=margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

fcv30<-ggplot(data_crop_corg,aes(x=frac_mean,y=soc_15_30cm))+
  geom_point(alpha=0.05,color=col_grid)+
  geom_smooth(method='lm',color=pal[3])+
  labs(
    y="Soil organic carbon content (dg per kg)",
    x="Mean fractional vegetation cover\n(FVC)")+
  geom_richtext(
    data=lab%>%filter(mod=="fcv30"),
    aes(x=x,y=y,label=name),
    family='fira con',size=size_r2,hjust=1,vjust=1,lineheight=0.35,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+ 
  annotate("text",x=0.1,y=900,hjust=0,vjust=1,label="15 to 30 cm depth",size=22,family="oswald")+
  scale_x_continuous(limits=c(0.1,0.9))+
  scale_y_continuous(limits=c(100,900))+
  theme_light()+
  theme(
    plot.background = element_rect(fill='white',color=NA),
    axis.title.x = element_text(size=size_tit,family = 'fira con',lineheight=0.30,margin=margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_blank(),
    axis.text = element_text(size=size_text,family = 'fira con')
  )

apar5<-ggplot(data_crop_corg,aes(x=apar_year,y=soc_0_5cm))+
  geom_point(alpha=0.05,color=col_grid)+
  geom_smooth(method='lm',color=pal[4])+
  labs(
    y="Soil organic carbon content (dg per kg)",
    x="Mean fractional vegetation cover\n(FVC)")+
  geom_richtext(
    data=lab%>%filter(mod=="apar5"),
    aes(x=x,y=y,label=name),
    family='fira con',size=size_r2,hjust=1,vjust=1,lineheight=0.35,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+ 
  scale_x_continuous(breaks=c(500,1000,1500))+
  scale_y_continuous(limits=c(100,900))+
  theme_light()+
  theme(
    plot.background = element_rect(fill='white',color=NA),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

apar15<-ggplot(data_crop_corg,aes(x=apar_year,y=soc_5_15cm))+
  geom_point(alpha=0.05,color=col_grid)+
  geom_smooth(method='lm',color=pal[5])+
  labs(
    y="Soil organic carbon content (dg per kg)",
    x="Mean fractional vegetation cover\n(FVC)")+
  geom_richtext(
    data=lab%>%filter(mod=="apar15"),
    aes(x=x,y=y,label=name),
    family='fira con',size=size_r2,hjust=1,vjust=1,lineheight=0.35,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt") 
  )+ 
  scale_x_continuous(breaks=c(500,1000,1500))+
  scale_y_continuous(limits=c(100,900))+
  theme_light()+
  theme(
    plot.background = element_rect(fill='white',color=NA),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

apar30<-ggplot(data_crop_corg,aes(x=apar_year,y=soc_15_30cm))+
  geom_point(alpha=0.05,color=col_grid)+
  geom_smooth(method='lm',color=pal[6])+
  labs(
    y="Soil organic carbon content (dg per kg)",
    x="Absorbed photosynthetic active radiation<br>(APAR, MJ.m<sup>-2</sup>.yr<sup>-1</sup>)")+
  geom_richtext(
    data=lab%>%filter(mod=="apar30"),
    aes(x=x,y=y,label=name),
    family='fira con',size=size_r2,hjust=1,vjust=1,lineheight=0.35,
    fill = NA, label.color = NA, 
    label.padding = grid::unit(rep(0, 4), "pt")
  )+ 
  scale_x_continuous(breaks=c(500,1000,1500))+
  scale_y_continuous(limits=c(100,900))+
  theme_light()+
  theme(
    plot.background = element_rect(fill='white',color=NA),
    axis.title.x = element_markdown(size=size_tit,family = 'fira con',lineheight=0.30,margin=margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text = element_text(size=size_text,family = 'fira con'),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Set resolution
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 30, 
  height = 45, 
  units = "cm", 
  dpi = 300 
)

# Assemble plots
fcv5+apar5+
  fcv15+apar15+
  fcv30+apar30+
  plot_layout(ncol = 2, widths = c(1, 1))


