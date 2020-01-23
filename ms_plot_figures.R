#------------------------------------------------------------------------------#
#                         Produce figures for manuscript                       #
#------------------------------------------------------------------------------#

rm(list=ls())
options(stringsAsFactors=FALSE)

# Load libraries
library(dplyr)
library(ggplot2)
library(rgdal)
library(leaflet)
library(lubridate)
library(reshape2)
library(scales)
library(RColorBrewer)
library(calibrate)
library(broom)
library(scatterpie)
library(maps)
library(tidyr)
library(mapplots)
library(maptools)
library(rgeos)
library(prettymapr)

# Load functions
source("ms_R/every_nth.R") # Replaces items in a vector with "", leaving the item every 'n' (used for x-axis labels)
source("ms_R/summarise_ts.R") # Uses the hist() function to summarise records over a timeframe
source("ms_R/shp_to_df.R") # Transforms shapefile to dataframe (for ggplot2), whilst retaining detail in the @data slot

#----- Setup variables ---------------------------------------------------------

# Set start date and end date for time series and for IBCM start
start_date <- as.Date("2018-01-01")
end_date <- as.Date("2019-07-31")
IBCM_start <- as.Date("2018-06-01") # Set start of IBCM

# Set plot colours
brewer_pal <- c("#C13215", "#d36f5b", "#969696", "#c0c0c0")

# Adjust scalebar function to only print 2 numbers
ms <- capture.output(map.scale)
ms <- ms[-length(ms)]
ms <- ms[-length(ms)]
ms <- gsub("(n = 2)", "n = 1", ms)
ms <- paste(ms, collapse="\n")
eval(parse(text=paste0("map.scale.new <- ", ms)))

# Set study regions and  districts
study_regs <- c("Mara", "Lindi", "Morogoro", "Mtwara")
study_dis <- c("Bunda", "Butiama", "Kilombero", "Kilosa", "Kilwa",
               "Lindi Rural", "Lindi Urban", "Liwale", "Masasi", "Masasi Township Authority",
               "Morogoro", "Morogoro Urban", "Mtwara Rural", "Mtwara Urban", "Musoma",
               "Musoma Municipal", "Nachingwea", "Nanyamba", "Nanyumbu", "Newala",
               "Rorya", "Ruangwa", "Serengeti", "Tandahimba", "Tarime", "Ulanga")

#----- Load data ---------------------------------------------------------------

HF <- read.csv("ms_data/processed_HF.csv") # processed health facility data for human bites
VET <- read.csv("ms_data/processed_VET.csv") # processed VET data for animal rabies cases
HF_locs <- read.csv("ms_data/Study_HF_locs.csv", na.strings="") # Health facility coordinates

#----- Process data ------------------------------------------------------------

# Subset HF data for first visit/positive clinical signs
table(HF$VISIT_STATUS, HF$Suspect)
HF <- HF[which(HF$Suspect!= "No data"),]; dim(HF)

# Read as a date object
HF$Date_proxy <- as.Date(HF$Date_proxy)
VET$DATE_OF_INVESTIGATION <- as.Date(VET$DATE_OF_INVESTIGATION)
VET$Date_proxy <- as.Date(VET$Date_proxy)

# Subset data to ensure it is within bounds
HF <- HF[which(HF$Date_proxy>=start_date & HF$Date_proxy<=end_date),]
VET <- VET[which(VET$Date_proxy>=start_date & VET$Date_proxy<=end_date),]

# Create subsets based on IBCM start dates
pre_ibcm_HF <- HF[which(HF$Date_proxy<HF$IBCM_start),]; nrow(pre_ibcm_HF)
post_ibcm_HF <- HF[which(HF$Date_proxy>=HF$IBCM_start),]; nrow(post_ibcm_HF)
pre_ibcm_VET <- VET[which(VET$Date_proxy<VET$IBCM_start),]; nrow(pre_ibcm_VET)
post_ibcm_VET <- VET[which(VET$Date_proxy>=VET$IBCM_start),]; nrow(post_ibcm_VET)

#----- Load shapefiles ---------------------------------------------------------

# Load centroids file
ward_centroids <- read.csv("ms_data/TZ_ward_centroids.csv")
dis_centroids <- read.csv("ms_data/TZ_centroids.csv")
dis_centroids <- dis_centroids[which(dis_centroids$Type=="District"),]

# Load shapefile
region_shp <- readOGR("ms_data/gis", "TZ_Region_2012_pop")
district_shp <- readOGR("ms_data/gis", "TZ_District_2012_pop")
ward_shp <- readOGR("ms_data/gis", "TZ_Ward_2012_pop")
vill_shp <- readOGR("ms_data/gis", "TZ_Village_2012_pop_combined")
prot_areas <- readOGR("ms_data/gis", "Protected_areas")
clip_prot_areas <- readOGR("ms_data/gis", "Protected_areas_clipped")

# Subset shapefiles
study_reg <- region_shp[which(region_shp$Region_Nam %in% study_regs),]
study_dis <- district_shp[which(district_shp$Region_Nam %in% study_regs),]
study_wards <- ward_shp[which(ward_shp$Region_Nam %in% study_regs),]

# Change column name to match rest of data
names(clip_prot_areas)[names(clip_prot_areas) == 'Reg_ID'] <- 'Region_Nam'

#----- Figure 1: Ward level map of population desntiy with HF overlaid ---------

# Read coordinate columns as numeric
HF_locs$Lat <- as.numeric(HF_locs$Lat)
HF_locs$Lon <- as.numeric(HF_locs$Lon)

# Only include rows with a Yes in the on_map column
HF_locs_df <- HF_locs[which(HF_locs$plot=="yes"),]
message("Number of HFs in study: ", nrow(HF_locs_df))

# Set breaks
summary(study_wards$pop_2012)
pop_breaks <- c(0,2500, 5000, 10000, 15000, 20000, 30000, 80000)
cols = colorRampPalette(c("white", brewer_pal[1]))
cols <- cols(7)
break_labels <- c("< 2,500", "2,500-5,000", "5,000-10,000", "10,000-15,000",
                  "15,000-20,000", "20,000-30,000", "> 30,000")

# Remove wards from ward shapefile that are lacking population information
study_ward_sub <- study_wards[which(!is.na(study_wards$pop_2012)),]

# Take only district hospitals from study area
HF_locs_df_sub <- HF_locs_df[which(HF_locs_df$Clin_Reg %in% unique(ward_shp$Region_Nam)),]

# Save figure
pdf("ms_figs/Figure_1.pdf", height=10, width=10)
plot(region_shp, border="darkgrey", lwd=2.5)
plot(district_shp, border="darkgrey", lwd=0.1, add=TRUE)
plot(study_ward_sub, col=cols[findInterval(study_ward_sub$pop_2012, pop_breaks, all.inside=T)],
     border=F, add=TRUE)
plot(study_dis, border="#474646", lwd=0.1, add=TRUE)
plot(study_reg, lwd=2.5, add=TRUE)
plot(prot_areas, col="grey", border=FALSE, add=TRUE)
points(x=HF_locs_df_sub$Lon, y=HF_locs_df_sub$Lat, cex=1.5, pch=21, bg="#0045ff", col="#668fff")
legend("topright", legend=break_labels, fill=cols, bty = "n",
       title="Population per Ward", cex=1.3)
text(x=35.5, y=-1.3, labels="Mara", cex=1.5)
text(x=35.7, y=-7.6, labels="Morogoro", cex=1.5)
text(x=40.2, y=-9, labels="Lindi", cex=1.5)
text(x=40, y=-11.3, labels="Mtwara", cex=1.5)
map.scale.new(relwidth=0.1, ratio=FALSE, cex=1.2, x=30, y=-11.5)
addnortharrow(pos="bottomleft", scale=0.8)
dev.off()

#----- Figure 3: Regional reporting of suspected rabies exposures --------------

# Setup dataframe for IBCM starts
IBCM_starts <- data.frame(region=c("Lindi", "Mara", "Morogoro", "Mtwara"),
                          ibcm_start=c(min(HF$IBCM_start[which(HF$LOCATION_OF_EVENT_REGION=="Lindi")], na.rm=TRUE),
                                       min(HF$IBCM_start[which(HF$LOCATION_OF_EVENT_REGION=="Mara")], na.rm=TRUE),
                                       min(HF$IBCM_start[which(HF$LOCATION_OF_EVENT_REGION=="Morogoro")], na.rm=TRUE),
                                       min(HF$IBCM_start[which(HF$LOCATION_OF_EVENT_REGION=="Mtwara")], na.rm=TRUE)),
                          ibcm_n=c(length(seq(from=start_date, to=as.Date(min(HF$IBCM_start[which(HF$LOCATION_OF_EVENT_REGION=="Lindi")], na.rm=TRUE)), by='month'))-0.5,
                                   length(seq(from=start_date, to=as.Date(min(HF$IBCM_start[which(HF$LOCATION_OF_EVENT_REGION=="Mara")], na.rm=TRUE)), by='month'))-0.5,
                                   length(seq(from=start_date, to=as.Date(min(HF$IBCM_start[which(HF$LOCATION_OF_EVENT_REGION=="Morogoro")], na.rm=TRUE)), by='month'))-0.5,
                                   length(seq(from=start_date, to=as.Date(min(HF$IBCM_start[which(HF$LOCATION_OF_EVENT_REGION=="Mtwara")], na.rm=TRUE)), by='month'))-0.5),
                          stringsAsFactors=FALSE)

# Count how many months from start date to IBCM start, then minus .5 to plot next to bar, not through it
IBCM_break_month <- length(seq(start_date, IBCM_start, by="months")) -0.5

# Subset for only first visit to Health facility and after start date
HF_sub <- HF[which(HF$VISIT_STATUS %in% c("first", "Positive") & HF$Date_proxy>start_date),]

# Use hist function to summarise bite data using breaks (HF)
monthly_summary_HF <- summarise_ts(HF_sub, by_reg=TRUE, by_dis=FALSE, timescale="month", IBCM_break=IBCM_break_month)
monthly_summary_HF <- monthly_summary_HF[which(monthly_summary_HF$region!="Kusini Pemba"),]

# Set status as a factor
monthly_summary_HF$status <- factor(monthly_summary_HF$status, levels=c("Healthy", "Suspect"))

# Set annotation text for plot
monthly_summary_HF$annotations <- ""
monthly_summary_HF$annotations[which(monthly_summary_HF$status=="Suspect" & monthly_summary_HF$region=="Lindi" & monthly_summary_HF$month==3)] <- "Pre-IBCM"
monthly_summary_HF$annotations[which(monthly_summary_HF$status=="Suspect" & monthly_summary_HF$region=="Lindi" & monthly_summary_HF$month==9)] <- "Post-IBCM"

# Use hist function to summarise death data using breaks (HF)
monthly_summary_deaths <- summarise_ts(HF, by_reg=TRUE, by_dis=FALSE, timescale="month", IBCM_break=IBCM_break_month, sep_col="VISIT_STATUS", sep_labels=c("first", "positive_clinical_signs"))
monthly_summary_deaths <- monthly_summary_deaths[which(monthly_summary_deaths$status=="positive_clinical_signs"),]
monthly_summary_deaths <- monthly_summary_deaths[-which(monthly_summary_deaths$count==0),]

# Use hist function to summarise case data using breaks (VET)
monthly_summary_LFO <- summarise_ts(VET, by_reg=TRUE, by_dis=FALSE, timescale="month", IBCM_break=IBCM_break_month, sep_col="Assessment")
monthly_summary_LFO <- monthly_summary_LFO %>%
  group_by(year, month, month_n, region, plot_label, IBCM) %>%
  summarise(count = sum(count))

# Use hist function to summarise positive data using breaks (VET)
monthly_summary_positive <- summarise_ts(VET[which(VET$SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME!="inconclusive"),], by_reg=TRUE, by_dis=FALSE, timescale="month", IBCM_break=IBCM_break_month, sep_col="SAMPLE_WAS_COLLECTED_LATERAL_FLOW_TEST_OUTCOME", sep_labels=c("negative", "positive"))
monthly_summary_positive <- monthly_summary_positive[which(monthly_summary_positive$status=="positive"),]
monthly_summary_positive <- monthly_summary_positive[-which(monthly_summary_positive$count==0),]

# Set the x-axis labels to show 2 months a year (Jan, Jul) but with ticks at every month
breaks <- seq(start_date, ceiling_date(end_date,'month'),"month")-1
x_temp <- breaks[2:length(breaks)]
x_breaks <- 1:length(x_temp)
full_labels = unique(monthly_summary_HF$plot_label)[seq(1, length(unique(monthly_summary_HF$plot_label)), 1)]
x_labels = every_nth(full_labels, 3, inverse = TRUE)

# Save plot
pdf("ms_figs/Figure_3.pdf", height=6, width=8)
ggplot() +
  geom_col(data=monthly_summary_HF, aes(x=month_n, y=count, fill=status)) +
  geom_point(data=monthly_summary_deaths, aes(x=month_n, y=145, size=count), shape=19, col=brewer_pal[1]) +
  geom_point(data=monthly_summary_positive, aes(x=month_n, y=135, size=count), shape=19, col="black") +
  geom_path(data=monthly_summary_LFO, aes(x=month_n, y=count), size=0.5) +
  labs(x="", y="Monthly patients") +
  scale_fill_manual(name="Rabies Status",
                    values=rev(brewer_pal[c(1,3)]), # set colours for bars
                    breaks=c("Suspect", "Healthy"), # set what is coloured (from status column)
                    labels=c(paste0("   High-risk (n=", format(sum(monthly_summary_HF$count[which(monthly_summary_HF$status=="Suspect")]), big.mark=","), ")   "),
                             paste0("   Low-risk (n=", format(sum(monthly_summary_HF$count[which(monthly_summary_HF$status=="Healthy")]), big.mark=","), ")   "))) +
  scale_size_continuous(name="", guide=guide_legend(order=3),
                        range = c(sqrt(min(monthly_summary_positive$count)), sqrt(max(monthly_summary_positive$count))),
                        breaks=c(min(monthly_summary_positive$count), max(monthly_summary_positive$count)),
                        labels=c(min(monthly_summary_positive$count), max(monthly_summary_positive$count))) +
  theme_classic() + facet_wrap(~region, ncol=2, nrow=2) +
  theme(plot.title = element_text(size=14, face="bold"),
        legend.title=element_blank(), # remove legend title
        legend.position="top", # set legend to appear above plot
        axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels to prevent overlap
  geom_vline(data=IBCM_starts, aes(xintercept=ibcm_n), linetype="dashed", size=0.6) +
  scale_y_continuous(limits=c(0, 145)) + # set y-axis limits
  scale_x_continuous(breaks=x_breaks, labels=x_labels) + # set x-axis breaks and labels as above
  geom_text(data=monthly_summary_HF, aes(x=month, y=100, label=annotations), fontface="italic")
dev.off()

#----- Figure 5: Map of spatial distribution of cases --------------------------

# Summarise suspect bites by Loc_ID at ward level
ward_suspect <- HF[which(HF$LOCATION_OF_EVENT_REGION %in% study_regs),] %>%
  group_by(Loc_ID_w) %>%
  summarise(n_suspect_bites=length(which(Suspect=="Suspect")))
summary(ward_suspect$n_suspect_bites)

map_breaks <- c(0,1,2,5,10,30)
map_cols <- colorRampPalette(c("white", brewer_pal[1]))
map_color_pal <- map_cols(5)
map_labels = c("0", "1-2", "2-5", "5-10", "10<")
ward_suspect_map <- merge(study_wards, ward_suspect, by.x="Loc_ID", by.y="Loc_ID_w", all.x=TRUE)
ward_suspect_map$n_suspect_bites[which(is.na(ward_suspect_map$n_suspect_bites))] <- 0

# Summarise LFO assessed Rabies cases by Loc_ID at ward level
ward_cases <- VET[which(VET$LOCATION_OF_EVENT_REGION %in% study_regs),] %>%
  group_by(LOCATION_OF_EVENT_REGION, Loc_ID_w) %>%
  summarise(n_cases=length(which(Assessment=="Suspect")))
summary(ward_cases$n_cases)
w_centroids <- ward_centroids[which(ward_centroids$Loc_ID %in% ward_cases$Loc_ID_w),]
w_centroids <- merge(w_centroids, ward_cases, by.x="Loc_ID", by.y="Loc_ID_w")
w_centroids <- w_centroids[-which(w_centroids$n_cases==0),]
colnames(w_centroids) <- c("Loc_ID", "Longitude", "Latitude", "Type",
                              "Region_Nam", "n_cases")

pdf("ms_figs/Figure_5.pdf", height=6, width=8)

# Increase margins to right of plot
par(oma = c(0, 0, 0, 8))

# Plot Mara
par(fig=c(0, 0.5, 0.5, 1), mar = c(1, 1, 1, 1))
plot(study_reg[which(study_reg$Region_Nam=="Mara"),], asp=1, main="Mara")#, xlim=c(32, 36), ylim=c(-5,1))
plot(ward_suspect_map[which(ward_suspect_map$Region_Nam=="Mara"),],
     col=map_color_pal[findInterval(ward_suspect_map$n_suspect_bites[which(ward_suspect_map$Region_Nam=="Mara")], map_breaks, all.inside=T)],
     border=F, add=TRUE)
plot(study_dis[which(study_dis$Region_Nam=="Mara"),], border="dimgrey", add=TRUE)
plot(clip_prot_areas[which(clip_prot_areas$Region_Nam=="Mara"),], col="darkgrey", border=F, add=TRUE)
plot(study_reg[which(study_reg$Region_Nam=="Mara"),], fill=NULL, add=TRUE)
points(x=w_centroids$Longitude[which(w_centroids$Region_Nam=="Mara")],
       y=w_centroids$Latitude[which(w_centroids$Region_Nam=="Mara")],
       pch=21, col="#0045ff", lwd=1, cex=sqrt(w_centroids$n_cases[which(w_centroids$Region_Nam=="Mara")]))
map.scale.new(y=-2.3, ratio=FALSE, cex=0.8)

# Plot Lindi
par(fig=c(0.5, 1, 0.5, 1), mar = c(1, 1, 1, 1), new=TRUE)
plot(study_reg[which(study_reg$Region_Nam=="Lindi"),], asp=1, main="Lindi")#, xlim=c(36.5, 40.5), ylim=c(-12,-6))
plot(ward_suspect_map[which(ward_suspect_map$Region_Nam=="Lindi"),],
     col=map_color_pal[findInterval(ward_suspect_map$n_suspect_bites[which(ward_suspect_map$Region_Nam=="Lindi")], map_breaks, all.inside=T)],
     border=F, add=TRUE)
plot(study_dis[which(study_dis$Region_Nam=="Lindi"),], border="dimgrey", add=TRUE)
plot(clip_prot_areas[which(clip_prot_areas$Region_Nam=="Lindi"),], col="darkgrey", border=F, add=TRUE)
plot(study_reg[which(study_reg$Region_Nam=="Lindi"),], fill=NULL, add=TRUE)
points(x=w_centroids$Longitude[which(w_centroids$Region_Nam=="Lindi")],
       y=w_centroids$Latitude[which(w_centroids$Region_Nam=="Lindi")],
       pch=21, col="#0045ff", lwd=1, cex=sqrt(w_centroids$n_cases[which(w_centroids$Region_Nam=="Lindi")]))
map.scale.new(x=36.8, y=-10.5, ratio=FALSE, relwidth=0.1, cex=0.8)

# Plot Morogoro
par(fig=c(0, 0.5, 0, 0.5), mar = c(1, 1, 1, 1), new=TRUE)
plot(study_reg[which(study_reg$Region_Nam=="Morogoro"),], asp=1, main="Morogoro")#, xlim=c(35, 39), ylim=c(-10,-6))
plot(ward_suspect_map[which(ward_suspect_map$Region_Nam=="Morogoro"),],
     col=map_color_pal[findInterval(ward_suspect_map$n_suspect_bites[which(ward_suspect_map$Region_Nam=="Morogoro")], map_breaks, all.inside=T)],
     border=F, add=TRUE)
plot(study_dis[which(study_dis$Region_Nam=="Morogoro"),], border="dimgrey", add=TRUE)
plot(clip_prot_areas[which(clip_prot_areas$Region_Nam=="Morogoro"),], col="darkgrey", border=F, add=TRUE)
plot(study_reg[which(study_reg$Region_Nam=="Morogoro"),], fill=NULL, add=TRUE)
points(x=w_centroids$Longitude[which(w_centroids$Region_Nam=="Morogoro")],
       y=w_centroids$Latitude[which(w_centroids$Region_Nam=="Morogoro")],
       pch=21, col="#0045ff", lwd=1, cex=sqrt(w_centroids$n_cases[which(w_centroids$Region_Nam=="Morogoro")])) #60190a
map.scale.new(x=37.5, y=-9.5, ratio=FALSE, cex=0.8)

# Plot Mtwara
par(fig=c(0.5, 1, 0, 0.5), mar = c(1, 1, 1, 1), new=TRUE)
plot(study_reg[which(study_reg$Region_Nam=="Mtwara"),], asp=1, main="Mtwara")#, xlim=c(37.5, 41.5), ylim=c(-13,-7))
plot(ward_suspect_map[which(ward_suspect_map$Region_Nam=="Mtwara"),],
     col=map_color_pal[findInterval(ward_suspect_map$n_suspect_bites[which(ward_suspect_map$Region_Nam=="Mtwara")], map_breaks, all.inside=T)],
     border=F, add=TRUE)
plot(study_dis[which(study_dis$Region_Nam=="Mtwara"),], border="dimgrey", add=TRUE)
plot(clip_prot_areas[which(clip_prot_areas$Region_Nam=="Mtwara"),], col="darkgrey", border=F, add=TRUE)
plot(study_reg[which(study_reg$Region_Nam=="Mtwara"),], fill=NULL, add=TRUE)
points(x=w_centroids$Longitude[which(w_centroids$Region_Nam=="Mtwara")],
       y=w_centroids$Latitude[which(w_centroids$Region_Nam=="Mtwara")],
       pch=21, col="#0045ff", lwd=1, cex=sqrt(w_centroids$n_cases[which(w_centroids$Region_Nam=="Mtwara")]))
map.scale.new(y=-11.5, ratio=FALSE, cex=0.8)

# Add polygon legend
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 4, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("topright", legend=map_labels, fill=map_color_pal, bty = "n",
       title="Reported high-risk bites", xjust=1, cex=1.1, xpd=TRUE)

# Add circle legend
par(fig = c(0, 1, 0, 1), oma = c(4, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottomright", legend=c("   1", "   10", "   25"),  col="#0045ff",  bty = "n", cex=1.1,
       pch=21, pt.lwd=1.5, pt.cex=c(sqrt(1), sqrt(10), sqrt(25)), title="Probable rabies cases", y.intersp=1.8)
dev.off()

par(mfrow=c(1,1), xpd=FALSE)
