library(here); library(ggplot2);  library(data.table); library(lubridate); library(dplyr); library(openxlsx); library(tidyr); library(magick)


#rm(list = ls())


#SiteID defined in main markdown. To run R script independently, replace "params$name" with the park's code
Hist.data <- paste0("multi-park-historical-v2/")
Future.data <- paste0("data/", params$name, "/")

##################
#Load Data
##################

load(paste0(Future.data,"input-data/Final_Environment.RData"))
OutDir <- Future.data
LongName <- park$UNIT_NAME
nps_boundary <- read.csv("nps_boundary.csv", header = TRUE)
ExpRegion <- read.csv("NPS_centroid_ExpRegion.csv", header=TRUE)
NPVuln <- read.csv("NPVuln_Park_Database.csv", header=TRUE)

CF_selected <- ifelse(nps_boundary$REGION[which(nps_boundary$UNIT_CODE == name)] == "Northeast", "WarmDry_HotWet", "WarmWet_HotDry")

if(CF_selected == "WarmWet_HotDry") {
  FutureSubset <- CFs_all[c(1,5)]; CFs = FutureSubset  # Pick pair of climate futures.
  CF_abbreviation <- "WW-HD"
  CF1.Name <- "Warm Wet"
  CF2.Name <- "Hot Dry"
  CFDir = paste0(OutDir,"WarmWet_HotDry/") # for .csv's
} else{
  FutureSubset <- CFs_all[c(4,2)]; CFs = FutureSubset  # Pick pair of climate futures.
  CF_abbreviation <- "WD-HW"
  CF1.Name <- "Warm Dry"
  CF2.Name <- "Hot Wet"
  CFDir = paste0(OutDir,"WarmDry_HotWet/") # for .csv's
}
TableDir = paste0(CFDir,"tables/") # for .csv's
FigDir = paste0(CFDir,"figures/") # for .csv's

D_Annual <- openxlsx::read.xlsx(xlsxFile=paste0(TableDir,SiteID,"_",CF_abbreviation,"_Plot_data.xlsx"),sheet="D_Annual")
recurrence <- read.csv(paste0(TableDir, "precip_recurrence_interval.csv"))
AnnualWB <- read.csv(paste0(TableDir,"WB-Annual.csv")) %>% 
  left_join(WB_GCMs,by="GCM") %>% 
  mutate(sum_d.in = sum_d.mm/ 25.4,
         sum_aet.in = sum_aet.mm / 25.4)
Drought.char <- read.csv(paste0(TableDir, "Drought_characteristics.csv"))


#Historical Data
Historical.AnnualMeans <- read.csv("Annual-Averages.csv", header = TRUE)
Historical.AnnualMeans <- filter(Historical.AnnualMeans, ID == SiteID)
Historical.Regression <- read.csv(paste0(Hist.data, park$UNIT_CODE, "-Regression Table.csv"))
Historical.Anomalies <- read.csv(paste0(Hist.data, "ALL-Anomalies-table-2.csv"))

# Historical
Exposure.Data <- data.frame(SiteID = SiteID)
Exposure.Data$nClim.Tavg.min <- min(Historical.AnnualMeans$TavgF)
Exposure.Data$nClim.Tavg.mean <- mean(Historical.AnnualMeans$TavgF)
Exposure.Data$nClim.Tavg.max <- max(Historical.AnnualMeans$TavgF)
Exposure.Data$nClim.Prcp.min <- min(Historical.AnnualMeans$PptIn)
Exposure.Data$nClim.Prcp.mean <- mean(Historical.AnnualMeans$PptIn)
Exposure.Data$nClim.Prcp.max <- max(Historical.AnnualMeans$PptIn)
Exposure.Data$Prcp.max.hist <- max(Baseline_all$PrcpIn)
Exposure.Data$Tavg.trend <- ifelse(Historical.Regression$YrCoeff.degF.in..100yrs.[2] >0, "increased", "decreased")
Exposure.Data$Prcp.trend <- ifelse(Historical.Regression$YrCoeff.degF.in..100yrs.[4] >0, "increased", "decreased")
Exposure.Data$Prcp.trend.1900 <- ifelse(Historical.Regression$YrCoeff.degF.in..100yrs.[3] >0, "increased", "decreased")
Exposure.Data$Tavg.rate.1900 <- Historical.Regression$YrCoeff.degF.in..100yrs.[1]
Exposure.Data$Tavg.rate.1970 <- Historical.Regression$YrCoeff.degF.in..100yrs.[2]
Exposure.Data$Prcp.rate.1900 <- Historical.Regression$YrCoeff.degF.in..100yrs.[3]
Exposure.Data$Prcp.rate.1970 <- Historical.Regression$YrCoeff.degF.in..100yrs.[4]

filtered_tavgRows <- Historical.Anomalies %>%
  group_by(SiteID) %>%
  filter(n() > 1)
selected_tavgRows <- filtered_tavgRows %>%
  slice(1)
selected_tavgRows2 <- filtered_tavgRows %>%
  slice(2)

Exposure.Data$Tavg.Anomalies.1 <- selected_tavgRows$hist.anomalies.tmean[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$Tavg.Anomalies.2 <- selected_tavgRows2$hist.anomalies.tmean[which(selected_tavgRows2$SiteID == park$UNIT_CODE)]
Exposure.Data$PrcpAbove.Anomalies.1 <- selected_tavgRows$hist.anomalies.above.prcp[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$PrcpAbove.Anomalies.2 <- selected_tavgRows2$hist.anomalies.above.prcp[which(selected_tavgRows2$SiteID == park$UNIT_CODE)]
Exposure.Data$PrcpBelow.Anomalies.1 <- selected_tavgRows$hist.anomalies.below.prcp[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$PrcpBelow.Anomalies.2 <- selected_tavgRows2$hist.anomalies.below.prcp[which(selected_tavgRows2$SiteID == park$UNIT_CODE)]
Exposure.Data$Tavg.Anomalies.recent.percent <- selected_tavgRows$recent.percent.tmean.anomaly[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$Tavg.Anomalies.years <- (Exposure.Data$Tavg.Anomalies.recent.percent/100)*22
Exposure.Data$PrcpAbove.Anomalies.recent.percent <- selected_tavgRows$recent.percent.above.prcp.anomaly[which(selected_tavgRows$SiteID == park$UNIT_CODE)]
Exposure.Data$ObservedPrcp <- nps_boundary$ObservedPrcpChange[which(nps_boundary$UNIT_CODE == park$UNIT_CODE)]

# Future
Exposure.Data$Future.DeltaTavg.min <- min(Future_Means$DeltaTavg)
Exposure.Data$Future.DeltaTavg.max <- max(Future_Means$DeltaTavg)
Exposure.Data$Future.DeltaPr.min <- min(Future_Means$DeltaPr*365)
Exposure.Data$Future.DeltaPr.max <- max(Future_Means$DeltaPr*365)
Exposure.Data$Future.DeltaPr.min.percent <- (min(Future_Means$DeltaPr*365))/(BaseMeanPr*365)*100
Exposure.Data$Future.DeltaPr.max.percent <- (max(Future_Means$DeltaPr*365))/(BaseMeanPr*365)*100
Exposure.Data$Future.DeltaPr.99.CF1 <- D_Annual$OverPrecip99[2]
Exposure.Data$Future.DeltaPr.99.CF2 <- D_Annual$OverPrecip99[3]

# Selecting based on GCM & CF name for largest projected rainfall events
prcp_row_index_CF1 <- match(CF1.Name, WB_GCMs$CF)
if (!is.na(prcp_row_index_CF1)){
  selected_GCM <- WB_GCMs$GCM[prcp_row_index_CF1]
  selected_rows <- which(Future_all$GCM %in% selected_GCM)

  if (length(selected_rows) > 0){
    Exposure.Data$Future.PrcpIn.CF1 <- max(Future_all$PrcpIn[selected_rows])
  }
}

prcp_row_index_CF2 <- match(CF2.Name, WB_GCMs$CF)
if (!is.na(prcp_row_index_CF2)){
  selected_GCM2 <- WB_GCMs$GCM[prcp_row_index_CF2]
  selected_rows2 <- which(Future_all$GCM %in% selected_GCM2)
  
  if (length(selected_rows2) > 0){
    Exposure.Data$Future.PrcpIn.CF2 <- max(Future_all$PrcpIn[selected_rows2])
  }
}

Exposure.Data$DeltaTavg.CF1 <- D_Annual$TavgF[2]
Exposure.Data$DeltaTavg.CF2 <- D_Annual$TavgF[3]
Exposure.Data$DeltaPrcp.CF1 <- D_Annual$PrcpIn[2]
Exposure.Data$DeltaPrcp.CF2 <- D_Annual$PrcpIn[3]
Exposure.Data$HI.Dan.CF1 <- D_Annual$HI.Dan[2]
Exposure.Data$HI.Dan.CF2 <- D_Annual$HI.Dan[3]
Exposure.Data$TmaxHigh <- round(HistTmaxHigh, 1)
Exposure.Data$Tmax99 <- round(HistTmax99, 1)
Exposure.Data$Future.DeltaPr.avg.percent.CF1 <- (Exposure.Data$DeltaPrcp.CF1/D_Annual$PrcpIn[1])*100
Exposure.Data$Future.DeltaPr.avg.percent.CF2 <- (Exposure.Data$DeltaPrcp.CF2/D_Annual$PrcpIn[1])*100

Exposure.Data$Hist_return50 <- round(recurrence$GEV[which(recurrence$return == 50 & recurrence$CF=="Historical")],1)
Exposure.Data$CF1_return50  <- round(recurrence$GEV[which(recurrence$return == 50 & recurrence$CF==CFs[1])],1)
Exposure.Data$CF2_return50  <- round(recurrence$GEV[which(recurrence$return == 50 & recurrence$CF==CFs[2])],1)
Exposure.Data$Hist_return100 <- round(recurrence$GEV[which(recurrence$return == 100 & recurrence$CF=="Historical")],1)
Exposure.Data$CF1_return100  <- round(recurrence$GEV[which(recurrence$return == 100 & recurrence$CF==CFs[1])],1)
Exposure.Data$CF2_return100  <- round(recurrence$GEV[which(recurrence$return == 100 & recurrence$CF==CFs[2])],1)
toleranceP <- 0.5
toleranceN <- -0.5
CF1_GEV <- recurrence %>% filter(CF == CFs[1]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(GEV)
CF2_GEV <- recurrence %>% filter(CF == CFs[2]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(GEV)
returnRow1 <- as.integer(recurrence %>% filter(CF == CFs[1]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(return))
if (CF1_GEV - Exposure.Data$Hist_return50 <= toleranceP | toleranceN){
  Exposure.Data$CF1_return.year <- as.integer(returnRow1)
} else {
  Exposure.Data$CF1_return.year <- "N/A"
}
returnRow2 <- as.integer(recurrence %>% filter(CF == CFs[2]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(return))
if (CF2_GEV - Exposure.Data$Hist_return50 <= toleranceP | toleranceN){
  Exposure.Data$CF2_return.year <- as.integer(returnRow2)
} else {
  Exposure.Data$CF2_return.year <- "N/A"
}
Exposure.Data$HistPrecip99 <- HistPr99
Exposure.Data$Hist.meanWB<-mean(AnnualWB$sum_d.in[which(AnnualWB$year<=2012)])
Exposure.Data$CF1.WBdelta <- mean(AnnualWB$sum_d.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                            AnnualWB$CF == CFs[1])]) - Exposure.Data$Hist.meanWB
Exposure.Data$CF2.WBdelta <- mean(AnnualWB$sum_d.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                            AnnualWB$CF == CFs[2])]) - Exposure.Data$Hist.meanWB
Exposure.Data <- Exposure.Data %>% mutate_if(is.numeric, round, digits=1) #Rounding all variables
D_Annual <- D_Annual %>% mutate_if(is.numeric, round, digits=1)
Drought.char <- Drought.char %>% mutate_if(is.numeric, round, digits=1)
Exposure.Data$CF1_return.percent <- round((1/Exposure.Data$CF1_return.year)*100,1)
Exposure.Data$CF2_return.percent <- round((1/Exposure.Data$CF2_return.year)*100,1)

##################################
# Customized climate futures text
## Listed by order in reports
##################################

######### Cover image formatting #########

# Define the path to your image
image_path <- paste0("exposure-report-photos/", park$UNIT_CODE, ".jpg")

# Read the image
img <- image_read(image_path)

# Define the desired dimensions (7.99 inches width and 7 inches height)
desired_width <- 7.99 * 300  # 7.99 inches at 300 DPI
desired_height <- 7 * 300 # 7 inches at 300 DPI

# Resize the image while maintaining aspect ratio
img_resized <- image_scale(img, geometry = paste0(desired_width, "x", desired_height, "^"))

# Crop the image to the exact dimensions
img_cropped <- image_crop(img_resized, geometry = paste0(desired_width, "x", desired_height, "+0+0!"))

# Display the cropped image
img_cropped

# Save the cropped image
output_path <- paste0("exposure-report-photos/", park$UNIT_CODE, "_cropped.jpg")
image_write(img_cropped, path = output_path)

######### Historical climate change #########
Exposure.Data$PercentTYearsCondition <- ifelse(Exposure.Data$Tavg.Anomalies.recent.percent > 9.2, paste0(", and these anomalies were exceeded in ", Exposure.Data$Tavg.Anomalies.years, " years since 2000 (", Exposure.Data$Tavg.Anomalies.recent.percent, "% of years)."), paste0("."))

######### Projected climate change and related impacts #########
# Precipitation changes either increasing for all models or just one
Exposure.Data$PrcpModels <- ifelse(Exposure.Data$Future.DeltaPr.min < 0, paste0("Projected changes in precipitation are less clear, with some models projecting a decrease in average annual precipitation by ", Exposure.Data$Future.DeltaPr.min, " inches (",Exposure.Data$Future.DeltaPr.min.percent,"%) and others projecting an increase of +", Exposure.Data$Future.DeltaPr.max, " inches (+",Exposure.Data$Future.DeltaPr.max.percent,"%)."), paste0("All climate models project increases in precipitation, ranging from +", Exposure.Data$Future.DeltaPr.min, " (+",Exposure.Data$Future.DeltaPr.min.percent,"%) to +", Exposure.Data$Future.DeltaPr.max, " inches (+",Exposure.Data$Future.DeltaPr.max.percent,"%)."))

# CF naming convention explanation
Exposure.Data$Name.Convention <- ifelse(CF1.Name == "Warm Wet", "climate future might be more arid than what the park experienced historically but the name denotes that it projects wetter conditions than the", "climate future might be wetter than what the park experienced historically but the name denotes that it projects drier conditions than the")

######### Annual average temperature and precipitation projections #########
# Future Tavg exceeds historical highs for which CFs
All.Means <- openxlsx::read.xlsx(xlsxFile=paste0(TableDir,SiteID,"_",CF_abbreviation,"_Plot_data.xlsx"),sheet="Means")
Exposure.Data$TavgCompare <- ifelse(All.Means$TavgF[2] > All.Means$TavgF[1] & All.Means$TavgF[3] > All.Means$TavgF[1], paste("both climate futures"), paste("the", CF2.Name, "future"))

######### Annual average temperature and precipitation projections #########
# Graph colors based on CF
Exposure.Data$Colors <- ifelse(CF1.Name == "Warm Wet", "blue, and the Hot Dry climate future in red", "orange, and the Hot Wet climate future in green")
ColorsSupplemental1 <- ifelse(CF1.Name == "Warm Wet", "blue ", "orange ")
ColorsSupplemental2 <- ifelse(CF1.Name == "Warm Wet", "red ", "green ")

# Future precipitation explanation
Exposure.Data$WettestCF <- ifelse(Exposure.Data$DeltaPrcp.CF1 > Exposure.Data$DeltaPrcp.CF2, CF1.Name, CF2.Name)
Exposure.Data$DriestCF <- ifelse(Exposure.Data$DeltaPrcp.CF1 < Exposure.Data$DeltaPrcp.CF2, CF1.Name, CF2.Name)
Exposure.Data$PrecipFutures <- ifelse(Exposure.Data$DeltaPrcp.CF1 > 0 & Exposure.Data$DeltaPrcp.CF2 > 0, paste("even very dry years could still occur under both climate futures, despite a positive trend in precipitation"), paste("even very dry years could still occur under the", Exposure.Data$WettestCF, "future and very wet years could still be experienced under the", Exposure.Data$DriestCF, "future"))
Exposure.Data$PrcpSeasons <- if(D_Annual$PrcpIn[2] > 0 & D_Annual$PrcpIn[3] > 0){
  "increase"
} else {
    if(D_Annual$PrcpIn[2] < 0 & D_Annual$PrcpIn[3] < 0){
      "decrease"
    } else {
      "increase for one climate future"
  }
}

######### Extreme temperature #########
# Figure 5 edits
Exposure.Data$ExtremeTemps <- ifelse(Exposure.Data$HI.Dan.CF2 > 4, paste0("Extreme temperatures are expected to increase at ", params$name, " under both climate futures (Figure 5), with an additional +", D_Annual$Tmax99[2], " days each year exceeding the historical 99th percentile (", Exposure.Data$Tmax99, " °F) threshold under the ", CF1.Name, " climate future and a more pronounced increase of +", D_Annual$Tmax99[3], " days each year under the ", CF2.Name, " future.

Dangerously hot days can pose health risks to park employees and visitors, particularly affecting vulnerable groups such as children, the elderly, and individuals with preexisting health conditions. The Occupational Safety and Health Administration (OSHA) has established guidelines associated with heat index classifications and protective measures that should be taken for ranges of heat index values (@OSHA). Dangerous heat index days are days that exceed a heat index (a combination of heat and humidity) of 105 °F. In 2004, the NPS Risk Management Office issued guidance that general heat stress controls should be applied when the heat index exceeds 105 °F, which is within the “dangerous” heat index range (NPS 2004). At ", params$name, ", the average days per year exceeding the dangerous heat index threshold is expected to increase in both the ", CF1.Name, " and ", CF2.Name, " climate futures (+", Exposure.Data$HI.Dan.CF1, " days and +", Exposure.Data$HI.Dan.CF2, " days, respectively)."),
                                     paste0("Extreme temperatures are expected to increase at ", params$name, " under both climate futures (Figure 5), with an additional +", D_Annual$Tmax99[2], " days each year exceeding the historical 99th percentile (",Exposure.Data$Tmax99," °F) under the ", CF1.Name, " climate future and a more pronounced increase of +", D_Annual$Tmax99[3], " days each year under the ", CF2.Name, " future."))

Exposure.Data$TempsFigures <- ifelse(Exposure.Data$HI.Dan.CF2 > 4, paste0("The upper bar graph represents the average number of days annually with temperatures greater than the historical 99th percentile (",Exposure.Data$Tmax99," °F) historically (1979-2012) and for the two climate futures (2050). The bottom bar graph represents the average annual number of dangerous heat index days historically and for the two climate futures. Dangerous heat index days are days that exceed 105 °F."),
                                     paste0("Metrics of extreme temperature at ", params$name,". The bar graph represents the average number of days annually with temperatures greater than the historical 99th percentile (",Exposure.Data$Tmax99," °F) historically (1979-2012) and for the two climate futures (2050)."))

# Set the path to JPG file
jpg_file <- paste0(FigDir,"OverTmax99-HI.Dan-Panel.jpg")

# Read JPG image
  image <- image_read(jpg_file)
  
  # Define the output PNG filename
  png_file <- sub("\\.jpg$", ".png", paste0(FigDir,"OverTmax99-HI.Dan-Panel.jpg"))
  
  # Convert JPG to PNG
  image_convert(image, format = "png") %>%
    image_write(png_file)
  
  cat("Converted", jpg_file, "to", png_file, "\n")

fig5code <- ifelse(Exposure.Data$HI.Dan.CF2 > 4, "OverTmax99-HI.Dan-Panel.png", "Tmax99-Annual-bar.png")

######### Extreme precipitation #########
# Trend in extreme precipitation for both CFs
Exposure.Data$ExPrcTrend <- case_when(
  (D_Annual$OverPrecip99[2] > 0 | D_Annual$OverPrecip99[3] > 0) & ((Exposure.Data$Future.PrcpIn.CF1 > Exposure.Data$Prcp.max.hist) & (Exposure.Data$Future.PrcpIn.CF2 > Exposure.Data$Prcp.max.hist)) ~
    paste("Extreme precipitation is projected to increase under both climate futures"),
  D_Annual$OverPrecip99[2] < 0 & D_Annual$OverPrecip99[3] < 0 & ((Exposure.Data$Future.PrcpIn.CF1 < Exposure.Data$Prcp.max.hist) & (Exposure.Data$Future.PrcpIn.CF2 < Exposure.Data$Prcp.max.hist)) ~
    paste("Extreme precipitation is projected to decrease under both climate futures"),
  (D_Annual$OverPrecip99[2] > 0 & D_Annual$OverPrecip99[3] < 0) & (Exposure.Data$Future.PrcpIn.CF2 < Exposure.Data$Prcp.max.hist) ~
    paste("Extreme precipitation is projected to increase under the", CF1.Name, "climate future but decrease under the", CF2.Name, "climate future"),
  (D_Annual$OverPrecip99[2] < 0 & D_Annual$OverPrecip99[3] > 0) & (Exposure.Data$Future.PrcpIn.CF1 < Exposure.Data$Prcp.max.hist) ~
    paste("Extreme precipitation is projected to increase under the", CF2.Name, "climate future but decrease under the", CF1.Name, "climate future"),
  TRUE ~ paste("Extreme precipitation is projected to remain similar under both climate futures")
)

# Trend in 99th percentile rainfall
Exposure.Data$Prcp99thTrend <- case_when(
  ((D_Annual$OverPrecip99[2] > 0 & D_Annual$OverPrecip99[2] < 1) & (D_Annual$OverPrecip99[3] > 0 & D_Annual$OverPrecip99[3] < 1)) ~
    paste("slightly increase under both climate futures"),
  D_Annual$OverPrecip99[2] > 1 & D_Annual$OverPrecip99[3] > 1 ~
    paste("increase under both climate futures"),
  D_Annual$OverPrecip99[2] > 1 & D_Annual$OverPrecip99[3] < 0 ~
    paste("increase under the", CF1.Name, "climate future"),
  D_Annual$OverPrecip99[2] < 0 & D_Annual$OverPrecip99[3] > 1 ~
    paste("increase under the", CF2.Name, "climate future"),
  D_Annual$OverPrecip99[2] > 1 & (D_Annual$OverPrecip99[3] < 1 & D_Annual$OverPrecip99[3] > 0) ~
    paste("increase under the", CF1.Name, "climate future and remain similar under the", CF2.Name, "climate future"),
  (D_Annual$OverPrecip99[2] < 1 & D_Annual$OverPrecip99[2] > 0) & D_Annual$OverPrecip99[3] > 1 ~
    paste("increase under the", CF2.Name, "climate future and remain similar under the", CF1.Name, "climate future"),
  ((D_Annual$OverPrecip99[2] > 0 & D_Annual$OverPrecip99[2] < 1) & (D_Annual$OverPrecip99[3] < 0)) ~
    paste("slightly increase under the", CF1.Name, "climate future"),
  ((D_Annual$OverPrecip99[2] < 0) & (D_Annual$OverPrecip99[3] > 0 & D_Annual$OverPrecip99[3] < 1)) ~
    paste("slightly increase under the", CF2.Name, "climate future"),
  TRUE ~ paste("remain similar under both climate futures")
)

# Trend in large precipitation events increasing for both CFs or just one
Exposure.Data$Prcp24HrTrend <- case_when(
  (Exposure.Data$Future.PrcpIn.CF1 > Exposure.Data$Prcp.max.hist) & (Exposure.Data$Future.PrcpIn.CF2 > Exposure.Data$Prcp.max.hist) ~
    paste0("higher not only in the ", CF1.Name, " scenario but also in the ", CF2.Name, " scenario"),
  Exposure.Data$Future.PrcpIn.CF1 > Exposure.Data$Prcp.max.hist ~
    paste0("higher in the ", CF1.Name, " scenario but not in the ", CF2.Name, " scenario"),
  Exposure.Data$Future.PrcpIn.CF2 > Exposure.Data$Prcp.max.hist ~
    paste0("higher in the ", CF2.Name, " scenario but not in the ", CF1.Name, " scenario"),
  TRUE ~ paste0("lower in both climate futures relative to the historical baseline")
)

######### Drought #########
# Boolean variables to simplify drought metrics and text
Exposure.Data$Severity.CF1 <- ifelse(Drought.char$Severity[which(Drought.char$CF == CF1.Name)] > Drought.char$Severity[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Severity.CF2 <- ifelse(Drought.char$Severity[which(Drought.char$CF == CF2.Name)] > Drought.char$Severity[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Duration.CF1 <- ifelse(Drought.char$Duration[which(Drought.char$CF == CF1.Name)] > Drought.char$Duration[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Duration.CF2 <- ifelse(Drought.char$Duration[which(Drought.char$CF == CF2.Name)] > Drought.char$Duration[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$DrtFree.CF1 <- ifelse(Drought.char$Drt.Free[which(Drought.char$CF == CF1.Name)] < Drought.char$Drt.Free[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$DrtFree.CF2 <- ifelse(Drought.char$Drt.Free[which(Drought.char$CF == CF2.Name)] < Drought.char$Drt.Free[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Freq.CF1 <- ifelse(Drought.char$Frequency[which(Drought.char$CF == CF1.Name)] > Drought.char$Frequency[which(Drought.char$CF == "Historical")], TRUE, FALSE)
Exposure.Data$Freq.CF2 <- ifelse(Drought.char$Frequency[which(Drought.char$CF == CF2.Name)] > Drought.char$Frequency[which(Drought.char$CF == "Historical")], TRUE, FALSE)

# Simple text substitutions for drought trends
Exposure.Data$DrtCF1Duration <- ifelse(Exposure.Data$Duration.CF1 == TRUE, "longer", "shorter")
Exposure.Data$DrtCF1Severity <- ifelse(Exposure.Data$Severity.CF1 == TRUE, "more", "less")

#  Complex text substitutions for similarities in drought conditions between CFs
## TRUE = Worsening drought conditions for the specific metric (e.g. shorter drought-free intervals = TRUE)
### NOT USING THIS ACTIVELY IN .RMD ANYMORE. Needs adjustments.
Exposure.Data$DrtTrendID <- paste(Exposure.Data$Severity.CF1, Exposure.Data$Duration.CF1, Exposure.Data$DrtFree.CF1, Exposure.Data$Severity.CF2, Exposure.Data$Duration.CF2, Exposure.Data$DrtFree.CF2, sep = "-")
trend_lookup <- list(
  "TRUE-TRUE-TRUE-TRUE-TRUE-TRUE" = paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought duration and severity are projected to increase and the drought-free interval is projected to decrease, relative to the past."),
  "FALSE-TRUE-TRUE-FALSE-TRUE-TRUE" = paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought duration and severity are projected to increase, but the drought-free interval is also projected to increase, relative to the past."),
  "FALSE-FALSE-TRUE-FALSE-FALSE-TRUE" = paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought duration is projected to increase, but severity is projected to decrease and the drought-free interval is projected to increase, relative to the past."),
  "FALSE-TRUE-FALSE-FALSE-TRUE-FALSE" = paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought severity is projected to increase, but duration is projected to decrease and the drought-free interval is projected to increase, relative to the past."),
  "TRUE-FALSE-TRUE-TRUE-FALSE-TRUE" = paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought duration is projected to increase and the drought-free interval is projected to decrease, but severity is also projected to decrease, relative to the past."),
  "TRUE-TRUE-FALSE-TRUE-TRUE-FALSE" = paste0("For both the ", CF1.Name, " and ", CF2.Name, " climate futures at ", params$name, ", drought severity is projected to increase considerably and the drought-free interval is projected to decrease, but the duration is also projected to decrease, relative to the past.")
)
Exposure.Data$DrtCFsTrend <- ifelse(Exposure.Data$DrtTrendID %in% names(trend_lookup), unlist(trend_lookup[Exposure.Data$DrtTrendID]), " ")


Exposure.Data$DrtTrend.CF2 <- case_when(
  Exposure.Data$Severity.CF2 == TRUE & Exposure.Data$DrtFree.CF2 == FALSE ~
    paste("increasingly severe compared to historical droughts, represented by the black bars in Figure 7. However, as we near mid-century (gray shaded area in Figure 7), drought-free intervals are also expected to increase. This means that managers should prepare for less frequent but more severe droughts than have been experienced in the past and consider adaptations for surface water and drought-intolerant plant species"),
  Exposure.Data$Severity.CF2 == TRUE & Exposure.Data$DrtFree.CF2 == TRUE ~
      paste("increasingly severe compared to historical droughts, represented by the black bars in Figure 7. Additionally, as we near mid-century (gray shaded area in Figure 7), drought-free intervals are expected to decrease. This means that managers should prepare for more frequent and severe droughts than have been experienced in the past and consider adaptations for surface water and drought-intolerant plant species"),
  Exposure.Data$Severity.CF2 == FALSE & Exposure.Data$DrtFree.CF2 == TRUE ~
      paste("less severe or remain similar to historical droughts, represented by the black bars in Figure 7. However, as we near mid-century (gray shaded area in Figure 7), drought-free intervals are also expected to decrease. This means that managers should prepare for more frequent but less severe droughts than have been experienced in the past and consider adaptations for surface water and drought-intolerant plant species"),
  Exposure.Data$Severity.CF2 == FALSE & Exposure.Data$DrtFree.CF2 == FALSE ~
      paste("less severe or remain similar to historical droughts, represented by the black bars in Figure 7. Additionally, as we near mid-century (gray shaded area in Figure 7), drought-free intervals are expected to increase. This means that parks may experience less frequent droughts with similar severity to what have been experienced in the past"),
  TRUE ~ "N/A"
)

Exposure.Data$DrtGrammar.CF1 <- ifelse(Exposure.Data$Severity.CF1 == Exposure.Data$DrtFree.CF1, paste("with"), paste("but"))

Exposure.Data$Drt.Severity.Length.CF1 <- case_when(
  Exposure.Data$Severity.CF1 == TRUE & Exposure.Data$DrtFree.CF1 == FALSE ~
    paste0(params$name, " is also projected to experience longer drought-free intervals between drought events"),
  Exposure.Data$Severity.CF1 == TRUE & Exposure.Data$DrtFree.CF1 == TRUE ~
    paste0("shorter drought-free intervals between drought events for ", params$name, " resources to recover"),
  Exposure.Data$Severity.CF1 == FALSE & Exposure.Data$DrtFree.CF1 == TRUE ~
    paste0(params$name, " is also projected to experience shorter drought-free intervals to recover between drought events"),
  Exposure.Data$Severity.CF1 == FALSE & Exposure.Data$DrtFree.CF1 == FALSE ~
    paste0("longer drought-free intervals between drought events for ", params$name, " resources to recover"),
  TRUE ~ paste0("N/A")
)


# Text substitutions for matching drought research
Exposure.Data$DrtRsrchID <- paste(Exposure.Data$DrtFree.CF1 | Exposure.Data$DrtFree.CF2,
                                  Exposure.Data$Severity.CF1 | Exposure.Data$Severity.CF2,
                                  Exposure.Data$Duration.CF1 | Exposure.Data$Duration.CF2, sep = "-")
rsrch_lookup <- list(
  "TRUE-TRUE-TRUE" = "These projections are aligned with research showing that climate change may lead to droughts that are longer and more severe than what has occurred historically, with shorter periods between drought events for resources to recover",
  "FALSE-TRUE-TRUE" = "These projections are aligned with research showing that climate change may lead to droughts that are longer and more severe than what has occurred historically",
  "FALSE-FALSE-TRUE" = "These projections are aligned with research showing that climate change may lead to droughts that are longer than what has occurred historically",
  "FALSE-TRUE-FALSE" = "These projections are aligned with research showing that climate change may lead to droughts that are more severe than what has occurred historically",
  "TRUE-FALSE-TRUE" = "These projections are aligned with research showing that climate change may lead to droughts that are longer and more frequent than what has occurred historically",
  "TRUE-TRUE-FALSE" = "These projections are aligned with research showing that climate change may lead to droughts that are more frequent and severe than what has occurred historically"
)
Exposure.Data$Drt.Research <- ifelse(Exposure.Data$DrtRsrchID %in% names(rsrch_lookup),
                                     unlist(rsrch_lookup[Exposure.Data$DrtRsrchID]),
                                     " ")


######### Plant-available water #########
# WB increasing/decreasing trend for one/all CFs
Exposure.Data$WBtrend <- case_when(
  Exposure.Data$CF1.WBdelta > 0 & Exposure.Data$CF2.WBdelta > 0 ~ "increase in both climate futures",
  Exposure.Data$CF1.WBdelta < 0 & Exposure.Data$CF2.WBdelta < 0 ~ "decrease in both climate futures",
  Exposure.Data$CF1.WBdelta < 0 & Exposure.Data$CF2.WBdelta > 0 ~
      paste0("decrease in the ", CF1.Name, " climate future and increase in the ", CF2.Name, " climate future"),
  Exposure.Data$CF1.WBdelta > 0 & Exposure.Data$CF2.WBdelta < 0 ~
      paste0("increase in the ", CF1.Name, " climate future and decrease in the ", CF2.Name, " climate future"),
  TRUE ~ "be the same in both climate futures"
)

Exposure.Data$WBExtremes <- case_when(
  Exposure.Data$CF1.WBdelta > 0 & Exposure.Data$CF2.WBdelta > 0 ~
    "more years that are drier than in the past, some notably wet years, and fewer years that would have historically been considered 'average.'",
  Exposure.Data$CF1.WBdelta < 0 & Exposure.Data$CF2.WBdelta < 0 ~
    "more years that are wetter than in the past, and fewer years that would have historically been considered 'average.'",
  Exposure.Data$CF1.WBdelta == 0 & Exposure.Data$CF2.WBdelta == 0 ~
    "similar conditions to the past.",
  TRUE ~ "more years that are wetter than in the past, some notably dry years, and fewer years that would have historically been considered 'average.'"
)

Exposure.Data$wbCF2 <- if(Exposure.Data$CF2.WBdelta > 0){
  "the average year will have a water deficit comparable to years that currently would be considered dry. Under this climate future, managers can expect most years to have reduced plant growth, lower stream flow, and increased fire risk and plant stress."
} else  {
  "the average year will have a water deficit comparable to what the park experiences historically."
}


################################# 
# Appendix
################################# 

# Appendix: Calculations for change
Exposure.Data$PrcpChangeCF1 <- Exposure.Data$Future.PrcpIn.CF1 - Exposure.Data$Prcp.max.hist
Exposure.Data$PrcpChangeCF2 <- Exposure.Data$Future.PrcpIn.CF2 - Exposure.Data$Prcp.max.hist
Exposure.Data$DrtDurChangeCF1 <- Drought.char$Duration[2] - Drought.char$Duration[1]
Exposure.Data$DrtDurChangeCF2 <- Drought.char$Duration[3] - Drought.char$Duration[1]
Exposure.Data$DrtFreeChangeCF1 <- Drought.char$Drt.Free[2] - Drought.char$Drt.Free[1]
Exposure.Data$DrtFreeChangeCF2 <- Drought.char$Drt.Free[3] - Drought.char$Drt.Free[1]
Exposure.Data$DrtSevChangeCF1 <- Drought.char$Severity[2] - Drought.char$Severity[1]
Exposure.Data$DrtSevChangeCF2 <- Drought.char$Severity[3] - Drought.char$Severity[1]

# Appendix: Seasonal changes
Exposure.Data$TempWinterHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Winter")]
Exposure.Data$DTempWinterCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Winter")]
Exposure.Data$DTempWinterCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Winter")]
Exposure.Data$TempSpringHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Spring")]
Exposure.Data$DTempSpringCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Spring")]
Exposure.Data$DTempSpringCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Spring")]
Exposure.Data$TempSummerHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Summer")]
Exposure.Data$DTempSummerCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Summer")]
Exposure.Data$DTempSummerCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Summer")]
Exposure.Data$TempFallHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Fall")]
Exposure.Data$DTempFallCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Fall")]
Exposure.Data$DTempFallCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Fall")]

Exposure.Data$PrcpWinterHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Winter")]
Exposure.Data$DPrcpWinterCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Winter")]
Exposure.Data$DPrcpWinterCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Winter")]
Exposure.Data$PrcpSpringHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Spring")]
Exposure.Data$DPrcpSpringCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Spring")]
Exposure.Data$DPrcpSpringCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Spring")]
Exposure.Data$PrcpSummerHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Summer")]
Exposure.Data$DPrcpSummerCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Summer")]
Exposure.Data$DPrcpSummerCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Summer")]
Exposure.Data$PrcpFallHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Fall")]
Exposure.Data$DPrcpFallCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Fall")]
Exposure.Data$DPrcpFallCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Fall")]

Exposure.Data$AET1 <- mean(AnnualWB$sum_aet.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                       AnnualWB$CF == CFs[1])]) - mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)])
Exposure.Data$AET2 <- mean(AnnualWB$sum_aet.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                       AnnualWB$CF == CFs[2])]) - mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)])
Exposure.Data$AET3 <- mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)])

Exposure.Data <- Exposure.Data %>% mutate_if(is.numeric, round, digits=1) #Rounding all variables


#################################    
# Creating alt text & Excel sheet
#################################

if (file.exists("AltTextWorkbook.xlsx")){
  file.remove("AltTextWorkbook.xlsx")
}

AltPrcpCFs <- case_when(
  D_Annual$PrcpIn[2] > 0 & D_Annual$PrcpIn[3] > 0 ~
    paste0("a positive trend in precipitation for both climate futures"),
  D_Annual$PrcpIn[2] < 0 & D_Annual$PrcpIn[3] < 0 ~
    paste0("a negative trend in precipitation for both climate futures"),
  D_Annual$PrcpIn[2] > 0 & D_Annual$PrcpIn[3] < 0 ~
      paste0("a positive trend in precipitation for the ", CF1.Name, " climate future and a negative trend for the ", CF2.Name, " future"),
  TRUE ~ paste0("a positive trend in precipitation for the ", CF2.Name, " climate future and a negative trend for the ", CF1.Name, " future")
)

AltExTemps <- ifelse(Exposure.Data$HI.Dan.CF2 > 4, paste("Plot comparing the increase in days over the historical 99th percentile temperature and average annual dangerous heat index days for both climate futures in relation to the historical period."), paste("Plot comparing the increase in days over the historical 99th percentile temperature days for both climate futures in relation to the historical period."))

AltExPrcp <- case_when(
  D_Annual$OverPrecip99[2] > 0 & D_Annual$OverPrecip99[3] > 0 ~
    paste("Graph showing an increase in days per year where precipitation is greater than the historical 99th percentile under both climate futures."),
  D_Annual$OverPrecip99[2] < 0 & D_Annual$OverPrecip99[3] < 0 ~
     paste("Graph showing a decrease in days per year where precipitation is greater than the historical 99th percentile under both climate futures."),
  D_Annual$OverPrecip99[2] > 0 & D_Annual$OverPrecip99[3] < 0 ~
    paste("Graph showing an increase in days per year where precipitation is greater than the historical 99th percentile under the", CF1.Name, "climate future but decrease under the", CF2.Name, "climate future."),
  D_Annual$OverPrecip99[2] < 0 & D_Annual$OverPrecip99[3] > 0 ~
    paste("Graph showing an increase in days per year where precipitation is greater than the historical 99th percentile under the", CF2.Name, "climate future but decrease under the", CF1.Name, "climate future."),
  TRUE ~ paste("Graph showing minimal change in days per year where precipitation is greater than the historical 99th percentile under either climate future.")
)

Exposure.Data$AltDroughtID <- paste(Exposure.Data$Severity.CF1, Exposure.Data$Duration.CF1, Exposure.Data$DrtFree.CF1, Exposure.Data$Severity.CF2, Exposure.Data$Duration.CF2, Exposure.Data$DrtFree.CF2, sep = "-")
  alt_trend_lookup <- list(
    "TRUE-TRUE-TRUE-TRUE-TRUE-TRUE" = paste0("Bar graphs showing drought duration and severity are projected to increase and the drought-free interval is projected to decrease for both climate futures."),
    "FALSE-FALSE-FALSE-FALSE-FALSE-FALSE" = paste0("Bar graphs showing drought duration and severity are projected to decrease and the drought-free interval is projected to increase for both climate futures."),
    "FALSE-TRUE-TRUE-FALSE-TRUE-TRUE" = paste0("Bar graphs showing drought severity is projected to decrease, but drought duration is projected to increase and the drought-free interval is projected to decrease for both climate futures."),
    "FALSE-FALSE-TRUE-FALSE-FALSE-TRUE" = paste0("Bar graphs showing drought duration and severity are projected to decrease, but the drought-free interval is also projected to decrease for both climate futures."),
    "FALSE-TRUE-FALSE-FALSE-TRUE-FALSE" = paste0("Bar graphs showing drought severity is projected to decrease and the drought-free interval is projected to increase, but duration is projected to increase for both climate futures."),
    "TRUE-FALSE-TRUE-TRUE-FALSE-TRUE" = paste0("Bar graphs showing drought severity is projected to increase and the drought-free interval is projected to decrease, but duration is projected to decrease for both climate futures."),
    "TRUE-TRUE-FALSE-TRUE-TRUE-FALSE" = paste0("Bar graphs showing drought severity and duration are projected to increase considerably, but the drought-free interval is projected to increase for both climate futures."),
    "FALSE-FALSE-FALSE-TRUE-TRUE-TRUE" = paste0("Bar graphs showing increasing severity and duration of droughts, and a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity and duration, with an increasing drought-free interval."),
    "TRUE-FALSE-FALSE-TRUE-TRUE-TRUE" = paste0("Bar graphs showing increasing severity and duration of droughts, and a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought duration and an increasing drought-free interval, but increasing drought severity."),
    "TRUE-TRUE-FALSE-TRUE-TRUE-TRUE" = paste0("Bar graphs showing increasing severity and duration of droughts, and a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and duration, with an increasing drought-free interval."),
    "FALSE-TRUE-FALSE-TRUE-TRUE-TRUE" = paste0("Bar graphs showing increasing severity and duration of droughts, and a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity and increasing drought-free interval, but increasing drought duration."),
    "FALSE-TRUE-TRUE-TRUE-TRUE-TRUE" = paste0("Bar graphs showing increasing severity and duration of droughts, and a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity, but an increasing drought duration and a shortened drought-free interval."),
    "FALSE-FALSE-TRUE-TRUE-TRUE-TRUE" = paste0("Bar graphs showing increasing severity and duration of droughts, and a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity and duration, but a shortened drought-free interval."),
    "TRUE-FALSE-TRUE-TRUE-TRUE-TRUE" = paste0("Bar graphs showing increasing severity and duration of droughts, and a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and a shortened drought-free interval, but a decrease in drought duration."),
    "FALSE-FALSE-FALSE-FALSE-TRUE-TRUE" = paste0("Bar graphs showing decreasing severity and a shorter drought-free interval for the ", CF2.Name, " climate future, with longer drought duration. The ", CF1.Name, " future shows decreasing drought severity, drought duration, and a longer drought-free interval."),
    "FALSE-FALSE-TRUE-FALSE-FALSE-FALSE" = paste0("Bar graphs showing decreasing drought severity and duration, with a longer drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity and duration, and slightly shorter drought-free interval compared to the historical period."),
    "FALSE-FALSE-FALSE-TRUE-FALSE-FALSE" = paste0("Bar graphs showing increasing severity of droughts, shorter duration, and longer drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity, drought duration, and a longer drought-free interval."),
    "FALSE-FALSE-TRUE-TRUE-FALSE-FALSE" = paste0("Bar graphs showing increasing severity of droughts, shorter duration, and longer drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity, drought duration, and a shorter drought-free interval."),
    "TRUE-TRUE-TRUE-TRUE-TRUE-FALSE" = paste0("Bar graphs showing increasing duration and severity of droughts, with a longer drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity, duration, and a shorter drought-free interval compared to the historical period."),
    "FALSE-FALSE-TRUE-FALSE-TRUE-FALSE" = paste0("Bar graphs showing decreasing severity of droughts, with a longer drought-free interval and drought duration for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity and duration, and a shorter drought-free interval compared to the historical period."),
    "TRUE-TRUE-FALSE-TRUE-FALSE-TRUE" = paste0("Bar graphs showing increasing severity of droughts and a shorter drought-free interval, but decreasing drought duration for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and duration, but a longer drought-free interval compared to the historical period."),
    "TRUE-TRUE-TRUE-FALSE-TRUE-FALSE" = paste0("Bar graphs showing increasing duration of droughts, but a longer drought-free interval and similar severity for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and duration, with a shorter drought-free interval compared to the historical period."),
    "FALSE-FALSE-FALSE-FALSE-TRUE-FALSE" = paste0("Bar graphs showing decreasing duration of droughts, a longer drought-free interval, and similar severity for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity and duration, with a longer drought-free interval compared to the historical period."),
    "TRUE-FALSE-TRUE-FALSE-FALSE-TRUE" = paste0("Bar graphs showing decreasing severity and duration of droughts, and a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and duration, with a shorter drought-free interval compared to the historical period."),
    "TRUE-FALSE-FALSE-TRUE-TRUE-FALSE" = paste0("Bar graphs showing increasing severity and duration of droughts, but a longer drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity, but decreasing drought duration and a longer drought-free interval compared to the historical period."),
    "TRUE-TRUE-FALSE-FALSE-FALSE-FALSE" = paste0("Bar graphs showing decreasing severity and duration of droughts, and a longer drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and duration, but a longer drought-free interval compared to the historical period."),
    "TRUE-FALSE-FALSE-FALSE-FALSE-TRUE" = paste0("Bar graphs showing decreasing severity and duration of droughts, but a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity, but a longer drought-free interval and shorter duration compared to the historical period."),
    "TRUE-FALSE-FALSE-FALSE-FALSE-FALSE" = paste0("Bar graphs showing decreasing severity and duration of droughts, with a longer drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity, but a longer drought-free interval and shorter duration compared to the historical period."),
    "TRUE-TRUE-TRUE-FALSE-FALSE-TRUE" = paste0("Bar graphs showing decreasing severity and duration of droughts, but a shorter drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and duration, with a shorter drought-free interval compared to the historical period."),
    "TRUE-TRUE-TRUE-TRUE-FALSE-FALSE" = paste0("Bar graphs showing increasing severity of droughts, but shorter duration and a longer drought-free interval for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and duration, with a shorter drought-free interval compared to the historical period."),
    "TRUE-TRUE-TRUE-FALSE-FALSE-FALSE" = paste0("Bar graphs showing decreasing severity of droughts, shorter duration, and longer drought-free intervals for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and duration, with a shorter drought-free interval compared to the historical period."),
    "TRUE-FALSE-TRUE-TRUE-TRUE-FALSE" = paste0("Bar graphs showing increasing severity and duration of droughts, with longer drought-free intervals for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity, decreasing drought duration, and a shorter drought-free interval compared to the historical period."),
    "FALSE-FALSE-TRUE-TRUE-TRUE-FALSE" = paste0("Bar graphs showing increasing severity and duration of droughts, with longer drought-free intervals for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity and duration, but a shorter drought-free interval compared to the historical period."),
    "TRUE-TRUE-FALSE-FALSE-TRUE-FALSE" = paste0("Bar graphs showing decreasing severity of droughts and longer drought-free intervals, but increasing duration for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows increasing drought severity and duration, but a longer drought-free interval compared to the historical period."),
    "FALSE-FALSE-FALSE-TRUE-TRUE-FALSE" = paste0("Bar graphs showing increasing severity and duration of droughts, with longer drought-free intervals for the ", CF2.Name, " climate future. The ", CF1.Name, " future shows decreasing drought severity and duration, with a longer drought-free interval compared to the historical period."),
    "FALSE-FALSE-FALSE-FALE-FALSE-TRUE" = paste0("Bar graphs showing decreasing severity and duration of droughts under both climate futures. The ", CF2.Name, " climate future is projected to experience shorter drought-free intervals, and the ", CF1.Name, " future shows longer drought-free intervals compared to the historical period.")
  )
  AltDrought <- ifelse(Exposure.Data$AltDroughtID %in% names(alt_trend_lookup), unlist(alt_trend_lookup[Exposure.Data$AltDroughtID]), " ")


Alt1 <- paste0("")
Alt2 <- paste0("Scenic view of the park.")
Alt3 <- paste0("Upper plot showing an increasing trend in temperature from 1895 to 2022, and a more dramatic increase from 1970 to 2022. Lower plot showing precipitation has ", Exposure.Data$Prcp.trend, " since 1970 and ", Exposure.Data$Prcp.trend.1900, " for the entire period record.")
Alt4 <- paste0("Map from the 5th National Climate Assessment showing the total annual increase in the amount of precipitation falling in the heaviest 1% of events, categorized by region. The precipitation change for the heaviest events are as follows: Alaska, +21 inches; Northwest, +1; Southwest, +17; Hawai'i and US-Affiliated Pacific Islands, -19; Northern Great Plains, +24; Southern Great Plains, +21; Midwest, +45; Northeast, +60; Southeast, +37; and US Caribbean, -24.")
Alt5 <- paste0("Graph showing an increase in the projected average annual temperature for both climate futures compared to the 10-year running average for the historical period, 1979 to 2020.")
Alt6 <- paste0("Graph showing ", AltPrcpCFs, " compared to the historical period.")
Alt7 <- paste0(AltExTemps)
Alt8 <- paste0(AltExPrcp)
Alt9 <- paste0("SPEI graphs showing a timeseries for drought in each climate future. Both show more years with dry conditions compared to the historical period.")
Alt10 <- paste0(AltDrought)
Alt11 <- paste0("Plot showing that the mean annual climatic water deficit is projected to ", Exposure.Data$WBtrend, " compared to the historical period.")

wb <- createWorkbook()
addWorksheet(wb, "AltText")
altText <- c(Alt1, Alt2, Alt3, Alt4, Alt5, Alt6, Alt7, Alt8, Alt9, Alt10, Alt11)
writeData(wb, sheet = "AltText", x = altText, startCol = 1, startRow = 1)
saveWorkbook(wb, file = "AltTextWorkbook.xlsx")
