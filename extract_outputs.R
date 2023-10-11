# Script that extracts output from VE-State

library(visioneval)
ematmodel <- openModel(getwd())
ematmodelresults <- ematmodel$results()$Results[[ematmodel$modelName]]

DatastoreName <- file.path(ematmodelresults$resultsPath, ematmodel$setting("DatastoreName"))
DatastoreType <- ematmodel$setting("DatastoreType")
Ma <- unique(ematmodelresults$ModelState()$Geo$Marea)
Az <- unique(ematmodelresults$ModelState()$Geo$Azone)
Years <- ematmodel$RunParam_ls$Years

# Create an output directory if one doesn't exist
output_path <- file.path(ematmodelresults$resultsPath, "output")
if(!dir.exists(output_path)){
  dir.create(output_path)
}


#==============================================================
#Define function to calculate metropolitan performance measures
#==============================================================
calcMetropolitanMeasures <- 
  function(Year, Ma, DstoreLocs_ = c("Datastore"), DstoreType = "RD") {
    
    #Prepare for datastore queries
    #-----------------------------
    QPrep_ls <- prepareForDatastoreQuery(
      DstoreLocs_ = DstoreLocs_,
      DstoreType = DstoreType
    )
    
    #Define function to create a data frame of measures
    #--------------------------------------------------
    makeMeasureDataFrame <- function(DataNames_, Ma) {
      if (length(Ma) > 1) {
        Data_XMa <- t(sapply(DataNames_, function(x) get(x)))
      } else {
        Data_XMa <- t(t(sapply(DataNames_, function(x) get(x))))
      }
      colnames(Data_XMa) <- Ma
      Measures_ <- gsub("_Ma", "", DataNames_)
      Units_ <- 
        unname(sapply(DataNames_, function(x) attributes(get(x))$Units))
      Description_ <- 
        unname(sapply(DataNames_, function(x) attributes(get(x))$Description))
      Data_df <- cbind(
        Measure = Measures_,
        data.frame(Data_XMa),
        Units = Units_,
        Description = Description_
      )
      rownames(Data_df) <- NULL
      Data_df
    }

    
    #=========================    
    #HOUSEHOLD CHARACTERISTICS
    #=========================

    #Number of households in Marea
    #-----------------------------
    MareaHhNum_Ma <- summarizeDatasets(
      Expr = "count(HhSize)",
      Units_ = c(
        HhSize = "",
        Marea = ""),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhNum_Ma <- MareaHhNum_Ma[match(Ma, MareaHhNum_Ma$Marea),"Measure"]
    attributes(MareaHhNum_Ma) <- 
      list(Units = "Households",
           Description = "Number of households residing in Marea")

    #Population of Marea
    #-------------------
    MareaHhPop_Ma <- summarizeDatasets(
      Expr = "sum(HhSize)",
      Units_ = c(
        HhSize = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhPop_Ma <- MareaHhPop_Ma[match(Ma, MareaHhPop_Ma$Marea), "Measure"]
    attributes(MareaHhPop_Ma) <- 
      list(Units = "Persons",
           Description = "Number of persons residing in Marea")
    
    #Number of workers in Marea
    #--------------------------
    MareaHhWorkers_Ma <- summarizeDatasets(
      Expr = "sum(Workers)",
      Units_ = c(
        Workers = "PRSN",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhWorkers_Ma <- MareaHhWorkers_Ma[match(Ma, MareaHhWorkers_Ma$Marea), "Measure"]
    attributes(MareaHhWorkers_Ma) <- 
      list(Units = "Workers",
           Description = "Number of workers residing in Marea")
    
    #Total household income of Marea
    #-------------------------------
    MareaHhIncome_Ma <- summarizeDatasets(
      Expr = "sum(Income)",
      Units_ = c(
        Income = "USD",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhIncome_Ma <- MareaHhIncome_Ma[match(Ma, MareaHhIncome_Ma$Marea), "Measure"]
    attributes(MareaHhIncome_Ma) <- 
      list(Units = "Base year dollars",
           Description = "Total annual income of households residing in Marea")
    
    #Number of drivers in Marea
    #--------------------------
    MareaHhDrivers_Ma <- summarizeDatasets(
      Expr = "sum(Drivers)",
      Units_ = c(
        Drivers = "PRSN",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhDrivers_Ma <- MareaHhDrivers_Ma[match(Ma, MareaHhDrivers_Ma$Marea), "Measure"]
    attributes(MareaHhDrivers_Ma) <- 
      list(Units = "Drivers",
           Description = "Number of drivers residing in Marea")
    
    #Number of vehicles owned by households in Marea
    #-----------------------------------------------
    MareaHhVehicles_Ma <- summarizeDatasets(
      Expr = "sum(NumAuto) + sum(NumLtTrk)",
      Units_ = c(
        NumAuto = "VEH",
        NumLtTrk = "VEH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhVehicles_Ma <- MareaHhVehicles_Ma[match(Ma, MareaHhVehicles_Ma$Marea), "Measure"]
    attributes(MareaHhVehicles_Ma) <- 
      list(Units = "Household light-duty vehicles",
           Description = "Total number of light-duty vehicles owned/leased by households residing in Marea")

    #Marea number of light trucks
    #----------------------------
    MareaHhLightTrucks_Ma <- summarizeDatasets(
      Expr = "sum(NumLtTrk)",
      Units_ = c(
        NumLtTrk = "VEH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhLightTrucks_Ma <- MareaHhLightTrucks_Ma[match(Ma, MareaHhLightTrucks_Ma$Marea), "Measure"]
    
    #Marea light-truck vehicle proportion
    #------------------------------------
    MareaHhLtTrkProp_Ma <- MareaHhLightTrucks_Ma / MareaHhVehicles_Ma
    attributes(MareaHhLtTrkProp_Ma) <- 
      list(Units = "Light truck proportion of household vehicles",
           Description = "Light truck proportion of light-duty vehicles owned/leased by households residing in the Marea")
    
    #Average household vehicle age for Marea
    #---------------------------------------
    MareaHhAveVehAge_Ma <- summarizeDatasets(
      Expr = "mean(Age[VehicleAccess == 'Own'])",
      Units_ = c(
        Age = "YR",
        VehicleAccess = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Vehicle",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhAveVehAge_Ma <- MareaHhAveVehAge_Ma[match(Ma, MareaHhAveVehAge_Ma$Marea), "Measure"]
    attributes(MareaHhAveVehAge_Ma) <- 
      list(Units = "Years",
           Description = "Average age of vehicles owned/leased by households residing in the Marea")
    
    #Average car service light truck proportion of car service DVMT
    #--------------------------------------------------------------
    MareaCarSvcLtTrkDvmtProp_Ma <- summarizeDatasets(
      Expr = "sum(DvmtProp[VehicleAccess != 'Own' & Type == 'LtTrk']) / sum(DvmtProp[VehicleAccess != 'Own'])",
      Units_ = c(
        DvmtProp = "",
        VehicleAccess = "",
        Type = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Vehicle",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaCarSvcLtTrkDvmtProp_Ma <- MareaCarSvcLtTrkDvmtProp_Ma[match(Ma, MareaCarSvcLtTrkDvmtProp_Ma$Marea), "Measure"]
    attributes(MareaCarSvcLtTrkDvmtProp_Ma) <- 
      list(Units = "Proportion",
           Description = "Average proportion car service vehicle DVMT in light trucks used by households residing in the Marea")
    
    #Average car service vehicle age for Marea
    #-----------------------------------------
    MareaCarSvcAveVehAge_Ma <- summarizeDatasets(
      Expr = "mean(Age[VehicleAccess != 'Own'])",
      Units_ = c(
        Age = "YR",
        VehicleAccess = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Vehicle",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaCarSvcAveVehAge_Ma <- MareaCarSvcAveVehAge_Ma[match(Ma, MareaCarSvcAveVehAge_Ma$Marea), "Measure"]
    attributes(MareaCarSvcAveVehAge_Ma) <- 
      list(Units = "Years",
           Description = "Average age of car service vehicles used by households residing in the Marea")
    
    #Average commercial service vehicle light truck proportion
    #---------------------------------------------------------
    MareaComSvcLtTrkDvmtProp <- summarizeDatasets(
      Expr = "ComSvcLtTrkProp",
      Units_ = c(
        ComSvcLtTrkProp = ""
      ),
      Table = "Region",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaComSvcLtTrkDvmtProp <- MareaComSvcLtTrkDvmtProp[,"Measure"]
    MareaComSvcLtTrkDvmtProp_Ma <- rep(MareaComSvcLtTrkDvmtProp, length(Ma))
    attributes(MareaComSvcLtTrkDvmtProp_Ma) <- 
      list(Units = "Proportion",
           Description = "Light truck proportion of commercial service vehicle DVMT in the Marea")
    rm(MareaComSvcLtTrkDvmtProp)

    #Average commercial service vehicle age
    #--------------------------------------
    MareaComSvcAveVehAge <- summarizeDatasets(
      Expr = "AveComSvcVehicleAge",
      Units_ = c(
        AveComSvcVehicleAge = "YR"
      ),
      Table = "Region",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaComSvcAveVehAge <- MareaComSvcAveVehAge[,"Measure"]
    MareaComSvcAveVehAge_Ma <- rep(MareaComSvcAveVehAge, length(Ma))
    attributes(MareaComSvcAveVehAge_Ma) <- 
      list(Units = "Years",
           Description = "Average age of commercial service vehicles used in the Marea")
    rm(MareaComSvcAveVehAge)
        
    #Number of households in urbanized area
    #--------------------------------------
    HhNum_Ma <- summarizeDatasets(
      Expr = "count(HhSize[LocType == 'Urban'])",
      Units_ = c(
        HhSize = "",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhNum_Ma <- HhNum_Ma[match(Ma, HhNum_Ma$Marea), "Measure"]
    attributes(HhNum_Ma) <- 
      list(Units = "Households",
           Description = "Number of households residing in urbanized area")
    
    #Population in urbanized area
    #----------------------------
    HhPop_Ma <- summarizeDatasets(
      Expr = "sum(HhSize[LocType == 'Urban'])",
      Units_ = c(
        HhSize = "",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhPop_Ma <- HhPop_Ma[match(Ma, HhPop_Ma$Marea), "Measure"]
    attributes(HhPop_Ma) <- 
      list(Units = "Persons",
           Description = "Number of persons residing in urbanized area")
    
    #Average household size of urbanized area households
    #---------------------------------------------------
    HhAveSize_Ma <- HhPop_Ma / HhNum_Ma
    
    #Number of workers
    #-----------------
    HhWorkers_Ma <- summarizeDatasets(
      Expr = "sum(Workers[LocType == 'Urban'])",
      Units_ = c(
        Workers = "PRSN",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhWorkers_Ma <- HhWorkers_Ma[match(Ma, HhWorkers_Ma$Marea), "Measure"]
    attributes(HhWorkers_Ma) <- 
      list(Units = "Workers",
           Description = "Number of workers residing in urbanized area")
    
    #Average workers per household
    #-----------------------------
    HhAveNumWkr_Ma <- HhWorkers_Ma / HhNum_Ma
    
    #Total household income
    #----------------------
    HhIncome_Ma <- summarizeDatasets(
      Expr = "sum(Income[LocType == 'Urban'])",
      Units_ = c(
        Income = "USD",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhIncome_Ma <- HhIncome_Ma[match(Ma, HhIncome_Ma$Marea), "Measure"]
    attributes(HhIncome_Ma) <- 
      list(Units = "Base year dollars",
           Description = "Total annual income of households residing in urbanized area")
    
    #Average income per household
    #----------------------------
    HhAveIncPerHh_Ma <- HhIncome_Ma / HhNum_Ma
    attributes(HhAveIncPerHh_Ma) <- 
      list(Units = "Base year dollars per household",
           Description = "Average annual income of households residing in urbanized area")
    
    #Average income per person
    #-------------------------
    HhAveIncPerPrsn_Ma <- HhIncome_Ma / HhPop_Ma
    attributes(HhAveIncPerPrsn_Ma) <- 
      list(Units = "Base year dollars per person",
           Description = "Average annual income per person of households residing in urbanized area")
    
    #Average income per worker
    #-------------------------
    HhAveIncPerWkr_Ma <- HhIncome_Ma / HhWorkers_Ma
    attributes(HhAveIncPerWkr_Ma) <- 
      list(Units = "Base year dollars per worker",
           Description = "Average annual income per worker of households residing in urbanized area")
    
    #Number of drivers
    #-----------------
    HhDrivers_Ma <- summarizeDatasets(
      Expr = "sum(Drivers[LocType == 'Urban'])",
      Units_ = c(
        Drivers = "PRSN",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhDrivers_Ma <- HhDrivers_Ma[match(Ma, HhDrivers_Ma$Marea), "Measure"]
    attributes(HhDrivers_Ma) <- 
      list(Units = "Drivers",
           Description = "Number of drivers residing in urbanized area")
    
    #Average number of drivers per household
    #---------------------------------------
    HhAveDvrPerHh_Ma <- HhDrivers_Ma / HhNum_Ma
    attributes(HhAveDvrPerHh_Ma) <- 
      list(Units = "Drivers per household",
           Description = "Average number of drivers in households residing in urbanized area")
    
    #Average number of drivers per person
    #------------------------------------
    HhAveDvrPerPrsn_Ma <- HhDrivers_Ma / HhPop_Ma
    attributes(HhAveDvrPerPrsn_Ma) <- 
      list(Units = "Drivers per person",
           Description = "Average number of drivers per person residing in urbanized area")
    
    #Average number of drivers per worker
    #------------------------------------
    HhAveDvrPerWkr_Ma <- HhDrivers_Ma / HhWorkers_Ma
    attributes(HhAveDvrPerWkr_Ma) <- 
      list(Units = "Drivers per worker",
           Description = "Average number of drivers per worker residing in urbanized area")
    
    #Number of vehicles
    #------------------
    HhVehicles_Ma <- summarizeDatasets(
      Expr = "sum(NumAuto[LocType == 'Urban']) + sum(NumLtTrk[LocType == 'Urban'])",
      Units_ = c(
        NumAuto = "VEH",
        NumLtTrk = "VEH",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhVehicles_Ma <- HhVehicles_Ma[match(Ma, HhVehicles_Ma$Marea),"Measure"]
    attributes(HhVehicles_Ma) <- 
      list(Units = "Household light-duty vehicles",
           Description = "Total number of light-duty vehicles owned/leased by households residing in urbanized area")
    
    #Average number of vehicles per household
    #----------------------------------------
    HhAveVehPerHh_Ma <- HhVehicles_Ma / HhNum_Ma
    attributes(HhAveVehPerHh_Ma) <- 
      list(Units = "Household light-duty vehicles per household",
           Description = "Average number of light-duty vehicles owned/leased by households residing in urbanized area")
    
    #Average number of vehicles per person
    #-------------------------------------
    HhAveVehPerPrsn_Ma <- HhVehicles_Ma / HhPop_Ma
    attributes(HhAveVehPerPrsn_Ma) <- 
      list(Units = "Household light-duty vehicles per person",
           Description = "Average number of household light-duty vehicles per person residing in urbanized area")
    
    #Average number of vehicles per worker
    #-------------------------------------
    HhAveVehPerWkr_Ma <- HhVehicles_Ma / HhWorkers_Ma
    attributes(HhAveVehPerWkr_Ma) <- 
      list(Units = "Household light-duty vehicles per worker",
           Description = "Average number of household light-duty vehicles per worker residing in urbanized area")
    
    #Average number of vehicles per driver
    #-------------------------------------
    HhAveVehPerDvr_Ma <- HhVehicles_Ma / HhDrivers_Ma
    attributes(HhAveVehPerDvr_Ma) <- 
      list(Units = "Household light-duty vehicles per driver",
           Description = "Average number of household light-duty vehicles per driver residing in urbanized area")
    
    #Number of light trucks
    #----------------------
    HhLightTrucks_Ma <- summarizeDatasets(
      Expr = "sum(NumLtTrk[LocType == 'Urban'])",
      Units_ = c(
        NumLtTrk = "VEH",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhLightTrucks_Ma <- HhLightTrucks_Ma[match(Ma, HhLightTrucks_Ma$Marea),"Measure"]
    
    #Light-truck vehicle proportion
    #------------------------------
    HhLtTrkProp_Ma <- HhLightTrucks_Ma / HhVehicles_Ma
    attributes(HhLtTrkProp_Ma) <- 
      list(Units = "Light truck proportion of household vehicles",
           Description = "Light truck proportion of light-duty vehicles owned/leased by households residing in urbanized area")
    
    #Total daily work parking cost
    #-----------------------------
    HhTotDailyWkrParkingCost_Ma <- summarizeDatasets(
      Expr = "sum(ParkingCost[LocType == 'Urban'])",
      Units_ = c(
        ParkingCost = "",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = list(
        Worker = c("ParkingCost"),
        Household = c("Marea", "LocType")
      ),
      Key = "HhId",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhTotDailyWkrParkingCost_Ma <- HhTotDailyWkrParkingCost_Ma[match(Ma, HhTotDailyWkrParkingCost_Ma$Marea), "Measure"]
    attributes(HhTotDailyWkrParkingCost_Ma) <-
      list(Units = "USD per day",
           Description = "Total daily work parking expenditures by households living in the urbanized portion of the Marea")
    
    #Total daily non-work parking cost
    #---------------------------------
    HhTotDailyOthParkingCost_Ma <- summarizeDatasets(
      Expr = "sum(OtherParkingCost[LocType == 'Urban'])",
      Units = c(
        OtherParkingCost = "",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhTotDailyOthParkingCost_Ma <- HhTotDailyOthParkingCost_Ma[match(Ma, HhTotDailyOthParkingCost_Ma$Marea), "Measure"]
    attributes(HhTotDailyOthParkingCost_Ma) <-
      list(Units = "USD per day",
           Description = "Total daily non-work parking expenditures by households living in the urbanized portion of the Marea")
    
    #Average daily household parking cost
    #------------------------------------
    HhAveDailyParkingCost_Ma <- 
      (HhTotDailyWkrParkingCost_Ma + HhTotDailyOthParkingCost_Ma) / HhNum_Ma
    attributes(HhAveDailyParkingCost_Ma) <-
      list(Units = "USD per day",
           Description = "Average daily parking expenditures by households living in the urbanized portion of the Marea")
    
    #Proportion of households that have reduced car ownership due to use of car services
    #-----------------------------------------------------------------------------------
    #All households
    PropHhReduceVehicleOwnership_Ma <- summarizeDatasets(
      Expr = "sum(OwnCostSavings > 0 & LocType == 'Urban') / count(OwnCostSavings[LocType == 'Urban'])",
      Units = c(
        OwnCostSavings = "",
        LocType = "",
        Marea = ""
      ),
      Table = list(
        Household = c("OwnCostSavings", "Marea"),
        Bzone = c("LocType")
      ),
      By_ = "Marea",
      Key = "Bzone",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    PropHhReduceVehicleOwnership_Ma <- PropHhReduceVehicleOwnership_Ma[match(Ma, PropHhReduceVehicleOwnership_Ma$Marea), "Measure"]
    attributes(PropHhReduceVehicleOwnership_Ma) <-
      list(Units = "proportion of households",
           Description = "Proportion of households living in the urbanized portion of the Marea that reduce vehicle ownership due to car service availability")
    #Households in high density (>= 10,000 persons per square mile)
    PropHhHiDenReduceVehicleOwnership_Ma <- summarizeDatasets(
      Expr = "sum(OwnCostSavings > 0 & LocType == 'Urban' & D1B >= 10000) / count(OwnCostSavings[LocType == 'Urban' & D1B >= 10000])",
      Units = c(
        OwnCostSavings = "",
        LocType = "",
        Marea = "",
        D1B = "PRSN/SQMI"
      ),
      Table = list(
        Household = c("OwnCostSavings", "Marea"),
        Bzone = c("LocType", "D1B")
      ),
      By_ = "Marea",
      Key = "Bzone",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    PropHhHiDenReduceVehicleOwnership_Ma <- PropHhHiDenReduceVehicleOwnership_Ma[match(Ma, PropHhHiDenReduceVehicleOwnership_Ma$Marea), "Measure"]
    attributes(PropHhHiDenReduceVehicleOwnership_Ma) <-
      list(Units = "proportion of households",
           Description = "Proportion of households living in the high density urbanized portion of the Marea that reduce vehicle ownership due to car service availability")
    #Households in medium density (>= 4,000 & < 10,000 persons per square mile)
    PropHhMedDenReduceVehicleOwnership_Ma <- summarizeDatasets(
      Expr = "sum(OwnCostSavings > 0 & LocType == 'Urban' & D1B >= 4000 & D1B < 10000) / count(OwnCostSavings[LocType == 'Urban' & D1B >= 4000 & D1B < 10000])",
      Units = c(
        OwnCostSavings = "",
        LocType = "",
        Marea = "",
        D1B = "PRSN/SQMI"
      ),
      Table = list(
        Household = c("OwnCostSavings", "Marea"),
        Bzone = c("LocType", "D1B")
      ),
      By_ = "Marea",
      Key = "Bzone",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    PropHhMedDenReduceVehicleOwnership_Ma <- PropHhMedDenReduceVehicleOwnership_Ma[match(Ma, PropHhMedDenReduceVehicleOwnership_Ma$Marea), "Measure"]
    attributes(PropHhMedDenReduceVehicleOwnership_Ma) <-
      list(Units = "proportion of households",
           Description = "Proportion of households living in the medium density urbanized portion of the Marea that reduce vehicle ownership due to car service availability")
    
    #Proportion of households that have high level car service available to them
    #---------------------------------------------------------------------------
    PropHhWithHiCarSvc_Ma <- summarizeDatasets(
      Expr = "sum(NumHh[CarSvcLevel == 'High' & LocType == 'Urban']) / sum(NumHh[LocType == 'Urban'])",
      Units = c(
        NumHh = "",
        CarSvcLevel = "",
        LocType = "",
        Marea = ""
      ),
      Table = "Bzone",
      By_ = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    PropHhWithHiCarSvc_Ma <- PropHhWithHiCarSvc_Ma[match(Ma, PropHhWithHiCarSvc_Ma$Marea), "Measure"]
    attributes(PropHhWithHiCarSvc_Ma) <-
      list(Units = "proportion of households",
           Description = "Proportion of households living in Bzones in the urbanized portion of the Marea that have high car service availability")
    
    #Data frame of household characteristics
    #---------------------------------------
    HhCharacteristics_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "MareaHhNum_Ma",
        "MareaHhPop_Ma",
        "MareaHhWorkers_Ma",
        "MareaHhIncome_Ma",
        "MareaHhDrivers_Ma",
        "MareaHhVehicles_Ma",
        "MareaHhLtTrkProp_Ma",
        "MareaHhAveVehAge_Ma",
        "MareaCarSvcLtTrkDvmtProp_Ma",
        "MareaCarSvcAveVehAge_Ma",
        "MareaComSvcLtTrkDvmtProp_Ma",
        "MareaComSvcAveVehAge_Ma",
        "HhNum_Ma",
        "HhPop_Ma",
        "HhAveSize_Ma",
        "HhWorkers_Ma",
        "HhAveNumWkr_Ma",
        "HhIncome_Ma",
        "HhAveIncPerHh_Ma",
        "HhAveIncPerPrsn_Ma",
        "HhAveIncPerWkr_Ma",
        "HhVehicles_Ma",
        "HhAveVehPerHh_Ma",
        "HhAveVehPerPrsn_Ma",
        "HhAveVehPerWkr_Ma",
        "HhAveVehPerDvr_Ma",
        "HhLtTrkProp_Ma",
        "HhTotDailyWkrParkingCost_Ma",
        "HhTotDailyOthParkingCost_Ma",
        "HhAveDailyParkingCost_Ma",
        "PropHhReduceVehicleOwnership_Ma",
        "PropHhHiDenReduceVehicleOwnership_Ma",
        "PropHhMedDenReduceVehicleOwnership_Ma",
        "PropHhWithHiCarSvc_Ma"
      ),
      Ma = Ma
    )

    #============================    
    #Daily Vehicle Miles Traveled
    #============================
    
    #Marea commercial service vehicle DVMT
    #-------------------------------------
    MareaComSvcDvmt_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanDvmt + ComSvcTownDvmt + ComSvcRuralDvmt)",
      Units = c(
        ComSvcUrbanDvmt = "MI/DAY",
        ComSvcTownDvmt = "MI/DAY",
        ComSvcRuralDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaComSvcDvmt_Ma <- MareaComSvcDvmt_Ma[match(Ma, MareaComSvcDvmt_Ma$Marea), "Measure"]
    attributes(MareaComSvcDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Commercial service vehicle daily vehicle miles traveled attributable to the demand of households and businesses located in the Marea"
    )
    
    #Marea public transit 'van' DVMT
    #-------------------------------
    MareaVanDvmt_Ma <- summarizeDatasets(
      Expr = "sum(VanDvmt)",
      Units = c(
        VanDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaVanDvmt_Ma <- MareaVanDvmt_Ma[match(Ma, MareaVanDvmt_Ma$Marea), "Measure"]
    attributes(MareaVanDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by on-demand transit vans in the Marea."
    )
    
    #Marea household DVMT
    #--------------------
    MareaHhDvmt_Ma <- summarizeDatasets(
      Expr = "sum(UrbanHhDvmt + TownHhDvmt + RuralHhDvmt)",
      Units_ = c(
        UrbanHhDvmt = "MI/DAY",
        TownHhDvmt = "MI/DAY",
        RuralHhDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhDvmt_Ma <- MareaHhDvmt_Ma[match(Ma, MareaHhDvmt_Ma$Marea), "Measure"]
    attributes(MareaHhDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by households residing in the Marea"
    )
    
    #Marea light-duty vehicle DVMT
    #-----------------------------
    MareaLdvDvmt_Ma <- MareaHhDvmt_Ma + MareaVanDvmt_Ma + MareaComSvcDvmt_Ma
    attributes(MareaLdvDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Sum of daily vehicle miles traveled by households residing in the Marea, commercial service travel attributable to the demand of Marea households and businesses, and on-demand transit van travel in the Marea."
    )
    
    #Marea car service proportion of household DVMT
    #----------------------------------------------
    MareaCarSvcPropHhDvmt_Ma <- summarizeDatasets(
      Expr = "sum(Dvmt[VehicleAccess != 'Own'] * DvmtProp[VehicleAccess != 'Own']) / sum(Dvmt * DvmtProp)",
      Units = c(
        Dvmt = "",
        VehicleAccess = "",
        DvmtProp = "",
        Marea = ""
      ),
      By_ <- "Marea",
      Table = list(
        Household = c("Dvmt", "Marea"),
        Vehicle = c("VehicleAccess", "DvmtProp")
      ),
      Key = "HhId",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaCarSvcPropHhDvmt_Ma <- MareaCarSvcPropHhDvmt_Ma[match(Ma, MareaCarSvcPropHhDvmt_Ma$Marea), "Measure"]
    attributes(MareaCarSvcPropHhDvmt_Ma) <- list(
      Units = "Proportion of DVMT",
      Description = "Proportion of the DVMT of households residing in the Marea using car services"
    )

    #Urbanized area car service proportion of household DVMT
    #-------------------------------------------------------
    CarSvcPropHhDvmt_Ma <- summarizeDatasets(
      Expr = "sum(Dvmt[VehicleAccess != 'Own' & LocType == 'Urban'] * DvmtProp[VehicleAccess != 'Own' & LocType == 'Urban']) / sum(Dvmt[LocType == 'Urban'] * DvmtProp[LocType == 'Urban'])",
      Units = c(
        Dvmt = "",
        VehicleAccess = "",
        DvmtProp = "",
        Marea = "",
        LocType = ""
      ),
      By_ <- "Marea",
      Table = list(
        Household = c("Dvmt", "Marea", "LocType"),
        Vehicle = c("VehicleAccess", "DvmtProp")
      ),
      Key = "HhId",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    CarSvcPropHhDvmt_Ma <- CarSvcPropHhDvmt_Ma[match(Ma, CarSvcPropHhDvmt_Ma$Marea), "Measure"]
    attributes(CarSvcPropHhDvmt_Ma) <- list(
      Units = "Proportion of DVMT",
      Description = "Proportion of the DVMT of households residing in the urbanized area using car services"
    )
    
    #Urbanized area commercial service vehicle DVMT
    #----------------------------------------------
    ComSvcDvmt_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanDvmt)",
      Units = c(
        ComSvcUrbanDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    ComSvcDvmt_Ma <- ComSvcDvmt_Ma[match(Ma, ComSvcDvmt_Ma$Marea), "Measure"]
    attributes(ComSvcDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Commercial service vehicle daily vehicle miles traveled attributable to the demand of households and businesses located in the urbanized area"
    )
    
    #Urbanized area public transit 'van' DVMT
    #----------------------------------------
    VanDvmt_Ma <- summarizeDatasets(
      Expr = "sum(VanDvmt)",
      Units = c(
        VanDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    VanDvmt_Ma <- VanDvmt_Ma[match(Ma, VanDvmt_Ma$Marea), "Measure"]
    attributes(VanDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by on-demand transit vans in the urbanized area."
    )
    
    #Urbanized area household DVMT
    #-----------------------------
    HhDvmt_Ma <- summarizeDatasets(
      Expr = "sum(UrbanHhDvmt)",
      Units = c(
        UrbanHhDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhDvmt_Ma <- HhDvmt_Ma[match(Ma, HhDvmt_Ma$Marea), "Measure"]
    attributes(HhDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by households residing in the urbanized area"
    )
    
    #Urbanized area light-duty vehicle DVMT
    #--------------------------------------
    LdvDvmt_Ma <- HhDvmt_Ma + VanDvmt_Ma + ComSvcDvmt_Ma
    attributes(LdvDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Sum of daily vehicle miles traveled by households residing in the urbanized area, commercial service travel attributable to the demand of urbanized area households and businesses, and on-demand transit van travel in the urbanized area."
    )
    
    #Urban roadway light-duty vehicle DVMT
    #-------------------------------------
    LdvRoadDvmt_Ma <- summarizeDatasets(
      Expr = "sum(LdvFwyDvmt + LdvArtDvmt + LdvOthDvmt)",
      Units = c(
        LdvFwyDvmt = "MI/DAY",
        LdvArtDvmt = "MI/DAY",
        LdvOthDvmt = "MI/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    LdvRoadDvmt_Ma <- LdvRoadDvmt_Ma[match(Ma, LdvRoadDvmt_Ma$Marea), "Measure"]
    attributes(LdvRoadDvmt_Ma) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled by light-duty vehicles on roadways within the urbanized area"
    )
    
    #Urbanized area household DVMT per household
    #-------------------------------------------
    AveHhDvmtPerHh_Ma <- HhDvmt_Ma / HhNum_Ma
    attributes(AveHhDvmtPerHh_Ma) <- list(
      Units = "Miles per day per household",
      Description = "Average daily vehicle miles traveled per household residing within the urbanized area"
    )
    
    #Urbanized area household DVMT per person
    #----------------------------------------
    AveHhDvmtPerPrsn_Ma <- HhDvmt_Ma / HhPop_Ma
    attributes(AveHhDvmtPerHh_Ma) <- list(
      Units = "Miles per day per person",
      Description = "Average daily household vehicle miles of households residing within the urbanized area per person"
    )
    
    #Urbanized area household DVMT per driver
    #----------------------------------------
    AveHhDvmtPerDvr_Ma <- HhDvmt_Ma / HhDrivers_Ma
    attributes(AveHhDvmtPerDvr_Ma) <- list(
      Units = "Miles per day per driver",
      Description = "Average daily household vehicle miles of households residing within the urbanized area per driver"
    )
    
    #Urbanized area household DVMT per vehicle
    #-----------------------------------------
    AveHhDvmtPerVeh_Ma <- HhDvmt_Ma / HhVehicles_Ma
    attributes(AveHhDvmtPerVeh_Ma) <- list(
      Units = "Miles per day per vehicle",
      Description = "Average daily household vehicle miles of households residing within the urbanized area per household vehicle"
    )
    
    #Urbanized area light-duty vehicle DVMT per household
    #----------------------------------------------------
    AveLdvDvmtPerHh_Ma <- LdvDvmt_Ma / HhNum_Ma
    attributes(AveLdvDvmtPerHh_Ma) <- list(
      Units = "Miles per day per household",
      Description = "Average of all daily light-duty vehicle miles traveled attributable to urbanized area households and businesses per household"
    )
    
    #Urbanized area light-duty vehicle DVMT per person
    #-------------------------------------------------
    AveLdvDvmtPerPrsn_Ma <- LdvDvmt_Ma / HhPop_Ma
    attributes(AveLdvDvmtPerPrsn_Ma) <- list(
      Units = "Miles per day per person",
      Description = "Average of all daily light-duty vehicle miles traveled attributable to urbanized area households and businesses per person"
    )
    
    #Urbanized area light-duty vehicle DVMT per driver
    #-------------------------------------------------
    AveLdvDvmtPerDvr_Ma <- LdvDvmt_Ma / HhDrivers_Ma
    attributes(AveLdvDvmtPerDvr_Ma) <- list(
      Units = "Miles per day per driver",
      Description = "Average of all daily light-duty vehicle miles traveled attributable to urbanized area households and businesses per driver"
    )
    
    #Urbanized area light-duty vehicle DVMT per vehicle
    #--------------------------------------------------
    AveLdvDvmtPerVeh_Ma <- LdvDvmt_Ma / HhVehicles_Ma
    attributes(AveLdvDvmtPerVeh_Ma) <- list(
      Units = "Miles per day per vehicle",
      Description = "Average of all daily light-duty vehicle miles traveled attributable to urbanized area households and businesses per vehicle"
    )
    
    #Ratio of urbanized area household DVMT to light-duty DVMT
    #---------------------------------------------------------
    PropHhDvmt_Ma <- HhDvmt_Ma / LdvDvmt_Ma
    attributes(PropHhDvmt_Ma) <- list(
      Units = "Proportion of LDV DVMT",
      Description = "Household DVMT proportion of light-duty vehicle DVMT attributable to urbanized area households and businesses"
    )
    
    #Ratio of urbanized area commercial service DVMT to light-duty DVMT
    #------------------------------------------------------------------
    PropComSvcDvmt_Ma <- ComSvcDvmt_Ma / LdvDvmt_Ma
    attributes(PropComSvcDvmt_Ma) <- list(
      Units = "Proportion of LDV DVMT",
      Description = "Commercial service DVMT proportion of light-duty vehicle DVMT attributable to urbanized area households and businesses"
    )
    
    #Ratio of urbanized area public transit van DVMT to light-duty DVMT
    #------------------------------------------------------------------
    PropVanDvmt_Ma <- VanDvmt_Ma / LdvDvmt_Ma
    PropComSvcDvmt_Ma <- ComSvcDvmt_Ma / LdvDvmt_Ma
    attributes(PropVanDvmt_Ma) <- list(
      Units = "Proportion of LDV DVMT",
      Description = "Public transit van DVMT proportion of light-duty vehicle DVMT attributable to urbanized area households and businesses"
    )
    
    #Data frame of DVMT values
    #-------------------------
    Dvmt_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "MareaHhDvmt_Ma",
        "MareaComSvcDvmt_Ma",
        "MareaVanDvmt_Ma",
        "MareaLdvDvmt_Ma",
        "MareaCarSvcPropHhDvmt_Ma",
        "HhDvmt_Ma",
        "ComSvcDvmt_Ma",
        "VanDvmt_Ma",
        "LdvDvmt_Ma",
        "LdvRoadDvmt_Ma",
        "AveHhDvmtPerHh_Ma",
        "AveHhDvmtPerPrsn_Ma",
        "AveHhDvmtPerDvr_Ma",
        "AveHhDvmtPerVeh_Ma",
        "AveLdvDvmtPerHh_Ma",
        "AveLdvDvmtPerPrsn_Ma",
        "AveLdvDvmtPerDvr_Ma",
        "AveLdvDvmtPerVeh_Ma",
        "PropHhDvmt_Ma",
        "PropComSvcDvmt_Ma",
        "PropVanDvmt_Ma",
        "CarSvcPropHhDvmt_Ma"
      ),
      Ma = Ma
    )


    #================================================
    #FUEL CONSUMPTION AND CO2E PRODUCTION OF VEHICLES
    #================================================

    #Household fuel consumption for Marea
    #------------------------------------
    MareaHhGGE_Ma <- summarizeDatasets(
      Expr = "sum(DailyGGE)",
      Units = c(
        DailyGGE = "GGE/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhGGE_Ma <- MareaHhGGE_Ma[match(Ma, MareaHhGGE_Ma$Marea), "Measure"]
    attributes(MareaHhGGE_Ma) <- list(
      Units = "Gas gallon equivalents per day",
      Description = "Average daily fuel consumption for the travel of households residing in the Marea"
    )
    
    #Commercial service fuel consumption for Marea
    #---------------------------------------------
    MareaComSvcGGE_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanGGE + ComSvcNonUrbanGGE)",
      Units = c(
        ComSvcUrbanGGE = "GGE/DAY",
        ComSvcNonUrbanGGE = "GGE/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaComSvcGGE_Ma <- MareaComSvcGGE_Ma[match(Ma, MareaComSvcGGE_Ma$Marea), "Measure"]
    attributes(MareaComSvcGGE_Ma) <- list(
      Units = "Gas gallon equivalents per day",
      Description = "Average daily fuel consumption for commercial services vehicle travel arising from households and businesses located in the Marea"
    )
    
    #Public transit van fuel consumption for Marea
    #---------------------------------------------
    MareaVanGGE_Ma <- summarizeDatasets(
      Expr = "sum(VanGGE)",
      Units = c(
        VanGGE = "GGE/DAY",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaVanGGE_Ma <- MareaVanGGE_Ma[match(Ma, MareaVanGGE_Ma$Marea), "Measure"]
    attributes(MareaVanGGE_Ma) <- list(
      Units = "Gas gallon equivalents per day",
      Description = "Average daily fuel consumption for public transit van in the Marea"
    )
    
    #Light-duty vehicle fuel consumption for Marea
    #---------------------------------------------
    MareaLdvGGE_Ma <- MareaHhGGE_Ma + MareaComSvcGGE_Ma + MareaVanGGE_Ma
    attributes(MareaVanGGE_Ma) <- list(
      Units = "Gas gallon equivalents per day",
      Description = "Average daily fuel consumption for light-duty vehicle travel attributable to households and businesses in the Marea"
    )
    
    #Household CO2e for Marea
    #------------------------
    MareaHhCO2e_Ma <- summarizeDatasets(
      Expr = "sum(DailyCO2e)",
      Units = c(
        DailyCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaHhCO2e_Ma <- MareaHhCO2e_Ma[match(Ma, MareaHhCO2e_Ma$Marea), "Measure"]
    attributes(MareaHhCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from light-duty vehicle travel by households residing in the Marea"
    )
    
    #Commercial service CO2e for Marea
    #---------------------------------
    MareaComSvcCO2e_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanCO2e + ComSvcNonUrbanCO2e)",
      Units = c(
        ComSvcUrbanCO2e = "MT/YR",
        ComSvcNonUrbanCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaComSvcCO2e_Ma <- MareaComSvcCO2e_Ma[match(Ma, MareaComSvcCO2e_Ma$Marea), "Measure"]
    attributes(MareaComSvcCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from commercial service light-duty vehicle travel attributable to households and businesses in the Marea"
    )
    
    #Van CO2e for Marea
    #------------------
    MareaVanCO2e_Ma <- summarizeDatasets(
      Expr = "sum(VanCO2e)",
      Units = c(
        VanCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MareaVanCO2e_Ma <- MareaVanCO2e_Ma[match(Ma, MareaVanCO2e_Ma$Marea), "Measure"]
    attributes(MareaVanCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from public transit van travel in the Marea"
    )
    
    #Light-duty vehicle CO2e for Marea
    #---------------------------------
    MareaLdvCO2e_Ma <- MareaHhCO2e_Ma + MareaVanCO2e_Ma + MareaComSvcCO2e_Ma
    attributes(MareaLdvCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from light-duty vehicle travel of households and businesses in the Marea"
    )
    
    #Light-duty vehicle CO2e per person for Marea
    #--------------------------------------------
    MareaLdvCO2ePerPrsn_Ma <- MareaLdvCO2e_Ma / MareaHhPop_Ma
    attributes(MareaLdvCO2ePerPrsn_Ma) <- list(
      Units = "Metric tons CO2e per year per person",
      Description = "Average per capita annual production of greenhouse gas emissions from light-duty vehicle travel of households and businesses in the Marea"
    )

    #Household CO2e for urbanized area
    #---------------------------------
    HhCO2e_Ma <- summarizeDatasets(
      Expr = "sum(DailyCO2e[LocType == 'Urban'])",
      Units = c(
        DailyCO2e = "MT/YR",
        LocType = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HhCO2e_Ma <- HhCO2e_Ma[match(Ma, HhCO2e_Ma$Marea), "Measure"]
    attributes(HhCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from light-duty vehicle travel by households residing in the urbanized area"
    )
    
    #Commercial service CO2e for urbanized area
    #------------------------------------------
    ComSvcCO2e_Ma <- summarizeDatasets(
      Expr = "sum(ComSvcUrbanCO2e)",
      Units = c(
        ComSvcUrbanCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    ComSvcCO2e_Ma <- ComSvcCO2e_Ma[match(Ma, ComSvcCO2e_Ma$Marea), "Measure"]
    attributes(ComSvcCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from commercial service light-duty vehicle travel attributable to households and businesses in the urbanized area"
    )
    
    #Van CO2e for urbanized area
    #---------------------------
    VanCO2e_Ma <- summarizeDatasets(
      Expr = "sum(VanCO2e)",
      Units = c(
        VanCO2e = "MT/YR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    VanCO2e_Ma <- VanCO2e_Ma[match(Ma, VanCO2e_Ma$Marea), "Measure"]
    attributes(VanCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from public transit van travel in the urbanized area"
    )
    
    #Bus CO2e rate for urbanized area
    #--------------------------------
    BusCO2eRate_Ma <- summarizeDatasets(
      Expr = "sum(BusCO2eRate) * 1000",
      Units = c(
        BusCO2eRate = "KG/MI",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    BusCO2eRate_Ma <- BusCO2eRate_Ma[match(Ma, BusCO2eRate_Ma$Marea), "Measure"]
    attributes(BusCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of public transit bus travel in the urbanized area"
    )
    
    #Heavy truck CO2e rate for urbanized area
    #----------------------------------------
    HvyTrkAveUrbanCO2eRate_Ma <- summarizeDatasets(
      Expr = "sum(HvyTrkAveUrbanCO2eRate) * 1000",
      Units = c(
        HvyTrkAveUrbanCO2eRate = "KG/MI",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HvyTrkAveUrbanCO2eRate_Ma <- HvyTrkAveUrbanCO2eRate_Ma[match(Ma, HvyTrkAveUrbanCO2eRate_Ma$Marea), "Measure"]
    attributes(HvyTrkAveUrbanCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of heavy truck travel in the urbanized area"
    )
    #Light-duty vehicle CO2e for urbanized area
    #------------------------------------------
    LdvCO2e_Ma <- HhCO2e_Ma + VanCO2e_Ma + ComSvcCO2e_Ma
    attributes(LdvCO2e_Ma) <- list(
      Units = "Metric tons CO2e per year",
      Description = "Average annual production of greenhouse gas emissions from light-duty vehicle travel of households and businesses in the urbanized area"
    )
    
    #Light-duty vehicle CO2e per capita for urbanized area
    #-----------------------------------------------------
    LdvCO2ePerPrsn_Ma <- LdvCO2e_Ma / HhPop_Ma
    attributes(LdvCO2ePerPrsn_Ma) <- list(
      Units = "Metric tons CO2e per year per person",
      Description = "Average per capita annual production of greenhouse gas emissions from light-duty vehicle travel of households and businesses in the urbanized area"
    )
    
    #Household CO2e rate for urbanized area
    #--------------------------------------
    HhCO2eRate_Ma <- (1e6 * HhCO2e_Ma) / (365 * HhDvmt_Ma)
    attributes(HhCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of vehicle travel by households residing in the urbanized area"
    )
    
    #Commercial service CO2e rate for urbanized area
    #-----------------------------------------------
    ComSvcCO2eRate_Ma <- (1e6 * ComSvcCO2e_Ma) / (365 * ComSvcDvmt_Ma)
    attributes(ComSvcCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of commercial service vehicle travel attributable to households and businesses in the urbanized area"
    )
    
    #Van CO2e rate for urbanized area
    #--------------------------------
    VanCO2eRate_Ma <- (1e6 * VanCO2e_Ma) / (365 * VanDvmt_Ma)
    attributes(VanCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of public transit van travel in the urbanized area"
    )
    
    #Light-duty vehicle CO2e rate
    #----------------------------
    LdvCO2eRate_Ma <- (1e6 * LdvCO2e_Ma) / (365 * LdvDvmt_Ma)
    attributes(LdvCO2eRate_Ma) <- list(
      Units = "Grams CO2e per mile",
      Description = "Average greenhouse gas emissions per mile of light-duty vehicle travel attributable to households and businesses in the urbanized area"
    )
    
    #Data frame of fuel and CO2e values
    #----------------------------------
    CO2e_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "MareaHhGGE_Ma",
        "MareaComSvcGGE_Ma",
        "MareaVanGGE_Ma",
        "MareaLdvGGE_Ma",
        "MareaHhCO2e_Ma",
        "MareaComSvcCO2e_Ma",
        "MareaVanCO2e_Ma",
        "MareaLdvCO2e_Ma",
        "MareaLdvCO2ePerPrsn_Ma",
        "HhCO2e_Ma",
        "ComSvcCO2e_Ma",
        "VanCO2e_Ma",
        "BusCO2eRate_Ma",
        "HvyTrkAveUrbanCO2eRate_Ma",
        "LdvCO2e_Ma",
        "LdvCO2ePerPrsn_Ma",
        "HhCO2eRate_Ma",
        "ComSvcCO2eRate_Ma",
        "VanCO2eRate_Ma",
        "LdvCO2eRate_Ma"
        ),
      Ma = Ma
      )
    
    #====================        
    #CONGESTION AND SPEED
    #====================
    
    #Extreme congestion on freeways
    #------------------------------
    FwyDvmtPropExtCong <- summarizeDatasets(
      Expr = "FwyDvmtPropExtCong",
      Units = c(
        FwyDvmtPropExtCong = "proportion",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    FwyDvmtPropExtCong <- FwyDvmtPropExtCong[match(Ma, FwyDvmtPropExtCong$Marea), "Measure"]
    attributes(FwyDvmtPropExtCong) <- list(
      Units = "proportion",
      Description = "Average freeway speed (miles per hour) when congestion is extreme"
    )
    #Extreme congestion on arterials
    #-------------------------------
    ArtDvmtPropExtCong <- summarizeDatasets(
      Expr = "ArtDvmtPropExtCong",
      Units = c(
        ArtDvmtPropExtCong = "proportion",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    ArtDvmtPropExtCong <- ArtDvmtPropExtCong[match(Ma, ArtDvmtPropExtCong$Marea), "Measure"]
    attributes(ArtDvmtPropExtCong) <- list(
      Units = "proportion",
      Description = "Average arterial speed (miles per hour) when congestion is extreme"
    )
    #Severe congestion on freeways
    #------------------------------
    FwyDvmtPropSevCong <- summarizeDatasets(
      Expr = "FwyDvmtPropSevCong",
      Units = c(
        FwyDvmtPropSevCong = "proportion",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    FwyDvmtPropSevCong <- FwyDvmtPropSevCong[match(Ma, FwyDvmtPropSevCong$Marea), "Measure"]
    attributes(FwyDvmtPropSevCong) <- list(
      Units = "proportion",
      Description = "Average freeway speed (miles per hour) when congestion is Severe"
    )
    #Severe congestion on arterials
    #-------------------------------
    ArtDvmtPropSevCong <- summarizeDatasets(
      Expr = "ArtDvmtPropSevCong",
      Units = c(
        ArtDvmtPropSevCong = "proportion",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    ArtDvmtPropSevCong <- ArtDvmtPropSevCong[match(Ma, ArtDvmtPropSevCong$Marea), "Measure"]
    attributes(ArtDvmtPropSevCong) <- list(
      Units = "proportion",
      Description = "Average arterial speed (miles per hour) when congestion is Severe"
    )
    #Travel time reliability with extreme congestion on freeways
    #----------------------------------------------------------
    FwyExtCongTTI <- summarizeDatasets(
      Expr = "FwyExtCong_TTI",
      Units = c(
        FwyExtCong_TTI = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    FwyExtCongTTI <- FwyExtCongTTI[match(Ma, FwyExtCongTTI$Marea), "Measure"]
    attributes(FwyExtCongTTI) <- list(
      Units = "proportion",
      Description = "Average freeway travel time index when congestion is extreme"
    )
    #Travel time reliability with extreme congestion on arterials
    #------------------------------------------------------------
    ArtExtCongTTI <- summarizeDatasets(
      Expr = "ArtExtCong_TTI",
      Units = c(
        ArtExtCong_TTI = "",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    ArtExtCongTTI <- ArtExtCongTTI[match(Ma, ArtExtCongTTI$Marea), "Measure"]
    attributes(ArtExtCongTTI) <- list(
      Units = "proportion",
      Description = "Average arterial travel time index when congestion is extreme"
    )
    #Light-duty vehicle average speed
    #--------------------------------
    LdvAveSpeed <- summarizeDatasets(
      Expr = "LdvAveSpeed",
      Units = c(
        LdvAveSpeed = "MI/HR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    LdvAveSpeed <- LdvAveSpeed[match(Ma, LdvAveSpeed$Marea), "Measure"]
    attributes(LdvAveSpeed) <- list(
      Units = "Miles per hour",
      Description = "Average speed (miles per hour) of light-duty vehicle travel on urban area roads"
    )
    #Heavy truck average speed
    #-------------------------
    HvyTrkAveSpeed <- summarizeDatasets(
      Expr = "HvyTrkAveSpeed",
      Units = c(
        HvyTrkAveSpeed = "MI/HR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HvyTrkAveSpeed <- HvyTrkAveSpeed[match(Ma, HvyTrkAveSpeed$Marea), "Measure"]
    attributes(HvyTrkAveSpeed) <- list(
      Units = "Miles per hour",
      Description = "Average speed (miles per hour) of heavy truck travel on urban area roads"
    )
    #Bus average speed
    #-----------------
    BusAveSpeed <- summarizeDatasets(
      Expr = "(BusAveSpeed)",
      Units = c(
        BusAveSpeed = "MI/HR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    BusAveSpeed <- BusAveSpeed[match(Ma, BusAveSpeed$Marea), "Measure"]
    attributes(BusAveSpeed) <- list(
      Units = "Miles per hour",
      Description = "Average speed (miles per hour) of bus travel on urban area roads"
    )
    #Non-urban average speed
    #-----------------------
    NonUrbanAveSpeed <- summarizeDatasets(
      Expr = "NonUrbanAveSpeed",
      Units = c(
        NonUrbanAveSpeed = "MI/HR",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    NonUrbanAveSpeed <- NonUrbanAveSpeed[match(Ma, NonUrbanAveSpeed$Marea), "Measure"]
    attributes(NonUrbanAveSpeed) <- list(
      Units = "Miles per hour",
      Description = "Average speed (miles per hour) of vehicle travel on non-urban area roads"
    )
    
    #Data frame of congestion and speed measures
    #-------------------------------------------
    Cong_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "FwyDvmtPropSevCong",
        "ArtDvmtPropSevCong",
        "FwyDvmtPropExtCong",
        "ArtDvmtPropExtCong",
        "FwyExtCongTTI",
        "ArtExtCongTTI",
        "LdvAveSpeed",
        "HvyTrkAveSpeed",
        "BusAveSpeed",
        "NonUrbanAveSpeed"
      ),
      Ma = Ma
    )
    
    #======       
    #SAFETY
    #======
    
    #Auto crashes with fatalities
    #----------------------------
    AutoFatalCrash <- summarizeDatasets(
      Expr = "sum(AutoFatalCrashUrban + AutoFatalCrashTown + AutoFatalCrashRural)",
      Units = c(
        AutoFatalCrashUrban = "CRASH",
        AutoFatalCrashTown = "CRASH", 
        AutoFatalCrashRural = "CRASH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    AutoFatalCrash <- AutoFatalCrash[match(Ma, AutoFatalCrash$Marea), "Measure"]
    attributes(AutoFatalCrash) <- list(
      Units = "Crashes",
      Description = "Number of yearly auto fatal crashes in Marea."
    )
    
    #Auto crashes with injuries
    #--------------------------
    AutoInjuryCrash <- summarizeDatasets(
      Expr = "sum(AutoInjuryCrashUrban + AutoInjuryCrashTown + AutoInjuryCrashRural)",
      Units = c(
        AutoInjuryCrashUrban = "CRASH",
        AutoInjuryCrashTown = "CRASH", 
        AutoInjuryCrashRural = "CRASH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    AutoInjuryCrash <- AutoInjuryCrash[match(Ma, AutoInjuryCrash$Marea), "Measure"]
    attributes(AutoInjuryCrash) <- list(
      Units = "Crashes",
      Description = "Number of yearly auto injury crashes in Marea."
    )
    
    #Bike crashes with fatalities
    #----------------------------
    BikeFatalCrash <- summarizeDatasets(
      Expr = "sum(BikeFatalCrash)",
      Units = c(
        BikeFatalCrash = "CRASH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    BikeFatalCrash <- BikeFatalCrash[match(Ma, BikeFatalCrash$Marea), "Measure"]
    attributes(BikeFatalCrash) <- list(
      Units = "Crashes",
      Description = "Number of yearly bike fatal crashes in Marea."
    )
    
    #Bike crashes with injuries
    #--------------------------
    BikeInjuryCrash <- summarizeDatasets(
      Expr = "sum(BikeInjuryCrash)",
      Units = c(
        BikeInjuryCrash = "CRASH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    BikeInjuryCrash <- BikeInjuryCrash[match(Ma, BikeInjuryCrash$Marea), "Measure"]
    attributes(BikeInjuryCrash) <- list(
      Units = "Crashes",
      Description = "Number of yearly bike injury crashes in Marea."
    )
    
    #Walk crashes with fatalities
    #----------------------------
    WalkFatalCrash <- summarizeDatasets(
      Expr = "sum(WalkFatalCrash)",
      Units = c(
        WalkFatalCrash = "CRASH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    WalkFatalCrash <- WalkFatalCrash[match(Ma, WalkFatalCrash$Marea), "Measure"]
    attributes(WalkFatalCrash) <- list(
      Units = "Crashes",
      Description = "Number of yearly walk fatal crashes in Marea."
    )
    
    #Walk crashes with injuries
    #--------------------------
    WalkInjuryCrash <- summarizeDatasets(
      Expr = "sum(WalkInjuryCrash)",
      Units = c(
        WalkInjuryCrash = "CRASH",
        Marea = ""
      ),
      By_ = "Marea",
      Table = "Marea",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    WalkInjuryCrash <- WalkInjuryCrash[match(Ma, WalkInjuryCrash$Marea), "Measure"]
    attributes(WalkInjuryCrash) <- list(
      Units = "Crashes",
      Description = "Number of yearly walk injury crashes in Marea."
    )
    
    #Data frame of safety measures
    #-----------------------------
    Safe_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "AutoFatalCrash",
        "AutoInjuryCrash",
        "BikeFatalCrash",
        "BikeInjuryCrash",
        "WalkFatalCrash",
        "WalkInjuryCrash"
      ),
      Ma = Ma
    )
    
    #Return data frame of all results
    #--------------------------
    rbind(
      HhCharacteristics_df,
      # LuCharacteristics_df,
      Dvmt_df,
      CO2e_df,
      Cong_df,
      Safe_df
      # Cost_df
    )
  }


for (Year in getYears()) {
  cat(paste0("metro_measures_", Year, ".csv"), "\n")
  write.csv(calcMetropolitanMeasures(Year = Year, Ma = Ma,
                                     DstoreLocs_ = DatastoreName, DstoreType = DatastoreType),
            row.names = FALSE,
            file = file.path(output_path, paste0("metro_measures_", Year, ".csv")))
}


calcCountyMeasures <- 
  function(Year, Az, DstoreLocs_ = c("Datastore"), DstoreType = "RD") {
    
    #Prepare for datastore queries
    #-----------------------------
    QPrep_ls <- prepareForDatastoreQuery(
      DstoreLocs_ = DstoreLocs_,
      DstoreType = DstoreType
    )
    
    #Define function to create a data frame of measures
    #--------------------------------------------------
    makeMeasureDataFrame <- function(DataNames_, Az) {
      if (length(Az) > 1) {
        Data_XAz <- t(sapply(DataNames_, function(x) get(x)))
      } else {
        Data_XAz <- t(t(sapply(DataNames_, function(x) get(x))))
      }
      colnames(Data_XAz) <- Az
      Measures_ <- gsub("_Az", "", DataNames_)
      Units <- 
        unname(sapply(DataNames_, function(x) attributes(get(x))$Units))
      Description_ <- 
        unname(sapply(DataNames_, function(x) attributes(get(x))$Description))
      Data_df <- cbind(
        Measure = Measures_,
        data.frame(Data_XAz),
        Units = Units,
        Description = Description_
      )
      rownames(Data_df) <- NULL
      Data_df
    }
    
    #=========================    
    #HOUSEHOLD CHARACTERISTICS
    #=========================
    
    #Number of households in Marea
    #-----------------------------
    Households <- summarizeDatasets(
      Expr = "count(HhSize)",
      Units = c(
        HhSize = "",
        Azone = ""
        ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    Households <- Households[match(Az, Households$Azone), "Measure"]
    attributes(Households) <- 
      list(Units = "Households",
           Description = "Number of households residing in Azone")
    
    #Population
    #----------
    Population <- summarizeDatasets(
      Expr = "sum(HhSize)",
      Units = c(
        HhSize = "PRSN",
        Azone = ""
        ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    Population <- Population[match(Az, Population$Azone), "Measure"]
    attributes(Population) <- list(
      Units = "persons",
      Description = "Total population"
    )
    
    #Population in urban households
    #------------------------------
    PopulationUrban <- summarizeDatasets(
      Expr = "sum(HhSize[LocType == 'Urban'])",
      Units = c(
        HhSize = "PRSN",
        LocType = "category",
        Azone = ""
        ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    PopulationUrban <- PopulationUrban[match(Az, PopulationUrban$Azone), "Measure"]
    attributes(PopulationUrban) <- list(
      Units = "person",
      Description = "Urban population"
    )
    
    #Population in town households
    #------------------------------
    PopulationTown <- summarizeDatasets(
      Expr = "sum(HhSize[LocType == 'Town'])",
      Units = c(
        HhSize = "PRSN",
        LocType = "category",
        Azone = ""
        ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    PopulationTown <- PopulationTown[match(Az, PopulationTown$Azone), "Measure"]
    attributes(PopulationTown) <- list(
      Units = "person",
      Description = "Town population"
    )
    
    #Population in rural households
    #------------------------------
    PopulationRural <- summarizeDatasets(
      Expr = "sum(HhSize[LocType=='Rural'])",
      Units = c(
        HhSize = "PRSN",
        LocType = "category",
        Azone = ""
        ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    PopulationRural <- PopulationRural[match(Az, PopulationRural$Azone), "Measure"]
    attributes(PopulationRural) <- list(
      Units = "person",
      Description = "Rural population"
    )
    
    #Data frame of household characteristics
    #---------------------------------------
    HhCharacteristics_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "Households",
        "PopulationUrban",
        "PopulationTown",
        "PopulationRural"
      ),
      Az = Az
    )
    
    #====================   
    #Daily Miles Traveled
    #====================
    
    #Household DVMT
    #--------------
    HouseholdDvmt <- summarizeDatasets(
      Expr = "sum(Dvmt)",
      Units = c(
        Dvmt = "MI/DAY",
        Azone = ""
        ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    HouseholdDvmt <- HouseholdDvmt[match(Az, HouseholdDvmt$Azone), "Measure"]
    attributes(HouseholdDvmt) <- list(
      Units = "miles per day",
      Description = "Total DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters"
    )
    
    #County household DVMT per capita
    #-----------------------------------
    HouseholdDvmtPerPrsn <- HouseholdDvmt / Population
    attributes(HouseholdDvmtPerPrsn ) <- list(
      Units = "Miles per day",
      Description = "Daily vehicle miles traveled per capita residing in the Azone"
    )
    
    #Transit trips
    #-------------
    TransitTrips <- summarizeDatasets(
      Expr = "sum(TransitTrips)",
      Units = c(
        TransitTrips = "TRIPS/YR",
        Azone = ""
        ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    TransitTrips <- TransitTrips[match(Az, TransitTrips$Azone), "Measure"]
    attributes(TransitTrips) <- list(
      Units = "trips per year",
      Description = "Annual total household transit trips"
    )
    
    #Transit trips per capita
    #------------------------
    TransitTripsPerPrsn <- TransitTrips / Population
    attributes(TransitTripsPerPrsn) <- list(
      Units = "trips per year per capita",
      Description = "Annual transit trips per capita"
    )
    
    #Bike trips
    #----------
    BikeTrips <- summarizeDatasets(
      Expr = "sum(BikeTrips)",
      Units = c(
        BikeTrips = "TRIPS/YR",
        Azone = ""
      ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    BikeTrips <- BikeTrips[match(Az, BikeTrips$Azone), "Measure"]
    attributes(BikeTrips) <- list(
      Units = "trips per year",
      Description = "Annual total household bike trips"
    )
    
    #Bike trips per capita
    #---------------------
    BikeTripsPerPrsn <- BikeTrips / Population
    attributes(BikeTripsPerPrsn) <- list(
      Units = "trips per year per capita",
      Description = "Annual bike trips per capita"
    )
    
    #Walk trips
    #----------
    WalkTrips <- summarizeDatasets(
      Expr = "sum(WalkTrips)",
      Units = c(
        WalkTrips = "TRIPS/YR",
        Azone = ""
      ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    WalkTrips <- WalkTrips[match(Az, WalkTrips$Azone), "Measure"]
    attributes(WalkTrips) <- list(
      Units = "trips per year",
      Description = "Annual total household walk trips"
    )
    
    #Walk trips per capita
    #---------------------
    WalkTripsPerPrsn <- WalkTrips / Population
    attributes(WalkTripsPerPrsn) <- list(
      Units = "trips per year per capita",
      Description = "Annual walk trips per capita"
    )
    
    #Number of household vehicles
    #----------------------------
    NumHouseholdVehicles <- summarizeDatasets(
      Expr = "sum(NumAuto) + sum(NumLtTrk)",
      Units =  c(
        NumAuto = "VEH",
        NumLtTrk = "VEH",
        Azone = ""
      ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    NumHouseholdVehicles <- NumHouseholdVehicles[match(Az, NumHouseholdVehicles$Azone), "Measure"]
    attributes(NumHouseholdVehicles) <- list(
      Units = "vehicles",
      Description = "Number of vehicles owned or leased by households"
    )
    
    #Data frame of DVMT values
    #-------------------------
    Dvmt_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "HouseholdDvmt",
        "HouseholdDvmtPerPrsn",
        "TransitTripsPerPrsn",
        "BikeTripsPerPrsn",
        "WalkTripsPerPrsn",
        "NumHouseholdVehicles"
      ),
      Az = Az
    )
    
    #======================
    #Vehicle Ownership Cost
    #======================
    
    #Ownership cost proportion
    #-------------------------
    OwnCostProp <- summarizeDatasets(
      Expr = "sum(OwnCost)/sum(Income)",
      Units = c(
        OwnCost = "USD",
        Income = "USD",
        Azone = ""
      ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    OwnCostProp <- OwnCostProp[match(Az, OwnCostProp$Azone), "Measure"]
    attributes(OwnCostProp) <- list(
      Units = "proportion",
      Description = "Vehicle ownership cost as a proportion of household income"
    )
    
    #Ownership cost for households with less than 25K income
    #-------------------------------------------------------
    OwnCostPropHhLess25K <- summarizeDatasets(
      Expr = "sum(OwnCost[Income < 25000])/sum(Income[Income < 25000])",
      Units = c(
        OwnCost = "USD",
        Income = "USD",
        Azone = ""
      ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    OwnCostPropHhLess25K <- OwnCostPropHhLess25K[match(Az, OwnCostPropHhLess25K$Azone), "Measure"]
    attributes(OwnCostPropHhLess25K) <- list(
      Units = "proportion",
      Description = "Vehicle ownerhsip cost as a proportion of household income for households earning less than 25K"
    )
    
    #Operating cost
    #--------------
    VehCostProp <- summarizeDatasets(
      Expr = "sum(AveVehCostPM * Dvmt)/sum(Income)",
      Units = c(
        AveVehCostPM = "USD",
        Dvmt = "MI/DAY",
        Income = "USD",
        Azone = ""
      ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    VehCostProp <- VehCostProp[match(Az, VehCostProp$Azone), "Measure"]
    attributes(VehCostProp) <- list(
      Units = "proportion",
      Description = "Vehicle operating cost as a proportion of household income"
    )
    
    #Operating cost for households with less than 25K income
    #-------------------------------------------------------
    VehCostPropHhLess25K <- summarizeDatasets(
      Expr = "sum(AveVehCostPM[Income < 25000] * Dvmt[Income < 25000])/sum(Income[Income < 25000])",
      Units = c(
        AveVehCostPM = "USD",
        Dvmt = "MI/DAY",
        Income = "USD",
        Azone = ""
      ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    VehCostPropHhLess25K <- VehCostPropHhLess25K[match(Az, VehCostPropHhLess25K$Azone), "Measure"]
    attributes(VehCostPropHhLess25K) <- list(
      Units = "proportion",
      Description = "Vehicle operating cost as a proportion of household income for households earning less than 25K"
    )
    
    #Operating cost for households with residents 65 and older
    #--------------------------------------------------------
    VehCostPropHhAge65Plus <- summarizeDatasets(
      Expr = "sum(AveVehCostPM[Age65Plus > 0] * Dvmt[Age65Plus > 0])/sum(Income[Age65Plus > 0])",
      Units = c(
        AveVehCostPM = "USD",
        Dvmt = "MI/DAY",
        Income = "USD",
        Age65Plus = "AGE",
        Azone = ""
      ),
      By_ = "Azone",
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    VehCostPropHhAge65Plus <- VehCostPropHhAge65Plus[match(Az, VehCostPropHhAge65Plus$Azone), "Measure"]
    attributes(VehCostPropHhAge65Plus) <- list(
      Units = "proportion",
      Description = "Vehicle operating cost as a proportion of household income for households with residents age 65 and older"
    )
    
    #Data frame of cost values
    #-------------------------
    Cost_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "OwnCostProp",
        "OwnCostPropHhLess25K",
        "VehCostProp",
        "VehCostPropHhLess25K",
        "VehCostPropHhAge65Plus"
      ),
      Az = Az
    )
    
    #Return data frame of all results
    #--------------------------
    rbind(
      HhCharacteristics_df,
      Dvmt_df,
      Cost_df
    )
  }


for (Year in getYears()) {
  cat(paste0("county_measures_", Year, ".csv"), "\n")
  write.csv(calcCountyMeasures(Year = Year, Az = Az,
                               DstoreLocs_ = DatastoreName, DstoreType = DatastoreType),
            row.names = FALSE,
            file = file.path(output_path, paste0("county_measures_", Year, ".csv")))
}

calcCountyLocMeasures <- 
  function(Year, Az, DstoreLocs_ = c("Datastore"), DstoreType = "RD") {
    
    #Prepare for datastore queries
    #-----------------------------
    QPrep_ls <- prepareForDatastoreQuery(
      DstoreLocs_ = DstoreLocs_,
      DstoreType = DstoreType
    )
    
    #Get county-location-type combinations
    #-------------------------------------
    AzLoc <- c()
    for (a in Az) {
      az <- rep(a, 3)
      az[1] <- paste0(az[1],"_","Rural")
      az[2] <- paste0(az[2],"_","Town")
      az[3] <- paste0(az[3],"_","Urban")
      AzLoc <- c(AzLoc, az)
    }
    
    #Define function to create a data frame of measures
    #--------------------------------------------------
    makeMeasureDataFrame <- function(DataNames_, Az) {
      if (length(Az) > 1) {
        Data_XAzLoc <- t(sapply(DataNames_, function(x) get(x)))
      } else {
        Data_XAzLoc <- t(t(sapply(DataNames_, function(x) get(x))))
      }
      colnames(Data_XAzLoc) <- AzLoc
      Measures_ <- gsub("_AzLoc", "", DataNames_)
      Units_ <- 
        unname(sapply(DataNames_, function(x) attributes(get(x))$Units))
      Description_ <- 
        unname(sapply(DataNames_, function(x) attributes(get(x))$Description))
      Data_df <- cbind(
        Measure = Measures_,
        data.frame(Data_XAzLoc),
        Units = Units_,
        Description = Description_
      )
      rownames(Data_df) <- NULL
      Data_df
    }
    
    #=========================    
    #HOUSEHOLD CHARACTERISTICS
    #=========================
    
    #Population
    #----------
    Population <- summarizeDatasets(
      Expr = "sum(HhSize)",
      Units_ = c(
        HhSize = "PRSN",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(Population$Azone, "_", Population$LocType)
    Population <- setNames(Population$Measure, MeasureNames)[AzLoc]
    attributes(Population) <- list(
      Units = "persons",
      Description = "Total population"
    )
    
    #Number of households
    #--------------------
    Households <- summarizeDatasets(
      Expr = "count(HhSize)",
      Units_ = c(
        HhSize = "",
        Azone = "",
        LocType = ""
        ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(Households$Azone, "_", Households$LocType)
    Households <- setNames(Households$Measure, MeasureNames)[AzLoc]
    attributes(Households) <- 
      list(Units = "Households",
           Description = "Number of households residing in Azone Location Type")
    
    #Data frame of household characteristics
    #---------------------------------------
    HhCharacteristics_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "Population",
        "Households"
      ),
      Az = Az
    )
    
    #Return data frame of all results
    #--------------------------
    rbind(
      HhCharacteristics_df
    )
    
    #====================   
    #Daily Miles Traveled
    #====================
    
    #Household DVMT per capita
    #-------------------------
    HouseholdDvmtPerPrsn <- summarizeDatasets(
      Expr = "sum(Dvmt)/sum(HhSize)",
      Units = c(
        Dvmt = "MI/DAY",
        HhSize = "people",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(HouseholdDvmtPerPrsn$Azone, "_", HouseholdDvmtPerPrsn$LocType)
    HouseholdDvmtPerPrsn <- setNames(HouseholdDvmtPerPrsn$Measure, MeasureNames)[AzLoc]
    attributes(HouseholdDvmtPerPrsn) <- list(
      Units = "miles per day",
      Description = "Total DVMT (in owned and car service vehicles) per capita in households and non-institutional group quarters"
    )
    
    #Household car service DVMT
    #--------------------------
    HouseholdCarSvcDvmt <- summarizeDatasets(
      Expr = "sum(Dvmt[VehicleAccess != 'Own'] * DvmtProp[VehicleAccess != 'Own'])",
      Units = c(
        Dvmt = "MI/DAY",
        DvmtProp = "",
        VehicleAccess = "",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = list(
        Household = c("Dvmt", "LocType", "Azone"),
        Vehicle = c("DvmtProp", "VehicleAccess")
      ),
      Key = "HhId",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(HouseholdCarSvcDvmt$Azone, "_", HouseholdCarSvcDvmt$LocType)
    HouseholdCarSvcDvmt <- setNames(HouseholdCarSvcDvmt$Measure, MeasureNames)[AzLoc]
    attributes(HouseholdCarSvcDvmt) <- list(
      Units = "miles per day",
      Description = "Total DVMT in car service vehicles of persons in households and non-institutional group quarters"
    )
    
    #Household car service DVMT per capita
    #-------------------------------------
    HouseholdCarSvcDvmtPerPrsn <- HouseholdCarSvcDvmt/Population
    attributes(HouseholdCarSvcDvmtPerPrsn) <- list(
      Units = "miles per day",
      Description = "Per capita household DVMT in car service vehicles of persons in households and non-institutional group quarters"
    )
    
    #Transit PMT per capita
    #----------------------
    TransitPMTPerPrsn <- summarizeDatasets(
      Expr = "sum(TransitPMT)/sum(HhSize)",
      Units_ = c(
        TransitPMT = "MI/DAY",
        HhSize = "people",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(TransitPMTPerPrsn$Azone, "_", TransitPMTPerPrsn$LocType)
    TransitPMTPerPrsn <- setNames(TransitPMTPerPrsn$Measure, MeasureNames)[AzLoc]
    attributes(TransitPMTPerPrsn) <- list(
      Units = "miles per day",
      Description = "Annual total household transit PMT per capita"
    )
    
    #Bike PMT per capita
    #-------------------
    BikePMTPerPrsn <- summarizeDatasets(
      Expr = "sum(BikePMT)/sum(HhSize)",
      Units_ = c(
        BikePMT = "MI/DAY",
        HhSize = "people",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(BikePMTPerPrsn$Azone, "_", BikePMTPerPrsn$LocType)
    BikePMTPerPrsn <- setNames(BikePMTPerPrsn$Measure, MeasureNames)[AzLoc]
    attributes(BikePMTPerPrsn) <- list(
      Units = "miles per day",
      Description = "Annual total household bike PMT per capita"
    )
    
    #Walk PMT per capita
    #---------------------
    WalkPMTPerPrsn <- summarizeDatasets(
      Expr = "sum(WalkPMT)/sum(HhSize)",
      Units_ = c(
        WalkPMT = "MI/DAY",
        HhSize = "people",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(WalkPMTPerPrsn$Azone, "_", WalkPMTPerPrsn$LocType)
    WalkPMTPerPrsn <- setNames(WalkPMTPerPrsn$Measure, MeasureNames)[AzLoc]
    attributes(WalkPMTPerPrsn) <- list(
      Units = "miles per day",
      Description = "Annual total household walk PMT per capita"
    )
    
    
    #Data frame of DVMT values
    #-------------------------
    Dvmt_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "HouseholdDvmtPerPrsn",
        "HouseholdCarSvcDvmt",
        "HouseholdCarSvcDvmtPerPrsn",
        "TransitPMTPerPrsn",
        "BikePMTPerPrsn",
        "WalkPMTPerPrsn"
      ),
      Az = Az
    )
    
    #======================
    #Vehicle Ownership Cost
    #======================
    
    #Ownership cost proportion
    #-------------------------
    OwnCostProp <- summarizeDatasets(
      Expr = "sum(OwnCost)/sum(Income)",
      Units_ = c(
        OwnCost = "USD",
        Income = "USD",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(OwnCostProp$Azone, "_", OwnCostProp$LocType)
    OwnCostProp <- setNames(OwnCostProp$Measure, MeasureNames)[AzLoc]
    attributes(OwnCostProp) <- list(
      Units = "proportion",
      Description = "Vehicle ownership cost as a proportion of household income"
    )
    
    #Ownership cost for households with less than 25K income
    #-------------------------------------------------------
    OwnCostPropHhLess25K <- summarizeDatasets(
      Expr = "sum(OwnCost[Income < 25000])/sum(Income[Income < 25000])",
      Units_ = c(
        OwnCost = "USD",
        Income = "USD",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(OwnCostPropHhLess25K$Azone, "_", OwnCostPropHhLess25K$LocType)
    OwnCostPropHhLess25K <- setNames(OwnCostPropHhLess25K$Measure, MeasureNames)[AzLoc]
    attributes(OwnCostPropHhLess25K) <- list(
      Units = "proportion",
      Description = "Vehicle ownerhsip cost as a proportion of household income for households earning less than 25K"
    )
    
    #Operating cost
    #--------------
    VehCostProp <- summarizeDatasets(
      Expr = "sum(AveVehCostPM * Dvmt)/sum(Income)",
      Units_ = c(
        AveVehCostPM = "USD",
        Dvmt = "MI/DAY",
        Income = "USD",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(VehCostProp$Azone, "_", VehCostProp$LocType)
    VehCostProp <- setNames(VehCostProp$Measure, MeasureNames)[AzLoc]
    attributes(VehCostProp) <- list(
      Units = "proportion",
      Description = "Vehicle operating cost as a proportion of household income"
    )
    
    #Operating cost for households with less than 25K income
    #-------------------------------------------------------
    VehCostPropHhLess25K <- summarizeDatasets(
      Expr = "sum(AveVehCostPM[Income < 25000] * Dvmt[Income < 25000])/sum(Income[Income < 25000])",
      Units_ = c(
        AveVehCostPM = "USD",
        Dvmt = "MI/DAY",
        Income = "USD",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(VehCostPropHhLess25K$Azone, "_", VehCostPropHhLess25K$LocType)
    VehCostPropHhLess25K <- setNames(VehCostPropHhLess25K$Measure, MeasureNames)[AzLoc]
    attributes(VehCostPropHhLess25K) <- list(
      Units = "proportion",
      Description = "Vehicle operating cost as a proportion of household income for households earning less than 25K"
    )
    
    #Operating cost for households with residents 65 and older
    #--------------------------------------------------------
    VehCostPropHhAge65Plus <- summarizeDatasets(
      Expr = "sum(AveVehCostPM[Age65Plus > 0] * Dvmt[Age65Plus > 0])/sum(Income[Age65Plus > 0])",
      Units_ = c(
        AveVehCostPM = "USD",
        Dvmt = "MI/DAY",
        Income = "USD",
        Age65Plus = "AGE",
        Azone = "",
        LocType = ""
      ),
      By_ = c("LocType", "Azone"),
      Table = "Household",
      Group = Year,
      QueryPrep_ls = QPrep_ls
    )
    MeasureNames <- paste0(VehCostPropHhAge65Plus$Azone, "_", VehCostPropHhAge65Plus$LocType)
    VehCostPropHhAge65Plus <- setNames(VehCostPropHhAge65Plus$Measure, MeasureNames)[AzLoc]
    attributes(VehCostPropHhAge65Plus) <- list(
      Units = "proportion",
      Description = "Vehicle operating cost as a proportion of household income for households with residents age 65 and older"
    )
    
    #Data frame of cost values
    #-------------------------
    Cost_df <- makeMeasureDataFrame(
      DataNames_ = c(
        "OwnCostProp",
        "OwnCostPropHhLess25K",
        "VehCostProp",
        "VehCostPropHhLess25K",
        "VehCostPropHhAge65Plus"
      ),
      Az = Az
    )
    
    #Return data frame of all results
    #--------------------------
    rbind(
      HhCharacteristics_df,
      Dvmt_df,
      Cost_df
    )
    
  }

for (Year in getYears()) {
  cat(paste0("county_location_measures_", Year, ".csv"), "\n")
  write.csv(calcCountyLocMeasures(Year = Year, Az = Az,
                                  DstoreLocs_ = DatastoreName, DstoreType = DatastoreType),
            row.names = FALSE,
            file = file.path(output_path, paste0("county_location_measures_", Year, ".csv")))
}

calcStateValidationMeasures <- 
  function(Years, BaseYear, DstoreLocs_ = c("Datastore"), DstoreType = "RD") {
    
    #Prepare for datastore queries
    #-----------------------------
    QPrep_ls <- prepareForDatastoreQuery(
      DstoreLocs_ = DstoreLocs_,
      DstoreType = DstoreType
    )
    
    #================================================
    #DEFINE FUNCTION TO CALCULATE MEASURES FOR A YEAR
    #================================================
    calcStateMeasures <- function(Year) {
      
      #--------------------------------------------------
      #Define function to create a data frame of measures
      #--------------------------------------------------
      makeMeasureDataFrame <- function(DataNames_, Year) {
        Data_X <- t(t(sapply(DataNames_, function(x) get(x))))
        colnames(Data_X) <- Year
        Measures_ <- rownames(Data_X)
        Units_ <- 
          unname(sapply(DataNames_, function(x) attributes(get(x))$Units))
        Description_ <- 
          unname(sapply(DataNames_, function(x) attributes(get(x))$Description))
        Data_df <- cbind(
          Measure = Measures_,
          data.frame(Data_X),
          Units = Units_,
          Description = Description_
        )
        rownames(Data_df) <- NULL
        colnames(Data_df) <- c("Measure", Year, "Units", "Description")
        Data_df
      }
      
      #-----------------------------------------
      #Population, Income, and Per Capita Income
      #-----------------------------------------
      #Population
      #----------
      Population <- summarizeDatasets(
        Expr = "sum(HhSize)",
        Units_ = c(
          HhSize = "PRSN"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      Population <- Population[,"Measure"]
      attributes(Population) <- list(
        Units = "persons",
        Description = "Total population"
      )
      
      
      #Population in urban households
      #------------------------------
      PopulationUrban <- summarizeDatasets(
        Expr = "sum(HhSize[LocType == 'Urban'])",
        Units_ = c(
          HhSize = "PRSN",
          LocType = "category"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      PopulationUrban <- PopulationUrban[,"Measure"]
      attributes(PopulationUrban) <- list(
        Units = "person",
        Description = "Urban population"
      )
      
      #Population in town households
      #------------------------------
      PopulationTown <- summarizeDatasets(
        Expr = "sum(HhSize[LocType == 'Town'])",
        Units_ = c(
          HhSize = "PRSN",
          LocType = "category"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      PopulationTown <- PopulationTown[,"Measure"]
      attributes(PopulationTown) <- list(
        Units = "person",
        Description = "Town population"
      )
      
      #Population in rural households
      #------------------------------
      PopulationRural <- summarizeDatasets(
        Expr = "sum(HhSize[LocType=='Rural'])",
        Units_ = c(
          HhSize = "PRSN",
          LocType = "category"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      PopulationRural <- PopulationRural[,"Measure"]
      attributes(PopulationRural) <- list(
        Units = "person",
        Description = "Rural population"
      )
      
      #Households
      #----------
      Households <- summarizeDatasets(
        Expr = "count(HhSize)",
        Units_ = c(
          HhSize = "PRSN"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      Households <- Households[,"Measure"]
      attributes(Households) <- list(
        Units = "households",
        Description = "Total households"
      )
      
      #Households less than 25K hh income
      #----------------------------------
      HouseholdsIncLess25K <- summarizeDatasets(
        Expr = "sum(Income < 25000)",
        Units_ = c(
          Income = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdsIncLess25K <- HouseholdsIncLess25K[,"Measure"]
      attributes(HouseholdsIncLess25K) <- list(
        Units = "households",
        Description = "Households earning less than 25K"
      )
      
      #Households between 25K to 50K hh income
      #---------------------------------------
      HouseholdsInc25Kto50K <- summarizeDatasets(
        Expr = "sum((Income >= 25000) & (Income < 50000))",
        Units_ = c(
          Income = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdsInc25Kto50K <- HouseholdsInc25Kto50K[,"Measure"]
      attributes(HouseholdsInc25Kto50K) <- list(
        Units = "households",
        Description = "Households earning between 25K to 50K"
      )
      
      #Households over 50K hh income
      #-----------------------------
      HouseholdsIncOver50K <- Households - HouseholdsIncLess25K - HouseholdsInc25Kto50K
      attributes(HouseholdsIncOver50K) <- list(
        Units = "households",
        Description = "Households earning over 50K"
      )
      
      # #Income
      # Income <- summarizeDatasets(
      #   Expr = "sum(Income)",
      #   Units_ = c(
      #     Income = "USD"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # Income <- Income[,"Measure"]
      # attributes(Income) <- list(
      #   Units = paste(BaseYear, "dollars per year"),
      #   Description = "Total personal income"
      # )
      # #Per Capita Income
      # PerCapInc <- Income / Population
      # attributes(PerCapInc) <- list(
      #   Units = paste(BaseYear, "dollars per person per year"),
      #   Description = "Average per capita income"
      # )
      
      #----
      #DVMT
      #----
      #Household DVMT
      #--------------
      HouseholdDvmt <- summarizeDatasets(
        Expr = "sum(Dvmt)",
        Units = c(
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDvmt <- HouseholdDvmt[,"Measure"]
      attributes(HouseholdDvmt) <- list(
        Units = "miles per day",
        Description = "Total DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters"
      )
      
      #DVMT per capita
      #---------------
      HouseholdDvmtPerPrsn <- HouseholdDvmt/Population
      attributes(HouseholdDvmtPerPrsn) <- list(
        Units = "miles per day per capita",
        Description = "Average per capita household DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters"
      )
      
      #Urban household DVMT
      #--------------------
      HouseholdDvmtUrban <- summarizeDatasets(
        Expr = "sum(Dvmt[LocType == 'Urban'])",
        Units = c(
          Dvmt = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDvmtUrban <- HouseholdDvmtUrban[,"Measure"]
      attributes(HouseholdDvmtUrban) <- list(
        Units = "miles per day",
        Description = "DVMT for urban households"
      )
      
      #Town household DVMT
      #--------------------
      HouseholdDvmtTown <- summarizeDatasets(
        Expr = "sum(Dvmt[LocType == 'Town'])",
        Units = c(
          Dvmt = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDvmtTown <- HouseholdDvmtTown[,"Measure"]
      attributes(HouseholdDvmtTown) <- list(
        Units = "miles per day",
        Description = "DVMT for town households"
      )
      
      #Rural household DVMT
      #--------------------
      HouseholdDvmtRural <- summarizeDatasets(
        Expr = "sum(Dvmt[LocType == 'Rural'])",
        Units = c(
          Dvmt = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDvmtRural <- HouseholdDvmtRural[,"Measure"]
      attributes(HouseholdDvmtRural) <- list(
        Units = "miles per day",
        Description = "DVMT for rural households"
      )
      
      #Urban household DVMT per capita
      #-------------------------------
      HouseholdDvmtPerPrsnUrban <- HouseholdDvmtUrban / PopulationUrban
      attributes(HouseholdDvmtPerPrsnUrban) <- list(
        Units = "miles per day",
        Description = "Per capita DVMT for urban households"
      )
      
      #Town household DVMT per capita
      #------------------------------
      HouseholdDvmtPerPrsnTown <- HouseholdDvmtTown / PopulationTown
      attributes(HouseholdDvmtPerPrsnTown) <- list(
        Units = "miles per day",
        Description = "Per capita DVMT for town households"
      )
      
      #Rural household DVMT per capita
      #-------------------------------
      HouseholdDvmtPerPrsnRural <- HouseholdDvmtRural / PopulationRural
      attributes(HouseholdDvmtPerPrsnRural) <- list(
        Units = "miles per day",
        Description = "Per capita DVMT for rural households"
      )
      
      #Household DVMT less than 25K hh income
      #--------------------------------------
      HouseholdDvmtIncLess25K <- summarizeDatasets(
        Expr = "sum(Dvmt[Income < 25000])",
        Units_ = c(
          Income = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDvmtIncLess25K <- HouseholdDvmtIncLess25K[,"Measure"]
      attributes(HouseholdDvmtIncLess25K) <- list(
        Units = "miles per day",
        Description = "Total DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters with hh income less than 25K"
      )
      
      #Household DVMT between 25K to 50K hh income
      #-------------------------------------------
      HouseholdDvmtInc25Kto50K <- summarizeDatasets(
        Expr = "sum(Dvmt[(Income >= 25000) & (Income < 50000)])",
        Units_ = c(
          Income = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDvmtInc25Kto50K <- HouseholdDvmtInc25Kto50K[,"Measure"]
      attributes(HouseholdDvmtInc25Kto50K) <- list(
        Units = "miles per day",
        Description = "Total DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters with hh income between 25K to 50K"
      )
      
      #Households over 50K hh income
      #-----------------------------
      HouseholdDvmtIncOver50K <- HouseholdDvmt - HouseholdDvmtIncLess25K - HouseholdDvmtInc25Kto50K
      attributes(HouseholdDvmtIncOver50K) <- list(
        Units = "miles per day",
        Description = "Total DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters with hh income over 50K"
      )
      
      #DVMT per household
      #------------------
      HouseholdDvmtPerHh <- HouseholdDvmt/Households
      attributes(HouseholdDvmtPerHh) <- list(
        Units = "miles per day per household",
        Description = "Average household DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters"
      )
      
      #DVMT per household less than 25K hh income
      #------------------------------------------
      HouseholdDvmtPerHhIncLess25K <- HouseholdDvmtIncLess25K /HouseholdsIncLess25K
      attributes(HouseholdDvmtPerHhIncLess25K) <- list(
        Units = "miles per day per household",
        Description = "Average household DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters with earnings less than 25K"
      )
      
      #DVMT per household between 25K to 50K hh income
      #-----------------------------------------------
      HouseholdDvmtPerHhInc25Kto50K <- HouseholdDvmtInc25Kto50K /HouseholdsInc25Kto50K
      attributes(HouseholdDvmtPerHhInc25Kto50K) <- list(
        Units = "miles per day per household",
        Description = "Average household DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters with earnings between 25K to 50K"
      )
      #DVMT per household over 50K hh income
      #-------------------------------------
      HouseholdDvmtPerHhIncOver50K <- HouseholdDvmtIncOver50K /HouseholdsIncOver50K
      attributes(HouseholdDvmtPerHhIncOver50K) <- list(
        Units = "miles per day per household",
        Description = "Average household DVMT (in owned and car service vehicles) of persons in households and non-institutional group quarters with earnings over 50K"
      )
      
      #Household car service DVMT
      #--------------------------
      HouseholdCarSvcDvmt <- summarizeDatasets(
        Expr = "sum(Dvmt[VehicleAccess != 'Own'] * DvmtProp[VehicleAccess != 'Own'])",
        Units = c(
          Dvmt = "MI/DAY",
          DvmtProp = "",
          VehicleAccess = ""
        ),
        Table = list(
          Household = c("Dvmt"),
          Vehicle = c("DvmtProp", "VehicleAccess")
        ),
        Key = "HhId",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdCarSvcDvmt <- HouseholdCarSvcDvmt[,"Measure"]
      attributes(HouseholdCarSvcDvmt) <- list(
        Units = "miles per day",
        Description = "Total DVMT in car service vehicles of persons in households and non-institutional group quarters"
      )
      
      #Household car service DVMT per household
      #----------------------------------------
      HouseholdCarSvcDvmtPerHh <- HouseholdCarSvcDvmt/Households
      attributes(HouseholdCarSvcDvmtPerHh) <- list(
        Units = "miles per day per household",
        Description = "Average household DVMT in car service vehicles of persons in households and non-institutional group quarters"
      )
      
      #Household car service DVMT per capita
      #-------------------------------------
      HouseholdCarSvcDvmtPerPrsn <- HouseholdCarSvcDvmt/Population
      attributes(HouseholdCarSvcDvmtPerPrsn) <- list(
        Units = "miles per day per capita",
        Description = "Per capita household DVMT in car service vehicles of persons in households and non-institutional group quarters"
      )
      
      #Urban car service DVMT
      #----------------------
      HouseholdCarSvcDvmtUrban <- summarizeDatasets(
        Expr = "sum(Dvmt[VehicleAccess!='Own' & LocType=='Urban'] * DvmtProp[VehicleAccess!='Own' & LocType=='Urban'])",
        Units = c(
          Dvmt = "MI/DAY",
          DvmtProp = "",
          VehicleAccess = "",
          LocType = ""
        ),
        Table = list(
          Household = c("Dvmt", "LocType"),
          Vehicle = c("DvmtProp", "VehicleAccess")
        ),
        Key = "HhId",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdCarSvcDvmtUrban <- HouseholdCarSvcDvmtUrban[,"Measure"]
      attributes(HouseholdCarSvcDvmtUrban) <- list(
        Units = "miles per day",
        Description = "Total DVMT in car service vehicles of persons in urban households and non-institutional group quarters"
      )
      
      #Urban car service DVMT per capita
      #---------------------------------
      HouseholdCarSvcDvmtPerPrsnUrban <- HouseholdCarSvcDvmtUrban/PopulationUrban
      attributes(HouseholdCarSvcDvmtPerPrsnUrban) <- list(
        Units = "miles per day",
        Description = "Per capita household DVMT in car service vehicles of persons in urban households and non-institutional group quarters"
      )
      
      #Town car service DVMT
      #---------------------
      HouseholdCarSvcDvmtTown <- summarizeDatasets(
        Expr = "sum(Dvmt[VehicleAccess!='Own' & LocType=='Town'] * DvmtProp[VehicleAccess!='Own' & LocType=='Town'])",
        Units = c(
          Dvmt = "MI/DAY",
          DvmtProp = "",
          VehicleAccess = "",
          LocType = ""
        ),
        Table = list(
          Household = c("Dvmt", "LocType"),
          Vehicle = c("DvmtProp", "VehicleAccess")
        ),
        Key = "HhId",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdCarSvcDvmtTown <- HouseholdCarSvcDvmtTown[,"Measure"]
      attributes(HouseholdCarSvcDvmtTown) <- list(
        Units = "miles per day",
        Description = "Total DVMT in car service vehicles of persons in town households and non-institutional group quarters"
      )
      
      #Town car service DVMT per capita
      #---------------------------------
      HouseholdCarSvcDvmtPerPrsnTown <- HouseholdCarSvcDvmtTown/PopulationTown
      attributes(HouseholdCarSvcDvmtPerPrsnTown) <- list(
        Units = "miles per day",
        Description = "Per capita household DVMT in car service vehicles of persons in town households and non-institutional group quarters"
      )
      
      #Rural car service DVMT
      #----------------------
      HouseholdCarSvcDvmtRural <- summarizeDatasets(
        Expr = "sum(Dvmt[VehicleAccess!='Own' & LocType=='Rural'] * DvmtProp[VehicleAccess!='Own' & LocType=='Rural'])",
        Units = c(
          Dvmt = "MI/DAY",
          DvmtProp = "",
          VehicleAccess = "",
          LocType = ""
        ),
        Table = list(
          Household = c("Dvmt", "LocType"),
          Vehicle = c("DvmtProp", "VehicleAccess")
        ),
        Key = "HhId",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdCarSvcDvmtRural <- HouseholdCarSvcDvmtRural[,"Measure"]
      attributes(HouseholdCarSvcDvmtRural) <- list(
        Units = "miles per day",
        Description = "Total DVMT in car service vehicles of persons in rural households and non-institutional group quarters"
      )
      
      #Rural car service DVMT per capita
      #---------------------------------
      HouseholdCarSvcDvmtPerPrsnRural <- HouseholdCarSvcDvmtRural/PopulationRural
      attributes(HouseholdCarSvcDvmtPerPrsnRural) <- list(
        Units = "miles per day",
        Description = "Per capita household DVMT in car service vehicles of persons in rural households and non-institutional group quarters"
      )
      
      #Household driverless DVMT
      #-------------------------
      HouseholdDriverlessDvmt <- summarizeDatasets(
        Expr = "sum(Dvmt * DriverlessDvmtProp)",
        Units = c(
          Dvmt = "MI/DAY",
          DriverlessDvmtProp = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDriverlessDvmt <- HouseholdDriverlessDvmt[,"Measure"]
      attributes(HouseholdDriverlessDvmt) <- list(
        Units = "miles per day",
        Description = "Total household DVMT in driverless vehicles"
      )
      
      #Urban household driverless DVMT per capita
      #------------------------------------------
      HouseholdDriverlessDvmtPerPrsnUrban <- summarizeDatasets(
        Expr = "sum(Dvmt[LocType == 'Urban'] * DriverlessDvmtProp[LocType == 'Urban']) / sum(HhSize[LocType == 'Urban'])",
        Units = c(
          Dvmt = "MI/DAY",
          DriverlessDvmtProp = "", 
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDriverlessDvmtPerPrsnUrban <- HouseholdDriverlessDvmtPerPrsnUrban[,"Measure"]
      attributes(HouseholdDriverlessDvmtPerPrsnUrban) <- list(
        Units = "miles per day",
        Description = "Total DVMT in driverless vehicles of persons in urban households and non-institutional group quarters"
      )
      
      #Town household driverless DVMT per capita
      #-----------------------------------------
      HouseholdDriverlessDvmtPerPrsnTown <- summarizeDatasets(
        Expr = "sum(Dvmt[LocType == 'Town'] * DriverlessDvmtProp[LocType == 'Town']) / sum(HhSize[LocType == 'Town'])",
        Units = c(
          Dvmt = "MI/DAY",
          DriverlessDvmtProp = "", 
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDriverlessDvmtPerPrsnTown <- HouseholdDriverlessDvmtPerPrsnTown[,"Measure"]
      attributes(HouseholdDriverlessDvmtPerPrsnTown) <- list(
        Units = "miles per day",
        Description = "Total DVMT in driverless vehicles of persons in town households and non-institutional group quarters"
      )
      
      #Rural household driverless DVMT per capita
      #------------------------------------------
      HouseholdDriverlessDvmtPerPrsnRural <- summarizeDatasets(
        Expr = "sum(Dvmt[LocType == 'Rural'] * DriverlessDvmtProp[LocType == 'Rural']) / sum(HhSize[LocType == 'Rural'])",
        Units = c(
          Dvmt = "MI/DAY",
          DriverlessDvmtProp = "", 
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdDriverlessDvmtPerPrsnRural <- HouseholdDriverlessDvmtPerPrsnRural[,"Measure"]
      attributes(HouseholdDriverlessDvmtPerPrsnRural) <- list(
        Units = "miles per day",
        Description = "Total DVMT in driverless vehicles of persons in rural households and non-institutional group quarters"
      )
      
      #Driverless DVMT per household
      #----------------------------
      HouseholdDriverlessDvmtPerHh <- HouseholdDriverlessDvmt/Households
      attributes(HouseholdDriverlessDvmtPerHh) <- list(
        Units = "miles per day per household",
        Description = "DVMT in driverless vehicles per household"
      )
      
      #Adjusted driverless DVMT
      #------------------------
      AdjDriverlessDvmt <- summarizeDatasets(
        Expr = "sum(Dvmt * AVLvl5DvmtAdjProp)",
        Units_ =  c(
          Dvmt = "MI/DAY",
          AVLvl5DvmtAdjProp = "proportions"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      AdjDriverlessDvmt <- AdjDriverlessDvmt[,"Measure"]
      attributes(AdjDriverlessDvmt) <- list(
        Units = "miles per day",
        Description = "driverless DVMT adjusted"
      )
      
      #Adjusted driverless DVMT per household
      #--------------------------------------
      AdjDriverlessDvmtPerHh <- AdjDriverlessDvmt/Households
      attributes(AdjDriverlessDvmtPerHh) <- list(
        Units = "miles per day per household",
        Description = "Adj DVMT in driverless vehicles per household"
      )
      
      #Car service deadhead DVMT
      #-------------------------
      CarSvcDeadheadDvmt <- summarizeDatasets(
        Expr = "sum(Dvmt * CarSvcDeadheadDvmtAdjProp)",
        Units_ =  c(
          Dvmt = "MI/DAY",
          CarSvcDeadheadDvmtAdjProp = "proportions"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      CarSvcDeadheadDvmt <- CarSvcDeadheadDvmt[,"Measure"]
      attributes(CarSvcDeadheadDvmt) <- list(
        Units = "miles per day",
        Description = "deadhead dvmt of car service"
      )
      
      #Car service deadhead DVMT per capita
      #------------------------------------
      CarSvcDeadheadDvmtPerPrsn <- CarSvcDeadheadDvmt/Population
      attributes(CarSvcDeadheadDvmtPerPrsn) <- list(
        Units = "miles per day per capita",
        Description = "deadhead dvmt of car service per capita"
      )
      
      #Commercial service DVMT
      #-----------------------
      ComSvcDvmt <- summarizeDatasets(
        Expr = "sum(ComSvcUrbanDvmt) + sum(ComSvcTownDvmt) + sum(ComSvcRuralDvmt)",
        Units = c(
          ComSvcUrbanDvmt = "MI/DAY",
          ComSvcTownDvmt = "MI/DAY",
          ComSvcRuralDvmt = "MI/DAY"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      ComSvcDvmt <- ComSvcDvmt[,"Measure"]
      attributes(ComSvcDvmt) <- list(
        Units = "miles per day",
        Description = "Total DVMT of commercial service vehicles"
      )
      
      #Public Transit Van DVMT
      PTVanDvmt <- summarizeDatasets(
        Expr = "sum(VanDvmt)",
        Units = c(
          VanDvmt = "MI/DAY"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      PTVanDvmt <- PTVanDvmt[,"Measure"]
      attributes(PTVanDvmt) <- list(
        Units = "miles per day",
        Description = "Total DVMT of public transit vans"
      )
      #Light-duty Vehicle DVMT
      LdvDvmt <- HouseholdDvmt + ComSvcDvmt + PTVanDvmt
      attributes(LdvDvmt) <- list(
        Units = "miles per day",
        Description = "Total DVMT of household vehicles, commercial service vehicles, and public transit vans"
      )
      #Heavy truck DVMT
      HvyTruckDvmt <- summarizeDatasets(
        Expr = "sum(HvyTrkUrbanDvmt) + sum(HvyTrkNonUrbanDvmt)",
        Units = c(
          HvyTrkUrbanDvmt = "MI/DAY",
          HvyTrkNonUrbanDvmt = "MI/DAY"
        ),
        Table = "Region",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HvyTruckDvmt <- HvyTruckDvmt[,"Measure"]
      attributes(HvyTruckDvmt) <- list(
        Units = "miles per day",
        Description = "Total DVMT of heavy trucks"
      )
      #Bus DVMT
      BusDvmt <- summarizeDatasets(
        Expr = "sum(BusDvmt)",
        Units = c(
          BusDvmt = "MI/DAY"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BusDvmt <- BusDvmt[,"Measure"]
      attributes(BusDvmt) <- list(
        Units = "miles per day",
        Description = "Total DVMT of public transit busses"
      )
      #Heavy duty vehicle DVMT
      HdvDvmt <- HvyTruckDvmt + BusDvmt
      attributes(HdvDvmt) <- list(
        Units = "miles per day",
        Description = "Total DVMT of heavy trucks and public transit busses"
      )
      #Total DVMT
      TotalDvmt <- LdvDvmt + HdvDvmt
      attributes(TotalDvmt) <- list(
        Units = "miles per day",
        Description = "Total DVMT of light-duty vehicles and heavy duty vehicles"
      )
      
      #Total DVMT per capita
      TotalDvmtPerPrsn <- TotalDvmt/Population
      attributes(TotalDvmtPerPrsn) <- list(
        Units = "miles per day",
        Description = "Per capita DVMT of light-duty vehicles and heavy duty vehicles"
      )
      
      #----------------
      #Gasoline Gallons
      #----------------
      #Household daily GGE
      #-------------------
      HouseholdGGE <- summarizeDatasets(
        Expr = "sum(DailyGGE)",
        Units_ = c(
          DailyGGE = "GGE/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdGGE <- HouseholdGGE[,"Measure"]
      attributes(HouseholdGGE) <- list(
        Units = "gallons per day",
        Description = "Total gasoline consumed by household vehicles"
      )
      
      #Commercial Service Vehicle GGE
      #------------------------------
      ComSvcGGE <- summarizeDatasets(
        Expr = "sum(ComSvcNonUrbanGGE) + sum(ComSvcUrbanGGE)",
        Units_ = c(
          ComSvcNonUrbanGGE = "GGE/DAY",
          ComSvcUrbanGGE = "GGE/DAY"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      ComSvcGGE <- ComSvcGGE[,"Measure"]
      attributes(ComSvcGGE) <- list(
        Units = "gallons per day",
        Description = "Total gasoline consumed by commercial service vehicles"
      )
      
      #Public Transit Van GGE
      #----------------------
      PTVanGGE <- summarizeDatasets(
        Expr = "sum(VanGGE)",
        Units_ = c(
          VanGGE = "GGE/DAY"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      PTVanGGE <- PTVanGGE[,"Measure"]
      attributes(PTVanGGE) <- list(
        Units = "gallons per day",
        Description = "Total gasoline consumed by public transit vans"
      )
      
      #Bus GGE
      #-------
      BusGGE <- summarizeDatasets(
        Expr = "sum(BusGGE)",
        Units_ = c(
          BusGGE = "GGE/DAY"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BusGGE <- BusGGE[,"Measure"]
      attributes(BusGGE) <- list(
        Units = "gallons per day",
        Description = "Total gasoline consumed by public transit busses"
      )
      
      #Heavy Truck GGE
      #---------------
      HvyTrkGGE <- summarizeDatasets(
        Expr = "sum(HvyTrkUrbanGGE) + sum(HvyTrkNonUrbanGGE)",
        Units_ = c(
          HvyTrkUrbanGGE = "GGE/DAY",
          HvyTrkNonUrbanGGE = "GGE/DAY"
        ),
        Table = "Region",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HvyTrkGGE <- HvyTrkGGE[,"Measure"]
      attributes(BusGGE) <- list(
        Units = "gallons per day",
        Description = "Total gasoline consumed by heavy trucks"
      )
      
      #Total GGE
      #---------
      TotalGGE <- HouseholdGGE + ComSvcGGE + PTVanGGE + BusGGE + HvyTrkGGE
      attributes(TotalGGE) <- list(
        Units = "gallons per day",
        Description = "Total gasoline consumed by light and heavy duty vehicles"
      )
      
      #Total GGE per capita
      #--------------------
      TotalGGEPerCapita <- TotalGGE/Population
      attributes(TotalGGEPerCapita) <- list(
        Units = "gallons per day per capita",
        Description = "Per capita gasoline consumed by light and heavy duty vehicles"
      )
      # #Total Non-household GGE
      # TotalNonHHGGE <- ComSvcGGE + PTVanGGE + BusGGE + HvyTrkGGE
      # attributes(TotalNonHHGGE) <- list(
      #   Units = "gallons per day",
      #   Description = "Total gasoline consumed by non-household light and heavy duty vehicles"
      # )
      
      #Light-duty vehicle average speed
      #--------------------------------
      LdvAveSpeed <- summarizeDatasets(
        Expr = "mean(LdvAveSpeed, na.rm=TRUE)",
        Units_ = c(
          LdvAveSpeed = "MI/HR"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      LdvAveSpeed <- LdvAveSpeed[,"Measure"]
      attributes(LdvAveSpeed) <- list(
        Units = "Miles per Hour",
        Description = "Average speed (miles per hour) of light-duty vehicle travel"
      )
      
      #-------------------
      #Light-Duty Vehicles
      #-------------------
      #Number of household vehicles
      #----------------------------
      NumHouseholdVehicles <- summarizeDatasets(
        Expr = "sum(NumAuto) + sum(NumLtTrk)",
        Units_ =  c(
          NumAuto = "VEH",
          NumLtTrk = "VEH"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      NumHouseholdVehicles <- NumHouseholdVehicles[,"Measure"]
      attributes(NumHouseholdVehicles) <- list(
        Units = "vehicles",
        Description = "Number of vehicles owned or leased by households"
      )
      
      # #Number of driverless household vehicles
      # #---------------------------------------
      # NumDriverlessVehicles <- summarizeDatasets(
      #   Expr = "sum(Driverless[VehicleAccess=='Own'])",
      #   Units_ =  c(
      #     VehicleAccess = "category",
      #     Driverless = "proportions"
      #   ),
      #   Table = "Vehicle",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # NumDriverlessVehicles <- NumDriverlessVehicles[,"Measure"]
      # attributes(NumDriverlessVehicles) <- list(
      #   Units = "vehicles",
      #   Description = "Number of driverless vehicles owned or leased by households"
      # )
      
      # #Proportion of driverless vehicles
      # #---------------------------------
      # DriverlessVehicleProp <- NumDriverlessVehicles/NumHouseholdVehicles
      # attributes(DriverlessVehicleProp) <- list(
      #   Units = "proportions",
      #   Description = "Proportion of driverless vehicles owned or leased by households"
      # )
      
      # #----------
      # #Population
      # #----------
      # Age0to14 = summarizeDatasets(
      #   Expr = "sum(Age0to14)",
      #   Units_ = c(
      #     Age0to14 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Age0to14) <- list(
      #   Units = "persons",
      #   Description = "Number of persons age 0 to 14"
      # )
      # Age15to19 = summarizeDatasets(
      #   Expr = "sum(Age15to19)",
      #   Units_ = c(
      #     Age15to19 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Age15to19) <- list(
      #   Units = "persons",
      #   Description = "Number of persons age 15 to 19"
      # )
      # Age20to29 = summarizeDatasets(
      #   Expr = "sum(Age20to29)",
      #   Units_ = c(
      #     Age20to29 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Age20to29) <- list(
      #   Units = "persons",
      #   Description = "Number of persons age 20 to 29"
      # )
      # Age30to54 = summarizeDatasets(
      #   Expr = "sum(Age30to54)",
      #   Units_ = c(
      #     Age30to54 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Age30to54) <- list(
      #   Units = "persons",
      #   Description = "Number of persons age 30 to 54"
      # )
      # Age55to64 = summarizeDatasets(
      #   Expr = "sum(Age55to64)",
      #   Units_ = c(
      #     Age55to64 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Age55to64) <- list(
      #   Units = "persons",
      #   Description = "Number of persons age 55 to 64"
      # )
      # Age65Plus = summarizeDatasets(
      #   Expr = "sum(Age65Plus)",
      #   Units_ = c(
      #     Age65Plus = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Age65Plus) <- list(
      #   Units = "persons",
      #   Description = "Number of persons age 65 and older"
      # )
      # TotalPopulation = summarizeDatasets(
      #   Expr = "sum(HhSize)",
      #   Units_ = c(
      #     HhSize = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(TotalPopulation) <- list(
      #   Units = "persons",
      #   Description = "Number of persons"
      # )
      # 
      # #-------
      # #Drivers
      # #-------
      # Drv15to19 = summarizeDatasets(
      #   Expr = "sum(Drv15to19)",
      #   Units_ = c(
      #     Drv15to19 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Drv15to19) <- list(
      #   Units = "persons",
      #   Description = "Number of licensed drivers age 15 to 19"
      # )
      # Drv20to29 = summarizeDatasets(
      #   Expr = "sum(Drv20to29)",
      #   Units_ = c(
      #     Drv20to29 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Drv20to29) <- list(
      #   Units = "persons",
      #   Description = "Number of licensed drivers age 20 to 29"
      # )
      # Drv30to54 = summarizeDatasets(
      #   Expr = "sum(Drv30to54)",
      #   Units_ = c(
      #     Drv30to54 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Drv30to54) <- list(
      #   Units = "persons",
      #   Description = "Number of licensed drivers age 30 to 54"
      # )
      # Drv55to64 = summarizeDatasets(
      #   Expr = "sum(Drv55to64)",
      #   Units_ = c(
      #     Drv55to64 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Drv55to64) <- list(
      #   Units = "persons",
      #   Description = "Number of licensed drivers age 55 to 64"
      # )
      # Drv65Plus = summarizeDatasets(
      #   Expr = "sum(Drv65Plus)",
      #   Units_ = c(
      #     Drv65Plus = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Drv65Plus) <- list(
      #   Units = "persons",
      #   Description = "Number of licensed drivers age 65 and older"
      # )
      # TotalDrivers = summarizeDatasets(
      #   Expr = "sum(Drivers)",
      #   Units_ = c(
      #     Drivers = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(TotalDrivers) <- list(
      #   Units = "persons",
      #   Description = "Number of licensed drivers"
      # )
      # 
      # #-------
      # #Workers
      # #-------
      # Wkr15to19 = summarizeDatasets(
      #   Expr = "sum(Wkr15to19)",
      #   Units_ = c(
      #     Wkr15to19 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Wkr15to19) <- list(
      #   Units = "persons",
      #   Description = "Number of workers age 15 to 19"
      # )
      # Wkr20to29 = summarizeDatasets(
      #   Expr = "sum(Wkr20to29)",
      #   Units_ = c(
      #     Wkr20to29 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Wkr20to29) <- list(
      #   Units = "persons",
      #   Description = "Number of workers age 20 to 29"
      # )
      # Wkr30to54 = summarizeDatasets(
      #   Expr = "sum(Wkr30to54)",
      #   Units_ = c(
      #     Wkr30to54 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Wkr30to54) <- list(
      #   Units = "persons",
      #   Description = "Number of workers age 30 to 54"
      # )
      # Wkr55to64 = summarizeDatasets(
      #   Expr = "sum(Wkr55to64)",
      #   Units_ = c(
      #     Wkr55to64 = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Wkr55to64) <- list(
      #   Units = "persons",
      #   Description = "Number of workers age 55 to 64"
      # )
      # Wkr65Plus = summarizeDatasets(
      #   Expr = "sum(Wkr65Plus)",
      #   Units_ = c(
      #     Wkr65Plus = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(Wkr65Plus) <- list(
      #   Units = "persons",
      #   Description = "Number of workers age 65 and older"
      # )
      # TotalWorkers = summarizeDatasets(
      #   Expr = "sum(Workers)",
      #   Units = c(
      #     Workers = "PRSN"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(TotalWorkers) <- list(
      #   Units = "persons",
      #   Description = "Number of workers"
      # )
      # 
      # #---------------------------------------
      # #Average Light-duty Vehicle Fuel Economy
      # #---------------------------------------
      # AverageLdvMpg <- LdvDvmt / (HouseholdGGE + ComSvcGGE + PTVanGGE)
      # attributes(AverageLdvMpg) <- list(
      #   Units = "miles per gallon",
      #   Description = "Average fuel economy of light-duty vehicles"
      # )
      
      #----------------------------------------------
      #Average Light-duty Vehicle GHG Emissions Rates
      #----------------------------------------------
      #Household daily CO2e
      #--------------------
      HouseholdCO2e <- summarizeDatasets(
        Expr = "sum(DailyCO2e)",
        Units_ = c(
          DailyCO2e = "GM/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdCO2e <- HouseholdCO2e[,"Measure"]
      attributes(HouseholdCO2e) <- list(
        Units = "grams per day",
        Description = "Daily greenhousehouse gas emissions of household vehicles"
      )
      
      #Household daily CO2e per capita
      #-------------------------------
      HouseholdCO2ePerPrsn <- HouseholdCO2e / Population
      attributes(HouseholdCO2ePerPrsn) <- list(
        Units = "grams per day per capita",
        Description = "Per capita Daily greenhousehouse gas emissions of household vehicles"
      )
      
      #Household daily CO2e per mile
      #-----------------------------
      HouseholdCO2ePerMile <- HouseholdCO2e / HouseholdDvmt
      attributes(HouseholdCO2ePerMile) <- list(
        Units = "grams per day per capita",
        Description = "Per mile Daily greenhousehouse gas emissions of household vehicles"
      )
      
      #Urban household daily CO2e
      #--------------------------
      HouseholdCO2eUrban <- summarizeDatasets(
        Expr = "sum(DailyCO2e[LocType=='Urban'])",
        Units_ = c(
          DailyCO2e = "GM/DAY",
          LocType = "category"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdCO2eUrban <- HouseholdCO2eUrban[,"Measure"]
      attributes(HouseholdCO2eUrban) <- list(
        Units = "grams per day",
        Description = "Daily greenhousehouse gas emissions of household vehicles in urban area"
      )
      
      #Town household daily CO2e
      #--------------------------
      HouseholdCO2eTown <- summarizeDatasets(
        Expr = "sum(DailyCO2e[LocType=='Town'])",
        Units_ = c(
          DailyCO2e = "GM/DAY",
          LocType = "category"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdCO2eTown <- HouseholdCO2eTown[,"Measure"]
      attributes(HouseholdCO2eTown) <- list(
        Units = "grams per day",
        Description = "Daily greenhousehouse gas emissions of household vehicles in town area"
      )
      
      #Rural household daily CO2e
      #--------------------------
      HouseholdCO2eRural <- summarizeDatasets(
        Expr = "sum(DailyCO2e[LocType=='Rural'])",
        Units_ = c(
          DailyCO2e = "GM/DAY",
          LocType = "category"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HouseholdCO2eRural <- HouseholdCO2eRural[,"Measure"]
      attributes(HouseholdCO2eRural) <- list(
        Units = "grams per day",
        Description = "Daily greenhousehouse gas emissions of household vehicles in Rural area"
      )
      
      #Urban household daily CO2e per capita
      #-------------------------------------
      HouseholdCO2ePerPrsnUrban <- HouseholdCO2eUrban/PopulationUrban
      attributes(HouseholdCO2ePerPrsnUrban) <- list(
        Units = "grams per day",
        Description = "Per capita greenhousehouse gas emissions of household vehicles in urban area"
      )
      
      #Rural household daily CO2e per capita
      #-------------------------------------
      HouseholdCO2ePerPrsnRural <- HouseholdCO2eRural/PopulationRural
      attributes(HouseholdCO2ePerPrsnRural) <- list(
        Units = "grams per day",
        Description = "Per capita greenhousehouse gas emissions of household vehicles in rural area"
      )
      
      #Urban household daily CO2e per mile
      #-----------------------------------
      HouseholdCO2ePerMileUrban <- HouseholdCO2eUrban / HouseholdDvmtUrban
      attributes(HouseholdCO2ePerMileUrban) <- list(
        Units = "grams per day",
        Description = "Per mile greenhousehouse gas emissions of household vehicles in urban area"
      )
      
      #Town household daily CO2e per mile
      #-----------------------------------
      HouseholdCO2ePerMileTown <- HouseholdCO2eTown / HouseholdDvmtTown
      attributes(HouseholdCO2ePerMileTown) <- list(
        Units = "grams per day",
        Description = "Per mile greenhousehouse gas emissions of household vehicles in town area"
      )
      
      #Rural household daily CO2e per mile
      #-------------------------------------
      HouseholdCO2ePerMileRural <- HouseholdCO2eRural / HouseholdDvmtRural
      attributes(HouseholdCO2ePerMileRural) <- list(
        Units = "grams per day",
        Description = "Per mile greenhousehouse gas emissions of household vehicles in rural area"
      )
      #Commercial Service Vehicle CO2e
      ComSvcCO2e <- summarizeDatasets(
        Expr = "sum(ComSvcNonUrbanCO2e) + sum(ComSvcUrbanCO2e)",
        Units_ = c(
          ComSvcNonUrbanCO2e = "GM/DAY",
          ComSvcUrbanCO2e = "GM/DAY"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      ComSvcCO2e <- ComSvcCO2e[,"Measure"]
      attributes(ComSvcCO2e) <- list(
        Units = "grams per day",
        Description = "Daily greenhousehouse gas emissions of commercial service vehicles"
      )
      #Public Transit Van CO2e
      PTVanCO2e <- summarizeDatasets(
        Expr = "sum(VanCO2e)",
        Units_ = c(
          VanCO2e = "GM/DAY"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      PTVanCO2e <- PTVanCO2e[,"Measure"]
      attributes(PTVanCO2e) <- list(
        Units = "grams per day",
        Description = "Daily greenhousehouse gas emissions of public transit vans"
      )
      #Light-duty Vehicle CO2e
      LdvCO2e <- HouseholdCO2e + ComSvcCO2e + PTVanCO2e
      attributes(LdvCO2e) <- list(
        Units = "grams per day",
        Description = "Daily greenhousehouse gas emissions of light-duty vehicles"
      )
      #Heavy truck CO2e
      HvyTruckCO2e <- summarizeDatasets(
        Expr = "sum(HvyTrkUrbanCO2e) + sum(HvyTrkNonUrbanCO2e)",
        Units = c(
          HvyTrkUrbanCO2e = "MI/DAY",
          HvyTrkNonUrbanCO2e = "MI/DAY"
        ),
        Table = "Region",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HvyTruckCO2e <- HvyTruckCO2e[,"Measure"]
      attributes(HvyTruckCO2e) <- list(
        Units = "miles per day",
        Description = "Total CO2e of heavy trucks"
      )
      #Bus CO2e
      BusCO2e <- summarizeDatasets(
        Expr = "sum(BusCO2e)",
        Units = c(
          BusCO2e = "MI/DAY"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BusCO2e <- BusCO2e[,"Measure"]
      attributes(BusCO2e) <- list(
        Units = "miles per day",
        Description = "Total CO2e of public transit busses"
      )
      #Heavy duty vehicle CO2e
      HdvCO2e <- HvyTruckCO2e + BusCO2e
      attributes(HdvCO2e) <- list(
        Units = "miles per day",
        Description = "Total CO2e of heavy trucks and public transit busses"
      )
      #Total CO2e
      TotalCO2e <- LdvCO2e + HdvCO2e
      attributes(TotalCO2e) <- list(
        Units = "miles per day",
        Description = "Total CO2e of light-duty vehicles and heavy duty vehicles"
      )
      #
      # #HouseholdCO2eRate
      # HouseholdCO2eRate <- HouseholdCO2e / HouseholdDvmt
      # attributes(HouseholdCO2eRate) <- list(
      #   Units = "grams per mile",
      #   Description = "Average greenhousehouse gas emissions rate of household vehicles"
      # )
      # #ComSvcCO2eRate
      # ComSvcCO2eRate <- ComSvcCO2e / ComSvcDvmt
      # attributes(ComSvcCO2eRate) <- list(
      #   Units = "grams per mile",
      #   Description = "Average greenhousehouse gas emissions rate of commercial service vehicles"
      # )
      # #PTVanCO2eRate
      # PTVanCO2eRate <- PTVanCO2e / PTVanDvmt
      # attributes(PTVanCO2eRate) <- list(
      #   Units = "grams per mile",
      #   Description = "Average greenhousehouse gas emissions rate of public transit vans"
      # )
      # #LdvCO2eRate
      # LdvCO2eRate <- LdvCO2e / LdvDvmt
      # attributes(LdvCO2eRate) <- list(
      #   Units = "grams per mile",
      #   Description = "Average greenhousehouse gas emissions rate of light-duty vehicles"
      # )
      
      #---------------------------------------
      #Household trips characteristics
      #---------------------------------------
      
      #SOV to Bike trips throught SOV diversion
      #----------
      SovToBikeTrip <- summarizeDatasets(
        Expr = "sum(SovToBikeTrip)",
        Units_ = c(
          SovToBikeTrip = "TRIPS/YR"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      SovToBikeTrip <- SovToBikeTrip[,"Measure"]
      attributes(SovToBikeTrip) <- list(
        Units = "trips per year",
        Description = "Annual total household bike trips in 2050"
      )
      
      #Bike trips per capita
      #---------------------
      SovToBikeTripPerCapita <- SovToBikeTrip/Population
      attributes(SovToBikeTripPerCapita) <- list(
        Units = "trips per year per capita",
        Description = "Annual bike trips per capita"
      )
      
      #Transit trips
      #-------------
      TransitTrips <- summarizeDatasets(
        Expr = "sum(TransitTrips)",
        Units_ = c(
          TransitTrips = "TRIPS/YR"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      TransitTrips <- TransitTrips[,"Measure"]
      attributes(TransitTrips) <- list(
        Units = "trips per year",
        Description = "Annual total household transit trips in 2050"
      )
      
      #Transit trips per capita
      #------------------------
      TransitTripsPerCapita <- TransitTrips/Population
      attributes(TransitTripsPerCapita) <- list(
        Units = "trips per year per capita",
        Description = "Annual transit trips per capita"
      )
      
      #Transit PMT
      #--------
      TransitPMT <- summarizeDatasets(
        Expr = "sum(TransitPMT)",
        Units_ = c(
          TransitPMT = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      TransitPMT <- TransitPMT[,"Measure"]
      attributes(TransitPMT) <- list(
        Units = "miles per day",
        Description = "Transit PMT in 2050"
      )
      
      #Urban transit PMT per capita
      #----------------------------
      TransitPMTPerPrsnUrban <- summarizeDatasets(
        Expr = "sum(TransitPMT[LocType=='Urban'])/sum(HhSize[LocType=='Urban'])",
        Units_ = c(
          TransitPMT = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      TransitPMTPerPrsnUrban <- TransitPMTPerPrsnUrban[,"Measure"]
      attributes(TransitPMTPerPrsnUrban) <- list(
        Units = "miles per day",
        Description = "Per capita transit PMT by urban households in 2050"
      )
      
      #Town transit PMT per capita
      #---------------------------
      TransitPMTPerPrsnTown <- summarizeDatasets(
        Expr = "sum(TransitPMT[LocType=='Town'])/sum(HhSize[LocType=='Town'])",
        Units_ = c(
          TransitPMT = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      TransitPMTPerPrsnTown <- TransitPMTPerPrsnTown[,"Measure"]
      attributes(TransitPMTPerPrsnTown) <- list(
        Units = "miles per day",
        Description = "Per capita transit PMT by town households in 2050"
      )
      
      #Rural transit PMT per capita
      #----------------------------
      TransitPMTPerPrsnRural <- summarizeDatasets(
        Expr = "sum(TransitPMT[LocType=='Rural'])/sum(HhSize[LocType=='Rural'])",
        Units_ = c(
          TransitPMT = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      TransitPMTPerPrsnRural <- TransitPMTPerPrsnRural[,"Measure"]
      attributes(TransitPMTPerPrsnRural) <- list(
        Units = "miles per day",
        Description = "Per capita transit PMT by rural households in 2050"
      )
      
      # TransitTripsHHLess25K <- summarizeDatasets(
      #   Expr = "sum(TransitTrips[Income < 25000])",
      #   Units_ = c(
      #     TransitTrips = "TRIPS/YR",
      #     Income = "USD"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(TransitTripsHHLess25K) <- list(
      #   Units = "trips per year",
      #   Description = "Annual total household transit trips by household earning less than 25K in 2050"
      # )
      # TransitTripsHH25Kto50K <- summarizeDatasets(
      #   Expr = "sum(TransitTrips[(Income >= 25000) & (Income < 50000)])",
      #   Units_ = c(
      #     TransitTrips = "TRIPS/YR",
      #     Income = "USD"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(TransitTripsHH25Kto50K) <- list(
      #   Units = "trips per year",
      #   Description = "Annual total household transit trips by household earning between 25K to 50K in 2050"
      # )
      # TransitTripsHHOver50K <- TransitTrips - TransitTripsHHLess25K - TransitTripsHH25Kto50K
      # attributes(TransitTripsHHOver50K) <- list(
      #   Units = "trips per year",
      #   Description = "Annual total household transit trips by household earning over 50K in 2050"
      # )
      
      #Bike trips
      #----------
      BikeTrips <- summarizeDatasets(
        Expr = "sum(BikeTrips)",
        Units_ = c(
          BikeTrips = "TRIPS/YR"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BikeTrips <- BikeTrips[,"Measure"]
      attributes(BikeTrips) <- list(
        Units = "trips per year",
        Description = "Annual total household bike trips in 2050"
      )
      
      #Bike trips per capita
      #---------------------
      BikeTripsPerCapita <- BikeTrips/Population
      attributes(BikeTripsPerCapita) <- list(
        Units = "trips per year per capita",
        Description = "Annual bike trips per capita"
      )
      
      #Bike PMT
      #--------
      BikePMT <- summarizeDatasets(
        Expr = "sum(BikePMT)",
        Units_ = c(
          BikePMT = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BikePMT <- BikePMT[,"Measure"]
      attributes(BikePMT) <- list(
        Units = "miles per day",
        Description = "Bike PMT in 2050"
      )
      
      #Urban bike PMT per capita
      #-------------------------
      BikePMTPerPrsnUrban <- summarizeDatasets(
        Expr = "sum(BikePMT[LocType=='Urban'])/sum(HhSize[LocType=='Urban'])",
        Units_ = c(
          BikePMT = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BikePMTPerPrsnUrban <- BikePMTPerPrsnUrban[,"Measure"]
      attributes(BikePMTPerPrsnUrban) <- list(
        Units = "miles per day",
        Description = "Per capita bike PMT by urban households in 2050"
      )
      
      #Town bike PMT per capita
      #------------------------
      BikePMTPerPrsnTown <- summarizeDatasets(
        Expr = "sum(BikePMT[LocType=='Town'])/sum(HhSize[LocType=='Town'])",
        Units_ = c(
          BikePMT = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BikePMTPerPrsnTown <- BikePMTPerPrsnTown[,"Measure"]
      attributes(BikePMTPerPrsnTown) <- list(
        Units = "miles per day",
        Description = "Per capita bike PMT by town households in 2050"
      )
      
      #Rural bike PMT per capita
      #-------------------------
      BikePMTPerPrsnRural <- summarizeDatasets(
        Expr = "sum(BikePMT[LocType=='Rural'])/sum(HhSize[LocType=='Rural'])",
        Units_ = c(
          BikePMT = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BikePMTPerPrsnRural <- BikePMTPerPrsnRural[,"Measure"]
      attributes(BikePMTPerPrsnRural) <- list(
        Units = "miles per day",
        Description = "Per capita bike PMT by rural households in 2050"
      )
      
      # BikeTripsHHLess25K <- summarizeDatasets(
      #   Expr = "sum(BikeTrips[Income < 25000])",
      #   Units_ = c(
      #     BikeTrips = "TRIPS/YR",
      #     Income = "USD"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(BikeTripsHHLess25K) <- list(
      #   Units = "trips per year",
      #   Description = "Annual total household bike trips by household earning less than 25K in 2050"
      # )
      # BikeTripsHH25Kto50K <- summarizeDatasets(
      #   Expr = "sum(BikeTrips[(Income >= 25000) & (Income < 50000)])",
      #   Units_ = c(
      #     BikeTrips = "TRIPS/YR",
      #     Income = "USD"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(BikeTripsHH25Kto50K) <- list(
      #   Units = "trips per year",
      #   Description = "Annual total household bike trips by household earning between 25K to 50K in 2050"
      # )
      # BikeTripsHHOver50K <- BikeTrips - BikeTripsHHLess25K - BikeTripsHH25Kto50K
      # attributes(BikeTripsHHOver50K) <- list(
      #   Units = "trips per year",
      #   Description = "Annual total household bike trips by household earning over 50K in 2050"
      # )
      
	  # Total Bike Trips
	  TotalBikeTrips = SovToBikeTrip + BikeTrips	  
	  
      attributes(TotalBikeTrips) <- list(
        Units = "trips per year",
        Description = "Annual household bike trips and sov diverted bike trips"
      )
	  
	  # Total Bike Trips per capital
	  TotalBikeTripsPerCapita = TotalBikeTrips/Population
	  
	  attributes(TotalBikeTripsPerCapita) <- list(
        Units = "trips per year per capita",
        Description = "Annual household bike trips and sov diverted bike trips per capita"
      )
      #Walk PMT
      #--------
      WalkPMT <- summarizeDatasets(
        Expr = "sum(WalkPMT)",
        Units_ = c(
          WalkPMT = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      WalkPMT <- WalkPMT[,"Measure"]
      attributes(WalkPMT) <- list(
        Units = "miles per day",
        Description = "Walk PMT in 2050"
      )
      
      #Urban walk PMT per capita
      #-------------------------
      WalkPMTPerPrsnUrban <- summarizeDatasets(
        Expr = "sum(WalkPMT[LocType=='Urban'])/sum(HhSize[LocType=='Urban'])",
        Units_ = c(
          WalkPMT = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      WalkPMTPerPrsnUrban <- WalkPMTPerPrsnUrban[,"Measure"]
      attributes(WalkPMTPerPrsnUrban) <- list(
        Units = "miles per day",
        Description = "Per capita walk PMT by urban households in 2050"
      )
      
      #Town walk PMT per capita
      #------------------------
      WalkPMTPerPrsnTown <- summarizeDatasets(
        Expr = "sum(WalkPMT[LocType=='Town'])/sum(HhSize[LocType=='Town'])",
        Units_ = c(
          WalkPMT = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      WalkPMTPerPrsnTown <- WalkPMTPerPrsnTown[,"Measure"]
      attributes(WalkPMTPerPrsnTown) <- list(
        Units = "miles per day",
        Description = "Per capita walk PMT by town households in 2050"
      )
      
      #Rural walk PMT per capita
      #-------------------------
      WalkPMTPerPrsnRural <- summarizeDatasets(
        Expr = "sum(WalkPMT[LocType=='Rural'])/sum(HhSize[LocType=='Rural'])",
        Units_ = c(
          WalkPMT = "MI/DAY",
          HhSize = "",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      WalkPMTPerPrsnRural <- WalkPMTPerPrsnRural[,"Measure"]
      attributes(WalkPMTPerPrsnRural) <- list(
        Units = "miles per day",
        Description = "Per capita walk PMT by rural households in 2050"
      )
      
      #Walk trips
      #----------
      WalkTrips <- summarizeDatasets(
        Expr = "sum(WalkTrips)",
        Units_ = c(
          WalkTrips = "TRIPS/YR"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      WalkTrips <- WalkTrips[,"Measure"]
      attributes(WalkTrips) <- list(
        Units = "trips per year",
        Description = "Annual total household walk trips in 2050"
      )
      
      #Walk trips per capita
      WalkTripsPerCapita <- WalkTrips/Population
      attributes(WalkTripsPerCapita) <- list(
        Units = "trips per year per capita",
        Description = "Annual walk trips per capita"
      )
      
      
      # WalkTripsHHLess25K <- summarizeDatasets(
      #   Expr = "sum(WalkTrips[Income < 25000])",
      #   Units_ = c(
      #     WalkTrips = "TRIPS/YR",
      #     Income = "USD"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(WalkTripsHHLess25K) <- list(
      #   Units = "trips per year",
      #   Description = "Annual total household walk trips by household earning less than 25K in 2050"
      # )
      # WalkTripsHH25Kto50K <- summarizeDatasets(
      #   Expr = "sum(WalkTrips[(Income >= 25000) & (Income < 50000)])",
      #   Units_ = c(
      #     WalkTrips = "TRIPS/YR",
      #     Income = "USD"
      #   ),
      #   Table = "Household",
      #   Group = Year,
      #   QueryPrep_ls = QPrep_ls
      # )
      # attributes(WalkTripsHH25Kto50K) <- list(
      #   Units = "trips per year",
      #   Description = "Annual total household walk trips by household earning between 25K to 50K in 2050"
      # )
      # WalkTripsHHOver50K <- WalkTrips - WalkTripsHHLess25K - WalkTripsHH25Kto50K
      # attributes(WalkTripsHHOver50K) <- list(
      #   Units = "trips per year",
      #   Description = "Annual total household walk trips by household earning over 50K in 2050"
      # )
      
      #---------------------------------------
      #Congestion characteristics
      #---------------------------------------
      #Extreme Congestion
      #------------------
      MetroFwyDvmtPropExtCong <- summarizeDatasets(
        Expr = "mean(FwyDvmtPropExtCong[Marea=='Metro'])",
        Units_ = c(
          FwyDvmtPropExtCong = "proportion",
          Marea = ""
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      MetroFwyDvmtPropExtCong <- MetroFwyDvmtPropExtCong[,"Measure"]
      attributes(MetroFwyDvmtPropExtCong) <- list(
        Units = "proportion",
        Description = "Proportion of freeway DVMT occurring when congestion is extreme in Metro area in 2050"
      )
      #Severe Congestion
      #-----------------
      MetroFwyDvmtPropSevCong <- summarizeDatasets(
        Expr = "mean(FwyDvmtPropSevCong[Marea=='Metro'])",
        Units_ = c(
          FwyDvmtPropSevCong = "proportion",
          Marea = ""
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      MetroFwyDvmtPropSevCong <- MetroFwyDvmtPropSevCong[,"Measure"]
      attributes(MetroFwyDvmtPropSevCong) <- list(
        Units = "proportion",
        Description = "Proportion of freeway DVMT occurring when congestion is severe in Metro area in 2050"
      )
      
      #---------------------------------------
      #Safety measures
      #---------------------------------------
      #Fatal auto crashes in urban areas
      #---------------------------------
      AutoFatalUrban <- summarizeDatasets(
        Expr = "sum(AutoFatalCrashUrban)",
        Units_ = c(
          AutoFatalCrashUrban = "CRASH"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      AutoFatalUrban <- AutoFatalUrban[,"Measure"]
      attributes(AutoFatalUrban) <- list(
        Units = "CRASH/YR",
        Description = "Number of yearly atuo fatal crashes in Urban area in 2050"
      )
      
      #Fatal auto crashes in rural areas
      #---------------------------------
      AutoFatalRural <- summarizeDatasets(
        Expr = "sum(AutoFatalCrashRural)",
        Units_ = c(
          AutoFatalCrashRural = "CRASH"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      AutoFatalRural <- AutoFatalRural[,"Measure"]
      attributes(AutoFatalRural) <- list(
        Units = "CRASH/YR",
        Description = "Number of yearly atuo fatal crashes in Rural area in 2050"
      )
      
      #Injury auto crashes in urban areas
      #----------------------------------
      AutoInjuryUrban <- summarizeDatasets(
        Expr = "sum(AutoInjuryCrashUrban)",
        Units_ = c(
          AutoInjuryCrashUrban = "CRASH"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      AutoInjuryUrban <- AutoInjuryUrban[,"Measure"]
      attributes(AutoInjuryUrban) <- list(
        Units = "CRASH/YR",
        Description = "Number of yearly atuo fatal crashes in Urban area in 2050"
      )
      
      #Injury auto crashes in rural areas
      #----------------------------------
      AutoInjuryRural <- summarizeDatasets(
        Expr = "sum(AutoInjuryCrashRural)",
        Units_ = c(
          AutoInjuryCrashRural = "CRASH"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      AutoInjuryRural <- AutoInjuryRural[,"Measure"]
      attributes(AutoInjuryRural) <- list(
        Units = "CRASH/YR",
        Description = "Number of yearly atuo fatal crashes in Rural area in 2050"
      )
      
      #Bike fatal crashes
      #Fatal bike crashes in rural areas
      #----------------------------------
      BikeFatal <- summarizeDatasets(
        Expr = "sum(BikeFatalCrash)",
        Units_ = c(
          BikeFatalCrash = "CRASH"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BikeFatal <- BikeFatal[,"Measure"]
      attributes(BikeFatal) <- list(
        Units = "CRASH/YR",
        Description = "Number of yearly bike fatal crashes in  area in 2050"
      )
      
      #Bike fatal crashes
      #Injury auto crashes in rural areas
      #----------------------------------
      BikeInjury <- summarizeDatasets(
        Expr = "sum(BikeInjuryCrash)",
        Units_ = c(
          BikeInjuryCrash = "CRASH"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      BikeInjury <- BikeInjury[,"Measure"]
      attributes(BikeInjury) <- list(
        Units = "CRASH/YR",
        Description = "Number of yearly bike injury crashes in area in 2050"
      )
      
      #Walk fatal crashes
      #Fatal bike crashes in rural areas
      #----------------------------------
      WalkFatal <- summarizeDatasets(
        Expr = "sum(WalkFatalCrash)",
        Units_ = c(
          WalkFatalCrash = "CRASH"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      WalkFatal <- WalkFatal[,"Measure"]
      attributes(WalkFatal) <- list(
        Units = "CRASH/YR",
        Description = "Number of yearly bike fatal crashes in  area in 2050"
      )
      
      #Walk fatal crashes
      #Injury auto crashes in rural areas
      #----------------------------------
      WalkInjury <- summarizeDatasets(
        Expr = "sum(WalkInjuryCrash)",
        Units_ = c(
          WalkInjuryCrash = "CRASH"
        ),
        Table = "Marea",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      WalkInjury <- WalkInjury[,"Measure"]
      attributes(WalkInjury) <- list(
        Units = "CRASH/YR",
        Description = "Number of yearly bike injury crashes in area in 2050"
      )
      
      
      #---------------------------------------
      #Vehicle Ownership Cost
      #---------------------------------------
      #Ownership cost
      #--------------
      OwnCostProp <- summarizeDatasets(
        Expr = "sum(OwnCost)/sum(Income)",
        Units_ = c(
          OwnCost = "USD",
          Income = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      OwnCostProp <- OwnCostProp[,"Measure"]
      attributes(OwnCostProp) <- list(
        Units = "proportion",
        Description = "Vehicle ownership cost as a proportion of household income in 2050"
      )
      
      #Urban ownership cost
      #--------------------
      OwnCostPropUrban <- summarizeDatasets(
        Expr = "sum(OwnCost[LocType=='Urban'])/sum(Income[LocType=='Urban'])",
        Units_ = c(
          OwnCost = "USD",
          Income = "USD",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      OwnCostPropUrban <- OwnCostPropUrban[,"Measure"]
      attributes(OwnCostPropUrban) <- list(
        Units = "proportion",
        Description = "Vehicle ownership cost as a proportion of urban household income in 2050"
      )
      
      #Town ownership cost
      #-------------------
      OwnCostPropTown <- summarizeDatasets(
        Expr = "sum(OwnCost[LocType=='Town'])/sum(Income[LocType=='Town'])",
        Units_ = c(
          OwnCost = "USD",
          Income = "USD",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      OwnCostPropTown <- OwnCostPropTown[,"Measure"]
      attributes(OwnCostPropTown) <- list(
        Units = "proportion",
        Description = "Vehicle ownership cost as a proportion of town household income in 2050"
      )
      
      #Rural ownership cost
      #-------------------
      OwnCostPropRural <- summarizeDatasets(
        Expr = "sum(OwnCost[LocType=='Rural'])/sum(Income[LocType=='Rural'])",
        Units_ = c(
          OwnCost = "USD",
          Income = "USD",
          LocType = ""
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      OwnCostPropRural <- OwnCostPropRural[,"Measure"]
      attributes(OwnCostPropRural) <- list(
        Units = "proportion",
        Description = "Vehicle ownership cost as a proportion of rural household income in 2050"
      )
      
      
      #Ownership cost for households with less than 25K income
      #-------------------------------------------------------
      OwnCostPropHhLess25K <- summarizeDatasets(
        Expr = "sum(OwnCost[Income < 25000])/sum(Income[Income < 25000])",
        Units_ = c(
          OwnCost = "USD",
          Income = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      OwnCostPropHhLess25K <- OwnCostPropHhLess25K[,"Measure"]
      attributes(OwnCostPropHhLess25K) <- list(
        Units = "proportion",
        Description = "Vehicle ownerhsip cost as a proportion of household income for households earning less than 25K in 2050"
      )
      
      #Ownership cost for households between 50k and 25K income
      #--------------------------------------------------------
      OwnCostPropHh25Kto50K <- summarizeDatasets(
        Expr = "sum(OwnCost[Income >= 25000 & Income < 50000])/sum(Income[Income >= 25000 & Income < 50000])",
        Units_ = c(
          OwnCost = "USD",
          Income = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      OwnCostPropHh25Kto50K <- OwnCostPropHh25Kto50K[,"Measure"]
      attributes(OwnCostPropHh25Kto50K) <- list(
        Units = "proportion",
        Description = "Vehicle ownerhsip cost as a proportion of household income for households earning between 25K to 50K in 2050"
      )
      
      #Ownership cost for households with more than 50K income
      #-------------------------------------------------------
      OwnCostPropHhOver50K <- summarizeDatasets(
        Expr = "sum(OwnCost[Income >= 50000])/sum(Income[Income >= 50000])",
        Units_ = c(
          OwnCost = "USD",
          Income = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      OwnCostPropHhOver50K <- OwnCostPropHhOver50K[,"Measure"]
      attributes(OwnCostPropHhOver50K) <- list(
        Units = "proportion",
        Description = "Vehicle ownerhsip cost as a proportion of household income for households earning over 50K in 2050"
      )
      
      #Operating cost
      #--------------
      VehCostProp <- summarizeDatasets(
        Expr = "sum(AveVehCostPM * Dvmt)/sum(Income)",
        Units_ = c(
          AveVehCostPM = "USD",
          Dvmt = "MI/DAY",
          Income = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      VehCostProp <- VehCostProp[,"Measure"]
      attributes(VehCostProp) <- list(
        Units = "proportion",
        Description = "Vehicle operating cost as a proportion of household income"
      )
      
      #Operating cost for households with less than 25K income
      #-------------------------------------------------------
      VehCostPropHhLess25K <- summarizeDatasets(
        Expr = "sum(AveVehCostPM[Income < 25000] * Dvmt[Income < 25000])/sum(Income[Income < 25000])",
        Units_ = c(
          AveVehCostPM = "USD",
          Dvmt = "MI/DAY",
          Income = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      VehCostPropHhLess25K <- VehCostPropHhLess25K[,"Measure"]
      attributes(VehCostPropHhLess25K) <- list(
        Units = "proportion",
        Description = "Vehicle operating cost as a proportion of household income for households earning less than 25K"
      )
      
      #Operating cost for households with residents 65 and older
      #--------------------------------------------------------
      VehCostPropHhAge65Plus <- summarizeDatasets(
        Expr = "sum(AveVehCostPM[Age65Plus > 0] * Dvmt[Age65Plus > 0])/sum(Income[Age65Plus > 0])",
        Units_ = c(
          AveVehCostPM = "USD",
          Dvmt = "MI/DAY",
          Income = "USD",
          Age65Plus = "AGE"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      VehCostPropHhAge65Plus <- VehCostPropHhAge65Plus[,"Measure"]
      attributes(VehCostPropHhAge65Plus) <- list(
        Units = "proportion",
        Description = "Vehicle operating cost as a proportion of household income for households with residents age 65 and older"
      )
      
      #---------------------------
      # Cost and Revenue Summaries
      #---------------------------
      RoadUseTax <- summarizeDatasets(
        Expr = "sum(AveRoadUseTaxPM * Dvmt)",
        Units = c(
          AveRoadUseTaxPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      RoadUseTax <- RoadUseTax[,"Measure"]
      attributes(RoadUseTax) <- list(
        Units = "dollars",
        Description = "Total taxes collected per mile of household fuel tax, VMT tax, and congestion fees"
      )
      
      #Annual household fuel taxes
      #---------------------------
      HhFuelTax <- summarizeDatasets(
        Expr = "sum(AveFuelTaxPM * Dvmt) * 365",
        Units = c(
          AveFuelTaxPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HhFuelTax <- HhFuelTax[,"Measure"]
      attributes(HhFuelTax) <- list(
        Units = "dollars",
        Description = "Annual household fuel taxes charged to hydrocarbon consuming vehicles"
      )
      
      #Annual household PEV fuel taxes
      #-------------------------------
      HhPevFuelTax <- summarizeDatasets(
        Expr = "sum(AvePevChrgPM * Dvmt) * 365",
        Units = c(
          AvePevChrgPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      HhPevFuelTax <- HhPevFuelTax[,"Measure"]
      attributes(HhPevFuelTax) <- list(
        Units = "dollars",
        Description = "Annual household fuel taxes charged to plug-in electric vehicles"
      )
     
      #Fuel taxes
      #----------
      FuelTax <- summarizeDatasets(
        Expr = "mean(FuelTax)",
        Units_ = c(
          FuelTax = "USD"
        ),
        Table = "Azone",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      FuelTax <- FuelTax[,"Measure"]
      attributes(FuelTax) <- list(
        Units = "dollars",
        Description = "Tax per gas gallon equivalent of fuel in dollars"
      )
      
      #Annual commercial service fuel taxes
      #------------------------------------
      ComSvcFuelTax <- (ComSvcGGE * FuelTax) * 365
      attributes(ComSvcFuelTax ) <- list(
        Units = "dollars",
        Description = "Total fuel tax for commercial service vehicles"
      )
      
      #Annual fuel tax revenue
      #----------------------
      TotalFuelTax <- HhFuelTax + HhPevFuelTax + ComSvcFuelTax
      attributes(TotalFuelTax) <- list(
        Units = "dollars",
        Description = "Total annual fuel tax revenue from households (for both hydrocarbon and electric vehicles) and commercial vehicles"
      )
      
      #Annual VMT taxes
      #----------------
      VmtTax <- summarizeDatasets(
        Expr = "sum(VmtTax * Dvmt) * 365",
        Units_ = c(
          VmtTax = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      VmtTax <- VmtTax[,"Measure"]
      attributes(VmtTax) <- list(
        Units = "dollars",
        Description = "VMT tax collected in dollars"
      )
      
      #Annual extra VMT tax
      #--------------------
      ExtraVmtTax <- summarizeDatasets(
        Expr = "sum(ExtraVmtTax * Dvmt) * 365",
        Units_ = c(
          ExtraVmtTax = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      ExtraVmtTax <- ExtraVmtTax[,"Measure"]
      attributes(ExtraVmtTax) <- list(
        Units = "dollars",
        Description = "Extra VMT tax collected in dollars"
      )
      
      #Annual congestion fees
      #----------------------
      CongFee <- summarizeDatasets(
        Expr = "sum(AveCongPricePM * Dvmt) * 365",
        Units = c(
          AveCongPricePM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      CongFee <- CongFee[,"Measure"]
      attributes(CongFee) <- list(
        Units = "dollars",
        Description = "Annual congestion fees collected"
      )
      
      #Annual vehicle ownership taxes
      #------------------------------
      VehOwnTax <- summarizeDatasets(
        Expr = "sum(OwnTaxCost, na.rm=TRUE)",
        Units = c(
          OwnTaxCost = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      VehOwnTax <- VehOwnTax[,"Measure"]
      attributes(VehOwnTax) <- list(
        Units = "dollars",
        Description = "Annual household vehicle ownership taxes"
      )
      
      #Annual maintenance and repair user costs
      #----------------------------------------
      MRTCost <- summarizeDatasets(
        Expr = "sum(AveMRTCostPM * Dvmt) * 365",
        Units_ = c(
          AveMRTCostPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      MRTCost <- MRTCost[,"Measure"]
      attributes(MRTCost) <- list(
        Units = "dollars",
        Description = "Annual maintenance, repair, tire user costs"
      )
      
      #Annual energy user costs (from fuel and power)
      #----------------------------------------------
      EnergyCost <- summarizeDatasets(
        Expr = "sum(AveEnergyCostPM * Dvmt) * 365",
        Units_ = c(
          AveEnergyCostPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      EnergyCost <- EnergyCost[,"Measure"]
      attributes(EnergyCost) <- list(
        Units = "dollars",
        Description = "Annual energy (fuel and electric power) costs"
      )
      
      #Annual environmental and social costs
      #-------------------------------------
      SocEnvCost <- summarizeDatasets(
        Expr = "sum(AveSocEnvCostPM * Dvmt) * 365",
        Units_ = c(
          AveSocEnvCostPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      SocEnvCost <- SocEnvCost[,"Measure"]
      attributes(SocEnvCost) <- list(
        Units = "dollars",
        Description = "Annual cost of the social and environmental impacts from vehicle travel"
      )
      
      #Annual environmental costs
      #--------------------------
      EnvCost <- summarizeDatasets(
        Expr = "sum(AveEnvCostPM * Dvmt) * 365",
        Units_ = c(
          AveEnvCostPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      EnvCost <- EnvCost[,"Measure"]
      attributes(EnvCost) <- list(
        Units = "dollars",
        Description = "Annual cost of environmental and climate impacts from vehicle travel"
      )
      
      #Annual out-of-pocket environmental costs
      #----------------------------------------
      EnvCostPaid <- summarizeDatasets(
        Expr = "sum(AveEnvCostPaidPM * Dvmt) * 365",
        Units_ = c(
          AveEnvCostPaidPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      EnvCostPaid <- EnvCostPaid[,"Measure"]
      attributes(EnvCostPaid) <- list(
        Units = "dollars",
        Description = "Annual out-of-pocket cost of environmental and climate impacts from vehicle travel"
      )
      
      #Annual out-of-pocket social costs
      #---------------------------------
      SocCostPaid <- summarizeDatasets(
        Expr = "sum(AveSocCostPaidPM * Dvmt) * 365",
        Units_ = c(
          AveSocCostPaidPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      SocCostPaid <- SocCostPaid[,"Measure"]
      attributes(SocCostPaid) <- list(
        Units = "dollars",
        Description = "Annual out-of-pocket cost of social impacts from vehicle travel"
      )
      
      #Annual pay-as-you-drive insurance costs
      #---------------------------------------
      PaydInsCost <- summarizeDatasets(
        Expr = "sum(AvePaydInsCostPM * Dvmt) * 365",
        Units_ = c(
          AvePaydInsCostPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      PaydInsCost <- PaydInsCost[,"Measure"]
      attributes(PaydInsCost) <- list(
        Units = "dollars",
        Description = "Annual PAYD insurance"
      )
      
      #Annual car service costs
      #------------------------
      CarSvcCost <- summarizeDatasets(
        Expr = "sum(AveCarSvcCostPM * Dvmt) * 365",
        Units_ = c(
          AveCarSvcCostPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      CarSvcCost <- CarSvcCost[,"Measure"]
      attributes(CarSvcCost) <- list(
        Units = "dollars",
        Description = "Annual car service cost"
      )
      
      #Annual non-residential parking costs
      #------------------------------------
      NonResPkgCost <- summarizeDatasets(
        Expr = "sum(AveNonResPkgCostPM * Dvmt) * 365",
        Units_ = c(
          AveNonResPkgCostPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      NonResPkgCost <- NonResPkgCost[,"Measure"]
      attributes(NonResPkgCost) <- list(
        Units = "dollars",
        Description = "Annual non-residential parking cost"
      )
      
      #Annual vehicle operating costs
      #------------------------------
      TotalVehUseCost <- summarizeDatasets(
        Expr = "sum(AveVehCostPM * Dvmt) * 365",
        Units = c(
          AveVehCostPM = "USD",
          Dvmt = "MI/DAY"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      TotalVehUseCost <- TotalVehUseCost[,"Measure"]
      attributes(TotalVehUseCost) <- list(
        Units = "dollars",
        Description = "Annual household vehicle operating expenses for vehicle maintenance and repair, energy costs (from fuel and power), road use taxes (including congestion fees), environmental and social costs, non-residential parking costs, pay-as-you-drive insurance costs, and car service"
      )
      
      #Annual insurance costs
      #----------------------
      InsCost = summarizeDatasets(
        Expr = "sum(InsCost[HasPaydIns == 0])",
        Units = c(
          InsCost = "USD",
          HasPaydIns = "binary"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      InsCost <- InsCost[,"Measure"]
      attributes(InsCost) <- list(
        Units = "dollars",
        Description = "Annual vehicle insurance costs"
      )
      
      #Annual vehicle depreciation costs
      #---------------------------------
      DeprCost = summarizeDatasets(
        Expr = "sum(DeprCost)",
        Units = c(
          DeprCost = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      DeprCost <- DeprCost[,"Measure"]
      attributes(DeprCost) <- list(
        Units = "dollars",
        Description = "Annual household vehicle depreciation costs"
      )
      
      #Annual vehicle financing costs
      #------------------------------
      FinCost = summarizeDatasets(
        Expr = "sum(FinCost)",
        Units = c(
          FinCost = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      FinCost <- FinCost[,"Measure"]
      attributes(FinCost) <- list(
        Units = "dollars",
        Description = "Annual household vehicle financing costs"
      )
      
      #Annual residential parking costs
      #--------------------------------
      ResPkgCost = summarizeDatasets(
        Expr = "sum(ResPkgCost)",
        Units = c(
          ResPkgCost = "USD"
        ),
        Table = "Household",
        Group = Year,
        QueryPrep_ls = QPrep_ls
      )
      ResPkgCost <- ResPkgCost[,"Measure"]
      attributes(ResPkgCost) <- list(
        Units = "dollars",
        Description = "Annual residential parking costs"
      )
      
      #Annual vehicle ownership costs
      #------------------------------
      TotalVehOwnCost <- VehOwnTax + InsCost + DeprCost + FinCost + ResPkgCost
      attributes(TotalVehOwnCost) <- list(
        Units = "dollars",
        Description = "Annual household vehicle ownership costs for depreciation, financing, insurance, residential parking, and registration taxes"
      )

      #Total vehicle costs
      #-------------------
      TotalVehCost <- TotalVehUseCost + TotalVehOwnCost
      attributes(TotalVehCost) <- list(
        Units = "dollars",
        Description = "Total annual costs from vehicle ownership and operating expenses"
      )

      #Total tax revenue
      #-----------------
      TotalTaxRev <- TotalFuelTax + VehOwnTax + VmtTax
      attributes(TotalTaxRev) <- list(
        Units = "dollars",
        Description = "Total annual tax revenue collected from fuel taxes, VMT taxes, and vehicle ownership taxes"
      )
      
      #---------------------------------------
      #Data frame of household characteristics
      #---------------------------------------
      YearData_df <- makeMeasureDataFrame(
        DataNames_ = c(
          # "Population",
          # "PopulationUrban",
          # "PopulationRural",
          # "Households",
          "HouseholdsIncLess25K",
          "HouseholdsInc25Kto50K",
          "HouseholdsIncOver50K",
          # "Income",
          # "PerCapInc",
          "HouseholdDvmt",
          "HouseholdDvmtPerPrsn",
          "HouseholdDvmtPerPrsnUrban",
          "HouseholdDvmtPerPrsnTown",
          "HouseholdDvmtPerPrsnRural",
          "HouseholdDvmtIncLess25K",
          "HouseholdDvmtInc25Kto50K",
          "HouseholdDvmtIncOver50K",
          "HouseholdDvmtPerHh",
          "HouseholdDvmtPerHhIncLess25K",
          "HouseholdDvmtPerHhInc25Kto50K",
          "HouseholdDvmtPerHhIncOver50K",
          # "HouseholdCarSvcDvmt",
          "HouseholdCarSvcDvmtPerHh",
          "HouseholdCarSvcDvmtPerPrsn",
          "HouseholdCarSvcDvmtPerPrsnUrban",
          "HouseholdCarSvcDvmtPerPrsnTown",
          "HouseholdCarSvcDvmtPerPrsnRural",
          "HouseholdDriverlessDvmt",
          "HouseholdDriverlessDvmtPerPrsnUrban",
          "HouseholdDriverlessDvmtPerPrsnTown",
          "HouseholdDriverlessDvmtPerPrsnRural",
          "HouseholdDriverlessDvmtPerHh",
          "AdjDriverlessDvmt",
          "AdjDriverlessDvmtPerHh",
          "CarSvcDeadheadDvmt",
          "CarSvcDeadheadDvmtPerPrsn",
          # "ComSvcDvmt",
          # "PTVanDvmt",
          # "LdvDvmt",
          # "HvyTruckDvmt",
          # "BusDvmt",
          # "HdvDvmt",
          "TotalDvmt",
          "TotalDvmtPerPrsn",
          # "HouseholdGGE",
          # "ComSvcGGE",
          # "PTVanGGE",
          # "BusGGE",
          # "HvyTrkGGE",
          # "TotalGGE",
          "TotalGGEPerCapita",
          "LdvAveSpeed",
          # "NumHouseholdVehicles",
          # "NumDriverlessVehicles",
          # "DriverlessVehicleProp",
          # "Age0to14",
          # "Age15to19",
          # "Age20to29",
          # "Age30to54",
          # "Age55to64",
          # "Age65Plus",
          # "Drv15to19",
          # "Drv20to29",
          # "Drv30to54",
          # "Drv55to64",
          # "Drv65Plus",
          # "TotalDrivers",
          # "Wkr15to19",
          # "Wkr20to29",
          # "Wkr30to54",
          # "Wkr55to64",
          # "Wkr65Plus",
          # "TotalWorkers",
          # "AverageLdvMpg",
          # "HouseholdCO2e",
          "HouseholdCO2ePerPrsn",
          "HouseholdCO2ePerMile",
          # "HouseholdCO2eUrban",
          # "HouseholdCO2eRural",
          # "HouseholdCO2ePerPrsnUrban",
          # "HouseholdCO2ePerPrsnRural",
          "HouseholdCO2ePerMileUrban",
          "HouseholdCO2ePerMileTown",
          "HouseholdCO2ePerMileRural",
          # "ComSvcCO2e",
          # "PTVanCO2e",
          # "LdvCO2e",
          # "HouseholdCO2eRate",
          # "HouseholdCO2eUrban",
          # "HouseholdCO2eRural"
          # "ComSvcCO2eRate",
          # "PTVanCO2eRate",
          # "LdvCO2eRate",
          "TotalCO2e",
          "SovToBikeTrip",
          "SovToBikeTripPerCapita",
          "TransitTrips",
          "TransitTripsPerCapita",
          "TransitPMT",
          "TransitPMTPerPrsnUrban",
          "TransitPMTPerPrsnTown",
          "TransitPMTPerPrsnRural",
          "BikeTrips",
          "BikeTripsPerCapita",
          "BikePMT",
          "BikePMTPerPrsnUrban",
          "BikePMTPerPrsnTown",
          "BikePMTPerPrsnRural",
          "TotalBikeTrips",
          "TotalBikeTripsPerCapita",
          "WalkTrips",
          "WalkTripsPerCapita",
          "WalkPMT",
          "WalkPMTPerPrsnUrban",
          "WalkPMTPerPrsnTown",
          "WalkPMTPerPrsnRural",
          "MetroFwyDvmtPropExtCong",
          "MetroFwyDvmtPropSevCong",
          "AutoFatalUrban",
          "AutoInjuryUrban",
          "AutoFatalRural",
          "AutoInjuryRural",
          "BikeFatal",
          "BikeInjury",
          "WalkFatal",
          "WalkInjury",
          "OwnCostProp",
          "OwnCostPropUrban",
          "OwnCostPropTown",
          "OwnCostPropRural",
          "OwnCostPropHhLess25K",
          "OwnCostPropHh25Kto50K",
          "OwnCostPropHhOver50K",
          "VehCostProp",
          "VehCostPropHhLess25K",
          "VehCostPropHhAge65Plus",
          "RoadUseTax",
          "HhFuelTax",
          "HhPevFuelTax",
          "ComSvcFuelTax", 
          "TotalFuelTax",
          "VmtTax",
          "ExtraVmtTax",
          "CongFee",
          "VehOwnTax",
          "TotalTaxRev",
          "MRTCost",
          "EnergyCost",
          "SocEnvCost",
          "EnvCost",
          "EnvCostPaid",
          "SocCostPaid",
          "PaydInsCost",
          "CarSvcCost",
          "NonResPkgCost",
          "InsCost",
          "DeprCost",
          "FinCost",
          "ResPkgCost",
          "TotalVehOwnCost",
          "TotalVehUseCost",
          "TotalVehCost"
        ),
        Year = Year
      )
      
    }
    
    Results_ls <- list()
    for (Year in Years) {
      Results_ls[[match(Year,Years)]] <- calcStateMeasures(Year)
    }
    
    Values_df <- data.frame(do.call(cbind, lapply(Results_ls, function(x) x[,2])))
    names(Values_df) <- Years
    
    Results_df <- cbind(
      Measure = Results_ls[[1]]$Measure,
      Values_df,
      Units = Results_ls[[1]]$Units,
      Description = Results_ls[[1]]$Description
    )
    
    Results_df
    
  } 

BaseYear <- ematmodel$setting("BaseYear")
cat("state_validation_measures.csv", "\n")
write.csv(calcStateValidationMeasures(Years, BaseYear,
                                      DstoreLocs_ = DatastoreName, DstoreType = DatastoreType),
          row.names = FALSE,
          file = file.path(output_path, "state_validation_measures.csv"))