SZA_libRadTran_readAERONETValue <- function(AERONET_typeFile, AERONET_DataLevel, measurementDate) {
  # Read AERONET data saved in netCDF file corresponding to the closest time to the selected measurement
  library(RNetCDF) # netCDF functions
  library(lubridate) # Better date functions
  
  # Load AERONET data from files
  AERONET_file <- paste0('CIMEL_AEMET_', AERONET_typeFile, '_', format(measurementDate, '%y'), '0101_', format(measurementDate, '%y'),
                          '1231_Madrid_Version', '2', '_Level', toString(AERONET_DataLevel), '.nc')
  dirFichero <- file.path(./Data, AERONET_file, fsep = .Platform$file.sep)
  nc <- open.nc(dirFichero, write = FALSE)
  details <- file.inq.nc(nc)
  AERONETData_Dates_netCDF <- var.get.nc(nc, 1) # Variable 1 is Date and Time in netCDF file
  AERONETData_Dates <- as.POSIXlt((AERONETData_Dates_netCDF - 719529)*86400, origin = "1970-01-01", tz = "UTC")
  # Find out if there are measurements that day
  measurementsSelectedDay <- which(as.numeric(floor_date(AERONETData_Dates, "day")) == as.numeric(floor_date(measurementDate, "day")))
  if (is.null(measurementsSelectedDay)) {
    AERONETData <- NULL
  } else {
    AERONETData <- list(varNames = '', values = 0)
    # Select day and time of the measurements closest to the selected date
    indexClosestsMeasurements <-  which(abs(AERONETData_Dates-measurementDate) == min(abs(AERONETData_Dates-measurementDate)))
    for (numVar in 0:(details$nvars - 1)) {
      detailsVariable <- var.inq.nc(nc, numVar)
      valueClosest <- var.get.nc(nc, numVar, start = indexClosestsMeasurements, count = length(indexClosestsMeasurements))  
      AERONETData$varNames[numVar] <- detailsVariable$name
      AERONETData$values [numVar] <- valueClosest
    }
  }
  for (numAtt in 0:(details$ngatts - 1)) {
    attDetails <- att.inq.nc(nc, "NC_GLOBAL", numAtt)
    attValue <- att.get.nc(nc, "NC_GLOBAL", numAtt)
    attr(AERONETData, attDetails$name) <- attValue
  }
  close.nc(nc)
  return(AERONETData)
}