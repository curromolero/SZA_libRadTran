# Compare SZA calculated from libRadTran function ZENITH and taken from AERONET files
# Clear R workspace
rm(list = ls() )

# Selected Measurement
measurementDate <- as.POSIXlt(strptime(c("28/06/2013 16:45"), "%d/%m/%Y %H:%M", tz = "UTC"))
# Read AERONET data
AERONET_typeFile <- 'DUBOVIKfile'
AERONET_DataLevel <- 15 # level x10 to avoid dot problems: level 1.0 & 1.5 & 2.0 = level 10 & 15 & 20
AERONETData_closestDate <- SZA_libRadTran_readAERONETValue(AERONET_typeFile, AERONET_DataLevel, measurementDate)
AERONETData <- SZA_libRadTran_organizeAERONETData(AERONETData_closestDate)

sza_1020nm = AERONETData[[1]]$solar_zenith_angle_for_1020nm_scan
sza_Flux = AERONETData[[1]]$average_solar_zenith_angle_for_flux_calculation



CygwinText <- paste('../bin/zenith', 
                   '-a', attr(AERONETData_closestDate, "Latitude_degrees_north"),
                   '-o', -1 * attr(AERONETData_closestDate, "Longitude_degrees_east"), # Beware!!! West positive!!
                   '-y', format(AERONETData[[1]]$AERONET_date, '%Y %d %m %H %M')) # AERONET_Date is GMT, therefore UTC + 0h
# Call libRadTran from R using Cygwin. Not working yet
cat(cygwinText, sep = '\n', file = 'sendCommand.sh', append = TRUE)
callText <- paste('cmd.exe', '/c', 'c:\\cygwin64\\bin\\env', 'sendCommand.sh', sep = ' ')
system(callText, intern = TRUE)