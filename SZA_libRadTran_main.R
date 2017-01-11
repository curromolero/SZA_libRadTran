# Compare SZA calculated from libRadTran function ZENITH and taken from AERONET files
# Carga las funciones requeridas
dirFuncion <- file.path("//cendat2", "lidar", "PROACLIM_ForzamientoRadiativo", fsep = .Platform$file.sep)
source(file.path(dirFuncion, "leerMedidaAERONET.R", fsep = .Platform$file.sep))
source(file.path(dirFuncion, "organizarDatosAERONET.R", fsep = .Platform$file.sep))

# Clear R workspace
rm(list = ls() )

# Selected Measurement
measurementDate <- as.POSIXlt(strptime(c("28/06/2013 16:45"), "%d/%m/%Y %H:%M", tz = "UTC"))
# Read AERONET data
AERONET_typeFile <- 'DUBOVIKfile'
AERONET_DataLevel <- 15 # level x10 to avoid dot problems: level 1.0 & 1.5 & 2.0 = level 10 & 15 & 20
AERONETData_closestDate <- SZA_libRadTran_readAERONETValue(AERONET_typeFile, AERONET_DataLevel, measurementDate)
AERONETData <- organizeAERONETData(AERONETData_closestDate)

sza_Flux = AERONETData[[1]]$average_solar_zenith_angle_for_flux_calculation

(textoParaPegarEnCygwin <- paste('(../bin/zenith < ', ficheroINP_SinAerosoles, ' > ', OutFile_SinAerosoles, ') >&', 'verbose.txt', sep = ' '))
# Llama a libRadTran desde R usando Cygwin. Aun no funciona, pegar en Cygwin para obtener el fichero de salida
# textoLlamada <- paste('cmd.exe', '/c', 'c:\\cygwin64\\bin\\env', '/cygdrive/c/cygwin64/home/u4627/ejecutarSH_SinAerosoles.sh', sep = ' ')
# system(textoLlamada, intern = TRUE)