SZA_libRadTran_organizeAERONETData <- function(AERONETData_closestDate) {
  # Organize AERONET data into size distribution, refractive index and radiative transfer values
  
  # Refractive index
  valuesREFR <- list(wvl = c(0, 0, 0 ,0), REFR = c(0, 0, 0 ,0), REFI = c(0, 0, 0 ,0))
  namesREFR <- grep("REFR", AERONETData_closestDate$varNames, value=TRUE)
  namesREFI <- grep("REFI", AERONETData_closestDate$varNames, value=TRUE)
  for (i in 1:length(namesREFR)) {
    wavelength <- as.numeric(gsub("\\D", "", namesREFR[i]))
    indexREFR <- which(AERONETData_closestDate$varNames == namesREFR[i])
    indexREFI <- which(AERONETData_closestDate$varNames == namesREFI[i])
    valuesREFR$wvl[i] <- wavelength
    valuesREFR$REFR[i] <- AERONETData_closestDate$values[indexREFR]
    valuesREFR$REFI[i] <- AERONETData_closestDate$values[indexREFI]
  }
  
  # Size distributions
  nameNumeric <- which(!is.na(as.numeric(AERONETData_closestDate$varNames)))
  valuesDist <- list(sizeBins = c(1:length(nameNumeric)), dV = c(1:length(nameNumeric)))
  valuesDist$sizeBins <- as.numeric(AERONETData_closestDate$varNames[nameNumeric])/1E6
  valuesDist$dV <- AERONETData_closestDate$values[nameNumeric]
  additionalParam <- c('VolCon-T', 'EffRad-T', 'VolMedianRad-T', 'StdDev-T', 'VolCon-F', 'EffRad-F',
                            'VolMedianRad-F', 'StdDev-F', 'VolCon-C', 'EffRad-C', 'VolMedianRad-C', 'StdDev-C')
  for (numAtt in 1:length(additionalParam)) {
    indexAtt <- which(AERONETData_closestDate$varNames == additionalParam[numAtt])
    attr(valuesDist, additionalParam[numAtt]) <- AERONETData_closestDate$values[indexAtt]
  }
  # Check distribution
  # plot(valuesDist$sizeBins, valuesDist$dV, log = 'x')
  # abline(v = attributes(valuesDist)[7])
  # abline(v = attributes(valuesDist)[11])
  
  # RT values
  RT_data <- list('Altitude(BOA)(km)' = '',
                  'Altitude(TOA)(km)' = '',
                  'DownwardFlux(BOA)' = '',
                  'DownwardFlux(TOA)' = '',	
                  'UpwardFlux(BOA)' = '',
                  'UpwardFlux(TOA)' = '',
                  'RadiativeForcing(BOA)' = '',
                  'RadiativeForcing(TOA)' = '',
                  'ForcingEfficiency(BOA)' = '',
                  'ForcingEfficiency(TOA)' = '',
                  'DownwardFlux442-T' = '',
                  'DownwardFlux677-T' = '',
                  'DownwardFlux868-T' = '',
                  'DownwardFlux1020-T' = '',
                  'UpwardFlux442-T' = '',
                  'UpwardFlux677-T' = '',
                  'UpwardFlux868-T' = '',
                  'UpwardFlux1020-T' = '',
                  'DiffuseFlux442-T' = '',
                  'DiffuseFlux677-T' = '',
                  'DiffuseFlux868-T' = '',
                  'DiffuseFlux1020-T' = '')
  for (numTR in 1:length(RT_data)) {
    indexAtt <- which(AERONETData_closestDate$varNames == names(RT_data[numTR]))
    RT_data[numTR] <- AERONETData_closestDate$values[indexAtt]
  }
  
  # Albedo
  valuesAlbedo <- list(wvl = c(rep(0, times = 4)), albedo = c(rep(0, times = 4)))
  namesAlbedo <- grep("albedo", AERONETData_closestDate$varNames, value=TRUE)
  for (i in 1:length(namesAlbedo)) {
    wavelength <- as.numeric(gsub("\\D", "", namesAlbedo[i]))
    indexAlbedo <- which(AERONETData_closestDate$varNames == namesAlbedo[i])
    valuesAlbedo$wvl[i] <- wavelength
    valuesAlbedo$albedo[i] <- AERONETData_closestDate$values[indexAlbedo]
  }
  
  # AOTs
  valuesAOT <- list(wvl = c(rep(0, times = 16)), AOT = c(rep(0, times = 16)))
  valuesAOTExt <- list(wvl = c(rep(0, times = 4)), AOTExt_T = c(rep(0, times = 4)), AOTExt_F = c(rep(0, times = 4)), AOTExt_C = c(rep(0, times = 4)))
  valuesAOTAbs <- list(wvl = c(rep(0, times = 4)), AOTAbs = c(rep(0, times = 4)))
  namesAOT <- grep("AOT", AERONETData_closestDate$varNames, value=TRUE)
  for (i in 1:length(namesAOT)) {
    if (!grepl("AngstromParam", namesAOT[i])) { # AngstromParam excluded
      if (grepl("Ext", namesAOT[i])) { # Extinction AOT
        if (grepl("-T", namesAOT[i])) { # Extinction AOT Total
          wvl <- as.numeric(gsub("\\D", "", namesAOT[i]))
          indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
          valuesAOTExt$wvl[indexWvl] <- wvl
          valuesAOTExt$AOTExt_T[indexWvl] <- AERONETData_closestDate$values[which(AERONETData_closestDate$varNames == namesAOT[i])]
        } else if (grepl("-F", namesAOT[i])) { # Extinction AOT Fine
          wvl <- as.numeric(gsub("\\D", "", namesAOT[i]))
          indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
          valuesAOTExt$wvl[indexWvl] <- wvl
          valuesAOTExt$AOTExt_F[indexWvl] <- AERONETData_closestDate$values[which(AERONETData_closestDate$varNames == namesAOT[i])]
        } else if (grepl("-C", namesAOT[i])) { # Extinction AOT Coarse
          wvl <- as.numeric(gsub("\\D", "", namesAOT[i]))
          indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
          valuesAOTExt$wvl[indexWvl] <- wvl
          valuesAOTExt$AOTExt_C[indexWvl] <- AERONETData_closestDate$values[which(AERONETData_closestDate$varNames == namesAOT[i])]
        }
      } else if (grepl("Abs", namesAOT[i])) { # Absorption AOT
        wvl <- as.numeric(gsub("\\D", "", namesAOT[i]))
        indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
        valuesAOTAbs$wvl[indexWvl] <- wvl
        valuesAOTAbs$AOTAbs[indexWvl] <- AERONETData_closestDate$values[which(AERONETData_closestDate$varNames == namesAOT[i])]
      } else {
        wvl <- as.numeric(gsub("\\D", "", namesAOT[i]))
        indexWvl <- which(c(1640, 1020, 870, 675, 667, 555, 551, 532, 531, 500, 490, 443, 440, 412, 380, 340) %in% wvl)
        valuesAOT$wvl[indexWvl] <- wvl
        valuesAOT$AOT[indexWvl] <- AERONETData_closestDate$values[which(AERONETData_closestDate$varNames == namesAOT[i])]
      }
    }
  }
  
  # SSA
  valuesSSA <- list(wvl = c(rep(0, times = 4)), SSA = c(rep(0, times = 4)))
  namesSSA <- grep("SSA", AERONETData_closestDate$varNames, value=TRUE)
  for (i in 1:length(namesSSA)) {
    wavelength <- as.numeric(gsub("\\D", "", namesSSA[i]))
    indexSSA <- which(AERONETData_closestDate$varNames == namesSSA[i])
    valuesSSA$wvl[i] <- wavelength
    valuesSSA$SSA[i] <- AERONETData_closestDate$values[indexSSA]
  }
  
  # ASYM
  valuesASYM <- list(wvl = c(rep(0, times = 4)), ASYM_T = c(rep(0, times = 4)), ASYM_F = c(rep(0, times = 4)), ASYM_C = c(rep(0, times = 4)))
  namesASYM <- grep("ASYM", AERONETData_closestDate$varNames, value=TRUE)
  for (i in 1:length(namesASYM)) {
    wvl <- as.numeric(gsub("\\D", "", namesASYM[i]))
    indexWvl <- which(c(442, 677, 868, 1020) %in% wvl)
    valuesASYM$wvl[indexWvl] <- wvl
    if (grepl("-T", namesASYM[i])) { # ASYM Total
      valuesASYM$ASYM_T[indexWvl] <- AERONETData_closestDate$values[which(AERONETData_closestDate$varNames == namesASYM[i])]
    } else if (grepl("-F", namesASYM[i])) { # ASYM Fine
      valuesASYM$ASYM_F[indexWvl] <- AERONETData_closestDate$values[which(AERONETData_closestDate$varNames == namesASYM[i])]
    } else if (grepl("-C", namesASYM[i])) { # ASYM Coarse
      valuesASYM$ASYM_C[indexWvl] <- AERONETData_closestDate$values[which(AERONETData_closestDate$varNames == namesASYM[i])]
    }
  }

  # Measurement details
  measDetails <- list('AERONET_date' = '',
                      'Julian_Day' = '',
                      'average_solar_zenith_angle_for_flux_calculation' = '',
                      'solar_zenith_angle_for_1020nm_scan' = '',
                      'Water(cm)' = '', 
                      '870-440AngstromParam(AOTExt)-Total' = '',
                      '870-440AngstromParam(AOTAbsp)' = '',
                      'sky_error' = '',
                      'sun_error' = '',
                      'alpha440-870' = '',
                      'tau440(measured)' = '',
                      'sphericity' = '',
                      'if_level2_AOD' = '')
  measDetails$AERONET_date = as.POSIXlt((AERONETData_closestDate$values[1] - 719529)*86400, origin = "1970-01-01", tz = "UTC")
  for (numMedida in 2:length(measDetails)) {
    indexMedida <- which(AERONETData_closestDate$varNames == names(measDetails[numMedida]))
    measDetails[numMedida] <- AERONETData_closestDate$values[indexMedida]
  }
  
  listaDedatos <- list(measDetails, RT_data, valuesDist, valuesREFR, valuesAlbedo, valuesAOT, valuesAOTExt, valuesAOTAbs, valuesSSA, valuesASYM)
  return(listaDedatos)
}
