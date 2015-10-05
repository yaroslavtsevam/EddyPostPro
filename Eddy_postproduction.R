#setwd('Reddyproc')
library("REddyProc")
library("data.table")
library('plyr')
library("ggplot2")
library('ggthemes')
library('pastecs')
library("grid")
library("gridExtra")
library("cowplot")
library("scales")
library('openair')

library("RcppRoll") # for fast C++ Rolling mean
# Reading Data Function


# adapt_complex_csv -------------------------------------------------------


adapt_complex_csv = function(data){
  temp_dataset = data
  if (names(temp_dataset)[1] != "filename") {
    print(temp_dataset)
    print("Whoops strange csv, may be you've allready edited it in Excel? Trying to fix")
    names(temp_dataset) = (as.character(temp_dataset[1, ]))
    print('Names gotten')
  }

  temp_dataset_startline = which(as.character(temp_dataset$daytime) == "T" | as.character(temp_dataset$daytime) == "F" | as.numeric(temp_dataset$daytime) == 0 | as.numeric(temp_dataset$daytime) == 1)[1];
  temp_dataset = temp_dataset[temp_dataset_startline:length(temp_dataset[['date']]), ]
  print('Removing junk data')
  print("Checking data")
  print(temp_dataset$date[1:10])
  if (gregexpr(pattern ='\\.', as.character(temp_dataset[['date']][1]))[[1]][1] > -1) {
    temp_dataset[['date']] = paste(substr(temp_dataset[['date']],7,10),substr(temp_dataset[['date']],4,5),substr(temp_dataset[['date']],1,2),sep="-")
    print('New date constructed')
  }



  #print(temp_dataset)

  return(temp_dataset)
}

na_percent = function(x) {

  return(length(which(is.na(x)))/length(x))
}
df.mass.tonumeric = function(df) {
  return (lapply(joined_data, function(x){if(class(x)!="character"){x=as.numeric(x)}}))
}

# read_eddy_data ----------------------------------------------------------


read_eddy_data = function(data_path)
{
  #setwd(data_path)
  #file_list =paste(data_path, list.files(path pattern="eddypro*"),sep="/")
  file_list = paste(data_path,list.files(path = data_path, pattern="eddypro*"),sep="/")
  print("Found next eddypro result files:")
  print(file_list)
  for (file in file_list){

    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset = fread(file, header = "auto",sep="auto", autostart = 60, skip=1)
      print(paste(paste("File",file, sep = " "), "read", sep=" "))
      temp_dataset = adapt_complex_csv(temp_dataset)

      inter = na.exclude(match(names(temp_dataset),names(dataset)))
      #print(head(dataset))
      temp_dataset = temp_dataset[,inter, with = FALSE]
      dataset = data.table(dataset)[,inter, with = FALSE]
      #print(names(temp_dataset))
      stop_date_data = max(as.double(dataset[['DOY']]), na.rm=TRUE)
      print( paste("Stop date:", stop_date_data, sep=" "))
      start_date_temp = min(as.double(temp_dataset[['DOY']]), na.rm=TRUE)
      print( paste("Start date:", start_date_temp, sep=" "))
      if (start_date_temp < stop_date_data) {
        temp_dataset = temp_dataset[as.double(temp_dataset[['DOY']]) > stop_date_data ]
        print( paste("Rows read:",length(temp_dataset[['DOY']]), sep=" "))
      }
      print( paste("Rows read:",length(temp_dataset[['DOY']]), sep=" "))
      print(dataset$date[1])
      print(temp_dataset$date[1])
      dataset = rbind.fill(dataset, temp_dataset)
      rm(temp_dataset)
    }
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      print(paste(paste("File",file, sep = " "), "read", sep=" "))
      dataset = tryCatch({
        fread(file, header = "auto", autostart = 60, skip=1)
      },
      warning = function(war) {
        # warning handler picks up where error was generated
        print(paste("MY_WARNING:  ",war))
        return(fread(file, header = "auto", autostart = 60, skip=1, sep=";"))
      },
      error = function(err) {
        # error handler picks up where error was generated
        print(paste("MY_ERROR:  ",err))
        return(fread(file, header = "auto", autostart = 60, skip=1, sep=";"))
      },
      finally = {
        print(paste("File read:",file))
      })


      #temp_dataset_startline = which(as.character(dataset$daytime) == "T" | as.character(dataset$daytime) == "F")[1];
      #dataset = dataset[temp_dataset_startline:length(dataset[['DOY']]), ]
      dataset = adapt_complex_csv(dataset)
      print( paste("Rows read:", length(dataset[['DOY']]), sep=" "))
    }
  }
  #setwd('../')
  return (data.table(dataset))
}

# read_biomet_data --------------------------------------------------------


read_biomet_data = function(data_path)
{
  #setwd(data_path)
  file_list = paste(data_path,list.files(path = data_path, pattern="*.dat"),sep="/")

  for (file in file_list){

    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset = fread(file, header = "auto", autostart = 60, skip=1)
      temp_dataset_startline = grep(":",temp_dataset$TIMESTAMP)[1];
      temp_dataset = temp_dataset[temp_dataset_startline:length(temp_dataset$RECORD), ]

      dataset = rbind.fill(dataset, temp_dataset)
      rm(temp_dataset)
    }
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset = fread(file, header = "auto", autostart = 60, skip=1)
      #temp_dataset_startline = grep(":",dataset$TIMESTAMP)[1];
      #dataset = dataset[temp_dataset_startline[1]:length(dataset$RECORD), ]
    }

  }
  #setwd('../')
  return (data.table(dataset))
}



# join_for_gapfilling - Fill gap bt date ----------------------------------


join_for_gapfilling = function(data_,biometdata_){

  data_[data_ == -9999.0] = NA
  biometdata_[['TIMESTAMP']] = as.POSIXct(as.POSIXlt(biometdata_[['TIMESTAMP']],format="%Y-%m-%d %H:%M:%S", tz="MSK"))

  hours = as.numeric(substr(data_[['time']],0,2))
  mins = as.numeric(substr(data_[['time']],4,6))
  mins[ mins > 0 ] = 0.5
  hours = mins+hours

  EddyData.F = data.frame(as.integer(substr(data_[['date']],0,4)),
                          as.integer(data_[['DOY']]),
                          hours,data_[['date']],
                          as.numeric(data_[['co2_flux']]) + as.numeric(data_[['co2_strg']]),
                          as.numeric(data_[['h2o_flux']]) + as.numeric(data_[['h2o_strg']]),
                          as.numeric(data_[['LE']]), as.numeric(data_[['H']]),
                          as.numeric(data_[['air_temperature']]) - 273,
                          as.numeric(data_[['RH']]),
                          as.numeric(data_[['VPD']]) / 1000000000,
                          as.numeric(data_[['u*']]),
                          as.numeric(data_[['wind_speed']]),
                          as.numeric(data_[['wind_dir']]),
                          as.numeric(data_[['x_70%']]),
                          as.numeric(data_[['x_90%']]),
                          as.numeric(data_[['qc_h2o_flux']]),
                          as.numeric(data_[['rand_err_h2o_flux']]),
                          as.numeric(data_[['rand_err_co2_flux']]),
                          as.numeric(data_[['qc_co2_flux']]))



  names(EddyData.F) = c('Year','DoY','Hour','date','NEE','H2O_NEE','LE','H','Tair','rH','VPD','Ustar','wind_speed','wind_dir','x_70%','x_90%','QF_h2o','rand_err_h2o_flux','rand_err_co2_flux','QF')
  EddyDataWithPosix.F <- fConvertTimeToPosix(na.exclude(EddyData.F), 'YDH', Year.s='Year', Day.s='DoY', Hour.s='Hour')

  print(lapply(EddyDataWithPosix.F,class))

  EddyDataWithPosixNew.F = data.table(fill_gap_by_date(EddyDataWithPosix.F,"DateTime",21))
  print("Filled gaps")
  setnames(biometdata_,'TIMESTAMP','DateTime')
  setkey(biometdata_,'DateTime')
  setkey(EddyDataWithPosixNew.F,'DateTime')

  joined = merge(biometdata_[,c(1:57), with=FALSE],EddyDataWithPosixNew.F, all=TRUE, by=c('DateTime'),allow.cartesian=TRUE)



  #join = EddyDataWithPosixNew.F[biometdata_[,c(1,5,6,7,8,15,16,17,20,23,33,36,37,38,39,40,44,45,46,47,48,49,50,51,52,53,54,56,57), with=FALSE],roll=FALSE]
  print('Joined')
  #Converting column names to satisfy REddyProc convention
  if (any(names(biometdata_) == "Ts_avg_10cm_Avg")) {
    setnames(joined,"Ts_avg_10cm_Avg","Tsoil")
  }
  if (any(names(biometdata_) == "TSoil_1_Avg")) {
    setnames(joined,"TSoil_1_Avg","Tsoil")
  }
  if (any(names(biometdata_) == "PPFD_Avg")) {
    setnames(joined,"PPFD_Avg","PAR_Den_Avg")
  }
  if (any(names(biometdata_) == "UpTot_Avg")) {
    setnames(joined,"UpTot_Avg","Rg")
  }
  if (any(names(biometdata_) == "Rg_Avg")) {
    setnames(joined,"Rg_Avg","Rg")
  }
  if (any(names(biometdata_) == "VWC_1_Avg")) {
    setnames(joined,"VWC_1_Avg","SWC_1")
  }
  if (any(names(biometdata_) == "SWC_10cm_1_Avg")) {
    setnames(joined,"SWC_10cm_1_Avg","SWC_1")
  }

  joined[['Tsoil']]=as.numeric(joined[['Tsoil']])

  joined[['Rg']]=as.numeric(joined[['Rg']])
  joined = joined[3:length(joined[,1, with=FALSE][[1]])]
  if ( length(which(duplicated(joined))) > 0 ) {
    print ("Found duplicated lines:")
    print( length(which(duplicated(joined))) )
    joined = joined[!which(duplicated(joined))]
  }



  print("Starting big Gap fill")
  joined.tf = fill_gap_by_date(joined,"DateTime",77)
  print('Stoped big gap fill')
  joined.tf = data.table(joined.tf)
  td = joined.tf[,2:length(names(joined.tf)), with=FALSE]
  td[td == "NaN" | td == "NAN" | td == "NA"] = NA
  joined.tf = cbind(joined.tf[,1,with=FALSE],td)
  joined.tf[which(duplicated(joined.tf))] = NULL

  if ( length(which(is.na(joined.tf[['DateTime']]))) > 0 ) {
    joined.tf = joined.tf[!which(is.na(joined.tf[['DateTime']]))]
  }



  return(joined.tf)
}


# fill_gap_by_date  -------------------------------------------------------

fill_gap_by_date  = function(oldData,TimeVariableName,newDataNumberofColumns){
  mask = as.numeric(oldData[[TimeVariableName]][2:length(oldData[[TimeVariableName]])])-as.numeric(oldData[[TimeVariableName]][1:(length(oldData[[TimeVariableName]])-1)]) == 1800
  gap_length = 0
  NewData = oldData
  print("Starting iteration")
  #print(which(!mask))
  for (i in which(!mask))
  {
    #num_numeric_dates = seq(as.numeric(EddyDataWithPosix.F[['DateTime']][i]),as.numeric(EddyDataWithPosix.F[['DateTime']][i+1]), by = 1800)
    print("Gap start")
    print(oldData[[TimeVariableName]][(i)])
    print("Gap end")
    print(oldData[[TimeVariableName]][(i+1)])
    num_posix_dates = seq.POSIXt(oldData[[TimeVariableName]][(i)], oldData[[TimeVariableName]][(i+1)], by = as.difftime(30,units = "mins"))
    num_posix_dates = num_posix_dates[2:(length(num_posix_dates)-1)]
    gap = as.data.frame(cbind(num_posix_dates))
    gap[,2:newDataNumberofColumns] = "NA"
    #gap[,1] = as.POSIXlt(gap[,1], tz="GMT","1970-01-01 00:00:00")
    NewData = rbindlist(list(NewData[1:(i+gap_length),], gap, NewData[(i+1+gap_length):length(NewData[[TimeVariableName]]),]))
    gap_length = gap_length + length(num_posix_dates)
    print(paste(paste("Date gap ",i,"")," filled",""))
  }
  #NewData[NewData == "NA"] = NA
  return (NewData)
}


# footprint_for_angle -----------------------------------------------------


footprint_for_angle = function(angle_v, ks_, bs_, xp_, yp_)
{
  typeof(angle_v)
  kp = tan((90 - angle_v)/180*pi)
  bp = yp_ - kp*xp_

  xc = (bs_ - bp)/(kp - ks_)
  yc = (kp*bs_ - bp*ks_)/(kp - ks_)

  if (is.na(angle_v)) {return(NA)}
  if (angle_v <= 180) {
    xcs = xc[xc >= xp_]
    ycs = yc[xc >= xp_]
  }

  if (angle_v > 180) {
    xcs = xc[xc <= xp_]
    ycs = yc[xc <= xp_]
  }

  footprint  = min(((ycs - yp_)^2+(xcs - xp_)^2)^.5)
  return(footprint)
}


# max_footprints ----------------------------------------------------------


max_footprints = function(site_point, s_polygon, alldata, wind_var)
{
  angle_vector = as.numeric(alldata[[wind_var]])
  xp = as.double(site_point[1]+ 0.0)
  yp = as.double(site_point[2]+ 0.0)
  index = 1:length(s_polygon[,1])
  shift_index = c( 2:length(s_polygon[,1]), 1)
  ks  = (s_polygon[,2][shift_index]-s_polygon[,2][index])/(s_polygon[,1][shift_index]-s_polygon[,1][index])
  bs = (s_polygon[,2][shift_index]*s_polygon[,1][index]-s_polygon[,2][index]*s_polygon[,1][shift_index])/(s_polygon[,1][index]-s_polygon[,1][shift_index])


  A_max_footprints = sapply(angle_vector, function(x) footprint_for_angle(x, ks, bs, xp, yp))
  return(alldata[, max_footprint:= A_max_footprints])
}

# filter_by_quality -------------------------------------------------------


filter_by_quality = function(join_,tower_height){
  print("Starting filtering")
  join_[join_ < -9000] = NA
  join_[['NEE']] = as.numeric(join_[['NEE']])
  join_[['x_70%']] = as.numeric(join_[['x_70%']])
  join_[['QF']] = as.numeric(join_[['QF']])
  join_[['max_footprint']] = as.numeric(join_[['max_footprint']])

  join_.sigma = sd(join_[['NEE']], na.rm = TRUE)
  join_.mean = mean(join_[['NEE']], na.rm = TRUE)

  print(join_.sigma)
  print(join_.mean)


  join_[['NEE']][join_[['NEE']] < join_.mean - 3*join_.sigma] = NA
  join_[['NEE']][join_[['NEE']] > join_.mean + 3*join_.sigma ] = NA
  print("Left after 3sigma filter:")
  print(length(which(is.finite(join_$NEE))))
  join_[['NEE']][join_[['x_70%']] > join_[['max_footprint']] ] = NA
  print("Left after max footprint due to field size filter:")
  print(length(which(is.finite(join_$NEE))))
  join_[['NEE']][join_[['x_70%']] > tower_height * 100]= NA
  print("Left after max footprint due to tower height filter:")
  print(length(which(is.finite(join_$NEE))))
  join_[['NEE']][join_[['x_70%']] < 2 ] = NA
  print("Left after min footprint filter:")
  print(length(which(is.finite(join_$NEE))))
  join_[['NEE']][join_[['QF']] > 7 ] = NA
  print("Left after flux quality filter:")
  print(length(which(is.finite(join_$NEE))))
  join_[['H2O_NEE']][join_[['QF_h2o']] > 5 ]= NA
  join_[['H2O_NEE']][join_[['x_70%']] > join_[['max_footprint']]] = NA
  join_[['H2O_NEE']][join_[['x_70%']] > tower_height * 10] = NA
  join_[['H2O_NEE']][join_[['x_70%']] < 2 ] = NA

  return(join_)
}


# reddyproc_gapfill  ------------------------------------------------------


reddyproc_gapfill = function(join_){
  join_$NEE = as.numeric(join_$NEE)
  join_$LE = as.numeric(join_$LE)
  join_$H = as.numeric(join_$H)
  join_$Rg = as.numeric(join_$Rg)
  join_$Tair = as.numeric(join_$Tair)
  join_$Tsoil = as.numeric(join_$Tsoil)
  join_$rH = as.numeric(join_$rH)
  join_$VPD = as.numeric(join_$VPD)

  EddyProc.C = sEddyProc$new('Join', join_, c('NEE', 'LE', 'H', 'Rg', 'Tair', 'Tsoil', 'rH', 'VPD'))
  EddyProc.C$sMDSGapFill('NEE', FillAll.b=TRUE)
  EddyProc.C$sMDSGapFill('Rg', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('LE', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('H', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('Tair', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('rH', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('VPD', FillAll.b=FALSE)
  EddyProc.C$sMDSGapFill('Tsoil', FillAll.b=FALSE)
  TempDT = data.table(EddyProc.C$sTEMP[c('sDateTime','NEE_f','Rg_f','LE_f','H_f','Tair_f','rH_f','VPD_f','Tsoil_f')])
  TempDT[[1]] = TempDT[[1]] + as.difftime(15, units="mins")
  setkey(TempDT,'sDateTime')
  setkey(join_,'DateTime')
  FullData_ = join_[TempDT[,c('sDateTime','NEE_f','Rg_f','LE_f','H_f','Tair_f','rH_f','VPD_f','Tsoil_f'), with=FALSE],roll=FALSE]

  return(EddyProc.C)
}
reddyproc_extract_main_data = function(join_,EddyProc_){
  TempDT = data.table(EddyProc_$sTEMP[c('sDateTime','NEE_f','Rg_f','LE_f','H_f','Tair_f','rH_f','VPD_f','Tsoil_f')])
  TempDT[[1]] = TempDT[[1]] + as.difftime(15, units="mins")
  setkey(TempDT,'sDateTime')
  setkey(join_,'DateTime')
  FullData_ = join_[TempDT[,c('sDateTime','NEE_f','Rg_f','LE_f','H_f','Tair_f','rH_f','VPD_f','Tsoil_f'), with=FALSE],roll=FALSE]
  return(FullData_)
}



# add_separators ----------------------------------------------------------


add_separators = function(FullData_, lat, lon, zone){
  doy = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%j"))
  dom = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%d"))
  hour =  as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%H"))
  month_name = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%B"))
  month_number = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%m"))
  week = as.integer(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%U"))
  decimal_hour = as.numeric(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%H"))+as.numeric(strftime(as.POSIXlt(FullData_[,1, with=FALSE][[1]]), format="%M"))/60
  sunpos = fCalcSunPosition(doy, decimal_hour, lat, lon, zone)

  separations = data.table(data.frame( FullData_[['DateTime']],doy, dom, hour, month_name, month_number,week, sunpos[['SolTime']],sunpos[['SolDecl']],sunpos[['SolElev']]))
  names(separations) = c('sDateTime', 'Doy','DOM','hour','month_name','month_number','week','SolTime','SolDecl', 'SolElev')
  setkey(separations,'sDateTime')
  with_sep =  FullData_[separations[,c('sDateTime','Doy','DOM','hour','month_number','week','SolTime','SolDecl','SolElev'), with=FALSE],roll=FALSE]
  return(with_sep)
}



# PlotWindRoses -----------------------------------------------------------



PlotWindRoses = function(EddyData)
{
  Data = data.frame(EddyData$DateTime,as.numeric(EddyData$wind_speed),as.numeric(EddyData$wind_dir),as.numeric(EddyData$`x_70%`))
  names(Data) = c("date","ws","wd","footprint")
  windRose(Data)
  # one windRose for each year
  windRose(Data, type = "season")
  windRose(Data, type = "year")
  # windRose in 10 degree intervals with gridlines and width adjusted
  ## Not run:
  windRose(Data, angle = 10, width = 0.2, grid.line = 1)
  ## End(Not run)
  # pollutionRose of nox
  pollutionRose(Data, pollutant = "footprint")
  ## source apportionment plot - contribution to mean
  ## Not run:
  pollutionRose(Data, pollutant = "footprint", type = "season")
}
ma  = function(x,n=7){
  #filter(x,rep(1/n,n), sides=2) - untill dplyr breaks filter function - we'll use zoo

  #RcppRoll library
  return(c(x[1:(n-1)],roll_mean(x,n, align = "left")))
}



# daily_data --------------------------------------------------------------


daily_data = function(Data){
  #Data = Data[[1]]
  new_names = c()
  for (name in names(Data)){
    if ((class(Data[[name]])[1] == 'numeric' ) | (class(Data[[name]])[1] == 'integer' ) | (class(Data[[name]])[1] == 'character' )){
      daily_sum = tapply(as.numeric(Data[[name]]), Data$Doy, sum, na.rm = TRUE)
      daily_mean = tapply(as.numeric(Data[[name]]), Data$Doy, mean, na.rm = TRUE)
      daily_error = tapply(as.numeric(Data[[name]]), Data$Doy, sd)

      daily_error = daily_error/sqrt(length(Data[[name]]))*1.96

      if (exists('daily_sums')) {
        daily_sums = cbind(daily_sums, daily_sum)
        daily_means = cbind(daily_means, daily_mean)
        daily_errors = cbind(daily_errors, daily_error)

      }
      else{
        daily_sums = data.frame(daily_sum)
        daily_means = data.frame(daily_mean)
        daily_errors = data.frame(daily_error)

      }
      new_names = c(new_names,name)
    }
  }
  daily_cumsums_g  = cumsum(daily_sums) *12 *18 /10000

  names(daily_sums) = paste(new_names,"sums", sep="_")
  names(daily_means) = new_names
  names(daily_errors) = paste(new_names,"errors", sep="_")
  names(daily_cumsums_g) = paste(new_names,"cumsum", sep="_")
  #  PAR_margin_for_night = 5
  # Reco  = as.vector(by(Data[,c("NEE_f","PAR_Den_Avg"), with=FALSE], Data$Doy, function(x) mean(x[['NEE_f']][x[['PAR_Den_Avg']] < PAR_margin_for_night & x[['NEE_f']] > 0], na.rm=TRUE)*48 ))
  #  GPP  = as.vector(by(Data[,c("NEE_f","PAR_Den_Avg"), with=FALSE], Data$Doy, function(x) {
  #    y = mean(x[['NEE_f']][x[['PAR_Den_Avg']] < PAR_margin_for_night & x[['NEE_f']] > 0 ], na.rm=TRUE) - x[['NEE_f']][x[['PAR_Den_Avg']] > PAR_margin_for_night ]
  #    return(sum( y[y>0], na.rm=TRUE))}
  #   ))

  Reco  = as.vector(by(Data[,c("NEE_f","SolElev"), with=FALSE], Data$Doy, function(x) mean(x[['NEE_f']][x[['SolElev']] < 0 & x[['NEE_f']] > 0], na.rm=TRUE)*48 ))
  GPP  = as.vector(by(Data[,c("NEE_f","SolElev"), with=FALSE], Data$Doy, function(x) {
    y = mean(x[['NEE_f']][x[['SolElev']] < 0 & x[['NEE_f']] > 0 ], na.rm=TRUE) - x[['NEE_f']][x[['SolElev']] > 0 ]
    return(sum( y[y>0], na.rm=TRUE))}
  ))
  Reco[Reco == 'NaN'] = 0
  Reco[Reco == 'NA'] = 0
  Reco[Reco == NA] = 0
  GPP[GPP == 'NaN'] = 0
  GPP[GPP == 'NA'] = 0
  GPP[GPP == NA] = 0

  NA_count = tapply(as.numeric(Data[['NEE']]), Data$Doy, function(x) length(which(is.na(x))))
  NA_marker = NA_count

  NA_marker[NA_marker < 16] = 1

  NA_marker[NA_marker >= 16] = 0.1
  Reco_sum = cumsum(Reco *12*18/10000)
  GPP_sum = cumsum(GPP *12*18/10000)
  #add posix dates
  return(cbind(daily_sums, daily_means,daily_errors,daily_cumsums_g, NA_count,NA_marker, Reco, GPP, Reco_sum, GPP_sum ))
  #return(cbind(daily_sums, daily_means,daily_errors))
}



# weekly_data -------------------------------------------------------------


weekly_data = function(Data){
  new_names = c()
  #Data = Data[[1]]
  for (name in names(Data)){

    if ((class(Data[[name]])[1] == 'numeric' ) | (class(Data[[name]])[1] == 'integer' ) | (class(Data[[name]])[1] == 'character' )){

      weekly_sum = tapply(as.numeric(Data[[name]]), Data$week, sum, na.rm = TRUE)
      weekly_mean = tapply(as.numeric(Data[[name]]), Data$week, mean, na.rm = TRUE)
      weekly_error = tapply(as.numeric(Data[[name]]), Data$week, sd)
      weekly_error = weekly_error/sqrt(length(Data[[name]]))*1.96

      if (exists('weekly_sums')) {
        weekly_sums = cbind(weekly_sums, weekly_sum)
        weekly_means = cbind(weekly_means, weekly_mean)
        weekly_errors = cbind(weekly_errors, weekly_error)
      }
      else
      {
        weekly_sums = data.frame(weekly_sum)
        weekly_means = data.frame(weekly_mean)
        weekly_errors = data.frame(weekly_error)
      }
      new_names = c(new_names,name)
    }
  }
  names(weekly_sums) = paste(new_names,"sums", sep="_")
  names(weekly_means) = new_names
  names(weekly_errors) = paste(new_names,"errors", sep="_")
  return(cbind(weekly_sums, weekly_means, weekly_errors))
}
#redo like day


# month_data --------------------------------------------------------------


month_data = function(Data){
  mnew_names = c()
  for (name in names(Data)){
    if ((class(Data[[name]])[1] == 'numeric' ) | (class(Data[[name]])[1] == 'integer' ) | (class(Data[[name]])[1] == 'character' )){
      month_sum = tapply(as.numeric(Data[[name]]), Data$month_number, sum, na.rm = TRUE )
      month_mean = tapply(as.numeric(Data[[name]]), Data$month_number, mean, na.rm = TRUE)
      month_error = tapply(as.numeric(Data[[name]]), Data$month_number, sd)
      month_error = month_error/sqrt(length(Data[[name]]))*1.96

      if (exists('month_sums')) {
        month_sums = cbind(month_sums, month_sum)
        month_means = cbind(month_means, month_mean)
        month_errors = cbind(month_errors, month_error)
      }
      else{
        month_sums = data.frame(month_sum)
        month_means = data.frame(month_mean)
        month_errors = data.frame(month_error)
      }

      mnew_names = c(mnew_names,name)
    }
  }

  names(month_sums) = paste(mnew_names,"sums", sep="_")
  names(month_means) = mnew_names
  names(month_errors) = paste(mnew_names,"errors", sep="_")
  #add posix dates
  return(cbind(month_sums, month_means, month_errors))
}


# hourly_data -------------------------------------------------------------

hourly_data = function(AllData){

  #AllData = AllData[[1]]
  variables_names = names(AllData)[5:86]
  #using plyr because it's already used by ggplot
  hourly = list()
  for (variable in variables_names)
  {
    hour_means = c()
    hour_errors = c()
    hour_months  = c()
    for (m in 1:12)
    {
      Data = AllData[AllData$month_number == m,]
      hour_mean = tapply(Data[[variable]], Data$hour, mean, na.rm=TRUE)
      #print(hour_mean)
      hour_error = tapply(Data[[variable]], Data$hour, sd, na.rm = TRUE)
      hour_error = hour_error/sqrt(length(hour_error))*1.96
      hour_month = rep(m, length(hour_error))
      hour_means = c(hour_means, hour_mean)
      hour_errors = c(hour_errors, hour_error)
      hour_months  = c(hour_months, hour_month)

    }
    hour = rep(0:23, length(hour_means)/24)
    #print(hour)

    hourly[[which(variables_names == variable)]] =  data.frame(cbind(hour_means,hour_errors,hour_months,hour))
  }
  #setNames(hourly, variables_names)
  names(hourly) = variables_names
  return(hourly)
}


# hourly_NEE_period -------------------------------------------------------


hourly_NEE_period = function(AllData,start_date,stop_date){
  hour_means = c()
  hour_errors = c()
  hour_months  = c()
  #AllData=AllData[[1]]
  AllData = AllData[(AllData[['DateTime']] > as.POSIXct(start_date) & AllData[['DateTime']] < as.POSIXct(stop_date)),]
  nmax = max(na.exclude(AllData[['month_number']]))
  nmin = min(na.exclude(AllData[['month_number']]))
  for (m in nmin:nmax) {

    Data = AllData[AllData[['month_number']] == m,]

    hour_mean = tapply(Data[['NEE_f']], Data$hour, mean)
    hour_error = tapply(Data[['NEE_f']], Data$hour, sd)
    hour_error = hour_error/sqrt(length(hour_error))*1.96
    hour_month = tapply(Data[['month_number']], Data$hour, mean)
    hour_means = c(hour_means, hour_mean)
    hour_errors = c(hour_errors, hour_error)
    hour_months  = c(hour_months, hour_month)

  }
  hour = as.integer(names(hour_mean))
  return(data.frame(cbind(hour_means,hour_errors,hour_months,hour)))
}


# hourly_data_for_event ---------------------------------------------------

hourly_data_for_event = function(AllData, event_name){
  hour_means = c()
  hour_errors = c()


  Data = AllData[AllData[[event_name]],]

  hour_mean = tapply(Data[['NEE_f']], Data$hour, mean)
  hour_error = tapply(Data[['NEE_f']], Data$hour, sd)
  hour_error = hour_error/sqrt(length(hour_error))*1.96

  hour_means = c(hour_means, hour_mean)
  hour_errors = c(hour_errors, hour_error)

  hour = as.integer(names(hour_mean))
  return(data.frame(cbind(hour_means,hour_errors,hour)))
}
ForMotherRussia = function(Data){
  Data[['DateTime']] = Data[['DateTime']] + as.difftime(-240, units="mins")
  return(Data)
}



# insert_event_mask  ------------------------------------------------------


insert_event_mask = function(dt, datetime_column_name, event_start_date, event_stop_date, event_name){
  if (class(event_start_date)[1] == "POSIXct" && class(event_stop_date)[1] == "POSIXct") {
    event_mask = dt[[datetime_column_name]] > event_start_date & dt[[datetime_column_name]] < event_stop_date
    dt = data.table(dt)
    new_dt = dt[, event_name:= event_mask]
    setnames(new_dt, 'event_name',event_name)
    return (new_dt)
  }
  else
  {
    return (NULL)
  }
}


# add_event ---------------------------------------------------------------


add_events = function(events_file, allData, DateVarName){
  with_events = allData
  if (file.exists(events_file))
  {
    allevents = fread(events_file, header = "auto", sep = "auto")
    for (i in 1:length(allevents[,1, with=FALSE][[1]])){
      start_date = as.POSIXct(as.character(allevents[i,1, with =FALSE]))
      stop_date  = as.POSIXct(as.character(allevents[i,2, with =FALSE]))
      with_events = insert_event_mask(with_events,DateVarName,start_date,stop_date,as.character(allevents[i,3, with =FALSE]))
    }
  }else {
    print("No events file found")
  }
  return(with_events)
}
#+++ Aligning biomet and eddypro data to data.table

#================================================================================

# FullEddyPostProcess -----------------------------------------------------


FullEddyPostProcess = function(DataFolder,SiteUTM,SitePolygon,events_file,SiteCoordZone, tower_height){
  # Reading Data
  data = read_eddy_data(DataFolder)
  biometdata = read_biomet_data(DataFolder)

  # Forming data set for gap filling
  joined_data = join_for_gapfilling(data, biometdata)

  #Generating column of max footprints
  joined_data = max_footprints(SiteUTM, SitePolygon, joined_data,'wind_dir')
  # Pre Gap filling Filtering
  joined_data[, setdiff(colnames(joined_data),"DateTime")] <- as.data.table(sapply( joined_data[, setdiff(colnames(joined_data),"DateTime"), with=FALSE], as.numeric))

  join.filtered = filter_by_quality(joined_data,tower_height)

  #+++ Fill gaps in variables with MDS gap filling algorithm
  # Creating DataTable with filled and biomet data
  Reddyproc  = reddyproc_gapfill(join.filtered)
  FullData = reddyproc_extract_main_data(join.filtered, Reddyproc)
  FullData = ForMotherRussia(FullData)
  #Adding time periods
  FullData_with_Sep = add_separators(FullData,SiteCoordZone[1],SiteCoordZone[2],SiteCoordZone[3] )
  #Read event filed and add event's masks
  WithEvents = add_events(events_file,FullData_with_Sep,'DateTime')
  # WindRose
  WithEvents$SWC_1 = as.numeric(WithEvents$SWC_1)

  WithEvents$moisture_levels = cut(WithEvents$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))

  hourly_data = hourly_data(WithEvents)
  data_daily = daily_data(WithEvents)
  data_daily$moisture_levels = cut(data_daily$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
  data_weekly = weekly_data(WithEvents)
  data_monthly = month_data(WithEvents)


  AllData = list(dt = WithEvents, reddy = Reddyproc, hourly = hourly_data, daily = data_daily, weekly = data_weekly, monthly = data_monthly)
  AllData$dt$moisture_levels = cut(AllData$dt$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
  setkey(AllData$dt, 'DateTime')

  return(AllData)
}

# Add chamber daily data --------------------------------------------------

AddDailyChambers = function(filename,AllData){
  A_chamb = fread(filename, header = "auto")
  A_chamb$DateTime = as.POSIXct(as.POSIXlt(A_chamb$Date,format="%d.%m.%Y", tz="MSK"))
  A_chamb$Doy = as.integer(strftime(as.POSIXlt(A_chamb$DateTime), format="%j"))
  A_chamb = as.data.table(A_chamb)
  data_column = which(!(names(A_chamb)=="Date" | names(A_chamb)=="DateTime" ))
  new_names= names(A_chamb)[data_column]
  joined = merge(A_chamb[,data_column, with=FALSE],AllData$daily, all=TRUE, by=c('Doy'),allow.cartesian=TRUE)
  joined[, (new_names) := lapply(.SD, function(x) as.numeric(gsub(",",".", x)) ), .SDcols=new_names]
  return(joined)
}


# compare_plot ------------------------------------------------------------


compare_plot = function(tower_list,x_variable,y_variable, type,grouping_varaible=~hour_months,xlab="Time of day (Hour)", ylab=expression(paste(bold("NEE")," ( ",mu,"mol "," ",CO[2]," ",m^-2," ",s^-1, " )",sep="")),title="NEE_f for two towers, hourly", errorbar=FALSE){
  pd = position_dodge(.1)
  shape_list = factor(15,21,17,19)
  linetypes=c( "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  graph = ggplot()
  for (n in 1:length(tower_list)) {

    if (type=="diurnal") {
      graph = graph + geom_line(data = tower_list[[n]], aes_string(x=x_variable, y=y_variable),position=pd,linetype=linetypes[n], size=.75)
      graph = graph + geom_point(data = tower_list[[n]], aes_string(x=x_variable, y=y_variable),position=pd,shape=shape_list[n], size=2)}
    if (type=="cumul") {
      graph = graph + geom_line(data = tower_list[[n]], aes_string(x = x_variable, y=y_variable), position=pd,linetype=linetypes[n], size=.5  )
    }
    #graph = graph + geom_point(data = tower_list[[n]], aes_string(x=x_variable, y=y_variable),position=pd,shape=shape_list[n], size=2)
    if (errorbar){
      graph =graph + geom_errorbar(data = tower_list[[n]], aes_string(x=x_variable, y=y_variable, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), linetype=1,size=.1, width=.4, position=pd)
    }
  }

  graph =graph +geom_hline(yintercept = 0, linetype=2)

  if (type=="diurnal") {
    graph =graph +facet_wrap(grouping_varaible, ncol = 3)
  }

  graph =graph +xlab(xlab)
  graph =graph +ylab(ylab)
  graph =graph +theme_few(base_size = 15, base_family = "serif")
  graph =graph +theme(axis.title.y = element_text(size = 15, face="bold"))
  graph =graph +theme(axis.title.x = element_text(size =15, face="bold"))
  graph =graph +ggtitle(title)

  return(graph)
}



PlotDiurnal = function(DataList, startM=1, endM=12, riseplot = "TRUE", title_text="NEE for two towers, diurnal") {
  pd = position_dodge(.1)
  shape_list = as.factor(c(5,7,17,19,15,21))
  DP = ggplot()
  linetypes=c( "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  for (n in 1:length(DataList)) {
    #finding mean rise hour per month
    rise = as.vector(by(DataList[[n]]$dt[,c("hour","SolElev","Doy"), with=FALSE], DataList[[n]]$dt$month_number, function(x) mean(by(x, x$Doy, function(y) return(y$hour[ min(which(y$SolElev>0)) ])))))
    rise=rise-2
    #finding mean down hour per month
    down = as.vector(by(DataList[[n]]$dt[,c("hour","SolElev","Doy"), with=FALSE], DataList[[n]]$dt$month_number, function(x) mean(by(x, x$Doy, function(y) return(y$hour[ max(which(y$SolElev>0)) ])))))
    down=down-2
    #and putting it with it's month to df
    hour_months = as.numeric(levels(as.factor(DataList[[n]]$hourly$NEE_f$hour_months)))
    downrisedf = data_frame(hour_months,rise,down)

    #Subseting everything by startM and EndM
    DataList[[n]]$hourly$NEE_f = DataList[[n]]$hourly$NEE_f[DataList[[n]]$hourly$NEE_f$hour_months>=startM & DataList[[n]]$hourly$NEE_f$hour_months<=endM,]
    downrisedf = downrisedf[downrisedf$hour_months>=startM & downrisedf$hour_months<=endM,]

    #Plotting
    DP = DP + geom_errorbar(data = DataList[[n]]$hourly$NEE_f, aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), linetype=1,size=.1, width=.4, position=pd)

    DP = DP + geom_line(data = DataList[[n]]$hourly$NEE_f, aes(x=hour, y=hour_means),position=pd,size=.5, linetype=linetypes[n])
    DP = DP + geom_point(data = DataList[[n]]$hourly$NEE_f, aes(x=hour, y=hour_means),position=pd,size=2, shape=shape_list[n], fill=2)

    if(riseplot == TRUE){
      DP = DP + geom_vline(aes(xintercept = rise),downrisedf, linetype=linetypes[n])
      DP = DP + geom_vline(aes(xintercept = down),downrisedf, linetype=linetypes[n])
    }

  }


  DP = DP + geom_hline(yintercept = 0, linetype=2)
  #DP = DP + facet_wrap(~hour_months, ncol =3)
  if((endM-startM>3)|(endM-startM == 2))  {DP = DP + facet_wrap(~hour_months, ncol =3)} else
  {DP = DP + facet_wrap(~hour_months, ncol =2)}
  DP = DP + xlab("Time of day (Hour)")
  DP = DP + ylab(expression(paste(bold("NEE")," ( ",mu,"mol "," ",CO[2]," ",m^-2," ",s^-1, " )",sep="")))
  #μmol CO2 m-2s-1)")
  DP = DP + theme_few(base_size = 15, base_family = "serif")
  DP = DP + theme(axis.title.y = element_text(size = 15, face="bold"))
  DP = DP + theme(axis.title.x = element_text(size =15, face="bold")) #
  #DP = DP + ggtitle("NEE_f for two towers, hourly")
  DP = DP + ggtitle(title_text)
  return(DP)
}



PlotBiomet = function(DataList, filled=FALSE, startDoy, endDoy) {

  pd = position_dodge(.1)
  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = as.factor(c(15,21,17,19))
  color_list = c("white","black","grey50")
  Gr_PAR = ggplot()
  Gr_Tsoil = ggplot()
  Gr_water = ggplot()
  maxDoy = 1
  minDoy = 365
  for(n in 1:length(DataList)) {
    if (filled == TRUE ){
      plot_data = DataList[[n]]$daily_f
    } else {
      plot_data = DataList[[n]]$daily
    }
    maxDoy = c(maxDoy, max(plot_data$Doy))
    minDoy = c(minDoy, min(plot_data$Doy))

    if (!is.null( plot_data$PAR_Den_Avg)) {
      pd = position_dodge(.1*n)
      Gr_PAR = Gr_PAR + geom_line(data =  plot_data , aes(x=Doy, y=ma(PAR_Den_Avg)),linetype=linetypes[n], size=.8)
      Gr_PAR = Gr_PAR + geom_point(data = plot_data , aes(x=Doy, y=PAR_Den_Avg),size=2, shape=shape_list[n], fill=1, alpha = (.4+.15*n))

    }
    if (!is.null( plot_data$Tsoil_f)) {
      Gr_Tsoil = Gr_Tsoil + geom_line(data = plot_data , aes(x=Doy, y=ma(Tsoil_f)),linetype=linetypes[n], size=.8)
      Gr_Tsoil = Gr_Tsoil + geom_point(data = plot_data , aes(x=Doy, y=Tsoil_f),size=2, shape=shape_list[n], fill=1,alpha=.5)
    }
    if (!is.null( plot_data$SWC_1)) {
      Gr_water = Gr_water + geom_line(data =  plot_data, aes(x=Doy, y=ma(SWC_1*100)),linetype=linetypes[n], size=.8)
      Gr_water = Gr_water + geom_point(data = plot_data, aes(x=Doy, y=(SWC_1*100)),size=2, shape=shape_list[n], fill=1,alpha=.3)

    }
    if (!is.null( plot_data$Rain_mm_Tot_sums) || !is.null( plot_data$P_tot_sums) ){
      Gr_water = Gr_water + geom_rect(data =  plot_data, aes(x=Doy,xmin=Doy-.5,xmax=Doy+.5, y=Rain_mm_Tot_sums, ymin=0, xmin=2), fill=color_list[n], position=pd,linetype = 1, size = .1,alpha = .41, color = "black")
    }
  }


  Gr_PAR = Gr_PAR + xlab("Day of the year")
  Gr_PAR = Gr_PAR + ylab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep="")))
  #geom_vline(xintercept = 163, size=1, alpha=1)+
  Gr_PAR = Gr_PAR + theme_few(base_size = 18, base_family = "serif")
  Gr_PAR = Gr_PAR + theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_PAR = Gr_PAR + theme(plot.margin = unit(c(0,2,0,2), "lines"))
  Gr_PAR = Gr_PAR + theme(axis.title.x = element_blank())
  Gr_PAR = Gr_PAR + theme(axis.text.x = element_blank())
  Gr_PAR = Gr_PAR + theme(axis.ticks.x = element_blank())
  #ggtitle("DRAW mean PAR")


  Gr_Tsoil = Gr_Tsoil + xlab("Day of the year")
  Gr_Tsoil = Gr_Tsoil + ylab(expression(bold(paste(T["soil"]," at 5cm depth "," (", ring("C"),")",sep=""))))
  #geom_vline(xintercept = 163, size=3, alpha=.2)
  Gr_Tsoil = Gr_Tsoil + theme_few(base_size = 18, base_family = "serif")
  Gr_Tsoil = Gr_Tsoil + theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_Tsoil = Gr_Tsoil + theme(plot.margin = unit(c(0,2,0,2), "lines"))
  Gr_Tsoil = Gr_Tsoil + theme(axis.title.x = element_blank())
  Gr_Tsoil = Gr_Tsoil + theme(axis.text.x = element_blank())
  Gr_Tsoil = Gr_Tsoil + theme(axis.ticks.x = element_blank())
  #ggtitle("Tsoil A and B")

  #####Volumetric water content VWC A and B


  Gr_water = Gr_water +xlab("Day of the year")
  Gr_water = Gr_water +ylab(expression(bold(paste("SWC at 5cm depth (%),\n   precipitation (cm)"," ",sep=""))))

  if (!is.null(startDoy) && !is.null(endDoy) ){
    Gr_water = Gr_water +scale_x_continuous(breaks = round(seq(startDoy,endDoy, by = 30),1))
    Gr_water = Gr_water +coord_cartesian(xlim = c(startDoy, endDoy),ylim = c(0, 50))
    Gr_PAR = Gr_PAR +coord_cartesian(xlim = c(startDoy, endDoy))
    Gr_Tsoil = Gr_Tsoil +coord_cartesian(xlim = c(startDoy, endDoy))
    }
  else if (!is.null(startDoy) || !is.null(endDoy) ) {
      if (!is.null(endDoy)){
        Gr_water = Gr_water +scale_x_continuous(breaks = round(seq(min(minDoy)-1,endDoy, by = 30),1))
        Gr_water = Gr_water +coord_cartesian(xlim = c(min(minDoy), endDoy),ylim = c(0, 50))
        Gr_PAR = Gr_PAR +coord_cartesian(xlim = c(min(minDoy), endDoy))
        Gr_Tsoil = Gr_Tsoil +coord_cartesian(xlim = c(min(minDoy), endDoy))
      }
      if (!is.null(startDoy)){
        Gr_water = Gr_water +scale_x_continuous(breaks = round(seq(startDoy,max(maxDoy), by = 30),1))
        Gr_water = Gr_water +coord_cartesian(xlim = c(startDoy, max(maxDoy)),ylim = c(0, 50))
        Gr_PAR = Gr_PAR +coord_cartesian(xlim = c(startDoy, max(maxDoy)))
        Gr_Tsoil = Gr_Tsoil +coord_cartesian(xlim = c(startDoy, max(maxDoy)))
      }
  }
  else {
    Gr_water = Gr_water +scale_x_continuous(breaks = round(seq(min(minDoy)-1,max(maxDoy), by = 30),1))
    Gr_PAR = Gr_PAR +coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
    Gr_Tsoil = Gr_Tsoil +coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
    Gr_water = Gr_water +coord_cartesian(xlim = c(min(minDoy), max(maxDoy)),ylim = c(0, 50))
  }

  Gr_water = Gr_water +theme_few(base_size = 18, base_family = "serif")
  Gr_water = Gr_water +theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_water = Gr_water +theme(plot.margin = unit(c(0,2,0,2), "lines"))
  Gr_water = Gr_water +theme(axis.title.x = element_text(size =15, face="bold"))
  #ggtitle("Volumetric water content VWC A and B")



  grid.newpage()
  grid.draw(rbind(ggplotGrob(Gr_PAR), ggplotGrob(Gr_Tsoil), ggplotGrob(Gr_water), size = "first"))

}

PlotFluxSep = function(DataList,filled = FALSE, startDoy=1, endDoy=12) {

  pd = position_dodge(.1)
  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = as.factor(c(15,21,17,19))
  Gr_NEE = ggplot()
  Gr_Reco = ggplot()
  Gr_GPP = ggplot()
  maxDoy = 1
  minDoy = 365
  for(n in 1:length(DataList)) {
    if (filled == TRUE ){
      plot_data = DataList[[n]]$daily_f
    } else {
      plot_data = DataList[[n]]$daily
    }
    maxDoy = c(maxDoy, max(plot_data$Doy))
    minDoy = c(minDoy, min(plot_data$Doy))

    pd = position_dodge(.1*n)
    Gr_NEE = Gr_NEE + geom_line(data = plot_data, aes(x=Doy, y=ma(NEE_f_sums* 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_NEE = Gr_NEE + geom_point(data = plot_data , aes(x=Doy, y=NEE_f_sums* 12*18 /10000),position=pd,size=2, shape=shape_list[n], fill=n,alpha=.5)

   if(any(names(DataList[[n]]$daily)=="Rs_t")){
     Gr_Reco = Gr_Reco + geom_point(data = plot_data , aes(x=Doy, y=Rs_t),position=pd,size=2, shape=6,alpha=1, color="red",fill="red")
   }
   if(any(names(DataList[[n]]$daily)=="Rs_n")){
     Gr_Reco = Gr_Reco + geom_point(data = plot_data , aes(x=Doy, y=Rs_n),position=pd,size=2, shape=7,alpha=1, color="blue",fill="blue")
   }

    Gr_Reco = Gr_Reco + geom_line(data = plot_data, aes(x=Doy, y=ma(Reco * 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_Reco = Gr_Reco + geom_point(data = plot_data, aes(x=Doy, y=Reco* 12 * 18/10000),position=pd,size=2, shape=shape_list[n], fill = n, alpha = .5)

    Gr_GPP = Gr_GPP +geom_line(data = plot_data, aes(x=Doy, y=ma(GPP * 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_GPP = Gr_GPP +geom_point(data = plot_data, aes(x=Doy, y=GPP * 12 * 18/10000),position=pd,size=2, shape=shape_list[n], fill=n,alpha=.5)

  }
  Gr_NEE = Gr_NEE + geom_hline(yintercept = 0, size=.5, linetype = 2)
  #Gr_NEE = Gr_NEE +  geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)
  Gr_NEE = Gr_NEE + xlab("Day of the year")
  Gr_NEE = Gr_NEE + ylab(expression(paste(bold("NEE")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))
  #μmol CO2 m-2s-1)")+
  #Gr_NEE = Gr_NEE + geom_vline(xintercept = 207, size=1, alpha=.8)
  #scale_x_continuous(breaks = round(seq(120, max(Daily_A_114$Doy), by = 50),1))
  Gr_NEE = Gr_NEE + theme_few(base_size = 15, base_family = "serif")
  Gr_NEE = Gr_NEE + theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_NEE = Gr_NEE + theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_NEE = Gr_NEE + theme(axis.title.x = element_blank())
  Gr_NEE = Gr_NEE + theme(axis.text.x = element_blank())
  Gr_NEE = Gr_NEE + theme(axis.ticks.x = element_blank())
  #ggtitle("NEE_f daily sums for all year ")

  ###### Reco

  Gr_Reco = Gr_Reco + geom_hline(yintercept = 0, size=.5, linetype = 2)
  #Gr_Reco = Gr_Reco + geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)
  Gr_Reco = Gr_Reco + xlab("Day of the year")
  #Gr_Reco = Gr_Reco + geom_vline(xintercept = 163, size=3, alpha=.2)
  Gr_Reco = Gr_Reco + ylab(expression(paste(bold("Reco")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))
  #μmol CO2 m-2s-1)")+
  #Gr_Reco = Gr_Reco + scale_x_continuous(breaks = round(seq(min(DataList[[n]]$daily$Doy), max(DataList[[n]]$daily$Doy), by = 30),1))
  Gr_Reco = Gr_Reco + theme_few(base_size = 15, base_family = "serif")
  Gr_Reco = Gr_Reco + theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_Reco = Gr_Reco + theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_Reco = Gr_Reco + theme(axis.title.x = element_blank())
  Gr_Reco = Gr_Reco + theme(axis.text.x = element_blank())
  Gr_Reco = Gr_Reco + theme(axis.ticks.x = element_blank())
  # ggtitle("Reco daily sums for all year ")

  ###### GPP

  Gr_GPP = Gr_GPP + geom_hline(yintercept = 0, size=.5, linetype = 2)
  #Gr_GPP = Gr_GPP +geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)
  Gr_GPP = Gr_GPP +xlab("Day of the year")
  Gr_GPP = Gr_GPP +ylab(expression(paste(bold("GPP")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))
  #Gr_GPP = Gr_GPP +geom_vline(xintercept = 163, size=3, alpha=.2)
  #μmol CO2 m-2s-1)")+
  #Gr_GPP = Gr_GPP +scale_x_continuous(breaks = round(seq(min(DataList[[n]]$daily$Doy), max(DataList[[n]]$daily$Doy), by = 30),1))
  Gr_GPP = Gr_GPP +theme_few(base_size = 15, base_family = "serif")
  Gr_GPP = Gr_GPP +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_GPP = Gr_GPP +theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_GPP = Gr_GPP +theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_GPP = Gr_GPP +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  #ggtitle("GPP daily sums for all year ")

  if (!is.null(startDoy) && !is.null(endDoy) ){
    Gr_GPP = Gr_GPP +scale_x_continuous(breaks = round(seq(startDoy,endDoy, by = 30),1))
    Gr_GPP = Gr_GPP +coord_cartesian(xlim = c(startDoy, endDoy))
    Gr_NEE = Gr_NEE +coord_cartesian(xlim = c(startDoy, endDoy))
    Gr_Reco = Gr_Reco +coord_cartesian(xlim = c(startDoy, endDoy))
  }
  else if (!is.null(startDoy) || !is.null(endDoy) ) {
    if (!is.null(endDoy)){
      Gr_GPP = Gr_GPP + scale_x_continuous(breaks = round(seq(min(minDoy)-1,endDoy, by = 30),1))
      Gr_GPP = Gr_GPP + coord_cartesian(xlim = c(min(minDoy), endDoy))
      Gr_NEE = Gr_NEE + coord_cartesian(xlim = c(min(minDoy), endDoy))
      Gr_Reco = Gr_Reco + coord_cartesian(xlim = c(min(minDoy), endDoy))
    }
    if (!is.null(startDoy)){
      Gr_GPP = Gr_GPP + scale_x_continuous(breaks = round(seq(startDoy,max(maxDoy), by = 30),1))
      Gr_GPP = Gr_GPP + coord_cartesian(xlim = c(startDoy, max(maxDoy)))
      Gr_NEE = Gr_NEE + coord_cartesian(xlim = c(startDoy, max(maxDoy)))
      Gr_Reco = Gr_Reco + coord_cartesian(xlim = c(startDoy, max(maxDoy)))
    }
  }
  else {
    Gr_GPP = Gr_GPP + scale_x_continuous(breaks = round(seq(min(minDoy)-1,max(maxDoy), by = 30),1))
    Gr_GPP = Gr_GPP + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
    Gr_NEE = Gr_NEE + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
    Gr_Reco = Gr_Reco + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
  }
  grid.newpage()
  return(grid.draw(rbind(ggplotGrob(Gr_NEE), ggplotGrob(Gr_Reco), ggplotGrob(Gr_GPP), size = "first")))
}



PlotAllVegetationAligned = function (DataList,filled = FALSE) {
  pd = position_dodge(.1)
  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = as.factor(c(15,21,17,19))
  Gr_GPP = ggplot()
  Gr_NEE = ggplot()
  Gr_Reco = ggplot()
  maxDoy = 0
  minDoy = 365
  for(n in 1:length(DataList)) {
    if (filled == TRUE ){
      plot_data = DataList[[n]]$daily_f
    } else {
      plot_data = DataList[[n]]$daily
    }

    if(is.null(plot_data$vegetation_day)){
      stop(paste("Object ",n," in the list doesn't have 'vegetation_day' data"), call. = FALSE)
    }
    maxDoy = c(maxDoy, max(plot_data$vegetation_day, na.rm=TRUE))
    minDoy = c(minDoy, min(plot_data$vegetation_day, na.rm = TRUE))

    pd = position_dodge(.1*n)
    Gr_GPP = Gr_GPP + geom_line(data = plot_data, aes(x=vegetation_day, y=ma(GPP* 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_GPP = Gr_GPP + geom_point(data = plot_data , aes(x=vegetation_day, y=GPP* 12*18 /10000),position=pd,size=2, shape=shape_list[n], fill=n,alpha=.5)
    Gr_NEE = Gr_NEE + geom_line(data = plot_data, aes(x=vegetation_day, y=ma(NEE_f_sums * 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_NEE = Gr_NEE + geom_point(data = plot_data , aes(x=vegetation_day, y = NEE_f_sums * 12*18 /10000),position=pd,size=2, shape=shape_list[n], fill=n,alpha=.5)
    Gr_Reco = Gr_Reco + geom_line(data = plot_data, aes(x=vegetation_day, y=ma(Reco* 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_Reco = Gr_Reco + geom_point(data = plot_data , aes(x=vegetation_day, y=GPP* 12*18 /10000),position=pd,size=2, shape=shape_list[n], fill=n,alpha=.5)
    phases_lines = data.frame(levels = levels(as.factor(plot_data$phase)), veg_day = as.vector(by(plot_data, as.factor(plot_data$phase), function(x) min(x$vegetation_day))), label_height = max(plot_data$GPP, na.rm = TRUE)*(.9)*12*18 /10000)
    phases_lines = cbind(phases_lines[with(phases_lines, order(veg_day)), ], letter = c("", letters[1:length(levels(as.factor(plot_data$phase)))-1]))
    print(phases_lines)
    Gr_GPP = Gr_GPP + geom_vline(xintercept = phases_lines$veg_day, size=.5, linetype = linetypes[n], alpha = .5, label=phases_lines$levels)
    Gr_GPP = Gr_GPP + geom_text(data = phases_lines, aes(x = veg_day,y = label_height,label=letter, hjust = -1, alpha = 0.9,family="serif"), size=6, face="bold", )
    Gr_NEE = Gr_NEE + geom_vline(xintercept = phases_lines$veg_day, size=.5, linetype = linetypes[n], alpha = .5, label=phases_lines$levels)
    Gr_NEE = Gr_NEE + geom_text(data = phases_lines, aes(x = veg_day,y = label_height,label=letter, hjust = -1, alpha = 0.9,family="serif"), size=6, face="bold", )
    Gr_Reco = Gr_Reco + geom_vline(xintercept = phases_lines$veg_day, size=.5, linetype = linetypes[n], alpha = .5, label=phases_lines$levels)
    Gr_Reco = Gr_Reco + geom_text(data = phases_lines, aes(x = veg_day,y = label_height,label=letter, hjust = -1, alpha = 0.9,family="serif"), size=6, face="bold", )
    #x=phases_lines$veg_day, y=phases_lines$label_height,
    ##*(1+1:length(label_height)/20
  }

  Gr_GPP = Gr_GPP + geom_hline(yintercept = 0, size=.5, linetype = 2)
  #Gr_GPP = Gr_GPP +geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)
  Gr_GPP = Gr_GPP +xlab("Day of vegetation")
  Gr_GPP = Gr_GPP + ylab(expression(paste(bold("GPP")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))
  Gr_NEE = Gr_NEE + ylab(expression(paste(bold("NEE")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))
  Gr_Reco = Gr_Reco + ylab(expression(paste(bold("Reco")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))
  #Gr_GPP = Gr_GPP +geom_vline(xintercept = 163, size=3, alpha=.2)
  #μmol CO2 m-2s-1)")+
  #Gr_GPP = Gr_GPP +scale_x_continuous(breaks = round(seq(min(DataList[[n]]$daily$Doy), max(DataList[[n]]$daily$Doy), by = 30),1))
  Gr_Reco = Gr_Reco + theme_few(base_size = 15, base_family = "serif")
  Gr_Reco = Gr_Reco + theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_Reco = Gr_Reco + theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_Reco = Gr_Reco + theme(plot.margin = unit(c(1,2,1,2), "lines"))



  Gr_NEE = Gr_NEE + theme_few(base_size = 15, base_family = "serif")
  Gr_NEE = Gr_NEE + theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_NEE = Gr_NEE + theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_NEE = Gr_NEE + theme(plot.margin = unit(c(1,2,1,2), "lines"))

  Gr_GPP = Gr_GPP + theme_few(base_size = 15, base_family = "serif")
  Gr_GPP = Gr_GPP + theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_GPP = Gr_GPP + theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_GPP = Gr_GPP + theme(plot.margin = unit(c(1,2,1,2), "lines"))

  Gr_GPP = Gr_GPP + theme(legend.position="none")
  Gr_NEE = Gr_NEE + theme(legend.position="none")
  Gr_Reco = Gr_Reco + theme(legend.position="none")


  Gr_GPP = Gr_GPP + ggtitle("GPP daily sums for vegetation period")

  Gr_GPP = Gr_GPP + scale_x_continuous(breaks = round(seq(min(minDoy),max(maxDoy), by = 10),1))
  Gr_GPP = Gr_GPP + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
  Gr_NEE = Gr_NEE + scale_x_continuous(breaks = round(seq(min(minDoy),max(maxDoy), by = 10),1))
  Gr_NEE = Gr_NEE + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
  Gr_Reco = Gr_Reco + scale_x_continuous(breaks = round(seq(min(minDoy),max(maxDoy), by = 10),1))
  Gr_Reco = Gr_Reco + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))

  grid.newpage()
  return(grid.draw(rbind(ggplotGrob(Gr_NEE), ggplotGrob(Gr_Reco), ggplotGrob(Gr_GPP), size = "first")))

}


PlotGPPVegetationAligned = function (DataList,filled = FALSE) {
  pd = position_dodge(.1)
  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = as.factor(c(15,21,17,19))
  Gr_GPP = ggplot()
  Gr_bars = ggplot()
  maxDoy = 0
  minDoy = 365
  for(n in 1:length(DataList)) {
    if (filled == TRUE ){
      plot_data = DataList[[n]]$daily_f
    } else {
      plot_data = DataList[[n]]$daily
    }

    if(is.null(plot_data$vegetation_day)){
      stop(paste("Object ",n," in the list doesn't have 'vegetation_day' data"), call. = FALSE)
    }
    maxDoy = c(maxDoy, max(plot_data$vegetation_day, na.rm=TRUE))
    minDoy = c(minDoy, min(plot_data$vegetation_day, na.rm = TRUE))

    pd = position_dodge(.1*n)
    Gr_GPP = Gr_GPP + geom_line(data = plot_data, aes(x=vegetation_day, y=ma(GPP* 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_GPP = Gr_GPP + geom_point(data = plot_data , aes(x=vegetation_day, y=GPP* 12*18 /10000),position=pd,size=2, shape=shape_list[n], fill=n,alpha=.5)
    phases_lines = data.frame(levels = levels(as.factor(plot_data$phase)), veg_day = as.vector(by(plot_data, as.factor(plot_data$phase), function(x) min(x$vegetation_day))), label_height = max(plot_data$GPP, na.rm = TRUE)*(.9)*12*18 /10000)
    phases_lines = cbind(phases_lines[with(phases_lines, order(veg_day)), ], letter = c("a",letters[1:length(levels(as.factor(plot_data$phase)))-1]),field = rep(letters[n],times = length(phases_lines[,1])))
    phases_lines=phases_lines[-(length(phases_lines[,1])),]
    phases_lines=phases_lines[-1,]
    phases_lines$veg_day[1] = phases_lines$veg_day[1] + 1
    phases_lines[, 2][2:length(phases_lines[,1])] = phases_lines[, 2][2:length(phases_lines[,1])] - phases_lines[, 2][1:(length(phases_lines[,1]-1))]
    phases_lines[, 2][length(phases_lines[,1])] = max(plot_data$vegetation_day, na.rm=TRUE)-sum(phases_lines[, 2][1:(length(phases_lines[,1])-1)])
    text_pos = cumsum(phases_lines$veg_day)-phases_lines$veg_day*.5
    phases_lines = cbind(phases_lines, text_pos)
    if (n==1){
      bar_legend = phases_lines
    } else {
      bar_legend = rbind(bar_legend,phases_lines)
    }
    #Gr_GPP = Gr_GPP + geom_vline(xintercept = phases_lines$veg_day, size=.5, linetype = linetypes[n], alpha = .5, label=phases_lines$levels)
    #Gr_GPP = Gr_GPP + geom_text(data = phases_lines, aes(x = veg_day,y = label_height,label=letter, hjust = -1, alpha = 0.9,family="serif"), size=6, face="bold", )
  #x=phases_lines$veg_day, y=phases_lines$label_height,
  ##*(1+1:length(label_height)/20
    }

  print(bar_legend )

  Gr_bars = ggplot(bar_legend ,aes(x = field, y = veg_day)) + geom_bar(position = "stack", stat = "identity",color="black", aes(width = 1, fill= NA)) + geom_text(aes(label=letter, y = text_pos), color="black",size=4,position = position_dodge(height=0.1)) + coord_flip()  + scale_fill_few() + theme_few(base_size = 15, base_family = "serif") + theme(legend.position = "none",axis.text.x =element_blank(), axis.line = element_blank(), axis.title.x = element_blank(),axis.title.y = element_blank(), axis.ticks = element_line(size = 0), panel.border = element_blank(), panel.margin = unit(1, "mm"), plot.margin = unit(c(0,0,0,0), "lines"))
  Gr_GPP = Gr_GPP + geom_hline(yintercept = 0, size=.5, linetype = 2)
  #Gr_GPP = Gr_GPP +geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)
  #Gr_GPP = Gr_GPP +xlab("Day of vegetation")
  #Gr_GPP = Gr_GPP +ylab(expression(paste(bold("GPP")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))
  #Gr_GPP = Gr_GPP +geom_vline(xintercept = 163, size=3, alpha=.2)
  #μmol CO2 m-2s-1)")+
  #Gr_GPP = Gr_GPP +scale_x_continuous(breaks = round(seq(min(DataList[[n]]$daily$Doy), max(DataList[[n]]$daily$Doy), by = 30),1))
  Gr_GPP = Gr_GPP +theme_few(base_size = 15, base_family = "serif")
  Gr_GPP = Gr_GPP +theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_GPP = Gr_GPP +theme(axis.title.x = element_text(size =15, face="bold"))
  #Gr_GPP = Gr_GPP +theme(plot.margin = unit(c(1,2,1,2), "lines"))
  Gr_GPP = Gr_GPP + theme(legend.position="none")
  Gr_GPP = Gr_GPP + ggtitle("GPP daily sums for vegetation period")

  Gr_GPP = Gr_GPP + scale_x_continuous(breaks = round(seq(min(minDoy),max(maxDoy), by = 10),1))
  Gr_GPP = Gr_GPP + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
  grid.newpage()

  #return(grid.draw(rbind(ggplotGrob(Gr_GPP),ggplotGrob(Gr_bars))))
  #return(ggplotGrob(Gr_bars))
  #plot_grid(Gr_GPP,Gr_bars,  ncol=1, labels=c(),align = "h")
  #
  return(grid.arrange(Gr_GPP,Gr_bars,ncol=1, heights=unit(c(100,15), c("mm", "mm"))))
}

PlotFluxSepCum  = function(DataList,filled = FALSE, startDoy, endDoy) {

  pd = position_dodge(.1)
  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = as.factor(c(15,21,17,19))
  Gr_NEE_cum = ggplot()
  Gr_Reco_cum = ggplot()
  Gr_GPP_cum = ggplot()
  maxDoy = 1
  minDoy = 365
  for(n in 1:length(DataList)) {
    if (filled == TRUE ){
      plot_data = DataList[[n]]$daily_f
    } else {
      plot_data = DataList[[n]]$daily
    }
    maxDoy = c(maxDoy, max(plot_data$Doy))
    minDoy = c(minDoy, min(plot_data$Doy))
    pd = position_dodge(.1*n)
  plot_data[is.na(plot_data)] = 0 #because cumsum don't want to work with NA
    Gr_GPP_cum = Gr_GPP_cum + geom_line(data = plot_data, aes(x=Doy, y=cumsum(((GPP) * 12*18 /10000))), size = 1, linetype = linetypes[n], position=pd)
    Gr_NEE_cum = Gr_NEE_cum + geom_line(data = plot_data, aes(x=Doy, y=cumsum((NEE_f_sums * 12*18 /10000))), size = 1, linetype = linetypes[n], position=pd)
    #xlab("Day of the year ")+
    Gr_Reco_cum = Gr_Reco_cum + geom_line(data = plot_data, aes(x=Doy, y=cumsum((Reco * 12*18 /10000))), size = 1, linetype = linetypes[n], position=pd)
  }

  Gr_NEE_cum = Gr_NEE_cum + ylab(expression(paste(bold("Cumulative NEE")," ( g "," ",C[CO[2]]," ",m^-2," "," )",sep="")))
  #μmol CO2 m-2s-1)")+
  #Gr_NEE_cum = Gr_NEE_cum +scale_x_continuous(breaks = round(seq(min(DataList[[n]]$daily$Doy), max(DataList[[n]]$daily$Doy), by = 30),1))
  Gr_NEE_cum = Gr_NEE_cum +geom_hline(yintercept = 0, linetype=2)
  Gr_NEE_cum = Gr_NEE_cum + theme_few(base_size = 14, base_family = "serif")
  Gr_NEE_cum = Gr_NEE_cum + theme(axis.title.y = element_text(size =14, face="bold"))
  Gr_NEE_cum = Gr_NEE_cum + theme(axis.title.x = element_blank())
  Gr_NEE_cum = Gr_NEE_cum + theme(axis.text.x = element_blank())
  Gr_NEE_cum = Gr_NEE_cum + theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_NEE_cum = Gr_NEE_cum + theme(axis.ticks.x = element_blank())
  #+ggtitle("NEE_f cumulation for two towers total")

  #Gr_Reco_cum = Gr_Reco_cum + scale_x_continuous(breaks = round(seq(min(DataList[[n]]$daily$Doy), max(DataList[[n]]$daily$Doy), by = 30),1))
  #xlab("Day of the year ")+
  Gr_Reco_cum = Gr_Reco_cum + ylab(expression(paste(bold("Cumulative Reco")," ( g "," ",C[CO[2]]," ",m^-2," "," )",sep="")))
  Gr_Reco_cum = Gr_Reco_cum + geom_hline(yintercept = 0, linetype=2)
  Gr_Reco_cum = Gr_Reco_cum + theme_few(base_size = 14, base_family = "serif")
  Gr_Reco_cum = Gr_Reco_cum + theme(axis.title.y = element_text(size = 14, face="bold", hjust=0.2, vjust=1, lineheight = 45))
  Gr_Reco_cum = Gr_Reco_cum + theme(plot.margin = unit(c(0,1,0,1), "lines" ))
  Gr_Reco_cum = Gr_Reco_cum + theme(axis.title.x = element_blank())
  Gr_Reco_cum = Gr_Reco_cum + theme(axis.text.x = element_blank())
  Gr_Reco_cum = Gr_Reco_cum + theme(axis.ticks.x = element_blank())
  # ggtitle("NEE_f cumulation for two towers total")

  #Gr_GPP_cum = Gr_GPP_cum +  scale_x_continuous(breaks = round(seq(min(DataList[[n]]$daily$Doy), max(DataList[[n]]$daily$Doy), by = 30),1))
  Gr_GPP_cum = Gr_GPP_cum + xlab("Day of the year ")
  Gr_GPP_cum = Gr_GPP_cum + ylab(expression(paste(bold("Cumulative  GPP")," ( g "," ",C[CO[2]]," ",m^-2," )",sep="")))
  #μmol CO2 m-2s-1)")+
  Gr_GPP_cum = Gr_GPP_cum + geom_hline(yintercept = 0, linetype=2)
  Gr_GPP_cum = Gr_GPP_cum + theme_few(base_size = 14, base_family = "serif")
  Gr_GPP_cum = Gr_GPP_cum + theme(axis.title.y = element_text(size =14, face="bold"))
  Gr_GPP_cum = Gr_GPP_cum + theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_GPP_cum = Gr_GPP_cum + theme(axis.title.x = element_text(size =15, face="bold"))

  if (!is.null(startDoy) && !is.null(endDoy) ){
    print(c(startDoy,endDoy))
    Gr_GPP_cum = Gr_GPP_cum + scale_x_continuous(breaks = round(seq(startDoy,endDoy, by = 30),1))
    Gr_GPP_cum = Gr_GPP_cum + coord_cartesian(xlim = c(startDoy, endDoy))
    Gr_NEE_cum = Gr_NEE_cum + coord_cartesian(xlim = c(startDoy, endDoy))
    Gr_Rec_cumo = Gr_Reco_cum + coord_cartesian(xlim = c(startDoy, endDoy))
  }
  else if (!is.null(startDoy) || !is.null(endDoy) ) {
    if (!is.null(endDoy)){
      Gr_GPP_cum = Gr_GPP_cum + scale_x_continuous(breaks = round(seq(min(minDoy)-1,endDoy, by = 30),1))
      Gr_GPP_cum = Gr_GPP_cum + coord_cartesian(xlim = c(min(minDoy), endDoy))
      Gr_NEE_cum = Gr_NEE_cum + coord_cartesian(xlim = c(min(minDoy), endDoy))
      Gr_Reco_cum = Gr_Reco_cum + coord_cartesian(xlim = c(min(minDoy), endDoy))
    }
    if (!is.null(startDoy)){
      Gr_GPP_cum = Gr_GPP_cum + scale_x_continuous(breaks = round(seq(startDoy,max(maxDoy), by = 30),1))
      Gr_GPP_cum = Gr_GPP_cum + coord_cartesian(xlim = c(startDoy, max(maxDoy)))
      Gr_NEE_cum = Gr_NEE_cum + coord_cartesian(xlim = c(startDoy, max(maxDoy)))
      Gr_Reco_cum = Gr_Reco_cum + coord_cartesian(xlim = c(startDoy, max(maxDoy)))
    }
  }
  else {
    Gr_GPP_cum = Gr_GPP_cum + scale_x_continuous(breaks = round(seq(min(minDoy)-1,max(maxDoy), by = 30),1))
    Gr_GPP_cum = Gr_GPP_cum + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
    Gr_NEE_cum = Gr_NEE_cum + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
    Gr_Reco_cum = Gr_Reco_cum + coord_cartesian(xlim = c(min(minDoy), max(maxDoy)))
  }

  grid.newpage()
  return(grid.draw(rbind(ggplotGrob(Gr_NEE_cum), ggplotGrob(Gr_Reco_cum), ggplotGrob(Gr_GPP_cum), size = "first")))

}

# PlotGPPvsTsoil ----------------------------------------------------------

PlotGPPvsTsoil = function(DataList, by = FALSE) {
  pd = position_dodge(.1)
  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = c(1,17,18,19)
  color_list = c("black","red","white")

  Gr_GPP =  ggplot()

  for(n in 1:length(DataList)) {
    pd = position_dodge(.1*n)
    if (by == "SWC")
    {
      DataList[[n]]$daily = DataList[[n]]$daily[which(!is.na(DataList[[n]]$daily$moisture_levels)),]
    }
    #Gr_GPP = Gr_GPP +geom_line(data = DataList[[n]]$daily, aes(x=Tsoil, y=ma(GPP * 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_GPP = Gr_GPP +geom_point(data = DataList[[n]]$daily , aes(x=Tsoil, y=GPP* 12 * 18/10000),shape=shape_list[n], fill=color_list[n],position=pd,size=3, fill=n,alpha=.7)

  }
  if (by == "months")
  {
    Gr_GPP = Gr_GPP +  facet_wrap(~month_number, ncol =3)
  }
  if (by == "SWC")
  {
    Gr_GPP = Gr_GPP +  facet_wrap(~moisture_levels, ncol =3)
  }
  Gr_GPP = Gr_GPP + geom_hline(yintercept = 0, size=.5, linetype = 2)
  Gr_GPP = Gr_GPP + xlab(expression(bold(paste(T["soil"]," at 5cm depth "," (", ring("C"),")",sep=""))))
  Gr_GPP = Gr_GPP +ylab(expression(paste(bold("GPP")," ( ","g "," ",m^-2," ",d^-1, " )",sep="")))

  #μmol CO2 m-2s-1)")+
  Gr_GPP = Gr_GPP +scale_x_continuous(breaks = round(seq(0, max(DataList[[n]]$daily$Tsoil, na.rm =TRUE), by = 5),1))
  Gr_GPP = Gr_GPP +theme_few(base_size = 15, base_family = "serif")
  Gr_GPP = Gr_GPP +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_GPP = Gr_GPP +theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_GPP = Gr_GPP +theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_GPP = Gr_GPP +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  #ggtitle("NEE_f daily sums for all year ")
  return(Gr_GPP)
}

# PlotGPPvsPAR ------------------------------------------------------------


PlotGPPvsPAR = function(DataList, by = FALSE) {
  pd = position_dodge(.1)
  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = c(1,17,18,19)
  color_list = c("black","red","white")

  Gr_GPP =  ggplot()

  for(n in 1:length(DataList)) {
    pd = position_dodge(.1*n)
    if (by == "SWC")
    {
      DataList[[n]]$daily = DataList[[n]]$daily[which(!is.na(DataList[[n]]$daily$moisture_levels)),]
    }
    Gr_GPP = Gr_GPP +geom_point(data = DataList[[n]]$daily , aes(x=PAR_Den_Avg, y=GPP* 12 * 18/10000),shape=shape_list[n], fill=color_list[n],position=pd,size=3, fill=n,alpha=.7)

  }
  if (by == "months")
  {
    Gr_GPP = Gr_GPP +  facet_wrap(~month_number, ncol =3)
  }
  if (by == "SWC")
  {
    Gr_GPP = Gr_GPP +  facet_wrap(~moisture_levels, ncol =3)
  }


  Gr_GPP = Gr_GPP + geom_hline(yintercept = 0, size=.5, linetype = 2)
  Gr_GPP = Gr_GPP + xlab(expression(paste(bold("PAR average density per day "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep="")))
  Gr_GPP = Gr_GPP +ylab(expression(paste(bold("GPP")," ( ","g "," ",m^-2," ",d^-1, " )",sep="")))

  #μmol CO2 m-2s-1)")+
  Gr_GPP = Gr_GPP +scale_x_continuous(breaks = round(seq(0, max(DataList[[n]]$daily$PAR_Den_Avg, na.rm =TRUE), by = 100),1))
  Gr_GPP = Gr_GPP +theme_few(base_size = 15, base_family = "serif")
  Gr_GPP = Gr_GPP +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_GPP = Gr_GPP +theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_GPP = Gr_GPP +theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_GPP = Gr_GPP +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  #ggtitle("NEE_f daily sums for all year ")
  return(Gr_GPP)
}

# PlotGPPvsSWC ------------------------------------------------------------


PlotGPPvsSWC = function(DataList, by=FALSE) {
  pd = position_dodge(.1)
  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = c(1,17,18,19)
  color_list = c("black","red","white")

  Gr_GPP =  ggplot()

  for(n in 1:length(DataList)) {
    pd = position_dodge(.1*n)

    #Gr_GPP = Gr_GPP +geom_line(data = DataList[[n]]$daily, aes(x=Tsoil, y=ma(GPP * 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_GPP = Gr_GPP +geom_point(data = DataList[[n]]$daily , aes(x=SWC_1*100, y=GPP* 12 * 18/10000),shape=shape_list[n], fill=color_list[n],position=pd,size=3, fill=n,alpha=.7)

  }
  if (by == "months")
  {
    Gr_GPP = Gr_GPP + facet_wrap(~month_number, ncol =3)
  }

  #Gr_GPP = Gr_GPP +
  Gr_GPP = Gr_GPP + geom_hline(yintercept = 0, size=.5, linetype = 2)
  Gr_GPP = Gr_GPP +  xlab(expression(bold(paste("SWC at 5cm depth (%)"," ",sep=""))))
  Gr_GPP = Gr_GPP +ylab(expression(paste(bold("GPP")," ( ","g "," ",m^-2," ",d^-1, " )",sep="")))

  #μmol CO2 m-2s-1)")+
  Gr_GPP = Gr_GPP +scale_x_continuous(breaks = round(seq(0, max(DataList[[n]]$daily$PAR_Den_Avg, na.rm =TRUE), by = 10),1))
  Gr_GPP = Gr_GPP +theme_few(base_size = 15, base_family = "serif")
  Gr_GPP = Gr_GPP +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_GPP = Gr_GPP +theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_GPP = Gr_GPP +theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_GPP = Gr_GPP +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  #ggtitle("NEE_f daily sums for all year ")
  return(Gr_GPP)
}


# PlotRecovsTsoil ---------------------------------------------------------


PlotRecovsTsoil = function(DataList, by = FALSE) {
  pd = position_dodge(.1)
  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = c(1,17,18,19)
  color_list = c("black","red","white")

  Gr_Reco =  ggplot()

  for(n in 1:length(DataList)) {
    pd = position_dodge(.1*n)
    if (by == "SWC")
    {
      DataList[[n]]$daily = DataList[[n]]$daily[which(!is.na(DataList[[n]]$daily$moisture_levels)),]
    }
    #Gr_Reco = Gr_Reco +geom_line(data = DataList[[n]]$daily, aes(x=Tsoil, y=ma(Reco * 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_Reco = Gr_Reco +geom_point(data = DataList[[n]]$daily , aes(x=Tsoil, y=Reco* 12 * 18/10000),shape=shape_list[n], fill=color_list[n],position=pd,size=3, fill=n,alpha=.7)

  }

  if (by == "months")
  {
    Gr_Reco = Gr_Reco + facet_wrap(~month_number, ncol =3)
  }
  if (by == "SWC")
  {
    Gr_Reco = Gr_Reco + facet_wrap(~moisture_levels, ncol =3)
  }

  #Gr_Reco = Gr_Reco +
  Gr_Reco = Gr_Reco + geom_hline(yintercept = 0, size=.5, linetype = 2)
  Gr_Reco = Gr_Reco + xlab(expression(bold(paste(T["soil"]," at 5cm depth "," (", ring("C"),")",sep=""))))
  Gr_Reco = Gr_Reco +ylab(expression(paste(bold("Reco")," ( ","g "," ",m^-2," ",d^-1, " )",sep="")))

  #μmol CO2 m-2s-1)")+
  Gr_Reco = Gr_Reco +scale_x_continuous(breaks = round(seq(0, max(DataList[[n]]$daily$Tsoil, na.rm =TRUE), by = 5),1))
  Gr_Reco = Gr_Reco +theme_few(base_size = 15, base_family = "serif")
  Gr_Reco = Gr_Reco +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_Reco = Gr_Reco +theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_Reco = Gr_Reco +theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_Reco = Gr_Reco +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  #ggtitle("NEE_f daily sums for all year ")
  return(Gr_Reco)
}


# Combine shifted year period from two adjastent --------------------------

ShiftedPeriod = function(DataList, startDate){
  ShiftedData = list();
  return(ShiftedData)
}


# PlotRecovsPAR  ----------------------------------------------------------


PlotRecovsPAR = function(DataList, by=FALSE) {

  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = c(1,17,18,19)
  color_list = c("black","red","white")

  Gr_Reco =  ggplot()

  for(n in 1:length(DataList)) {
    pd = position_dodge(.1*n)
      if (by == "SWC")
    {
      DataList[[n]]$daily = DataList[[n]]$daily[which(!is.na(DataList[[n]]$daily$moisture_levels)),]
    }
    Gr_Reco = Gr_Reco +geom_point(data = DataList[[n]]$daily , aes(x=PAR_Den_Avg, y=Reco* 12 * 18/10000),shape=shape_list[n], fill=color_list[n],position=pd,size=3, fill=n,alpha=.7)
  }

  if (by == "months")
  {
    Gr_Reco = Gr_Reco + facet_wrap(~month_number, ncol =3)
  }
  if (by == "SWC")
  {
    Gr_Reco = Gr_Reco + facet_wrap(~moisture_levels, ncol =3)
  }

  Gr_Reco = Gr_Reco + geom_hline(yintercept = 0, size=.5, linetype = 2)
  Gr_Reco = Gr_Reco + xlab(expression(paste(bold("PAR average density per day "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep="")))
  Gr_Reco = Gr_Reco +ylab(expression(paste(bold("Reco")," ( ","g "," ",m^-2," ",d^-1, " )",sep="")))
  Gr_Reco = Gr_Reco +scale_x_continuous(breaks = round(seq(0, max(DataList[[n]]$daily$PAR_Den_Avg, na.rm =TRUE), by = 100),1))
  Gr_Reco = Gr_Reco +theme_few(base_size = 15, base_family = "serif")
  Gr_Reco = Gr_Reco +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_Reco = Gr_Reco +theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_Reco = Gr_Reco +theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_Reco = Gr_Reco +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  return(Gr_Reco)
}

# PlotRecovsSWC -----------------------------------------------------------


PlotRecovsSWC = function(DataList) {

  linetypes=c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
  shape_list = c(1,17,18,19)
  color_list = c("black","red","white")

  Gr_Reco =  ggplot()

  for(n in 1:length(DataList)) {
    pd = position_dodge(.1*n)

    #Gr_Reco = Gr_Reco +geom_line(data = DataList[[n]]$daily, aes(x=Tsoil, y=ma(Reco * 12*18 /10000)), size=.8, position=pd, linetype=linetypes[n])
    Gr_Reco = Gr_Reco +geom_point(data = DataList[[n]]$daily , aes(x=SWC_1*100, y=Reco* 12 * 18/10000),shape=shape_list[n], fill=color_list[n],position=pd,size=3, fill=n,alpha=.7)

  }
  #Gr_Reco = Gr_Reco +
  Gr_Reco = Gr_Reco + geom_hline(yintercept = 0, size=.5, linetype = 2)
  Gr_Reco = Gr_Reco +  xlab(expression(bold(paste("SWC at 5cm depth (%)"," ",sep=""))))
  Gr_Reco = Gr_Reco +ylab(expression(paste(bold("Reco")," ( ","g "," ",m^-2," ",d^-1, " )",sep="")))

  #μmol CO2 m-2s-1)")+
  Gr_Reco = Gr_Reco +scale_x_continuous(breaks = round(seq(0, max(DataList[[n]]$daily$PAR_Den_Avg, na.rm =TRUE), by = 10),1))
  Gr_Reco = Gr_Reco +theme_few(base_size = 15, base_family = "serif")
  Gr_Reco = Gr_Reco +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  Gr_Reco = Gr_Reco +theme(axis.title.y = element_text(size = 15, face="bold"))
  Gr_Reco = Gr_Reco +theme(axis.title.x = element_text(size =15, face="bold"))
  Gr_Reco = Gr_Reco +theme(plot.margin = unit(c(0,1,0,1), "lines"))
  #ggtitle("NEE_f daily sums for all year ")
  return(Gr_Reco)
}
FindNAlengths = function(x){

  m = as.numeric(is.na(x))
  l=0
  res=c()
  for(n in 1:length(M)){

    if (m[n]==1){
      l=l+1
    }else {
      if (l>0){res=c(res, l)}
      l=0
    }
  }

  lev = as.numeric(levels(as.factor(res)))
  counter = c()
  for(n in 1:length(lev)){
    counter = c(counter, length(which(A_na == lev[n])))
  }
  return(data.frame(lev,counter))
}
#a=3

# PeakCycle <- function(Data=as.vector(sunspots), SearchFrac=0.02){
#   # using package "wmtsa"
#   #the SearchFrac parameter just controls how much to look to either side
#   #of wavCWTPeaks()'s estimated maxima for a bigger value
#   #see dRange
#   Wave <- wavCWT(Data)
#   WaveTree <- wavCWTTree(Wave)
#   WavePeaks <- wavCWTPeaks(WaveTree, snr.min=5)
#   WavePeaks_Times <- attr(WavePeaks, which="peaks")[,"iendtime"]
#
#   NewPeakTimes <- c()
#   dRange <- round(SearchFrac*length(Data))
#   for(i in 1:length(WavePeaks_Times)){
#     NewRange <- max(c(WavePeaks_Times[i]-dRange, 1)):min(c(WavePeaks_Times[i]+dRange, length(Data)))
#     NewPeakTimes[i] <- which.max(Data[NewRange])+NewRange[1]-1
#   }
#
#   return(matrix(c(NewPeakTimes, Data[NewPeakTimes]), ncol=2, dimnames=list(NULL, c("PeakIndices", "Peaks"))))
# }
#
# dev.new(width=6, height=4)
# par(mar=c(4,4,0.5,0.5))
# plot(seq_along(as.vector(sunspots)), as.vector(sunspots), type="l")
# Sunspot_Ext <- PeakCycle()
# points(Sunspot_Ext, col="blue", pch=20)




#package.skeleton(list = c("read_eddy_data","read_biomet_data","join_for_gapfilling","max_footprints","filter_by_quality","reddyproc_gapfill","add_separators","add_events","PlotWindRoses","footprint_for_angle","fill_gap_by_date"), name = "EddyPostProcess")

