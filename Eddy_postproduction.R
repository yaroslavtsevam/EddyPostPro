#setwd('Reddyproc')
library("REddyProc")
library("data.table")
library('plyr')
library("ggplot2")
library('openair')
library('ggthemes')
library('pastecs')
library("gridExtra")
# Reading Data Function
adapt_complex_csv = function(data){
  temp_dataset = data
  if (names(temp_dataset)[1] != "filename") {
    print(temp_dataset)
    print("Whoops strange csv, may be you've allready edited it in Excel? Trying to fix")
    names(temp_dataset) = (as.character(temp_dataset[1, ]))
    print('Names gotten')
  }

    temp_dataset_startline = which(as.character(temp_dataset$daytime) == "T" | as.character(temp_dataset$daytime) == "F")[1];
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
      temp_dataset = fread(file, header = "auto", autostart = 60, skip=1)
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
      dataset = fread(file, header = "auto", autostart = 60, skip=1)
      #temp_dataset_startline = which(as.character(dataset$daytime) == "T" | as.character(dataset$daytime) == "F")[1];
      #dataset = dataset[temp_dataset_startline:length(dataset[['DOY']]), ]
      dataset = adapt_complex_csv(dataset)
      print( paste("Rows read:", length(dataset[['DOY']]), sep=" "))
    }
  }
  #setwd('../')
  return (data.table(dataset))
}

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

# Fill gap by date

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

  print("Going to filter out next amount of NEE data:")
  print(length(join_[['NEE']][join_[['NEE']] < join_.mean - 3*join_.sigma]))
  join_[['NEE']][join_[['NEE']] < join_.mean - 3*join_.sigma] = NA
  join_[['NEE']][join_[['NEE']] > join_.mean + 3*join_.sigma ] = NA
  join_[['NEE']][join_[['x_70%']] > join_[['max_footprint']] ] = NA
  join_[['NEE']][join_[['x_70%']] > tower_height * 10]= NA
  join_[['NEE']][join_[['x_70%']] < 2 ] = NA
  join_[['NEE']][join_[['QF']] > 7 ] = NA
  join_[['H2O_NEE']][join_[['QF_h2o']] > 5 ]= NA
  join_[['H2O_NEE']][join_[['x_70%']] > join_[['max_footprint']]] = NA
  join_[['H2O_NEE']][join_[['x_70%']] > tower_height * 10] = NA
  join_[['H20_NEE']][join_[['x_70%']] < 2 ] = NA

  return(join_)
}

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


PlotWindRoses = function(EddyData, wind_speed, wind_dir)
{ windRose(EddyData, ws=wind_speed, wd=wind_dir)
  # one windRose for each year
  windRose(EddyData, ws=wind_speed, wd=wind_dir,type = "season")
  windRose(EddyData, ws=wind_speed, wd=wind_dir,type = "year")
  # windRose in 10 degree intervals with gridlines and width adjusted
  ## Not run:
  windRose(EddyData, ws=wind_speed, wd=wind_dir, angle = 10, width = 0.2, grid.line = 1)
  ## End(Not run)
  # pollutionRose of nox
  pollutionRose(EddyData, ws=wind_speed, wd=wind_dir, pollutant = "x_70%")
  ## source apportionment plot - contribution to mean
  ## Not run:
  pollutionRose(EddyData, ws=wind_speed, wd=wind_dir, pollutant = "x_70%", type = "season", statistic = "prop.mean")
}
ma  = function(x,n=7){
  filter(x,rep(1/n,n), sides=2)
}

daily_data = function(Data){
  Data = Data[[1]]
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
  names(daily_sums) = paste(new_names,"sums", sep="_")
  names(daily_means) = new_names
  names(daily_errors) = paste(new_names,"errors", sep="_")

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
  Reco[Reco == 'NaN'] =0
  Reco[Reco == 'NA'] = 0
  Reco[Reco == NA] = 0
  GPP[GPP == 'NaN'] =0
  GPP[GPP == 'NA'] = 0
  GPP[GPP == NA] = 0

  NA_count = tapply(as.numeric(Data[['NEE']]), Data$Doy, function(x) length(which(is.na(x))))
  NA_marker = NA_count

  NA_marker[NA_marker < 16] = 1

  NA_marker[NA_marker >= 16] = 0.1

  #add posix dates
  return(cbind(daily_sums, daily_means,daily_errors, NA_count,NA_marker, Reco, GPP))
  #return(cbind(daily_sums, daily_means,daily_errors))
}


weekly_data = function(Data){
  new_names = c()
  Data = Data[[1]]
  for (name in names(Data)){
    if ((class(AllData_A[[name]])[1] == 'numeric' ) | (class(AllData_A[[name]])[1] == 'integer' ) | (class(AllData_A[[name]])[1] == 'character' )){
      weekly_sum = tapply(as.numeric(Data[[name]]), Data$week, sum, na.rm = TRUE)
      weekly_mean = tapply(as.numeric(Data[[name]]), Data$week, mean, na.rm = TRUE)
      weekly_error = tapply(as.numeric(Data[[name]]), Data$week, sd)
      weekly_error = weekly_error/sqrt(length(Data[[name]]))*1.96

      if (exists('weekly_sums')) {
        weekly_sums = cbind(weekly_sums, weekly_sum)
        weekly_means = cbind(weekly_means, weekly_mean)
        weekly_errors = cbind(weekly_errors, weekly_error)
      }
      else{
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
month_data = function(Data){
  mnew_names = c()
  for (name in names(Data)){
    if ((class(AllData_A[[name]])[1] == 'numeric' ) | (class(AllData_A[[name]])[1] == 'integer' ) | (class(AllData_A[[name]])[1] == 'character' )){
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
###Hourly by month
hourly_data = function(AllData){
  hour_means = c()
  hour_errors = c()
  hour_months  = c()
  AllData = AllData[[1]]

  for (m in 1:12) {

    Data = AllData[AllData[['month_number']] == m,]

    hour_mean = tapply(Data[['NEE_f']], Data$hour, mean)
    #print(hour_mean)
    hour_error = tapply(Data[['NEE_f']], Data$hour, sd)
    hour_error = hour_error/sqrt(length(hour_error))*1.96
    hour_month = rep(m, length(hour_error))
    hour_means = c(hour_means, hour_mean)
    hour_errors = c(hour_errors, hour_error)
    hour_months  = c(hour_months, hour_month)

  }
  hour = rep(0:23, length(hour_means)/24)
  #print(hour)
  return(data.frame(cbind(hour_means,hour_errors,hour_months,hour)))
}


hourly_NEE_period = function(AllData,start_date,stop_date){
  hour_means = c()
  hour_errors = c()
  hour_months  = c()
  AllData=AllData[[1]]
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


hourly_data_for_event = function(AllData, event_name){
  hour_means = c()
  hour_errors = c()


  Data = AllData[[1]][AllData[[1]][[event_name]],]

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

FullEddyPostProcess = function(DataFolder,SiteUTM,SitePolygon,events_file,SiteCoordZone, tower_height){
  # Reading Data
  data = read_eddy_data(DataFolder)
  biometdata = read_biomet_data(DataFolder)

  # Forming data set for gap filling
  joined_data = join_for_gapfilling(data, biometdata)

  #Generating column of max footprints
  joined_data = max_footprints(SiteUTM, SitePolygon, joined_data,'wind_dir')
  # Pre Gap filling Filtering
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
  AllData = list(WithEvents, Reddyproc)

  return(AllData)
}
#package.skeleton(list = c("read_eddy_data","read_biomet_data","join_for_gapfilling","max_footprints","filter_by_quality","reddyproc_gapfill","add_separators","add_events","PlotWindRoses","footprint_for_angle","fill_gap_by_date"), name = "EddyPostProcess")




#write.csv(AllData_A, file="Site_A_all.csv")
#write.csv(Combined, file="Two_towers.csv")





