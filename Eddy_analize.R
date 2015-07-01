# Loading postproduction --------------------------------------------------

source(file = "Eddy_postproduction.r", local=TRUE)


# Loading towers metadata -------------------------------------------------
###TODO: Make metdata loadable from tower folder

DataFolderA_13 = 'Data_A/'
DataFolderB_13 = 'Data_B/'
DataFolderA_14 = 'Data_A_14/'
DataFolderB_14 = 'Data_B_14/'
DataFolderO = 'Data_O'
Site_A = c(410041, 6188869)
Site_B = c(410155, 6188893)
Site_O = c(325574, 5668981)
site_polygon  = data.frame(as.numeric(c(409957,410179,410243,410014)),as.numeric(c(6188984,6189058,6188849,6188774)))
site_polygon_A  = data.frame(as.numeric(c(410109.5,410012.7,409977.3,410086)),as.numeric(c(6188807,6188739,6188905,6188942)))
site_polygon_B  = data.frame(as.numeric(c(410188,410225,410128,410086)),as.numeric(c(6188974,6188840,6188811,6188942)))
forest_polygon  = data.frame(as.numeric(c(409472,409943,409532,408959,408587,408471,408353,408124,408048,408083,408088,408041,408038,408237,408524,408547,409090 ,409323,409550 ,409328,409535,409454)),as.numeric(c(6186623,6186720,6187669,6187536,6187517,6187632,6187882,6188235,6188193,6187844,6187575,6187405,6187222 ,6186987,6186310,6186133,6185574,6185407,6185467,6186180,6186267,6186542)))
site_polygon_O = data.frame(as.numeric(c(325578,325965,325381,324931)), as.numeric(c(5669372,5669262,5667644,5667793 )))
events_O = 'Data_O/events.csv'
events_A = 'Data_A/events.csv'
events_B = 'Data_B/events.csv'
Site_coord_and_zone = c(55.837631, 37.564302, 4)
Site_coord_and_zone_O = c(51.14567, 36.50624, 4)
All_towers_height  = 1.5



AllData_A_13 = FullEddyPostProcess(DataFolderA_13,Site_A,site_polygon_A,events_A,Site_coord_and_zone,All_towers_height)

AllData_B_13 = FullEddyPostProcess(DataFolderB_13,Site_B,site_polygon_B,events_B,Site_coord_and_zone,All_towers_height)
#save(AllData_B, file="AllData_B_2013")
AllData_A_14 = FullEddyPostProcess(DataFolderA_14,Site_A,site_polygon_A,events_A,Site_coord_and_zone,All_towers_height)

AllData_B_14 = FullEddyPostProcess(DataFolderB_14,Site_B,site_polygon_B,events_B,Site_coord_and_zone,All_towers_height)
AllData_O = FullEddyPostProcess(DataFolderO,Site_O,site_polygon_O,events_O,Site_coord_and_zone_O, All_towers_height)
#save(AllData_B, file="AllData_B_2013")
AllData_O$daily_f = fread("O_13_filled.csv")
AllData_A_13$daily_f = fread("A_13_filled.csv")
AllData_A_13$daily_f$SWC_1 = AllData_A_13$daily_f$SWC_1/100
AllData_O$daily_f$SWC_1 = AllData_O$daily_f$SWC_1/100
#Adding PAR to site B as soon they very close
AllData_B_13$dt = merge(AllData_B_13$dt,AllData_A_13$dt[,c(1,40),with = FALSE], by = 'DateTime')
AllData_B_14$dt = merge(AllData_B_14$dt,AllData_A_14$dt[,c(1,40),with = FALSE], by = 'DateTime')

Quality_NEE = length(which(is.na(AllData_B$dt$NEE)))/length(AllData_B$dt$H2O_NEE)


# All types of graphs -----------------------------------------------------


#  NEE_f for several towers, diurnal ---------------------------------------

compare_plot(list(AllData_A_13$hourly$NEE_f,AllData_B_13$hourly$NEE_f,AllData_A_14$hourly$NEE_f,AllData_B_14$hourly$NEE_f), "hour", "hour_means","diurnal")


# NEE_f cumulation forseveral towers total --------------------------------
#source(file="Eddy_postproduction.r", local=TRUE)
compare_plot(list(AllData_A_13$daily,AllData_O$daily), "DoY", "NEE_f_cumsum","cumul", ylab=expression(paste(bold("Cumulative NEE")," ( g "," ",C[CO[2]]," ",m^-2," "," )",sep="")))



# Common old Blocks -------------------------------------------------------
##
##
##
##
source(file = "Eddy_postproduction.r", local=TRUE)
AllData_A_14$daily = AddDailyChambers("A_2014.csv",AllData_A_14)
AllData_B_14$daily$Rs_t = AddDailyChambers("A_2014.csv",AllData_B_14)
PlotBiomet(list(AllData_A_13, AllData_O), filled = TRUE, startDoy = 60, endDoy = 330)
AllData_A_14$daily$GPP_n = AllData_A_14$daily$Reco - AllData_A_14$daily$NEE
AllData_B_14$daily$GPP_n = AllData_B_14$daily$Reco - AllData_B_14$daily$NEE
AllData_A_14$daily$GPP_n_cum = cumsum(AllData_A_14$daily$GPP_n)
AllData_B_14$daily$GPP_n_cum = cumsum(AllData_A_14$daily$GPP_n)
PlotDiurnal(list(AllData_A_14, AllData_O))
PlotDiurnal(list(AllData_A_13, AllData_B_13))
SepFlux = PlotFluxSep(list(AllData_A_13, AllData_O))
SepFlux[[2]] + geom_point(data = chamb , aes(x = DOY, y = A_t),position = .1,size = 4,colour = "red", shape = 15, fill = 1, alpha = .5)
PlotFluxSepCum(list(AllData_A_14, AllData_B_14))
PlotFluxSepCum(list(AllData_A_13, AllData_B_13))
PlotGPPvsTsoil(list(AllData_A_13, AllData_O),by = "SWC")
PlotGPPvsPAR(list(AllData_A_13, AllData_O),by = "SWC")
PlotGPPvsSWC(list(AllData_A_13, AllData_O))

PlotRecovsTsoil(list(AllData_A_14, AllData_B_14))
PlotRecovsPAR(list(AllData_A_13, AllData_O),by = "SWC")
PlotRecovsSWC(list(AllData_A_13, AllData_O))


source(file = "Eddy_postproduction.r", local = TRUE)
PlotFluxAlignedDaily(datalist,events, start_event)
PlotFluxAlignedDate(datalist,events, start_event)

#Разобраться с нормальным разбиением потока и посмотреть, что там не так
#Добавить новые данные в dt
#Переписать все функции постороения графиков через общмй конструктор, с возможностью выбора между dt и daily
#Сделать график зависимости GPP от фазы
#Biomet урожай - 226 день, а всего 315, обрезать 230 днем PAR - убрать нули и линии, белые кружки и черные, SWC - заполнить пропуски

#GPP Reco Nee - обрезать 230 днем, 
#Сбросить данные для O и А - получасовые убрать Со2
#GPP - phase aligned - till 120d (110-230)
#всходы -11д, кущение - 31д, выход в трубку - 41д, молочгая спелость - 53д, восковая спелость - 64д
#общая спелость -66д
#Поискать выпадающие значения
#Суммы активных температур?
#Поиск пиков
#Идеи
#
AllData_O$daily$moisture_levels = cut(AllData_O$daily$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
AllData_O$daily[!is.na(AllData_O$daily$moisture_levels)]

!is.na(AllData_O$daily$moisture_levels)
#
###TODO Remake all types of graphs with compare plot
###TODO Add function of fast subseting tower by period - make function period
###TODO Extract Reco and GPP from Reddy and add to dt, compare it with mine

# Check what is in reddy part

AllData_A_13$reddy$sPlotDailySums('NEE_f','NEE_fsd')
AllData_A_14$reddy$sMRFluxPartition(Lat_deg.n=55.83708, Long_deg.n=37.56772, TimeZone_h.n=3) 
AllData_A_13$reddy$sMRFluxPartition(Lat_deg.n=55.83708, Long_deg.n=37.56772, TimeZone_h.n=3) 
AllData_B_14$reddy$sMRFluxPartition(Lat_deg.n=55.83708, Long_deg.n=37.56772, TimeZone_h.n=3) 
AllData_B_13$reddy$sMRFluxPartition(Lat_deg.n=55.83708, Long_deg.n=37.56772, TimeZone_h.n=3) 
AllData_A_14$reddy$sPlotDailySums('GPP_fqc')
AllData_A_13$reddy$sPlotHHFluxesY('GPP_fqc', Year.i=2013)

str(AllData_A_13$reddy$sTEMP)
AllData_A_13$reddy$sTEMP$Tair_f
#Location of DE-Tharandt






# Old stuff ---------------------------------------------------------------



PlotWindRoses(AllData_A_13$dt)
NA_count = tapply(as.numeric(AllData_A$dt$NEE),AllData_A$dt$Doy, function(x) x)
AllData_A$dt$SWC_1 = as.numeric(AllData_A$dt$SWC_1)
AllData_B$dt$SWC_1 = as.numeric(AllData_B$dt$SWC_1)
AllData_A_14$dt$moisture_levels = cut(AllData_A_14$dt$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
AllData_B_14$dt$moisture_levels = cut(AllData_B_14$dt$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
hourly_data_A = hourly_data(AllData_A$dt)
hourly_data_B = hourly_data(AllData_B$dt)
AllData_A_daily = daily_data(AllData_A$dt)
AllData_B_daily = daily_data(AllData_B$dt)
AllData_A_weekly = weekly_data(AllData_A$dt)
AllData_B_weekly = weekly_data(AllData_B$dt)
AllData_A_monthly = month_data(AllData_A$dt)
AllData_B_monthly = month_data(AllData_B$dt)
hourly_A_snow = hourly_NEE_period(AllData_A$dt,"2013-01-01","2013-04-16")
hourly_A_16_04_5_05 = hourly_NEE_period(AllData_A$dt,"2013-04-16","2013-05-05")
hourly_A_6_05_15_05 = hourly_NEE_period(AllData_A$dt,"2013-05-05","2013-05-15")
hourly_A_15_05_10_06 = hourly_NEE_period(AllData_A$dt,"2013-05-15","2013-06-10")
hourly_A_10_06_17_06 = hourly_NEE_period(AllData_A$dt,"2013-06-10","2013-06-17")
hourly_A_17_06_24_06 = hourly_NEE_period(AllData_A$dt,"2013-06-17","2013-06-24")
hourly_A_24_06_01_07 = hourly_NEE_period(AllData_A$dt,"2013-06-24","2013-07-01")
hourly_A_01_07_08_07 = hourly_NEE_period(AllData_A$dt,"2013-07-01","2013-07-08")
hourly_A_08_07_15_07 = hourly_NEE_period(AllData_A$dt,"2013-07-08","2013-07-15")
hourly_A_15_07_25_07 = hourly_NEE_period(AllData_A$dt,"2013-07-15","2013-07-25")
hourly_A_25_07_08_08 = hourly_NEE_period(AllData_A$dt,"2013-07-25","2013-08-08")
hourly_A_08_08_31_13 = hourly_NEE_period(AllData_A$dt,"2013-08-08","2013-12-31")
hourly_B_snow = hourly_NEE_period(AllData_B$dt,"2013-01-01","2013-04-16")
hourly_B_16_04_5_05 = hourly_NEE_period(AllData_B$dt,"2013-04-16","2013-05-05")
hourly_B_6_05_15_05 = hourly_NEE_period(AllData_B$dt,"2013-05-05","2013-05-15")
hourly_B_15_05_10_06 = hourly_NEE_period(AllData_B$dt,"2013-05-15","2013-06-10")
hourly_B_10_06_17_06 = hourly_NEE_period(AllData_B$dt,"2013-06-10","2013-06-17")
hourly_B_17_06_24_06 = hourly_NEE_period(AllData_B$dt,"2013-06-17","2013-06-24")
hourly_B_24_06_01_07 = hourly_NEE_period(AllData_B$dt,"2013-06-24","2013-07-01")
hourly_B_01_07_08_07 = hourly_NEE_period(AllData_B$dt,"2013-07-01","2013-07-08")
hourly_B_08_07_15_07 = hourly_NEE_period(AllData_B$dt,"2013-07-08","2013-07-15")
hourly_B_15_07_25_07 = hourly_NEE_period(AllData_B$dt,"2013-07-15","2013-07-25")
hourly_B_25_07_08_08 = hourly_NEE_period(AllData_B$dt,"2013-07-25","2013-08-08")
hourly_B_08_08_31_13 = hourly_NEE_period(AllData_B$dt,"2013-08-08","2013-12-31")
hourly_snow_cover_A = hourly_data_for_event(AllData_A$dt,'snow_cover')
hourly_snow_cover_B = hourly_data_for_event(AllData_B$dt,'snow_cover')
Daily_A_114 =  AllData_A_daily[AllData_A_daily[['Doy']] >114,]
Daily_B_114 =  AllData_B_daily[AllData_B_daily[['Doy']] >114,]
Daily_A_114$moisture_levels = cut(Daily_A_114$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
Daily_B_114$moisture_levels = cut(Daily_B_114$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
AllData_A_114 =  AllData_A$dt[AllData_A$dt[['Doy']] >114,]
Daily_A_114_b = Daily_A_114$dt[Daily_A_114$dt[['NA_count']]>47,]
Daily_B_114_b = Daily_B_114[Daily_B_114[['NA_count']]>47,]
AllData_B_114 =  AllData_B$dt[AllData_B$dt[['Doy']] >114,]
Weekly_A_114 =  AllData_A_weekly$dt[AllData_A_weekly$dt[['Doy']] >114,]
Weekly_A_90 =  AllData_A_weekly[AllData_A_weekly[['Doy']] >90,]
Weekly_B_114 =  AllData_B_weekly[AllData_B_weekly[['Doy']] >114,]
periods_a =c('hourly_A_snow','hourly_A_6_05_15_05','hourly_A_15_05_10_06','hourly_A_10_06_17_06','hourly_A_17_06_24_06','hourly_A_24_06_01_07','hourly_A_01_07_08_07','hourly_A_08_07_15_07','hourly_A_15_07_25_07','hourly_A_25_07_08_08','hourly_A_08_08_31_13')
periods_b = c('hourly_B_6_05_15_05','hourly_B_6_05_15_05','hourly_B_15_05_10_06','hourly_B_10_06_17_06','hourly_B_17_06_24_06','hourly_B_24_06_01_07','hourly_B_01_07_08_07','hourly_B_08_07_15_07','hourly_B_15_07_25_07','hourly_B_25_07_08_08','hourly_B_08_08_31_13')

############ NEE_f for two towers, hourly#########################

pd <- position_dodge(.1) # move them .05 to the left and right
hourly_data_As =hourly_data_A[hourly_data_A$hour_months > 1 & hourly_data_A$hour_months < 12,]
hourly_data_Bs =hourly_data_B[hourly_data_B$hour_months > 1 & hourly_data_B$hour_months < 12 ,]
DP = ggplot() +
  geom_errorbar(data =AllData_A_14$hourly$NEE_f, aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), linetype=1,size=.1, width=.4, position=pd) +
  geom_line(data =AllData_A_14$hourly$NEE_f, aes(x=hour, y=hour_means),position=pd,size=.5, linetype=2) +
  geom_point(data = AllData_A_14$hourly$NEE_f, aes(x=hour, y=hour_means),position=pd,size=2, shape=21, fill="white")+
  geom_errorbar(data = AllData_B_14$hourly$NEE_f, aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors),linetype=1,size=.1, width=.4, position=pd)  +
  geom_line(data =AllData_B_14$hourly$NEE_f, aes(x=hour, y=hour_means),position=pd,size=.5) +
  geom_point(data = AllData_B_14$hourly$NEE_f, aes(x=hour, y=hour_means),position=pd,size=2, shape=21, fill="black")+
  geom_hline(yintercept = 0, linetype=2)+
  facet_wrap(~hour_months, ncol =3)+
  xlab("Time of day (Hour)")+
  ylab(expression(paste(bold("NEE")," ( ",mu,"mol "," ",CO[2]," ",m^-2," ",s^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold")) #+
ggtitle("NEE_f for two towers, hourly")



############ NEE_f cumulation for two towers total#########################


Gr_NEE_cum = ggplot() +
  geom_line(data = AllData_A_14$daily, aes(x=Doy, y=cumsum((NEE_f_sums * 12*18 /10000))), size=1, linetype =2, position=pd) +
  geom_line(data = AllData_B_14$daily, aes(x=Doy, y=cumsum((NEE_f_sums * 12*18 /10000))),size=1,  position=pd) +
  #xlab("Day of the year ")+
  ylab(expression(paste(bold("Cumulative NEE")," ( g "," ",C[CO[2]]," ",m^-2," "," )",sep="")))+
  #μmol CO2 m-2s-1)")+
  scale_x_continuous(breaks = round(seq(min(AllData_A_14$daily$Doy), max(AllData_A_14$daily$Doy), by = 30),1))+
  geom_hline(yintercept = 0, linetype=2)+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size =16, face="bold")) +
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.ticks.x = element_blank())
#+ggtitle("NEE_f cumulation for two towers total")

Gr_Reco_cum = ggplot() +
  geom_line(data = AllData_A_14$daily, aes(x=Doy, y=cumsum((Reco * 12*18 /10000))), size=1,linetype =2, position=pd) +
  geom_line(data = AllData_B_14$daily, aes(x=Doy, y=cumsum((Reco * 12*18 /10000))),size=1,  position=pd) +
  scale_x_continuous(breaks = round(seq(min(AllData_A_14$daily$Doy), max(AllData_A_14$daily$Doy), by = 30),1))+
  #xlab("Day of the year ")+
  ylab(expression(paste(bold("Cumulative Reco")," ( g "," ",C[CO[2]]," ",m^-2," "," )",sep="")))+
  #μmol CO2 m-2s-1)")+
  geom_hline(yintercept = 0, linetype=2)+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 16, face="bold",hjust=0.2, vjust=1,lineheight = 45)) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
# ggtitle("NEE_f cumulation for two towers total")

Gr_GPP_cum = ggplot() +
  geom_line(data = AllData_A_14$daily, aes(x=Doy, y=cumsum((GPP * 12*18 /10000))), size=1, linetype =2, position=pd) +
  geom_line(data = AllData_B_14$daily, aes(x=Doy, y=cumsum((GPP * 12*18 /10000))),size=1, position=pd) +
  scale_x_continuous(breaks = round(seq(min(AllData_A_14$daily$Doy), max(AllData_A_14$daily$Doy), by = 30),1))+
  xlab("Day of the year ")+
  ylab(expression(paste(bold("Cumulative  GPP")," ( g "," ",C[CO[2]]," ",m^-2," )",sep="")))+
  #μmol CO2 m-2s-1)")+
  geom_hline(yintercept = 0, linetype=2)+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size =16, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_text(size =15, face="bold"))

grid.arrange(Gr_NEE_cum, Gr_Reco_cum, Gr_GPP_cum, ncol=1)
# ggtitle("NEE_f cumulation for two towers total")



############ NEE_f daily sums for all year ################################


Gr_NEE = ggplot() +
  geom_line(data = AllData_A_14$daily, aes(x=Doy, y=ma(NEE_f_sums* 12*18 /10000)), size=.8, position=pd, linetype=2) +
  geom_point(data = AllData_A_14$daily , aes(x=Doy, y=NEE_f_sums* 12*18 /10000),position=pd,size=2, shape=21, fill="white",alpha=.5)+
  geom_line(data = AllData_B_14$daily, aes(x=Doy, y=ma(NEE_f_sums* 12*18 /10000)), size=.8, position=pd,linetype=1) +
  geom_point(data = AllData_B_14$daily , aes(x=Doy, y=NEE_f_sums* 12*18 /10000),position=pd,size=2, shape=17, fill="white",alpha=.5)+
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)+
  xlab("Day of the year")+
  ylab(expression(paste(bold("NEE")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  geom_vline(xintercept = 207, size=1, alpha=.8)+
  #scale_x_continuous(breaks = round(seq(120, max(Daily_A_114$Doy), by = 50),1))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
#ggtitle("NEE_f daily sums for all year ")

###### Reco
Gr_Reco = ggplot() +
  geom_line(data = AllData_A_14$daily, aes(x=Doy, y=ma(Reco * 12*18 /10000)), size=.8, position=pd, linetype=2) +
  geom_point(data = AllData_A_14$daily , aes(x=Doy, y=Reco* 12 * 18/10000),position=pd,size=2, shape=21, fill="white",alpha=.5)+
  geom_line(data = AllData_B_14$daily, aes(x=Doy, y=ma(Reco * 12*18 /10000)), size=.8, position=pd,linetype=1) +
  geom_point(data = AllData_B_14$daily , aes(x=Doy, y=Reco* 12 * 18/10000),position=pd,size=2, shape=17, fill="white",alpha=.5)+
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)+
  xlab("Day of the year")+
  geom_vline(xintercept = 163, size=3, alpha=.2)+
  ylab(expression(paste(bold("Reco")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  scale_x_continuous(breaks = round(seq(min(AllData_A_14$daily$Doy), max(AllData_A_14$daily$Doy), by = 30),1))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
# ggtitle("Reco daily sums for all year ")

###### GPP
Gr_GPP = ggplot() +
  geom_line(data = AllData_A_14$daily, aes(x=Doy, y=ma(GPP * 12*18 /10000)), size=.8, position=pd, linetype=2) +
  geom_point(data = AllData_A_14$daily , aes(x=Doy, y=GPP* 12 * 18/10000),position=pd,size=2, shape=21, fill="white",alpha=.5)+
  geom_line(data = AllData_B_14$daily, aes(x=Doy, y=ma(GPP * 12*18 /10000)), size=.8, position=pd,linetype=1) +
  geom_point(data = AllData_B_14$daily , aes(x=Doy, y=GPP* 12 * 18/10000),position=pd,size=2, shape=17, fill="white",alpha=.5)+
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)+
  xlab("Day of the year")+
  ylab(expression(paste(bold("GPP")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  geom_vline(xintercept = 163, size=3, alpha=.2)+
  #μmol CO2 m-2s-1)")+
  scale_x_continuous(breaks = round(seq(min(AllData_A_14$daily$Doy), max(AllData_A_14$daily$Doy), by = 30),1))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))+
  theme(plot.margin = unit(c(0,1,0,1), "lines"))
#ggtitle("GPP daily sums for all year ")
grid.arrange(Gr_NEE, Gr_Reco, Gr_GPP, ncol=1)
#### For defined time gap ###############
pd <- position_dodge(1)
for (i in 1:length(periods_a))
{
  pdf(paste("plots/period_",i,".pdf",sep=""), paper = "a4r", width=10.75, height=5.73)
  print(
    ggplot() +
      geom_errorbar(data = get(periods_a[i]), aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), width=.5,alpha=.3, linetype=2) +
      geom_line(data =get(periods_a[i]), aes(x=hour, y=hour_means),linetype=2) +
      geom_point(data = get(periods_a[i]), aes(x=hour, y=hour_means),size=3, shape=21, fill="white")+
      geom_errorbar(data = get(periods_b[i]), aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), width=.5,alpha=.3, position=pd, linetype=2) +
      geom_line(data =get(periods_b[i]), aes(x=hour, y=hour_means),position=pd) +
      geom_point(data = get(periods_b[i]), aes(x=hour, y=hour_means),position=pd,size=3, shape=21, fill="black")+
      geom_vline(xintercept = 4, size=3, alpha=.2)+
      geom_hline(yintercept = 0, size=.5, linetype = 2)+
      facet_wrap(~hour_months)+
      xlab("Time of day (Hour)")+
      ylab(expression(paste(bold("NEE")," ( ",mu,"mol "," ",CO[2]," ",m^-2," ",s^-1, " )",sep="")))+
      #μmol CO2 m-2s-1)")+
      theme_few(base_size = 15, base_family = "serif")+
      theme(axis.title.y = element_text(size = 15, face="bold")) +
      theme(axis.title.x = element_text(size =15, face="bold"))+
      ggtitle(periods_a[i])
  )
  dev.off()
}
#### Dependecies from factors  - useless for NEE, we need to deconstruct it###########

ggplot() +
  
  geom_point(data = Daily_A_114 , aes(x=SWC_1*100, y=NEE_f_sums* 12 * 18/10000),position=pd,size=2, shape=1, fill="white")+
  
  geom_point(data = Daily_A_114 , aes(x=SWC_1*100, y=GPP*12 * 18/10000),position=pd,size=2, shape=2, fill="black")+
  geom_point(data = Daily_A_114 , aes(x=SWC_1*100, y=Reco* 12 * 18/10000),position=pd,size=2, shape=3, fill="grey")+
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  coord_cartesian(xlim = c(0, 50))+
  xlab("Soil water content (%)")+
  ylab(expression(paste(bold("NEE, GPP, Reco")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
#ggtitle("Dependecies from factors  - useless for NEE, we need to deconstruct it ")

#### Dependecies from factors  - useless for NEE, we need to deconstruct it###########

ggplot() +
  geom_point(data = AllData_A_14$daily , aes(x=Tsoil_f, y=Reco* 12 * 18/10000),position=pd,size=2, shape=1, fill="red")+
  geom_smooth(data = AllData_A_14$daily, aes(x=Tsoil_f, y=Reco* 12 * 18/10000), method = "lm", formula = y ~ x + I(x^3), size = 1 )+
  geom_point(data = AllData_A_14$daily , aes(x=Tsoil_f, y=GPP* 12 * 18/10000),position=pd,size=2, shape=2, fill="green")+
  geom_point(data = AllData_A_14$daily , aes(x=Tsoil_f, y=Reco* 12 * 18/10000),position=pd,size=2, shape=3, fill="blue")+
  facet_wrap(~moisture_levels, drop=TRUE)+
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  coord_cartesian(xlim = c(0, 25))+
  xlab("Temperature (C)")+
  ylab(expression(paste(bold("NEE")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
#ggtitle("Dependecies from factors  - useless for NEE, we need to deconstruct it ")
Daily_A_114$moisture_levels<-as.factor(Daily_A_114$moisture_levels)
ancova(NEE_f_sums ~ Tsoil_f + moisture_levels, data = Daily_A_114, layout=c(5,1))
#### Dependecies from factors  - useless for NEE, we need to deconstruct it###########

ggplot() +
  
  geom_point(data = AllData_A_14$daily  , aes(x=PAR_Den_Avg, y=GPP*12*18/10000),position=pd,size=2, shape=21, fill="white")+
  
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  
  ylab(expression(paste(bold(" GPP")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  xlab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep=""))) +
  #μmol CO2 m-2s-1)")+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
#ggtitle("Dependecies from factors  - useless for NEE, we need to deconstruct it ")

#########################################################


#### DRAW mean PAR

ggplot() +
  geom_line(data =  AllData_A_14$daily , aes(x=Doy, y=ma(PAR_Den_Avg)),position=pd) +
  geom_point(data = AllData_A_14$daily , aes(x=Doy, y=PAR_Den_Avg),position=pd,size=3, shape=21, fill="white")+
  
  xlab("Day of the year") +
  ylab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep=""))) +
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
#ggtitle("DRAW mean PAR")

Gr_PAR  = ggplot() +
  geom_line(data =  AllData_A_14$daily , aes(x=Doy, y=ma(PAR_Den_Avg)),position=pd) +
  geom_point(data = AllData_A_14$daily , aes(x=Doy, y=PAR_Den_Avg),position=pd,size=2, shape=21, fill="white")+
  coord_cartesian( ylim = c(0,595))+
  xlab("Day of the year") +
  ylab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep=""))) +
  #geom_vline(xintercept = 163, size=1, alpha=1)+
  theme_few(base_size = 18, base_family = "serif")+
  theme(axis.title.y = element_text(size = 16, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
#ggtitle("DRAW mean PAR")

temp  =  AllData_A_14$dt[AllData_A_14$dt[['DateTime']] > as.POSIXct("2013-12-02") & AllData_A_14$dt[['DateTime']] < as.POSIXct("2013-12-03"), ]
ggplot() +
  geom_point(data =temp, aes(x=hour, y=SolElev), position=pd,size=3, shape=21, fill="white") +
  #coord_cartesian(ylim = c(0, 600)) +
  xlab("Day of the year") +
  ylab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep=""))) +
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))



# ggtitle("DRAW mean PAR")

#####Tsoil A and B


Gr_Tsoil = ggplot() +
  geom_line(data = AllData_A_14$daily , aes(x=Doy, y=ma(Tsoil_f)),position=pd, size=.8) +
  geom_point(data = AllData_A_14$daily , aes(x=Doy, y=Tsoil_f),position=pd,size=2, shape=21, fill="white",alpha=.5)+
  
  geom_line(data = AllData_B_14$daily , aes(x=Doy, y=ma(Tsoil_f)),position=pd,size=.8,linetype=2) +
  geom_point(data = AllData_B_14$daily , aes(x=Doy, y=Tsoil_f),position=pd,size=2, shape=21, fill="black",alpha=.5)+
  xlab("Day of the year") +
  ylab(expression(bold(paste(T["soil"]," at 5cm depth "," (", ring("C"),")",sep="")))) +
  #geom_vline(xintercept = 163, size=3, alpha=.2)+
  theme_few(base_size = 18, base_family = "serif")+
  theme(axis.title.y = element_text(size = 16, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
#ggtitle("Tsoil A and B")

#####Volumetric water content VWC A and B

Gr_water = ggplot() +
  geom_line(data =  AllData_A_14$daily, aes(x=Doy, y=ma(SWC_1*100)),position=pd,linetype=2) +
  geom_point(data = AllData_A_14$daily, aes(x=Doy, y=(SWC_1*100)),position=pd,size=2, shape=21, fill="white",alpha=.3)+
  geom_rect(data =  AllData_A_14$daily, aes(x=Doy,xmin=Doy-1,xmax=Doy+1, y=Rain_mm_Tot_sums, ymin=0, xmin=3),  position=pd, size=3,alpha=.8) +
  geom_line(data =  AllData_B_14$daily, aes(x=Doy, y=ma(SWC_1*100)),position=pd) +
  geom_point(data = AllData_B_14$daily, aes(x=Doy, y=(SWC_1*100)),position=pd,size=2, shape=21, fill="black",alpha=.3)+
  coord_cartesian(xlim = c(1, 365, by=30),ylim = c(0, 40))+
  xlab("Day of the year") +
  ylab(expression(bold(paste("SWC at 5cm depth (%)"," ",sep="")))) +
  theme_few(base_size = 18, base_family = "serif")+
  theme(axis.title.y = element_text(size = 16, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,2), "lines"))+
  theme(axis.title.x = element_text(size =15, face="bold"))
#ggtitle("Volumetric water content VWC A and B")

biomet_graph = grid.arrange(Gr_PAR, Gr_Tsoil, Gr_water, ncol=1)

##### Precipitation

ggplot() +
  geom_rect(data =  Daily_A_114, aes(x=Doy,xmin=Doy-1,xmax=Doy+1, y=Rain_mm_Tot_sums, ymin=0, xmin=3),  position=pd, size=5) +
  coord_cartesian(xlim = c(110, 365))+
  xlab("Day of the year") +
  ylab(expression(bold(paste("Precipitation (mm)"," ",sep="")))) +
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))+
  ggtitle("Precipitation")
