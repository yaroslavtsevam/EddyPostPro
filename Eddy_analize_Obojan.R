source(file="Eddy_postproduction.r", local=TRUE)


DataFolderA = 'Data_A'
DataFolderB = 'Data_B'
DataFolderO = 'Data_O'
Site_A = c(410041, 6188869)
Site_B = c(410155, 6188893)
Site_O = c(325574, 5668981)
site_polygon  = data.frame(as.numeric(c(409957,410179,410243,410014)),as.numeric(c(6188984,6189058,6188849,6188774)))
site_polygon_A  = data.frame(as.numeric(c(410109.5,410012.7,409977.3,410086)),as.numeric(c(6188807,6188739
,6188905,6188942)))
site_polygon_B  = data.frame(as.numeric(c(410188,410225,410128,410086)),as.numeric(c(6188974,6188840,6188811,6188942)))
site_polygon_O = data.frame(as.numeric(c(325578,325965,325381,324931)), as.numeric(c(5669372,5669262,5667644,5667793 )))
forest_polygon  = data.frame(as.numeric(c(409472,409943,409532,408959,408587,408471,408353,408124,408048,408083,408088,408041,408038,408237,408524,408547,409090 ,409323,409550 ,409328,409535,409454)),as.numeric(c(6186623,6186720,6187669,6187536,6187517,6187632,6187882,6188235,6188193,6187844,6187575,6187405,6187222 ,6186987,6186310,6186133,6185574,6185407,6185467,6186180,6186267,6186542)))
events_A = 'Data_A/events.csv'
events_B = 'Data_B/events.csv'
events_O = 'Data_O/events.csv'
Site_coord_and_zone = c(55.837631, 37.564302, 4)
Site_coord_and_zone_O = c(51.14567, 36.50624, 4)
All_towers_height=1.5



AllData_O = FullEddyPostProcess (DataFolderO,Site_O,site_polygon_O,events_O,Site_coord_and_zone_O, All_towers_height)
AllData_A = FullEddyPostProcess (DataFolderA,Site_A,site_polygon_A,events_A,Site_coord_and_zone,All_towers_height)
AllData_B = FullEddyPostProcess (DataFolderB,Site_B,site_polygon_B,events_B,Site_coord_and_zone,All_towers_height)

setkey(AllData_A, 'DateTime')
setkey(AllData_B, 'DateTime')
#Adding PAR to site B as soon they very close
AllData_B = merge(AllData_B,AllData_A[,c(1,40),with=FALSE], by = 'DateTime')

#new_names = paste(names(AllData_A),"_a", sep="")
#names(AllData_A) = new_names
#new_names = paste(names(AllData_B),"_b", sep="")
#names(AllData_B) = new_names

#setnames(AllData_A,"DateTime_a","DateTime")
#setnames(AllData_B,"DateTime_b","DateTime")
#setkey(AllData_A,"DateTime")
#setkey(AllData_B,"DateTime")


#moving average for week






PlotWindRoses(AllData_A, 'wind_speed', 'wind_dir')
NA_count = tapply(as.numeric(AllData_A['NEE']),AllData_A$Doy, function(x) x)
AllData_A$SWC_1 = as.numeric(AllData_A$SWC_1)
AllData_O$SWC_1 = as.numeric(AllData_O$SWC_1)
AllData_A$moisture_levels = cut(as.numeric(AllData_A[[1]]$SWC_1), c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
AllData_O$moisture_levels = cut(as.numeric(AllData_O[[1]]$SWC_1), c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
hourly_data_A = hourly_data(AllData_A)
hourly_data_O = hourly_data(AllData_O)
AllData_A_daily = daily_data(AllData_A)
AllData_O_daily = daily_data(AllData_O)
AllData_A_weekly = weekly_data(AllData_A)
AllData_O_weekly = weekly_data(AllData_O)
AllData_A_monthly = month_data(AllData_A)
AllData_O_monthly = month_data(AllData_O)
hourly_A_snow = hourly_NEE_period(AllData_A,"2013-01-01","2013-04-16")
hourly_A_16_04_5_05 = hourly_NEE_period(AllData_A,"2013-04-16","2013-05-05")
hourly_A_6_05_15_05 = hourly_NEE_period(AllData_A,"2013-05-05","2013-05-15")
hourly_A_15_05_10_06 = hourly_NEE_period(AllData_A,"2013-05-15","2013-06-10")
hourly_A_10_06_17_06 = hourly_NEE_period(AllData_A,"2013-06-10","2013-06-17")
hourly_A_17_06_24_06 = hourly_NEE_period(AllData_A,"2013-06-17","2013-06-24")
hourly_A_24_06_01_07 = hourly_NEE_period(AllData_A,"2013-06-24","2013-07-01")
hourly_A_01_07_08_07 = hourly_NEE_period(AllData_A,"2013-07-01","2013-07-08")
hourly_A_08_07_15_07 = hourly_NEE_period(AllData_A,"2013-07-08","2013-07-15")
hourly_A_15_07_25_07 = hourly_NEE_period(AllData_A,"2013-07-15","2013-07-25")
hourly_A_25_07_08_08 = hourly_NEE_period(AllData_A,"2013-07-25","2013-08-08")
hourly_A_08_08_31_13 = hourly_NEE_period(AllData_A,"2013-08-08","2013-12-31")
hourly_O_snow = hourly_NEE_period(AllData_O,"2013-01-01","2013-04-16")
hourly_O_16_04_5_05 = hourly_NEE_period(AllData_O,"2013-04-16","2013-05-05")
hourly_O_6_05_15_05 = hourly_NEE_period(AllData_O,"2013-05-05","2013-05-15")
hourly_O_15_05_10_06 = hourly_NEE_period(AllData_O,"2013-05-15","2013-06-10")
hourly_O_10_06_17_06 = hourly_NEE_period(AllData_O,"2013-06-10","2013-06-17")
hourly_O_17_06_24_06 = hourly_NEE_period(AllData_O,"2013-06-17","2013-06-24")
hourly_O_24_06_01_07 = hourly_NEE_period(AllData_O,"2013-06-24","2013-07-01")
hourly_O_01_07_08_07 = hourly_NEE_period(AllData_O,"2013-07-01","2013-07-08")
hourly_O_08_07_15_07 = hourly_NEE_period(AllData_O,"2013-07-08","2013-07-15")
hourly_O_15_07_25_07 = hourly_NEE_period(AllData_O,"2013-07-15","2013-07-25")
hourly_O_25_07_08_08 = hourly_NEE_period(AllData_O,"2013-07-25","2013-08-08")
hourly_O_08_08_31_13 = hourly_NEE_period(AllData_O,"2013-08-08","2013-12-31")
hourly_snow_cover_A = hourly_data_for_event(AllData_A,'snow_cover')
hourly_snow_cover_O = hourly_data_for_event(AllData_O,'snow_cover')
Daily_A_114 =  AllData_A_daily[AllData_A_daily[['Doy']] >114,]
Daily_O_114 =  AllData_O_daily[AllData_O_daily[['Doy']] >114,]
Daily_O_114_250 =  Daily_O_114[Daily_O_114[['Doy']] < 250,]
Daily_A_114$moisture_levels = cut(Daily_A_114$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, laOels=c("<10%","<20%","<30%","<40"))
Daily_O_114$moisture_levels = cut(Daily_O_114$SWC_1, c(0,.1,.2,.3,.4), right=FALSE, labels=c("<10%","<20%","<30%","<40"))
AllData_A_114 =  AllData_A[AllData_A[['Doy']] >114,]
Daily_A_114_b = Daily_A_114[Daily_A_114[['NA_count']]>47,]
Daily_O_114_b = Daily_O_114[Daily_O_114[['NA_count']]>47,]
AllData_O_114 =  AllData_O[AllData_O[['Doy']] >114,]
Weekly_A_114 =  AllData_A_weekly[AllData_A_weekly[['Doy']] >114,]
Weekly_A_90 =  AllData_A_weekly[AllData_A_weekly[['Doy']] >90,]
Weekly_O_114 =  AllData_O_weekly[AllData_O_weekly[['Doy']] >114,]
periods_a =c('hourly_A_snow','hourly_A_6_05_15_05','hourly_A_15_05_10_06','hourly_A_10_06_17_06','hourly_A_17_06_24_06','hourly_A_24_06_01_07','hourly_A_01_07_08_07','hourly_A_08_07_15_07','hourly_A_15_07_25_07','hourly_A_25_07_08_08','hourly_A_08_08_31_13')
periods_O = c('hourly_O_6_05_15_05','hourly_O_6_05_15_05','hourly_O_15_05_10_06','hourly_O_10_06_17_06','hourly_O_17_06_24_06','hourly_O_24_06_01_07','hourly_O_01_07_08_07','hourly_O_08_07_15_07','hourly_O_15_07_25_07','hourly_O_25_07_08_08','hourly_O_08_08_31_13')

############ NEE_f for two towers, hourly#########################

pd <- position_dodge(.1) # move them .05 to the left and right
hourly_data_As =hourly_data_A[hourly_data_A$hour_months > 4 & hourly_data_A$hour_months < 10,]
hourly_data_Os =hourly_data_O[hourly_data_O$hour_months > 4 & hourly_data_O$hour_months < 10 ,]
hourly = ggplot() +
  #geom_errorbar(data =hourly_data_As, aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), linetype=1,size=.1, width=.4, position=pd) +
  #geom_line(data =hourly_data_As, aes(x=hour, y=hour_means),position=pd,size=.5, linetype=2) +
  #geom_point(data = hourly_data_As, aes(x=hour, y=hour_means),position=pd,size=2, shape=21, fill="white")+
  geom_errorbar(data = hourly_data_Os, aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors),linetype=1,size=.1, width=.4, position=pd) +
  geom_line(data =hourly_data_Os, aes(x=hour, y=hour_means),position=pd,size=.5) +
  geom_point(data = hourly_data_Os, aes(x=hour, y=hour_means),position=pd,size=2, shape=21, fill="black")+
  geom_hline(yintercept = 0, linetype=2)+
  facet_wrap(~hour_months, ncol =3)+
  xlab("Time of day (Hour)")+
  ylab(expression(paste(bold("NEE")," ( ",mu,"mol "," ",CO[2]," ",m^-2," ",s^-1, " )",sep="")))+
#μmol CO2 m-2s-1)")+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))# +
  #ggtitle("NEE_f for two towers, hourly")



############ NEE_f cumulation for two towers total#########################


Gr_NEE_cum = ggplot() +
  geom_line(data = Daily_A_114, aes(x=Doy, y=cumsum((NEE_f_sums * 12*18 /10000))), size=1, position=pd, linetype =2) +
  geom_line(data = Daily_O_114, aes(x=Doy, y=cumsum((NEE_f_sums * 12*18 /10000))),size=1, position=pd) +
  #xlab("Day of the year ")+
  ylab(expression(paste(bold("Cumulative NEE")," ( g "," ",C[CO[2]]," ",m^-2," "," )",sep="")))+
  #μmol CO2 m-2s-1)")+
  geom_hline(yintercept = 0, linetype=2)+
  coord_cartesian(xlim = c(110, 320))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size =10, face="bold")) +
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.ticks.x = element_blank())
  #+ggtitle("NEE_f cumulation for two towers total")

Gr_Reco_cum = ggplot() +
  geom_line(data = Daily_A_114, aes(x=Doy, y=cumsum((Reco * 12*18 /10000))),linetype =2, size=1, position=pd) +
  geom_line(data = Daily_O_114, aes(x=Doy, y=cumsum((Reco * 12*18 /10000))),size=1,  position=pd) +
  scale_x_continuous(breaks = round(seq(120, 360, by = 40),1))+
  #xlab("Day of the year ")+
  ylab(expression(paste(bold("Cumulative Reco")," ( g "," ",C[CO[2]]," ",m^-2," "," )",sep="")))+
  #μmol CO2 m-2s-1)")+
  geom_hline(yintercept = 0, linetype=2)+
  coord_cartesian(xlim = c(110, 320))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 10, face="bold",hjust=0.2, vjust=1,lineheight = 45)) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
 # ggtitle("NEE_f cumulation for two towers total")

Gr_GPP_cum = ggplot() +
  geom_line(data = Daily_A_114, aes(x=Doy, y=cumsum((GPP * 12*18 /10000))),linetype =2, size=1, position=pd) +
  geom_line(data = Daily_O_114, aes(x=Doy, y=cumsum((GPP * 12*18 /10000))),size=1,  position=pd) +
  scale_x_continuous(breaks = round(seq(120, 320, by = 40),1))+
  xlab("Day of the year ")+
  ylab(expression(paste(bold("Cumulative  GPP")," ( g "," ",C[CO[2]]," ",m^-2," )",sep="")))+
  #μmol CO2 m-2s-1)")+
  coord_cartesian(xlim = c(110, 320))+
  geom_hline(yintercept = 0, linetype=2)+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size =10, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_text(size =15, face="bold"))

grid.arrange(Gr_NEE_cum, Gr_Reco_cum, Gr_GPP_cum, ncol=1)
# ggtitle("NEE_f cumulation for two towers total")



############ NEE_f daily sums for all year ################################


Gr_NEE = ggplot() +
  geom_line(data = Daily_A_114, aes(x=Doy, y=ma(NEE_f_sums* 12*18 /10000)), size=.8, position=pd, linetype=2) +
  geom_point(data = Daily_A_114 , aes(x=Doy, y=NEE_f_sums* 12*18 /10000),position=pd,size=2, shape=21, fill="white",alpha=.5)+
  geom_line(data = Daily_O_114, aes(x=Doy, y=ma(NEE_f_sums* 12*18 /10000)), size=.8, position=pd,linetype=1) +
  geom_point(data = Daily_O_114 , aes(x=Doy, y=NEE_f_sums* 12*18 /10000),position=pd,size=2, shape=17, fill="white",alpha=.5)+
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  #geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)+
  xlab("Day of the year")+
  ylab(expression(paste(bold("NEE")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  scale_x_continuous(breaks = round(seq(120, max(Daily_O_114$Doy), by = 50),1))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
  #ggtitle("NEE_f daily sums for all year ")

###### Reco
Gr_Reco = ggplot() +
  geom_line(data = Daily_A_114, aes(x=Doy, y=ma(Reco * 12*18 /10000)), size=.8, position=pd, linetype=2) +
  geom_point(data = Daily_A_114 , aes(x=Doy, y=Reco* 12 * 18/10000),position=pd,size=2, shape=21, fill="white",alpha=.5)+
  geom_line(data = Daily_O_114, aes(x=Doy, y=ma(Reco * 12*18 /10000)), size=.8, position=pd,linetype=1) +
  geom_point(data = Daily_O_114 , aes(x=Doy, y=Reco* 12 * 18/10000),position=pd,size=2, shape=17, fill="white",alpha=.5)+
  #geom_hline(yintercept = 0, size=.5, linetype = 2)+
  #geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)+
  xlab("Day of the year")+
  ylab(expression(paste(bold("Reco")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  scale_x_continuous(breaks = round(seq(120, max(Daily_O_114$Doy), by = 50),1))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
 # ggtitle("Reco daily sums for all year ")

###### GPP
Gr_GPP = ggplot() +
  geom_line(data = Daily_A_114, aes(x=Doy, y=ma(GPP * 12*18 /10000)), size=.8, position=pd, linetype=2) +
  geom_point(data = Daily_A_114 , aes(x=Doy, y=GPP* 12 * 18/10000),position=pd,size=2, shape=21, fill="white",alpha=.5)+
  geom_line(data = Daily_O_114, aes(x=Doy, y=ma(GPP * 12*18 /10000)), size=.8, position=pd,linetype=1) +
  geom_point(data = Daily_O_114 , aes(x=Doy, y=GPP* 12 * 18/10000),position=pd,size=2, shape=17, fill="white",alpha=.5)+
  #geom_hline(yintercept = 0, size=.5, linetype = 2)+
  #geom_vline(xintercept = 250, size=.5, linetype = 1, alpha=.5, size=2)+
  xlab("Day of the year")+
  ylab(expression(paste(bold("GPP")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  scale_x_continuous(breaks = round(seq(120, max(Daily_A_114$Doy), by = 50),1))+
  theme_few(base_size = 15, base_family = "serif")+
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
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
    geom_errorbar(data = get(periods_o[i]), aes(x=hour, y=hour_means, ymin=hour_means-hour_errors, ymax=hour_means+hour_errors), width=.5,alpha=.3, position=pd, linetype=2) +
    geom_line(data =get(periods_o[i]), aes(x=hour, y=hour_means),position=pd) +
    geom_point(data = get(periods_o[i]), aes(x=hour, y=hour_means),position=pd,size=3, shape=21, fill="black")+
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
  geom_point(data = Daily_O_114 , aes(x=Tsoil_f, y=Reco* 12 * 18/10000),position=pd,size=2, shape=1, fill="red")+
  geom_smooth(data = Daily_O_114, aes(x=Tsoil_f, y=Reco* 12 * 18/10000), method = "lm", formula = y ~ x + I(x^3), size = 1 )+
  #geom_point(data = Daily_A_114 , aes(x=Tsoil_f, y=GPP* 12 * 18/10000),position=pd,size=2, shape=2, fill="green")+
  #geom_point(data = Daily_A_114 , aes(x=Tsoil_f, y=Reco* 12 * 18/10000),position=pd,size=2, shape=3, fill="blue")+
  facet_wrap(~moisture_levels, drop=TRUE,)+
  geom_hline(yintercept = 0, size=.5, linetype = 2)+
  coord_cartesian(xlim = c(0, 25))+
  xlab("Temperature (C)")+
  ylab(expression(paste(bold("Reco")," ( ","g "," ",C[CO[2]]," ",m^-2," ",d^-1, " )",sep="")))+
  #μmol CO2 m-2s-1)")+
  theme_few(base_size = 15, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
  #ggtitle("Dependecies from factors  - useless for NEE, we need to deconstruct it ")
Daily_A_114$moisture_levels<-as.factor(Daily_A_114$moisture_levels)
ancova(NEE_f_sums ~ Tsoil_f + moisture_levels, data = Daily_A_114, layout=c(5,1))
#### Dependecies from factors  - useless for NEE, we need to deconstruct it###########

Yt = ggplot() +

  geom_point(data = Daily_A_114 , aes(x=PAR_Den_Avg, y=GPP*12*18/10000),position=pd,size=2, shape=21, fill="white")+

  #geom_hline(yintercept = 0, size=.5, linetype = 2)+

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
  geom_line(data =  AllData_O_daily, aes(x=Doy, y=ma(PAR_Den_Avg)),position=pd) +
  geom_point(data = AllData_O_daily, aes(x=Doy, y=PAR_Den_Avg),position=pd,size=3, shape=21, fill="white")+

    xlab("Day of the year") +
    ylab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep=""))) +
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))
  #ggtitle("DRAW mean PAR")

Gr_PAR  = ggplot() +
  geom_line(data =  Daily_A_114, aes(x=Doy, y=ma(PAR_Den_Avg)),position=pd,linetype=2) +
  geom_point(data = Daily_A_114, aes(x=Doy, y=PAR_Den_Avg),position=pd,size=2, shape=21, fill="white", alpha=.5)+
  geom_line(data =  Daily_O_114, aes(x=Doy, y=ma(PAR_Den_Avg)),position=pd) +
  geom_point(data = Daily_O_114, aes(x=Doy, y=PAR_Den_Avg),position=pd,size=2, shape=21, fill="black", alpha=.5)+
  coord_cartesian( ylim = c(0,750))+
  xlab("Day of the year") +
  ylab(expression(paste(bold("PAR "),"( ", mu,"mol", " ",m^-2," ",s^-1," )",sep=""))) +
  theme_few(base_size = 18, base_family = "serif")+
  theme(axis.title.y = element_text(size = 13, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
  #ggtitle("DRAW mean PAR")

temp  =  AllData_A[AllData_A[['DateTime']] > as.POSIXct("2013-12-02") & AllData_A[['DateTime']] < as.POSIXct("2013-12-03"), ]
Zt = ggplot() +
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
  geom_line(data = Daily_A_114, aes(x=Doy, y=ma(Tsoil_f)),position=pd, size=.8) +
  geom_point(data = Daily_A_114, aes(x=Doy, y=Tsoil_f),position=pd,size=2, shape=21, fill="white",alpha=.5)+

  geom_line(data = Daily_O_114_250, aes(x=Doy, y=ma(Tsoil_f)),position=pd,size=.8,linetype=2) +
  geom_point(data = Daily_O_114_250, aes(x=Doy, y=Tsoil_f),position=pd,size=2, shape=21, fill="black",alpha=.5)+
  xlab("Day of the year") +
  ylab(expression(bold(paste(T["soil"]," at 5cm depth "," (", ring("C"),")",sep="")))) +

  theme_few(base_size = 18, base_family = "serif")+
  theme(axis.title.y = element_text(size = 13, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,1), "lines"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())
  #ggtitle("Tsoil A and B")

#####Volumetric water content VWC A and B

Gr_water = ggplot() +
  geom_line(data =  Daily_A_114, aes(x=Doy, y=ma(SWC_1*100)),position=pd,linetype=2) +
  geom_point(data = Daily_A_114, aes(x=Doy, y=(SWC_1*100)),position=pd,size=2, shape=21, fill="white",alpha=.3)+
  geom_rect(data =  Daily_A_114, aes(x=Doy,xmin=Doy-1,xmax=Doy+1, y=Rain_mm_Tot_sums, ymin=0, xmin=3),  position=pd, size=1,alpha=.4) +
  geom_rect(data =  Daily_O_114, aes(x=Doy,xmin=Doy-1,xmax=Doy+1, y=P_Tot_sums, ymin=0, xmin=3),  position=pd, size=1,alpha=1) +
  geom_line(data =  Daily_O_114, aes(x=Doy, y=ma(SWC_1*100)),position=pd) +
  geom_point(data = Daily_O_114, aes(x=Doy, y=(SWC_1*100)),position=pd,size=2, shape=21, fill="black",alpha=.3)+
  coord_cartesian(xlim = c(110, 365),ylim = c(0, 45))+
  scale_x_continuous(breaks = round(seq(120, 360, by = 30),1))+
  xlab("Day of the year") +
  ylab(expression(bold(paste("SWC at 5cm depth (%)"," ",sep="")))) +
  #theme_few(base_size = 18, base_family = "serif")+
  theme(axis.title.y = element_text(size = 13, face="bold")) +
  theme(plot.margin = unit(c(0,1,0,2), "lines"))+
  theme(axis.title.x = element_text(size =15, face="bold"))
  #ggtitle("Volumetric water content VWC A and B")

grid.arrange(Gr_PAR, Gr_Tsoil, Gr_water, ncol=1)

##### Precipitation

Gt = ggplot() +
  geom_rect(data =  Daily_A_114, aes(x=Doy,xmin=Doy-1,xmax=Doy+1, y=Rain_mm_Tot_sums, ymin=0, xmin=3),  position=pd, size=5) +
  coord_cartesian(xlim = c(110, 365))+
  xlab("Day of the year") +
  ylab(expression(bold(paste("Precipitation (mm)"," ",sep="")))) +
  theme_few(base_size = 20, base_family = "serif")+
  theme(axis.title.y = element_text(size = 15, face="bold")) +
  theme(axis.title.x = element_text(size =15, face="bold"))+
  ggtitle("Precipitation")









----------------------------------


