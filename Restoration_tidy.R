library(tidyr)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(stringr)



setwd('K:/env_instrument_data')



# *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~* GET DEPLOYMENT INFO *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*

deploy <- read.csv('./sensor_deployments.csv')%>%
  filter(project == 'Methods' | project == 'Restoration')%>%
  separate(launch.date, into = c('dep.mo','d','y'), sep = '/', remove = FALSE)%>%
  mutate(loc = as.factor(loc),
         sensor = as.character(sensor),
         dep.mo = as.factor(paste(y, dep.mo, sep = '.')),
         launch.time = if_else(launch.time == "0:00:00",
                               '0:01:00',
                               as.character(launch.time)),
         deploy.time = if_else(deploy.time == "0:00:00",
                               '0:01:00',
                               as.character(deploy.time)),
         retrieve.time = if_else(retrieve.time == "0:00:00",
                                 '23:59:00',
                                 as.character(retrieve.time)),
         launch = mdy_hms(paste(launch.date, launch.time)),
         dep = mdy_hms(paste(deploy.date, deploy.time)),
         end = mdy_hms(paste(retrieve.date, retrieve.time)),
         start = if_else(is.na(dep),
                         launch,
                         if_else(launch > dep,
                         launch,
                         dep)),
         site = as.character(site),
         type = as.character(type))%>%
  select(type, project, site, subsite, loc, dep.mo,sensor, start, end, comments)



# subset out temp and par data

t.dep <- deploy %>%
  filter(type == 'temp')%>%
  droplevels()

par.dep <- deploy %>%
  filter(type == 'par')%>%
  droplevels()






#* ~*~*~*~*~*~*~*~*~*~*~*~*~*~* TEMPERATURE ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~



# Compile all raw temp data files

tempnames <- list.files(path = "./Restoration/temp/data-raw/", pattern = '.csv', all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = FALSE)

read_tempnames_filename <- function(filename){
  ret <- read.csv(filename, skip = 2, header=F,
                  col.names = c('scan.no','dt','temp')
                  )
  ret$source <- filename #EDIT
  ret
}

rawtemp <- plyr::ldply(tempnames, read_tempnames_filename) #10/10/2022 this errors out saying it has more columns than column names... TIM



# Tidy up the raw temp data with all the pieces we need

t <- rawtemp %>%
  separate(source, into = c('x','project','type','x1','flnm'), sep = '/')%>%
  separate(flnm, into = c('site','subsite','loc','dep.yr','dep.mo','sensor'), sep = '_')%>%
  separate(sensor, into = c('sensor','x2'))%>%
  mutate(dt = mdy_hms(dt),
         dep.mo = as.integer(dep.mo))%>%
  separate(dt, into = c('cal','clock'), sep = ' ', remove = FALSE)%>%
  select(-x, -x1, -x2, -clock)%>%
  mutate(dep.mo = as.factor(paste(dep.yr, dep.mo,sep = '.')),
         subsite = as.factor(subsite),
         loc = as.factor(loc),
         project = as.factor(project),
         #qtr = as.factor(quarter(ymd(cal), with_year = TRUE, fiscal_start = 12))
  )%>%
  separate(dep.mo, into = c('y','m'), sep = '\\.', remove = FALSE)%>%
  mutate(dep = paste(site, y, m, subsite, loc ,sensor, sep = "_"))%>%
  #separate(qtr, into = c('y','q'), sep = '\\.', remove = FALSE)%>%
  #mutate(dep = paste(site, y, q, loc ,sensor, sep = "_"))%>%
  droplevels()



# Append deployment data to raw temp data

t1 <- full_join(t, t.dep, by = c('type','project','site','dep.mo','subsite','loc','sensor'))


# Crop raw-PAR to deployment dates and save output

t.crop <- t1%>%
  filter(!is.na(scan.no))%>%
  filter(dt >= start & dt < end)%>%
  select(site, y, m, subsite, loc, sensor, dt, temp, dep.mo, dep, start, end)

write.csv(t.crop, './Restoration/temp/data-processed/temp_raw_tidy_full.csv')



# ~ CREATE TEMPERATURE SUMMAIRES

t.daily <- t.crop %>%
  separate(dt, into = c('cal','clock'), sep = '\\s')%>%
  mutate(cal = ymd(cal))%>%
  group_by(site,subsite, loc, sensor, cal)%>%
  mutate(daily.mean.temp = mean(temp), 
            daily.sd = sd(temp), 
            daily.se = daily.sd/sqrt(n()), 
            daily.max = max(temp), 
            daily.min = min(temp),
            rolling.daily = zoo::rollmean(daily.mean.temp, k = 7, fill = NA),
            rolling.max = zoo::rollmean(daily.max, k = 7, fill = NA),
            rolling.min = zoo::rollmean(daily.min, k = 7, fill = NA))%>%
  ungroup()%>%
  mutate(plot.dep = paste(site, subsite, dep.mo, sep = '_'),
         ts = paste(loc, sensor, sep = "-"),
         cal = ymd(cal))%>%
  select(-clock, -temp)%>%
  #filter(!is.na(rolling.daily))%>%
  select(site, subsite, loc, dep.mo, y, m,cal, sensor, 
         daily.mean.temp, daily.sd, daily.se, daily.max, daily.min,
         rolling.daily, rolling.max, rolling.min , dep, ts, plot.dep, start, end)%>%
  unique()

write.csv(t.daily, './Restoration/temp/data-processed/Temp_daily_summary.csv')




t.mo <- t.crop %>%
  separate(dt, into = c('cal','clock'), sep = " ")%>%
  separate(cal, into = c('cal.y','cal.m', 'cal.d'), sep = '-')%>%
  select(-cal.d)%>%
  group_by(site, subsite, loc, cal.m)%>%
  mutate(qtr.avg.lt = mean(temp),
         qtr.avg.lt.sd = sd(temp),
         qtr.avg.lt.se = sd(temp)/sqrt(n()))%>%
  ungroup()%>%
  group_by(site, subsite, loc, cal.y, cal.m)%>%
  mutate(qtr.avg = mean (temp), 
         qtr.sd = sd(temp),
         qtr.se = qtr.sd/n())%>%
  ungroup()%>%
  mutate(cal.y = as.factor(cal.y))%>%
  select(-y,-m,-temp, -clock)%>%
  unique()

write.csv(t.mo, './Restoration/temp/data-processed/Temp_qrtly_summary.csv')



t.yr <- t.crop %>%
  group_by(site, subsite, loc)%>%
  mutate(yr.avg.lt = mean(temp),
         yr.avg.lt.sd = sd(temp),
         yr.avg.lt.se = sd(temp)/sqrt(n()))%>%
  ungroup()%>%
  separate(dt, into = c('cal','clock'), sep = " ")%>%
  separate(cal, into = c('cal.y','cal.m', 'cal.d'), sep = '-')%>%
  select(-clock, -cal.d, -cal.m)%>%
  group_by(site,subsite,loc, cal.y)%>%
  mutate(yr.avg = mean (temp), yr.sd = sd(temp), yr.se = yr.sd/n())%>%
  ungroup()%>%
  select(-sensor, -temp, -m, -y, -dep)%>%
  unique()

write.csv(t.yr, './Restoration/temp/data-processed/Temp_yrly_summary.csv')



# Save individual tidy temp data by yr, qtr, loc, sensor

# temp.deps <- unique(t.crop$dep)
# 
# for(i in temp.deps) {
#   
#   df.t <- filter(t.crop, dep == i) %>%
#     select(-dep)
#   
#   write.csv(df.t, paste0("./Restoration/temp/data-processed/raw_temp_tidy/", i,".csv"))
#   
# }






#~*~*~*~*~*~*~*~*~*~*~*~*~*~* ODYSSEY PAR  ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~



# Compile all raw PAR data files

parnames <- list.files(path = "./Restoration/par/data-raw/", pattern = '.csv|.CSV', all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = FALSE)

read_parnames_filename <- function(filename){
  ret <- read.csv(filename, skip = 10, header=F,
                  col.names = c('scan.no','cal','clock','count','count2','a','b','c','source','d','e','f','g','h'))
  ret$source <- filename #EDIT
  ret
}

rawpar <- plyr::ldply(parnames, read_parnames_filename) 



# Tidy up the raw PAR data with all the pieces we need

p <- rawpar %>%
  separate(source, into = c('x','project','type','x1','flnm'), sep = '/')%>%
  separate(flnm, into = c('site','subsite','loc','dep.yr','dep.mo','sensor'), sep = '_')%>%
  separate(sensor, into = c('sensor','x2'))%>%
  select(-scan.no,-x, -x1, -x2, -count2, -a, -b, -c, -e, -f, -g, -h)%>%
  mutate(dep.mo = as.factor(paste(dep.yr, as.integer(dep.mo),sep = '.')),
         subsite = as.factor(subsite),
         loc = as.factor(loc),
         dt = dmy_hms(paste(cal, clock, sep = ' ')),
         medium = 'water',
         #qtr = quarter(dmy(cal), with_year = TRUE, fiscal_start = 12)
  )%>%
  separate(dep.mo, into = c('y','m'), sep = '\\.', remove = FALSE)%>%
  mutate(dep = paste(site, y, m, subsite, loc ,sensor, sep = "_"))%>%
  #separate(qtr, into = c('y','q'), sep = '\\.', remove = FALSE)%>%
  #mutate(dep = paste(site, y, q, loc ,sensor, sep = "_"))%>%
  select(site,subsite,loc, dt, dep.mo, y,m, sensor, count, medium, dep)%>%
  droplevels()



#* Grab PAR calibration coefficients from calibration file

coefs <- read.csv('./Calibration_2019_12.csv')%>%
  select(sensor, medium, estimate) %>%
  mutate(sensor = as.character(sensor))



# Append calibration data to PAR data
# convert raw counts to calibrated counts
# convert calibrated counts to PAR

p1 <- left_join(p, coefs,by = c('sensor','medium'))%>%
  mutate(estimate = if_else(is.na(estimate), # If a sensor has no calibration event, use 1 as coefficient
                            1,
                            estimate),
         calib = count*estimate,
         PAR = count*estimate*900*0.000001)



# Append deployment data to calibrated PAR data

p2 <- full_join(p1, par.dep, by = c('site','subsite','loc','dep.mo','sensor'))%>%
  mutate(start = ymd_hms(start),
         end = ymd_hms(end))



# Crop raw-PAR to deployment dates and save output

p.crop <- p2 %>%
  group_by(site, subsite, loc, dep.mo, sensor)%>%
  filter(dt >= start & dt < end)%>%
  filter(!is.na(count))

write.csv(p.crop, './Restoration/par/data-processed/PAR_raw_tidy_full.csv')



# PAR OUTPUTS

p.daily <- p.crop %>%
  separate(dt, into = c('cal','clock'), sep = ' ')%>%
  group_by(site, subsite, loc, sensor, cal)%>%
  mutate(daily.par = sum(PAR))%>%
  ungroup()%>%
  select(-calib, -PAR, -count, - clock)%>%
  unique()%>%
  group_by(site, subsite, loc, sensor)%>%
  mutate(roll7 = zoo::rollmean(daily.par, k = 7, fill = NA))%>%
  ungroup()%>%
  mutate(plot.dep = paste(site,subsite, dep.mo, sep = '_'),
         ts = paste(loc, sensor, sep = "-"),
         cal = ymd(cal))%>%
  select(type, project, site, subsite, loc, dep.mo, y, m,cal, sensor, medium, daily.par, roll7, dep,ts, plot.dep, start, end)%>%
  unique()

write.csv(p.daily, './Restoration/par/data-processed/PAR_daily_summary.csv')



# # Save individual tidy data by yr, qtr, loc, sensor
# 
# par.deps <- unique(p.crop$dep)
# 
# for(i in par.deps) {
#   
#   df.t <- filter(p.crop, dep == i) %>%
#     select(-dep)
#   
#   write.csv(df.t, paste0("./Restoration/par/data-processed/raw_par_tidy/", i,".csv"))
#   
# }






#~*~*~*~*~*~*~*~*~*~*~*~*~*~* HOBO PRESSURE (DEPTH) & TEMP SENSORS  ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~



# Compile all raw DEPTH data files

depthnames <- list.files(path = "./Restoration/depth/data-raw/", pattern = '.csv|.CSV', all.files = FALSE, full.names = TRUE, recursive = FALSE, ignore.case = FALSE)

read_depthnames_filename <- function(filename){
  ret <- read.csv(filename, skip = 2, header=F,
                  col.names = c('scan.no','dt','kpa','temp'))
  ret$source <- filename #EDIT
  ret
}

rawdepth <- plyr::ldply(depthnames, read_depthnames_filename) 



# Tidy up the raw depth data with all the pieces we need

d <- rawdepth %>%
  separate(source, into = c('x','project','type','x1','flnm'), sep = '/')%>%
  separate(flnm, into = c('site','subsite','loc','dep.yr','dep.mo','sensor'), sep = '_')%>%
  separate(sensor, into = c('sensor','x2'))%>%
  mutate(dt = mdy_hms(dt),
         dep.mo = as.integer(dep.mo))%>%
  separate(dt, into = c('cal','clock'), sep = ' ', remove = FALSE)%>%
  select(-x, -x1, -x2, -clock)%>%
  mutate(dep.mo = as.factor(paste(dep.yr, dep.mo,sep = '.')),
         subsite = as.factor(subsite),
         loc = as.factor(loc),
         project = as.factor(project),
         #qtr = as.factor(quarter(ymd(cal), with_year = TRUE, fiscal_start = 12))
  )%>%
  separate(dep.mo, into = c('y','m'), sep = '\\.', remove = FALSE)%>%
  mutate(dep = paste(site, y, m, subsite, loc ,sensor, sep = "_"))%>%
  #separate(qtr, into = c('y','q'), sep = '\\.', remove = FALSE)%>%
  #mutate(dep = paste(site, y, q, loc ,sensor, sep = "_"))%>%
  droplevels()%>%
  select(project, type, site,subsite, loc, dep.mo, y, m,dt, sensor, kpa, temp, dep)

write.csv(d, './Restoration/depth/data-processed/depth_raw_full.csv')



#~*~*~*~*~*~*~*~*~*~*~*~*~*~* CREATE SOME QC DATA ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~



# Temp missing deployments

missing.deps <- t1 %>%
  filter(is.na(scan.no)) %>%
  select(dep.mo,site, subsite, loc, sensor, start, end, comments)

write.csv(missing.deps, './Restoration/temp/QC/Missing_deps.csv')



# Temp crop dates

t.crop.dates <- t1 %>%
  filter(!is.na(scan.no))%>%
  select(dep.mo, site, subsite, loc, sensor, start, end)%>%
  unique()

write.csv(t.crop.dates, './Restoration/temp/QC/temp_start_end_dates.csv')



# PAR missing deployments

missing.par.deps <- p2 %>%
  filter(is.na(count)) %>%
  select(dep.mo, site, subsite, loc, sensor, start, end)
  
write.csv(missing.par.deps, './Restoration/par/QC/Missing_par_deps.csv')



# PAR crop dates

raw.par.dates <- p2 %>%
  select(dep.mo, site, subsite, loc, sensor, start, end)%>%
  unique()%>%
  filter(!is.na(site))

write.csv(raw.par.dates, './Restoration/par/QC/raw_par_dates_.csv')






#~*~*~*~*~*~*~*~*~*~*~*~*~ MAKE SOME PRETTY QC PLOTS PLEASE ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~~**~*~*~*~*~*~*~



# Make deployment PAR plots for QC 

plot.deps <- unique(p.daily$plot.dep)


for(i in plot.deps) {
  
  df.p <- filter(p.daily, plot.dep == i)
  
  gg.p <- ggplot(df.p, aes(cal, roll7, colour = ts, group = ts))+
    geom_line()+
    geom_point(size = 2)+
    scale_x_date(date_breaks = "12 days", date_labels= "%d %b %Y")+
    labs(title = paste0(df.p$site, '-', df.p$subsite), 
         subtitle = paste0("7 day rolling average PAR: ", date(df.p$start), ' - ', date(df.p$end)),
         fill = "Location & sensor #")+
    ylab(bquote('PAR (mols' ~ m^-2~d^-1*')'))+
    xlab(element_blank())+
    #annotate("text", x = 1, y = 7.75, label = "A", size = 4)+
    #annotate("text", x = 1, y = 3.75, label = "B", size = 4)+
    geom_hline(yintercept = 3, color = "black", linetype = "dashed", size = 0.5)+
    geom_hline(yintercept = 7, color = "black", linetype = "dotted", size = 0.5)+
    ylim(0, 30)+
    theme_bw()+
    theme(axis.text.x = element_text(size=9, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 11),
          legend.title = element_blank())
  
  ggsave(plot = gg.p, path = "./Restoration/par/QC/plots/", filename = paste0(i,'.png'), device = 'png')
}



# Make deployment temp plots for QC 

temp.deps <- unique(t.daily$plot.dep)


for(n in temp.deps) {
  
  df.t <- filter(t.daily, plot.dep == n)
  
  gg.t <- ggplot(df.t, aes(cal, rolling.daily, colour = ts, group = ts))+
    geom_line()+
    geom_point(size = 2)+
    scale_x_date(date_breaks = "12 days", date_labels= "%d %b %Y")+
    #scale_color_manual(name = "Position",breaks=c("top","bot"), 
    #                    labels = c('Top','Bottom'), 
    #                    values = c('#7bbcd5', '#6e7cb9'))+
    labs(title = paste0(df.t$site, '-', df.t$subsite), 
         subtitle = paste0("7 day rolling average temperature: ", date(df.t$start), ' - ', date(df.t$end)),
         fill = "Location & sensor #")+
    ylab(bquote('Temperature ('~degree*C*')'))+
    xlab(element_blank())+
    ylim(0, 20)+
    theme_bw()+
    theme(axis.text.x = element_text(size=9, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 11),
          legend.title = element_blank())
  
  ggsave(plot = gg.t, path = "./Restoration/temp/QC/plots/", filename = paste0(n,'.png'), device = 'png')
}


# TEMPERATURE QC Plots



# Qtrly temp summary plot

# t.q.dev <- t.qrt %>%
#   mutate(dev = qtr.avg - qtr.avg.lt,
#          qtr = yq(qtr))
# 
# 
# q.dev <- ggplot()+
#   geom_point(data = t.q.dev, aes(qtr, dev, color = loc))+
#   geom_line(data = t.q.dev, aes(qtr, dev, color = loc, group = loc))+
#   labs(
#     title = 'Quarterly temperature deviations from long-term (12 yr) quarterly average temperature',
#     y = 'Temperature (C)'
#   )+
#   scale_color_manual(name = 'Transect',
#                      values =c('#edd756','#89689d', '#2c6184', 'black')
#   )+
#   scale_x_date(date_breaks = '6 months', date_labels = '%Y %b')+
#   theme_bw()+
#   theme(
#     panel.background = element_rect(fill = 'white'),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     legend.key = element_rect(fill = 'transparent', color = 'transparent'),
#     legend.background = element_rect(fill = 'transparent'),
#   )+
#   
#   facet_wrap(~loc)
# 
# ggsave(plot = q.dev, path = "./Restoration/temp/QC/plots/", filename = 'Qtrly_temp_deviations.png', device = 'png')
# 
# 
# 
# # Yrly temp summary plot
# 
# t.y.dev <- t.yr %>%
#   mutate(dev = yr.avg - yr.avg.lt) 
# 
# 
# y.dev <-ggplot()+
#   geom_point(data = t.y.dev, aes(yr, dev, color = loc))+
#   geom_line(data = t.y.dev, aes(yr, dev, color = loc, group = loc))+
#   
#   labs(
#     title = 'Annual temperature deviations from long-term (12 yr) average temperature',
#     y = 'Temperature (C)'
#   )+
#   scale_color_manual(name = 'Transect',
#                      values =c('#edd756','#89689d', '#2c6184', 'black')
#   )+
#   theme_bw()+
#   theme(
#     panel.background = element_rect(fill = 'white'),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     axis.title.x = element_blank(),
#     legend.key = element_rect(fill = 'transparent', color = 'transparent'),
#     legend.background = element_rect(fill = 'transparent'),
#   )+
#   
#   facet_wrap(~loc)
# 
# ggsave(plot = q.dev, path = "./Restoration/temp/QC/plots/", filename = 'Yrly_temp_deviations.png', device = 'png')
# 
