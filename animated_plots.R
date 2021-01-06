library(R.utils)
library(dplyr)
library(stringr)
library(FITfileR)
library(plotKML)
library(plyr)
library(lubridate)
library(gganimate)

# set working directory to folder of strava download

files_gpx = list.files("activities", pattern = "gpx.gz$", full = TRUE)
files_fit = list.files("activities", pattern = "fit.gz$", full = TRUE)

files = c(files_gpx, files_fit)
sapply(files, gunzip, overwrite = TRUE)

files_gpx = list.files("activities", pattern = "gpx$", full = TRUE)
files_fit = list.files("activities", pattern = "fit$", full = TRUE)

ffile = readFitFile(files_fit[1])
ffile_allrecords <- records(ffile) %>% bind_rows() %>% arrange(timestamp)
ffile_allrecords = ffile_allrecords[,c('timestamp', 'position_lat', 'position_long')]
activity = str_split(files_fit[1], "activities/", 2, simplify = T)[2]
activity = str_split(activity, ".fit", 2, simplify = T)[1]
ffile_allrecords$activity = activity
ffile_allrecords$index = 1
fdf = ffile_allrecords

for (i in 2:length(files_fit)){
	ffile = readFitFile(files_fit[i])
	print(i)
	if ("sport" %in% listMessageTypes(ffile)){
		print(getMessagesByType(ffile, message_type = "sport")$sport)
		ffile_allrecords <- records(ffile) %>% bind_rows() %>% arrange(timestamp)
		if ('position_lat' %in% colnames(ffile_allrecords)){
			ffile_allrecords = ffile_allrecords[,c('timestamp', 'position_lat', 'position_long')]
			activity = str_split(files_fit[i], "activities/", 2, simplify = T)[2]
			activity = str_split(activity, ".fit", 2, simplify = T)[1]
			ffile_allrecords$activity = activity
			ffile_allrecords$index = i
			fdf <- rbind(fdf, ffile_allrecords)
		}
	}
}



gfile = readGPX(files_gpx[1])
gfile_records = gfile$tracks[[1]][[1]]
activity = str_split(files_gpx[1], "activities/", 2, simplify = T)[2]
activity = str_split(activity, ".gpx", 2, simplify = T)[1]
gfile_records$activity = activity
gfile_records$index = 1
gdf = gfile_records

for (i in 2:length(files_gpx)){
	gfile = readGPX(files_gpx[i])
	gfile_records = gfile$tracks[[1]][[1]]
	activity = str_split(files_gpx[i], "activities/", 2, simplify = T)[2]
	activity = str_split(activity, ".gpx", 2, simplify = T)[1]
	gfile_records$activity = activity
	gfile_records$index = i
	gdf = rbind(gdf, gfile_records)
}


fdf2 = data.frame(fdf)
colnames(fdf2) = c('time', 'lat', 'lon', 'activity', 'index')
fdf2$type = 'fit'

gdf = gdf[,-3]
gdf2 = data.frame(time = gdf$time, lat = gdf$lat, lon = gdf$lon, activity = gdf$activity, index = gdf$index)
gdf2$type = 'gpx'

dfAll = rbind(fdf2, gdf2)
dfAll$activity = as.numeric(dfAll$activity)
dfAll$year = year(dfAll$time)
dfAll$year = year(dfAll$time)



# merging with activity csv

activities = read.csv('activities.csv') 
activities$year = substr(activities$Activity.Date, 8, 11)
activities$year2 = substr(activities$Activity.Date, 9, 12)
activities$year[activities$year2 == '2020'] = '2020'
activitiesSm = activities[,c('Activity.ID', 'Activity.Type', 'year', 'Filename')]
activitiesSm = activitiesSm[activitiesSm$year == '2020',]
activitiesSm$activity = substr(activitiesSm$Filename,12,21)

dfAll2020_type = dfAll[dfAll$year == 2020,]
dfAll2020_type = dfAll2020_type[dfAll2020_type$lat > 38 & dfAll2020_type$lat < 41,]
dfAll2020_type = dfAll2020_type[dfAll2020_type$lon > -76 & dfAll2020_type$lon < -72,]
dfAll2020_type = merge(dfAll2020_type, activitiesSm, by = 'activity', all.x = T)


# separate running and biking

dfAll2020_run = dfAll2020_type[dfAll2020_type$Activity.Type == 'Run' & !is.na(dfAll2020_type$Activity.Type),]
dfAll2020_bike = dfAll2020_type[dfAll2020_type$Activity.Type == 'Ride' & !is.na(dfAll2020_type$Activity.Type),]


# plotting running

dfAll2020_run$time2 = round(dfAll2020_run$time, 'days')
dfAll2020_run$time2 = as.numeric(factor(dfAll2020_run$time2))

dfAll2020_run = dfAll2020_run[!is.na(dfAll2020_run$time2),]

p_run = ggplot(dfAll2020_run, aes(x = lon, y = lat)) +
  coord_quickmap() +
  geom_point(alpha = 1, size = 0.04, color = '#E17C05') + theme_void()

p_run = p_run + transition_time(time2) +
   shadow_mark(alpha = 0.03, size = 0.03, color = 'black')

animate(p_run, nframes = 296, fps = 8, end_pause = 40, renderer = gifski_renderer(), height = 1200, width =1200)
anim_save("stravAnim2020_run_color.gif")


# plotting biking 

dfAll2020_bike$time2 = round(dfAll2020_bike$time, 'days')
dfAll2020_bike$time2 = as.numeric(factor(dfAll2020_bike$time2))

dfAll2020_bike = dfAll2020_bike[!is.na(dfAll2020_bike$time2),]

p_bike = ggplot(dfAll2020_bike, aes(x = lon, y = lat)) +
  coord_quickmap() +
  geom_point(alpha = 1, size = 0.04, color = '#73AF48') + theme_void()

p_bike = p_bike + transition_time(time2) +
   shadow_mark(alpha = 0.03, size = 0.03, color = 'black')

animate(p_bike, nframes = 296, fps = 8, end_pause = 40, renderer = gifski_renderer(), height = 1200, width =1200)
anim_save("stravAnim2020_bike_color.gif")
