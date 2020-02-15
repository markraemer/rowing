library(gganimate)
library(hms)
library(tidyverse)
library(gridExtra)
library(reshape)
library(geosphere)
library(plotly)
library(lubridate)
library(ggplotify)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

format_data <- function(dat) {
  dat = dat %>% dplyr::filter(ï..Type=='Data',Message=='record') %>% dplyr::select("timestamp"=Value.1, "lat"=Value.2, "lon"=Value.3, "distance"=Value.4,"speed"=Value.5,"stroke_rate"=Value.9)
  dat$distance = as.numeric(dat$distance)
  dat$speed = as.numeric(dat$speed)
  dat$timestamp = as.numeric(dat$timestamp)
  
  dat$lon = as.numeric(dat$lon) * 180 / (2^31)
  dat$lat = as.numeric(dat$lat) * 180 / (2^31)
  str(dat)
  
  dat$pace = (500/dat$speed)
  
  dat$distance_delta = c(0,dat$distance[2:length(dat$distance)]-dat$distance[1:(length(dat$distance)-1)])
  dat$pace2 = 500/dat$distance_delta
  
  t_bearing = c()
  # bearing
  for (i in seq(0, length(dat$lon)-2)) {
    bearing(c(dat$lon[i+2],dat$lat[i+2]),c(dat$lon[i+1],dat$lat[i+1]))
  }
  
  dat = dat[dat$speed>0,]
  
  dat$timestamp = seconds_to_period(c(0,dat$timestamp[2:length(dat$timestamp)]-dat$timestamp[1]))
  
  
  
  dat
}


format_data_c2 <- function(dat) {
  dat = dat %>% dplyr::filter(ï..Type=='Data',Message=='record') %>% dplyr::select("timestamp"=Value.1, "hr"=Value.2, "stroke_rate"=Value.3, "speed"=Value.5, "power"=Value.6)
  dat$speed = as.numeric(dat$speed)
  dat$timestamp = as.numeric(dat$timestamp)
  dat$pace = (500/dat$speed)
  dat = dat[dat$speed>0,]
  dat = dplyr::select(dat,timestamp, hr, stroke_rate, pace, power)
  dat$timestamp = seconds_to_period(c(0,dat$timestamp[2:length(dat$timestamp)]-dat$timestamp[1]))
  
  dat
}

plot_session <- function(file_csv,title) {
  dat = read.csv(file_csv, header = TRUE, stringsAsFactors = FALSE)
  dat = format_data(dat)
  dat$timestamp = seq(1,length(dat$timestamp))
  
  # duration formatting
  R <- as.hms(dat$timestamp)
  R <- paste('1970-01-01', R)
  R <- strptime(R, format="%Y-%m-%d %H:%M:%S")
  
  # pace formatting
  Rp <- as.hms(dat$pace)
  Rp <- paste('1970-01-01', Rp)
  Rp <- strptime(Rp, format="%Y-%m-%d %H:%M:%S")
  
  mode_strokerate = getmode(dat$stroke_rate[dat$stroke_rate>0])
  max_stroke_rate_ts = max(dat$timestamp[dat$stroke_rate==max(dat$stroke_rate)]) # timestamp with max stroke rate
  avg_pace = mean(dat$pace[dat$stroke_rate>=mode_strokerate-2&dat$stroke_rate<=mode_strokerate+2&dat$timestamp>max_stroke_rate_ts])
  avg_pace_y = (as.numeric(strptime(paste('1970-01-01', as.hms(avg_pace)), format="%Y-%m-%d %H:%M:%S"))*1000)
  min_pace =  paste(trunc(min(dat$pace[dat$stroke_rate>mode_strokerate])/60),":",trunc(min(dat$pace[dat$stroke_rate>mode_strokerate])%%60),sep = "")
    
  
  avg_text = paste("<br><b>race rate (mode)</b>: ",mode_strokerate, " s/m <b>race pace (mode-rate +/- 2)</b>: ", trunc(avg_pace/60),":",trunc(avg_pace%%60)," min/500m ","<b>distance</b>: ",max(dat$distance)-min(dat$distance),"m <b>duration (race)</b>: ",as.hms(length(which(dat$stroke_rate>0))), "<br><b>min pace</b>: ", min_pace, " <b>max stroke rate</b>:", max(dat$stroke_rate),sep = "")
  mode(dat$stroke_rate)
  
  line <- list(
    type = "line",
    line = list(color = "pink"),
    xref = "x",
    yref = "y"
  )
  
  lines <- list()
  line[["x0"]] <- min((as.numeric(R) * 1000))
  line[["x1"]] <- max((as.numeric(R) * 1000))
  line[["y0"]] <- avg_pace_y
  line[["y1"]] <- avg_pace_y
  lines <- c(lines, list(line))
  
  
  p2 <- plot_ly(dat, x = (as.numeric(R) * 1000)) %>%
    add_trace(y = (as.numeric(Rp) * 1000), name = 'pace',mode = 'lines', yaxis='y',text = paste(as.hms(dat$pace)),
              hoverinfo = 'text') %>%
    add_trace(y = dat$stroke_rate, name = paste('stroke rate',avg_text),mode = 'lines',yaxis='y2') %>%
    layout(title = title, 
           yaxis = list(type="date", tickformat="%M:%S",range=c(-3450000, -3540000)),
           yaxis2= list(
             overlaying = "y",
             side='right',
             showgrid=TRUE
           ),
           xaxis=list(type="date", tickformat="%M:%S"),
           shapes=lines,
           legend = list(orientation = 'h')
    )
  p2
}


# plotly

Sys.setenv("plotly_username"="markra")
Sys.setenv("plotly_api_key"="8nX2CqyEqgOY4mLjaXzJ")

### 8s 
# Wednesday
plot_session("data/VIII19-Wed.csv",'Summer VIIIs 2019 - Linacre M1 - Wed (bump New)')

# Thursday
plot_session("data/VIII19-thu.csv",'Summer VIIIs 2019 - Linacre M1 - Thu (row over)')

# Friday
plot_session("data/VIII19-fri.csv",'Summer VIIIs 2019 - Linacre M1 - Fri (bump Univ)')

# Saturday
plot_session("data/VIII19-sat.csv",'Summer VIIIs 2019 - Linacre M1 - Sat (claxon)')


# city bumps
## graphs only
plot_session("data/cb01.csv",'City Bumps #1 - bump')

plot_session("data/cb02.csv",'City Bumps #2 - bumped x1')

plot_session("data/cb03.csv",'City Bumps #3 - bumped x3')

plot_session("data/cb04.csv",'City Bumps #4 - row over')

### torpids
p1 <- plot_session("data/t19-wed.csv",'Torpids 2019 - Linacre M1 - Wed (row over)')

p2 <- plot_session("data/t19-thu.csv",'Torpids 2019 - Linacre M1 - Thu (row over)')

p3 <- plot_session("data/t19-fri.csv",'Torpids 2019 - Linacre M1 - Fri (row over)')

p4 <- plot_session("data/t19-sat.csv",'Torpids 2019 - Linacre M1 - Sat (bump Worcester)')
as.ggplot(p1)
subplot(p1,p2,nrows = 2)


# 2k tests
## 2k-02052019
dat = read.csv("data/2k-02052019.csv", header = TRUE, stringsAsFactors = FALSE)
dat=format_data_c2(dat)


ticks_rgn = pretty(dat$pace,n=((max(dat$pace)- min(dat$pace))/5))
lbls_rgn = as.hms(ticks_rgn)

ticks_rgnx = pretty(as.numeric(seconds(dat$timestamp)),n=(max(as.numeric(seconds(dat$timestamp)))/30))
lbls_rgnx = as.hms(ticks_rgnx)

ggplot(data=dat,aes(x=seconds(timestamp),y=pace,colour="pace")) +
  geom_line()  +
  geom_line(data=data.frame(x=seconds(dat$timestamp),y=dat$stroke_rate),aes(x,140-y,colour="rate"), group=2) +
  geom_line(data=data.frame(x=seconds(dat$timestamp),y=dat$hr),aes(x,120-y/6,colour="hr"), group=2) +
  theme(legend.position="right",legend.direction = "vertical",axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_reverse(limits = c(120,90), breaks=ticks_rgn, labels=lbls_rgn, sec.axis = sec_axis(trans = ~., breaks = seq(from = 120, to=90,by = -1), labels = as.character(seq(from = 20, to=50,by = 1)), name = "stroke rate")) +
  scale_x_continuous(name="time", limits=c(0, 420), labels = lbls_rgnx, breaks = ticks_rgnx) +
  scale_colour_manual(values = c("blue", "orange","red")) +
  geom_smooth(span = 0.3,method = lm,formula = y ~ splines::bs(x, 3), se = FALSE)

x = melt(dat,"timestamp")

ggplot(data=x,aes(x=seconds(timestamp),y=value,colour=variable)) +
  geom_line() +
  scale_x_continuous(name="time", limits=c(0, 420), labels = lbls_rgnx, breaks = ticks_rgnx) +
  theme(legend.position="right",legend.direction = "vertical",axis.text.x = element_text(angle = 45, hjust = 1))  +
  facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  geom_vline(aes(0,0),slope=1,xintercept=max(seconds(dat$timestamp)))


dat = read.csv("data/t19-wed.csv", header = TRUE, stringsAsFactors = FALSE)
dat = format_data(dat)
dat = dat[dat$pace<500&dat$stroke_rate>0,]
dat$timestamp = seq(1,length(dat$timestamp))
ticks_rgn = pretty(dat$pace,n=((max(dat$pace)- min(dat$pace))/5))
lbls_rgn = as.hms(ticks_rgn)
wed <- ggplot(data=dat,aes(x=seconds(timestamp),y=pace,colour="pace")) +
  geom_line()  +
  theme(legend.position="right",legend.direction = "vertical",axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_reverse(limits = c(140,80), breaks=ticks_rgn, labels=lbls_rgn, sec.axis = sec_axis(trans = ~., breaks = seq(from = 140, to=80,by = -4), labels = as.character(seq(from = 24, to=54,by = 2)), name = "stroke rate")) +
  geom_line(data=data.frame(x=seconds(dat$timestamp),y=dat$stroke_rate),aes(x,188-y*2,colour="rate"), group=2) +
  scale_colour_manual(values = c("blue", "red")) +
  ggtitle(paste("Wednesday - row over - ",seconds_to_period(max(dat$timestamp))))

library(plotKML)
library(ggplot2)
library(ggmap)

register_google(key="AIzaSyCqWL9oOQkrup31F87-xc-MoPR75k4Ublk")

ggplot(dat, aes(x = lon, y = lat)) +
  coord_quickmap() +
  geom_point()

mapImageData <- get_googlemap(center = c(lon = mean(dat$lon), lat = mean(dat$lat)),
                              zoom = 15,
                              #color = '',
                              scale = 2,
                              maptype = "terrain")
map <- ggmap(mapImageData, extent = "device") + # removes axes, etc.
#  geom_text(data=dat, aes(x=-1.253778,y=51.73299,label=paste(as.hms(dat$pace,),"min/500m")), size = 8, hjust = 0, color = "black") +
  geom_point(aes(x = lon,
                 y = lat),
             data = dat,
             colour = "red3",
             alpha = .5,
             size = 2) +
  transition_time(seconds(timestamp))
  
animate(map, nframes = length(dat$timestamp),fps=1, renderer = av_renderer(),width = 600, height = 400, res = 104)



