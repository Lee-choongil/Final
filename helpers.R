crime_M_S <- function (x,y){
  project_data1 <- x %>%
    select(date:wind_temp,y) %>%
    gather(crime_class, event, -(date:wind_temp))
  month_data <- project_data1 %>%
    mutate(date = month(date)) %>%
    group_by(date,crime_class) %>%
    filter(date >= 4 & date <= 9 ) %>%
    summarise(DCI = mean(dis_index),n= mean(event))
  ggplot(data = month_data, aes(x = DCI, y = n)) +
    ggtitle(y)+
    geom_point(color = "firebrick")+
    geom_text(aes(label = date), vjust = -0.5, size = 4)
}
crime_Y_S <- function(x,y,z){
  project_data1 <-x %>%
    select(date:wind_temp,y) %>%
    gather(crime_class, event, -(date:wind_temp))
  month_data <-project_data1 %>%
    mutate(date = month(date)) %>%
    filter(date == z)
  ggplot(data = month_data,aes(x = dis_index, y = event)) +
    ggtitle(paste(y,"in month ",z))+
    geom_point(color = "firebrick")
}
crime_M_W <- function (x,y){
  project_data1 <- x %>%
    select(date:wind_temp,y) %>%
    gather(crime_class, event, -(date:wind_temp))
  month_data <- project_data1 %>%
    mutate(date = month(date)) %>%
    group_by(date,crime_class) %>%
    filter(date >= 10 | date <= 3 ) %>%
    summarise(WCT = mean(wind_temp),n= mean(event))
  ggplot(data = month_data,aes(x = WCT, y = n)) +
    geom_point(color = "steelblue")+
    geom_text(aes(label = date), vjust = -0.5, size = 4)
}
murderplot = function (x,y,z){
  d1 <- filter(x, enroll == y)
  ggplot(d1, aes(x = temp,  y = murder)) +
    theme_minimal()+
    theme(
      plot.title = element_text(
        size = 20,
        face = "bold", colour = "darkblue"),
      axis.title = element_text(
        face = "bold", colour = "darkblue"),
      axis.text.x = element_text(
        colour = "darkblue"),
      axis.text.y = element_text(
        angle = 90, colour = "darkblue"))+
    ggtitle(y) +
    geom_point(color = z) +
    geom_text(aes(label = country), vjust = -0.5, size = 2.5)
}
murderenrollplot = function (x,y,z){
  d1 <- filter(x, enrollment_high >= y, enrollment_high <=z)
  ggplot(d1, aes(x = temp,  y = murder)) +
    theme_minimal()+
    theme(
      plot.title = element_text(
        size = 20,
        face = "bold", colour = "darkblue"),
      axis.title = element_text(
        face = "bold", colour = "darkblue"),
      axis.text.x = element_text(
        colour = "darkblue"),
      axis.text.y = element_text(
        angle = 90, colour = "darkblue"))+
    ggtitle("Murder Rate") +
    geom_point(color = "skyblue") +
    geom_text(aes(label = country), vjust = -0.5, size = 2.5)
}