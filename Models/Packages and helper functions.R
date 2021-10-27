rash_sancho <- euro_knockout_1 %>% filter(match_id==1569613,player_name %in% c("Marcus Rashford", "Jadon Sancho"))


distance_function <- function(x,y,x_end,y_end){
  
  # Transform coordinates to meters (assuming pitch is 105x68m)
  start_x <- round((x*105/100),1)
  end_x <- round((x_end*105/100),1)
  start_y <- round((y*68/100),1)
  end_y <- round((y_end*68/100),1)
  
  distance <- sqrt((end_x-start_x)^2 + (end_y-start_y)^2)
  
}

distance <- function(x_pos, y_pos){
  x_meters <- 95.4
  y_meters <- 76.25
  
  x_shift <- (100 - x_pos)*x_meters/100
  y_shift <- abs(50 - y_pos)*y_meters/100
  
  distance <- sqrt(x_shift*x_shift + y_shift*y_shift)
}


goal_angle <- function(x_pos, y_pos){
  x_meters <- 95.4
  y_meters <- 76.25
  
  x_shift <- (100 - x_pos)*x_meters/100
  y_shift <- (50 - y_pos)*y_meters/100
  
  angle <- atan((7.32*x_shift)/(x_shift*x_shift + y_shift*y_shift - (7.32/2)*(7.32/2)))
  angle <- ifelse(angle < 0, angle + pi, angle)
  
  angle_degrees <- angle*180/pi
}

pass_angle_function <- function(x, y,){
  x_meters <- 95.4
  y_meters <- 76.25
  
  x_shift <- (100 - x_pos)*x_meters/100
  y_shift <- (50 - y_pos)*y_meters/100
  
  angle <- atan((7.32*x_shift)/(x_shift*x_shift + y_shift*y_shift - (7.32/2)*(7.32/2)))
  angle <- ifelse(angle < 0, angle + pi, angle)
  
  angle_degrees <- angle*180/pi
}

pass_angle=atan2(end_y*0.68-y*0.68,end_x*1.05-x*1.05)*(180/pi)

# Apply the function to your dataframe
pass_data_1 <- pass_data %>%
  rowwise() %>%
  mutate(distance = distance_function(x, y,endX,endY))

StatsBombData <- StatsBombFreeEvents()
test_statsbomb <- StatsBombData %>%
  slice(1:10)

sb_data <-fromJSON("https://raw.githubusercontent.com/statsbomb/open-data/master/data/events/15946.json")

sqrt((76.1-88.5)^2 + (93-97.1)^2)

annotate(
  geom = "rect",
  xmin = 0 + 100- 17,
  xmax = 0 + 100,
  ymin = (0 + 100/2) - 57.8/2,
  ymax = (0 + 100/2) + 57.8/2,
  colour = "floralwhite",
  fill = NA)+
  #six yard box
  geom_rect(aes(xmin = 100 - 5.8, xmax = 100, ymin = (100/2) - 13.2, ymax = (100/2) + 13.2), fill = NA, col = "floralwhite", lwd = 0.5)+
  # Penalty spot
  #geom_circle(aes(x0 = 100 - 11.5, y0 = 100/2, r = 0.25), fill = NA, col = "floralwhite", lwd = 0.5)+
  annotate_penalty_box("floralwhite",NA,pitch_opta)+
  # goals
  geom_rect(aes(xmin = 100, xmax = 100 + 2, ymin = (100 / 2) - 5.8, ymax = (100 / 2) + 5.8), fill = NA, color = "floralwhite", lwd = 0.5)+
  # Penalty arc
  #ggforce::geom_arc(aes(x0 = 100 - 11.5, y0 = 100/2, r = 11.5, end = pi/2 + 0.9259284, start = pi/2 - 0.9259284),color = "red",lwd=1.5) +
  #  annotate_penalty_arc("floralwhite",NA,pitch_opta)
  #geom_arc(aes(x0 = 0, y0 = 0, r = 11.5, start = pi/2*11.5 - 0.9259284, end = pi/2*11.5 + 0.9259284), col = "floralwhite", lwd = 0.5) +