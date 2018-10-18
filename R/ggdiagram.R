##' Create a circle
##' 
##' @param center Numerical vector of length 2 specifying x, y coordinates of circle center
##' @param radius Radius of circle.
##' @param size Thickness of outline.
##' @param color Color of outline.
##' @param fill Fill color of shape.
##' @param alpha Transparency of fill.
##' @param linetype Line type.
##' @param label Label at center of shape.
##' @param label_color Text color of label.
##' @param label_size Text size of label.
##' @param rotate Degrees to rotate shape.
##' @param rotate_about Coordinates to rotate about.
##' @export

gg_circle <- function(center = c(0,0), 
                      radius = 10, 
                      size = .5,
                      color = "black",
                      fill = "white",
                      alpha = 1,
                      linetype = 1,
                      label = "",
                      label_color = "black",
                      label_size = 5,
                      rotate = 0,
                      rotateabout = center){
  angle <- seq(-pi, pi, length = 3600)
  x <- -radius * sin(angle) + center[1]
  y <- -radius * cos(angle) + center[2]
  df <- data.frame(x = (x - (rotateabout[1])) * cos(rotate/180 * pi) + 
                       rotateabout[1] - 
                       (y - (rotateabout[2])) * sin(rotate/180 * pi), 
                   y = (y - (rotateabout[2])) * cos(rotate/180 * pi) + 
                       rotateabout[2] + 
                       (x - (rotateabout[1])) * sin(rotate/180 * pi))
  
  labelx <- (center[1] - (rotateabout[1])) * cos(rotate/180 * pi) + 
    rotateabout[1] - 
    (center[2] - (rotateabout[2])) * sin(rotate/180 * pi)
  
  labely <- (center[2] - (rotateabout[2])) * cos(rotate/180 * pi) + 
    rotateabout[2] + (center[1] - (rotateabout[1])) * sin(rotate/180 * pi)
  
  shape <- geom_polygon(aes(x, y), 
                        color = color, 
                        fill = alpha(fill, alpha), 
                        size = size, 
                        data = df, 
                        linetype = linetype)
  shape$data <- df
  list(shape = shape,
       label = annotate("text", x = labelx, y = labely, label = label, 
                        size = label_size, color = label_color))
  
}

##' Create an empty point in plot in which arrows can connect 
##' to/from (i.e. for creating bend in arrow)
##' 
##' @param center Numerical vector of length 2 specifying x, y coordinates of point.
##' @param label Label at center of shape.
##' @param label_color Text color of label.
##' @param label_size Text size of label.
##' @param rotate Degrees to rotate shape.
##' @param rotate_about Coordinates to rotate about.
##' @export

gg_point <- function(center = c(0,0),
                     label="",
                     label_color="black",
                     label_size=5,
                     rotate=0,
                     rotateabout=center){
  x = center[1]
  y = center[2] 
  df<-data.frame(x = (x-(rotateabout[1]))*cos(rotate/180*pi)+rotateabout[1]-(y-(rotateabout[2]))*sin(rotate/180*pi), y = (y-(rotateabout[2]))*cos(rotate/180*pi)+rotateabout[2]+(x-(rotateabout[1]))*sin(rotate/180*pi))
  labelx<- (center[1]-(rotateabout[1]))*cos(rotate/180*pi)+rotateabout[1]-(center[2]-(rotateabout[2]))*sin(rotate/180*pi)
  labely<- (center[2]-(rotateabout[2]))*cos(rotate/180*pi)+rotateabout[2]+(center[1]-(rotateabout[1]))*sin(rotate/180*pi)
  
  shape <- list(NULL)
  shape$data <- df
  
  list(shape=shape,
       label=annotate("text", x = labelx, y =labely, label = label, 
                      size=label_size, color = label_color))
  
}



# Ellipse -----------------------------------------------------------------

##' Create an ellipse
##' 
##' @param center Numerical vector of length 2 specifying x, y coordinates of ellipse center
##' @param radiusx Horizontal radius of ellipse.
##' @param radiusy Vertical radius of ellipse.
##' @param size Thickness of outline.
##' @param color Color of outline.
##' @param fill Fill color of shape.
##' @param alpha Transparency of fill.
##' @param linetype Line type.
##' @param label Label at center of shape.
##' @param label_color Text color of label.
##' @param label_size Text size of label.
##' @param rotate Degrees to rotate shape.
##' @param rotate_about Coordinates to rotate about.
##' @export

gg_ellipse <- function(center = c(0, 0), 
                       radiusx = 10, 
                       radiusy = 5, 
                       size = .5,
                       color = "black",
                       fill = "white",
                       alpha = 1,
                       linetype = 1,
                       label = "",
                       label_color = "black",
                       label_size = 5,
                       rotate = 0,
                       rotateabout = center){
  
  angle <- seq(-pi, pi, length = 3600)
  
  x = -radiusx * sin(angle) + center[1]
  y = -radiusy * cos(angle) + center[2]
  df <- data.frame(x = (x - (rotateabout[1])) * cos(rotate/180 * pi) + 
                     rotateabout[1] - 
                     (y - (rotateabout[2])) * sin(rotate/180 * pi), 
                   y = (y - (rotateabout[2])) * cos(rotate/180 * pi) + 
                     rotateabout[2] + 
                     (x - (rotateabout[1])) * sin(rotate/180 * pi))
  labelx <- (center[1] - (rotateabout[1])) * cos(rotate/180 * pi) + 
    rotateabout[1] - 
    (center[2] - (rotateabout[2])) * sin(rotate/180 * pi)
  labely <- (center[2] - (rotateabout[2])) * cos(rotate/180 * pi) + 
    rotateabout[2] + 
    (center[1] - (rotateabout[1])) * sin(rotate/180 * pi)
  
  
  shape <- geom_polygon(aes(x, y), 
                        color = color, 
                        fill = alpha(fill, alpha), 
                        size = size, 
                        data = df, 
                        linetype = linetype)
  shape$data <- df
  list(shape = shape,
       label = annotate("text", x = labelx, y = labely, label = label, 
                        size = label_size, color = label_color))
  
  
}


# Square ------------------------------------------------------------------

##' Create a square
##' 
##' @param center Numerical vector of length 2 specifying x, y coordinates of square center
##' @param sidelength Length of square side.
##' @param size Thickness of outline.
##' @param color Color of outline.
##' @param fill Fill color of shape.
##' @param alpha Transparency of fill.
##' @param linetype Line type.
##' @param label Label at center of shape.
##' @param label_color Text color of label.
##' @param label_size Text size of label.
##' @param rotate Degrees to rotate shape.
##' @param rotate_about Coordinates to rotate about.
##' @export

gg_square <- function(center = c(0, 0), 
                      sidelength = 10, 
                      label = "",
                      color = "black",
                      size = .5,
                      fill = "white",
                      alpha = 1,
                      linetype = 1,
                      label_color = "black",
                      label_size = 5,
                      rotate = 0,
                      rotateabout = center){
  
  angle <- seq(-sidelength/2, sidelength/2, length = 900)
  x1 <- c(angle, rep(sidelength/2, 900), -angle, rep(-sidelength/2,900))
  y1 <- c(rep(sidelength/2,900), c(angle), rep(-sidelength/2, 900), c(-angle))
  df <- data.frame(x = (x1 + center[1] - (rotateabout[1])) * cos(rotate/180 * pi) + 
                     rotateabout[1] - 
                     (y1 + center[2] - (rotateabout[2])) * sin(rotate/180 * pi), 
                   y = (y1 + center[2] - (rotateabout[2])) * cos(rotate/180 * pi) + 
                     rotateabout[2] + 
                     (x1 + center[1] - (rotateabout[1])) * sin(rotate/180 * pi))
  labelx <- (center[1] - (rotateabout[1])) * cos(rotate/180 * pi) + 
    rotateabout[1] - 
    (center[2] - (rotateabout[2])) * sin(rotate/180 * pi)
  labely <- (center[2] - (rotateabout[2])) * cos(rotate/180 * pi) + 
    rotateabout[2] + 
    (center[1] - (rotateabout[1])) * sin(rotate/180 * pi)
  
  df <- rbind(df[451:3600, ], df[1:450, ])
  
  shape <- geom_polygon(aes(x, y), 
                        color = color, 
                        fill = alpha(fill, alpha),
                        size = size, 
                        data = df,
                        linetype = linetype)
  shape$data <- df
  list(shape = shape,
       label = annotate("text", x = labelx, y = labely, label = label, 
                        size = label_size, color = label_color))
  
}


# Rectangle ---------------------------------------------------------------


##' Create an ellipse
##' 
##' @param center Numerical vector of length 2 specifying x, y coordinates of ellipse center
##' @param height Height of rectangle.
##' @param width Width of rectangle.
##' @param size Thickness of outline.
##' @param color Color of outline.
##' @param fill Fill color of shape.
##' @param alpha Transparency of fill.
##' @param linetype Line type.
##' @param label Label at center of shape.
##' @param label_color Text color of label.
##' @param label_size Text size of label.
##' @param rotate Degrees to rotate shape.
##' @param rotate_about Coordinates to rotate about.
##' @export


gg_rect <- function(center = c(0,0), 
                    height = 10, 
                    width = 5,
                    label = "",
                    color = "black",
                    size = .5,
                    fill = "white",
                    alpha = 1,
                    linetype = 1,
                    label_color = "black",
                    label_size = 5,
                    rotate = 0,
                    rotateabout = center){
  
  angle1 <- seq(-height/2, height/2, length = 900)
  angle2 <- seq(-width/2, width/2, length = 900)
  x <- c(c(angle2), rep(width/2, 900), c(-angle2), rep(-width/2, 900))
  y <- c(rep(height/2, 900), c(-angle1), rep(-height/2, 900), c(angle1))
  
  x = x + center[1]
  y = y + center[2]
  df <- data.frame(x = (x - (rotateabout[1])) * cos(rotate/180 * pi) + 
                     rotateabout[1] - 
                     (y - (rotateabout[2])) * sin(rotate/180 * pi), 
                   y = (y - (rotateabout[2])) * cos(rotate/180 * pi) + 
                     rotateabout[2] + 
                     (x - (rotateabout[1])) * sin(rotate/180 * pi))
  df <- rbind(df[451:3600, ], df[1:450, ])
  labelx <- (center[1] - (rotateabout[1])) * cos(rotate/180 * pi) + 
    rotateabout[1] - 
    (center[2] - (rotateabout[2])) * sin(rotate/180 * pi)
  labely <- (center[2] - (rotateabout[2])) * cos(rotate/180 * pi) + 
    rotateabout[2] + 
    (center[1] - (rotateabout[1])) * sin(rotate/180 * pi)
  
  df1 <- df[c(451, 1351, 2251, 3151),]
  
  shape <- geom_polygon(aes(x, y), 
                        color = color, 
                        fill = alpha(fill, alpha), 
                        size = size, 
                        data = df,
                        linetype = linetype)
  shape$data <- df
  list(shape = shape,
       label = annotate("text", x = labelx, y = labely, label = label, 
                        size = label_size, color = label_color))
  
}

# Diagram -----------------------------------------------------------------


##' Create a new ggdiagram
##' 
##' @param xlim Numerical vector of length 2 specifying xmin and xmax 
##' @param ylim Numerical vector of length 2 specifying ymin and ymax 
##' @export


ggdiagram <- function(xlim = c(-50, 50), ylim = c(-50, 50)){
  
  ggplot() + 
    scale_x_continuous(limits = c(xlim[1], xlim[2]))+
    scale_y_continuous(limits = c(ylim[1], ylim[2]))+
    theme_void()
  
}



# Arrows ------------------------------------------------------------------

##' Create straight arrow connecting two shapes
##' 
##' @param shapes Numerical vector of length 2 specifying shapes to connect
##' @param connection1 Connection on shape 1. Can be "near" for nearest point
##' to other shape or "up", "down", "left", "right" for top, bottom, left, or 
##' right side of shape. Can also be a number from 1 to 360 specifying specific
##' point along the shape, where 1 corresponds to the top.
##' @param connection2 See above.
##' @param size Thickness of outline.
##' @param color Color of outline.
##' @param type Arrow head type. "open" or "closed".
##' @param shiftx distance to shift x coordinate of arrow start
##' @param shifty distance to shift y coordinate of arrow start
##' @param shiftxend distance to shift x coordinate of arrow end
##' @param shiftyend distance to shift y coordinate of arrow end
##' @param dodge Numeric vector of length 2 giving degrees to adjust 
##' arrow start and arrow end.
##' @param linetype Line type.
##' @export


gg_arrow <- function(shapes = NULL,
                      connection1 = "near", 
                      connection2 = "near",
                      size = 1,
                      color = "black",
                      type = 'open',
                      shiftx = 0,
                      shifty = 0,
                      shiftxend = 0,
                      shiftyend = 0,
                      dodge = c(0, 0),
                      linetype = 1
){   
  
  
  data1 <- shapes[[1]]$data
  data2 <- shapes[[3]]$data 
  
  if (connection2 == "u" | connection2 ==  "up"){
    connection2 = 1
  } 
  
  if (connection2 == "r" |connection2 ==  "right"){
    connection2 = 90
  } 
  
  if (connection2 == "d" |connection2 ==  "down"){
    connection2 = 180
  } 
  
  if (connection2 == "l" |connection2 ==  "left"){
    connection2 = 270
  } 
  
  
  
  
  if (connection1 == "u"|connection1 == "up"){
    connection1 = 1
  } 
  
  if (connection1 == "r"|connection1 == "right"){
    connection1 = 90
  } 
  
  if (connection1 == "d"|connection1 == "down"){
    connection1 = 180
  } 
  
  if (connection1 == "l"|connection1 == "left"){
    connection1 = 270
  } 
  
  if (connection1 == "near" & connection2 != "near"){
    x1 <- data2[connection2 * 10, 1]
    y1 <- data2[connection2 * 10, 2]
    x2 <- data1[, 1]
    y2 <- data1[, 2]
    connection1 <- which((sqrt((x1 - x2)^2 + (y1 - y2)^2)) == 
                           min(sqrt((x1 - x2)^2 + (y1 - y2)^2)))[1]/10
    
  } 
  
  if (connection2 == "near" & connection1 != "near"){
    x1 <- data1[connection1 * 10, 1]
    y1 <- data1[connection1 * 10, 2]
    x2 <- data2[, 1]
    y2 <- data2[, 2]
    connection2 <- which((sqrt((x1 - x2)^2 + (y1 - y2)^2)) == 
                           min(sqrt((x1 - x2)^2 + (y1 - y2)^2)))[1]/10
  } 
  
  if (connection2 == "near" & connection1 =="near"){
    x1 <- data1[, 1]
    y1 <- data1[, 2]
    x2 <- data2[, 1]
    y2 <- data2[, 2]
    
    
    mindistance = Inf
    
    for (i in 1:3600){
      x1 <- data1[i, 1]
      y1 <- data1[i, 2]
      
      distance <- min((sqrt((x1 - x2)^2 + (y1 - y2)^2)))
      
      if (distance < mindistance){
        mindistance = distance
        x1min = x1 
        y1min = y1
        x2min = x2
        y2min = y2
      }
      
    } 
    
    connection1 = which (data1[, 1] == x1min & data1[, 2] == y1min)/10
    x1 <- data1[connection1 * 10, 1]
    y1 <- data1[connection1 * 10, 2]
    x2 <- data2[, 1]
    y2 <- data2[, 2]
    connection2 <- which((sqrt((x1 - x2)^2 + (y1 - y2)^2)) == 
                           min(sqrt((x1 - x2)^2 + (y1 - y2)^2)))[1]/10
    
  } 
  
  
  x1 <- data1[connection1 * 10 + dodge[1] * 10, 1] + shiftx
  y1 <- data1[connection1 * 10 + dodge[1] * 10, 2] + shifty
  xend1 <- data2[connection2 * 10 + dodge[2] * 10,1] + shiftxend
  yend1 <- data2[connection2 * 10 + dodge[2] * 10,2] + shiftyend
  
  out <- geom_segment(mapping = aes_string(x = round(x1, 1), 
                                           y = round(y1, 1),
                                           xend = round(xend1, 1), 
                                           yend = round(yend1, 1)), 
                    size = size, 
                    color = color,
                    arrow = arrow(length = unit(.2, "in"),
                                  type = type),
                    linetype = linetype)
  return(out)
  
}   


# Curve -------------------------------------------------------------------

##' Create curved arrow connecting two shapes
##' 
##' @param shapes Numerical vector of length 2 specifying shapes to connect
##' @param connection1 Connection on shape 1. Can be "near" for nearest point
##' to other shape or "up", "down", "left", "right" for top, bottom, left, or 
##' right side of shape. Can also be a number from 1 to 360 specifying specific
##' point along the shape, where 1 corresponds to the top.
##' @param connection2 See above.
##' @param size Thickness of outline.
##' @param color Color of outline.
##' @param type Arrow head type. "open" or "closed".
##' @param curvature	A numeric value giving the amount of curvature. 
##' Negative values produce left-hand curves, positive values produce 
##' right-hand curves, and zero produces a straight line.
##' @param angle	A numeric value between 0 and 180, giving an amount 
##' to skew the control points of the curve. Values less than 90 skew 
##' the curve towards the start point and values greater than 90 skew 
##' the curve towards the end point.
##' @param ncp	The number of control points used to draw the curve. 
##' More control points creates a smoother curve.
##' @param shiftx distance to shift x coordinate of arrow start
##' @param shifty distance to shift y coordinate of arrow start
##' @param shiftxend distance to shift x coordinate of arrow end
##' @param shiftyend distance to shift y coordinate of arrow end
##' @param dodge Numeric vector of length 2 giving degrees to adjust 
##' arrow start and arrow end.
##' @param linetype Line type.
##' @export

gg_curve <- function(shapes = NULL,
                     connection1 = 1, 
                     connection2 = 1,
                     size = 1,
                     color = "black",
                     type = 'open',
                     angle = 0,
                     curvature = 0,
                     ncp = 10,
                     shiftx = 0,
                     shifty = 0,
                     shiftxend = 0,
                     shiftyend = 0,
                     dodge = c(0, 0),
                     linetype = 1
){   
  
  data1 <- shapes[[1]]$data
  data2 <- shapes[[3]]$data 
  
  
  
  
  
  if (connection2 == "u" | connection2 ==  "up"){
    connection2 = 1
  }
  
  if (connection2 == "r" |connection2 ==  "right"){
    connection2 = 90
  } 
  
  if (connection2 == "d" |connection2 ==  "down"){
    connection2 = 180
  } 
  
  if (connection2 == "l" |connection2 ==  "left"){
    connection2 = 270
  } 
  
  if (connection1 == "u"|connection1 == "up"){
    connection1 = 1
  } 
  
  if (connection1 == "r"|connection1 == "right"){
    connection1 = 90
  } 
  
  if (connection1 == "d"|connection1 == "down"){
    connection1 = 180
  }
  
  if (connection1 == "l"|connection1 == "left"){
    connection1 = 270
  } 
  
  
  if (connection1 == "near" & connection2 != "near"){
    x1 <- data2[connection2 * 10, 1]
    y1 <- data2[connection2 * 10, 2]
    x2 <- data1[, 1]
    y2 <- data1[, 2]
    connection1 <- which((sqrt((x1 - x2)^2 + (y1 - y2)^2)) == 
                           min(sqrt((x1 - x2)^2 + (y1 - y2)^2)))[1]/10
    
  } 
  
  if (connection2 == "near" & connection1 != "near"){
    x1 <- data1[connection1 * 10, 1]
    y1 <- data1[connection1 * 10, 2]
    x2 <- data2[, 1]
    y2 <- data2[, 2]
    connection2 <- which((sqrt((x1 - x2)^2 + (y1 - y2)^2)) == 
                           min(sqrt((x1 - x2)^2 + (y1 - y2)^2)))[1]/10
  } 
  
  if (connection2 == "near" & connection1 =="near"){
    x1 <- data1[, 1]
    y1 <- data1[, 2]
    x2 <- data2[, 1]
    y2 <- data2[, 2]
    
    
    mindistance = Inf
    
    for (i in 1:3600){
      x1 <- data1[i, 1]
      y1 <- data1[i, 2]
      
      distance <- min((sqrt((x1 - x2)^2 + (y1 - y2)^2)))
      
      if (distance < mindistance){
        mindistance = distance
        x1min = x1 
        y1min = y1
        x2min = x2
        y2min = y2
      }
      
    } 
    
    connection1 = which(data1[, 1] == x1min & data1[, 2] == y1min)/10
    x1 <- data1[connection1 * 10, 1]
    y1 <- data1[connection1 * 10, 2]
    x2 <- data2[, 1]
    y2 <- data2[, 2]
    connection2 <- which((sqrt((x1 - x2)^2 + (y1 - y2)^2))==
                           min(sqrt((x1 - x2)^2 + (y1 - y2)^2)))[1]/10
    
  } 
  
  
  x1 <- data1[connection1 * 10 + dodge[1] * 10, 1] + shiftx
  y1 <- data1[connection1 * 10 + dodge[1] * 10, 2] + shifty
  xend1 <- data2[connection2 * 10 + dodge[2] * 10, 1] + shiftxend
  yend1 <- data2[connection2 * 10 + dodge[2] * 10, 2] + shiftyend
  
  out <- geom_curve(mapping = aes_string(x = round(x1, 1), y = round(y1, 1), 
                                         xend = round(xend1, 1), yend = round(yend1, 1)), 
                  size = size, 
                  color = color, 
                  arrow = arrow(length = unit(.2, "in"), type = type),
                  angle = angle, 
                  curvature = curvature,
                  ncp = ncp,
                  linetype = linetype)
  return(out)
  
}   

