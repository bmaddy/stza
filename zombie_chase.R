library("animation")

## Initial state
################

create_field = function(d = 300){
  player = c("start", "end", "human", "zombie")
  field = as.data.frame(player)
  field$x = 0
  field$y = 0
  field$v = 0
  
  field$x[field$player == "end"] = d
  
  return(field)
}

add_zombie_location = function(field, x,y){
  field$x[field$player == "zombie"] = x
  field$y[field$player == "zombie"] = y
  
  return(field)
}

random_zombie_location = function(field, xBounds, yBounds, speed_range = FALSE){#Bounds must be of the form c(smallest, largest)
  field$x[field$player == "zombie"] = runif(1, min = min(xBounds), max = max(xBounds))
  field$y[field$player == "zombie"] = runif(1, min = min(yBounds), max = max(yBounds))
  
  if(speed_range[1]){field = randomize_zombie_speed(field, speed_range)}
  
  return(field)
}

add_speeds = function(field, players, speeds){
  for (p in 1:length(players)){
    field$v[field$player == players[p]] = speeds[p]
  }
  
  return(field)
}

randomize_zombie_speed = function(field, speed_range){
  field$v[field$player == "zombie"] = runif(1, speed_range[1], speed_range[2])
  
  return(field)
}

## Utility functions
####################

distance = function(xs, ys){
  d = sqrt((xs[1] - xs[2]) ^ 2 + (ys[1] - ys[2]) ^ 2)
  return(d)
}

locations = function(field){
  xh = field$x[field$player == "human"]
  yh = field$y[field$player == "human"]
  xz = field$x[field$player == "zombie"]
  yz = field$y[field$player == "zombie"]
  
  loc = c(xh, yh, xz, yz)
  return(loc)
}

## State transition functions
#############################

move_human = function(field, dt){
  xi = field$x[field$player == "human"]
  d = field$v[field$player == "human"] * dt
  
  field$x[field$player == "human"] = xi + d
  return(field)
}

move_zombie = function(field, dt){
  xi = field$x[field$player == "zombie"]
  d = field$v[field$player == "zombie"] * dt
  
  loc = locations(field)
  l = distance(c(loc[1], loc[3]), c(loc[2], loc[4]))
  
  dx = d * (loc[1] - loc[3]) / l
  dy = d * (loc[2] - loc[4]) / l
  
  field$x[field$player == "zombie"] = loc[3] + dx
  field$y[field$player == "zombie"] = loc[4] + dy
  
  return(field)
}

move_both = function(field, dt){
  field = move_human(field, dt)
  field = move_zombie(field, dt)
  
  return(field)
}

catch = function(field, arm_length = 1){
  locs = locations(field)
  return(distance(c(locs[1], locs[3]), c(locs[2], locs[4])) < arm_length)
}

safe = function(field, arm_length = 1){
  locs = locations(field)
  return(distance(c(locs[1], field$x[field$player == "end"]), c(locs[2], 0)) < arm_length)
}

run = function(field, dt, result_only = FALSE){
  locs = locations(field)
  record = data.frame("xh" = locs[1], "yh" = locs[2], "xz" = locs[3], "yz" = locs[4])
  
  while(! safe(field) & ! catch(field)){
    field = move_both(field, dt)
    if(! result_only){record = rbind(record, locations(field))}
  }
  
  if(result_only){return(safe(field))}
  return(record)
}

check_safety = function(field, dt){
  result = 0
  if(run(field, dt, TRUE)){result = 1}
  
  return(result)
}

check_many_locations = function(field, dt, n, xBounds, yBounds, speed_range = FALSE){
  this_field = random_zombie_location(field, xBounds, yBounds, speed_range)
  locs = locations(this_field)
  record = data.frame("xz" = locs[3], "yz" = locs[4], "outcome" = check_safety(this_field, dt))
  
  for (i in 2:n){
    this_field = random_zombie_location(field, xBounds, yBounds, speed_range)
    locs = locations(this_field)
    record = rbind(record, c(locs[3], locs[4], check_safety(this_field, dt)))
  }
  
  return(record)
}

recommend_run = function(zombie_loc, payment = 10000, cost = -1000000){
  recommendation = 0
  if(zombie_loc[2] > 170){recommendation = 1}
  return(recommendation)
}

## Results and plotting
#######################

show_paths = function(results, field){
  plot(results$xh, results$yh, 
       xlim = c(0, field$x[field$player == "end"]), ylim = c(0, field$x[field$player == "end"]),
       xlab = "x", ylab = "y", main = "Trajectories of a zombie chasing a human"
  )
  points(results$xz, results$yz, col = "red")
  legend("topright", legend = c("human", "zombie"), col = c("black", "red"), pch = c(1,1))
}

show_many_locations = function(results, field, xBounds = FALSE, yBounds = FALSE){
  x_limits = c(0, field$x[field$player == "end"])
  y_limits = c(0, field$x[field$player == "end"])
  if(xBounds){x_limits = c(0, xBounds[2])}
  if(yBounds){y_limits = c(0, yBounds[2])}
  
  lives = results[results$outcome == 1,]
  deaths = results[results$outcome == 0,]
  
  plot(deaths$xz, deaths$yz,
       xlim = x_limits, ylim = y_limits,
       ylab = "y", xlab = "x", main = "Outcome of running from A to B\nwith zombies starting at various locations"
  )
  points(lives$xz, lives$yz, col = "red")
  legend("topright", legend = c("Died", "Lived"), col = c("black", "red"), pch = c(1,1))
}

economics_of_trials = function(results, payment = 10000, cost = -1000000){
  results$rec = 0
  for(a in 1:nrow(results)){
    results$rec[a] = recommend_run(c(results$xz[a], results$yz[a]), payment, cost)
  }
  
  recommended = results[results$rec == 1,]
  recommended$money = payment
  recommended$money[recommended$outcome == 0] = cost
  
  return(sum(recommended$money))
}

animate_path = function(field, dt = 0.5){
  locs = locations(field)
  ani.options(interval = 0.1)
  ani.start()
  size = max(field[,2:3])
  
  while(! safe(field) & ! catch(field)){
    field = move_both(field, dt)
    plot(field$x[c(3,4)], field$y[c(3,4)], xlim = c(0,size), ylim = c(0,sie), col = c('red', 'black'), pch = c("h", "z"))
  }
  ani.stop()
}

