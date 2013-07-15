# Setup
############

constants = data.frame(breaking_point = 10,
                       distributed_stress = 1)

make_land = function(n){
  data = sample(0:(constants$breaking_point - 1),
                n*n,
                replace = TRUE)
  matrix(data, nrow = n, ncol = n)
}

# Transition
############

add_random_stress = function(land){
  x = sample(1:ncol(land), 1)
  y = sample(1:nrow(land), 1)
  land[x, y] = land[x, y] + 1
  land
}

distribute = function(constants, land, x, y){
  for(dx in -1:1){
    for(dy in -1:1){
      center = dx == 0 & dy == 0
      too_small = x+dx < 1 | y+dy < 1
      too_big = x+dx > ncol(land) | y+dy > nrow(land)
      if(!center & !too_big & !too_small){
        land[x+dx, y+dy] = land[x+dx, y+dy] + constants$distributed_stress
      }
    }
  }
  land
}

collapse_and_distribute_all = function(constants, land){
  result = land
  collapse_count = 0
  for(x in 1:ncol(land)){
    for(y in 1:nrow(land)){
      if(land[x, y] >= constants$breaking_point){
        result[x, y] = land[x, y] - constants$breaking_point
        collapse_count = collapse_count + 1
        result = distribute(constants, result, x, y)
      }
    }
  }
  list(land = result, collapse_count = collapse_count)
}

# Iterate
############

# calls collapse_and_distribute_all repatedly until there are no
# at the breaking_point
# returns: the new version of land and the total number of collapses
# that happened (collapse_count)
propagate = function(constants, land){
  collapse_count = 0
  while(max(land) > constants$breaking_point){
    collapse_data = collapse_and_distribute_all(constants, land)
    collapse_count = collapse_count + collapse_data$collapse_count
    land = collapse_data$land
  }
  list(land = land, collapse_count = collapse_count)
}

# This will iterate a total of n times. Each time it will add some
# random stress and propagate any collapses that happen.
# returns: a vector of collapses, one for each iteration n
add_stress_and_propagate = function(constants, land, n){
  collapses = c()
  for(i in 1:n){
    land = add_random_stress(land)
    propagated = propagate(constants, land)
    land = propagated$land
    collapses = append(collapses, propagated$collapse_count)
    plot_state(land)
  }
  list(land = land, collapses = collapses)
}

# Report
############

# makes a new blank plot and then draws the current state of land
# using rectangles. You'll need to add a call to this in the loop in
# add_stress_and_propagate
plot_state = function(land){
  plot(1, 1, type = "n", xlim = c(0, ncol(land)), ylim = c(0, nrow(land)))
  for(x in 1:ncol(land)){
    for(y in 1:nrow(land)){
      #rect(x-1, y-1, x, y, col = terrain.colors(10)[land[x,y]])
      rect(x-1, y-1, x, y, col = 'darkseagreen', density = 5*land[x,y])
    }
  }
}

# Starts an animation and calls add_stress_and_propagate to
# draw the plots
plot_state_over_time = function(constants, land, n){
  ani.options(interval = 0.1)
  ani.start()
  result = stress_and_propagate(constants, land, n, TRUE)
  ani.stop()
  result
}