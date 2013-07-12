library("animation")

## Setup
########

constants = data.frame(breaking_point = 10,
                       distributed_stress = 8)

make_land = function(constants, n = 10){
  data = sample(0:(constants$breaking_point - 1),
                n*n,
                replace = TRUE)
  land = matrix(data = data, nrow = n, ncol = n)
  land
}

## Transition
#############

add_random_stress = function(land){
  x = sample(1:ncol(land),1)
  y = sample(1:nrow(land),1)
  land[x,y] = land[x,y] + 1
  land
}

collapse = function(land, x, y, breaking_point = 8){
  land[x,y] >= breaking_point
}

distribute_moore = function(land, x, y){
  for(dx in -1:1){
    for(dy in -1:1){
      middle = dx == 0 & dy == 0
      too_big = x+dx > ncol(land) | y+dy > nrow(land)
      too_small = x+dx < 1 | y+dy < 1
      if(!middle & !too_big & !too_small){
        land[x+dx,y+dy] = land[x+dx,y+dy] + 1
      }
    }
  }
  land
}

collapse_and_distribute_all = function(land){
  result = land
  collapse_count = 0
  for(x in 1:ncol(land)){
    for(y in 1:nrow(land)){
      if(collapse(land,x,y)){
        collapse_count = collapse_count + 1
        result[x,y] = land[x,y] - 8
        result = distribute_moore(result, x, y)
      }
    }
  }
  list(land = result, collapse_count = collapse_count)
}


## Iterate
##########

propagate = function(land, n, make_plot = FALSE, stress_rate = 1){
  collapses = c()
  for(i in 1:n){
    for(j in 1:stress_rate){land = add_random_stress(land)}
    collapsed = collapse_and_distribute_all(land)
    land = collapsed$land
    collapses = append(collapses, collapsed$collapse_count)
    if(make_plot){plot_state(land)}
  }
  list(land = land, collapses = collapses)
}

## Stress and propagate separately method
#########################################

propagate_all = function(land, breaking_point = 10){
  collapse_count = 0
  while(max(land) > breaking_point){
    collapse_data = collapse_and_distribute_all(land)
    collapse_count = collapse_count + collapse_data$collapse_count
    land = collapse_data$land
  }
  list(land = land, collapse_count = collapse_count)
}

stress_then_propagate = function(land, n, make_plot = FALSE, stress_rate = 1){
  collapses = c()
  for(i in 1:n){
    for(j in 1:stress_rate){land = add_random_stress(land)}
    propagated = propagate_all(land)
    land = propagated$land
    collapses = append(collapses, propagated$collapse_count)
    if(make_plot){plot_state(land)}
  }
  list(land = land, collapses = collapses)
}

## Report
#########

plot_state = function(land){
  plot(1,1,type = "n", xlim = c(0,ncol(land)), ylim = c(0,nrow(land)))
  for(x in 1:ncol(land)){
    for(y in 1:nrow(land)){
      rect(x-1, y-1, x, y, col = terrain.colors(10)[land[x,y]])
    }
  }
}

plot_state_over_time = function(land, n){
  ani.options(interval = 0.1)
  ani.start()
  #result = propagate(land, n, TRUE, 15)
  result = stress_then_propagate(land, n, TRUE, 15)
  ani.stop()
  result
}

# land = make_grid(50)
# result = plot_state_over_time(land, 500)
