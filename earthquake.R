## Setup
########

make_grid = function(n = 10){
  land = matrix(data = 0, nrow = n, ncol = n)
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

collapse = function(land, x, y, breaking_point = 10){
  land[x,y] > breaking_point
}

distribute_moore = function(land, x, y, stress_distributed = 6){
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
  for(x in 1:ncol(land)){
    for(y in 1:nrow(land)){
      if(collapse(land,x,y)){
        result[x,y] = land[x,y] - 11
        result = distribute_moore(result, x, y)
      }
    }
  }
  result
}

count_collapsors = function(land){
  
}

## Iterate
##########

propagate = function(land, n, make_plot = FALSE, stress_rate = 1){
  for(i in 1:n){
    for(j in 1:stress_rate){land = add_random_stress(land)}
    land = collapse_and_distribute_all(land)
    if(make_plot){plot_state(land)}
  }
  land
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
  propagate(land, n, TRUE, 15)
  ani.stop()
}