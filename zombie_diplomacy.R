constants = data.frame(avg_stubbornness = 50,
                       max_influence = 10,
                       quality = 75)

make_zombies = function(deviation_range, n){
  data.frame(deviation=sample(deviation_range, 200, replace = TRUE),
             standing=FALSE)
}

influence = function(constants, percent_standing){
  constants$max_influence*percent_standing
}

rise = function(constants, zombies){
  percent_standing = length(zombies$standing[zombies$standing==TRUE])/nrow(zombies)
  zombies$standing = constants$quality + influence(constants, percent_standing) > constants$avg_stubbornness + zombies$deviation
  zombies
}

experiment = function(constants, zombies, n){
  result = c()
  for(i in 1:n){
    zombies = rise(constants, zombies)
    result = c(result, length(zombies$standing[zombies$standing==TRUE]))
  }
  result
}

# Usage:
# zombies = make_zombies(0:49, 200)
# zombies = make_zombies(c(0:5, 44:49), 200)
# experiment(constants, zombies, 10)

# more likely to have ovation:
# Higher quality
# if deviation range is larger, but the ovation takes longer

# less likely to have ovation:
# Higher threshold
# if e is polarized distribution

# if deviation range is smaller, likely to have a fast outcome in either direction
