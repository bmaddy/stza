constants = data.frame(#avg_stubbornness = 50,
                       max_influence = 10,
                       quality = 75)

make_zombies = function(stubbornness_range, n){
  data.frame(stubbornness = sample(stubbornness_range, n, replace = TRUE),
             #deviation=sample(deviation_range, n, replace = TRUE),
             standing=FALSE)
}

influence = function(constants, percent_standing){
  constants$max_influence*percent_standing
}

rise = function(constants, zombies){
  percent_standing = length(zombies$standing[zombies$standing==TRUE])/nrow(zombies)
  #zombies$standing = constants$quality + influence(constants, percent_standing) > constants$avg_stubbornness + zombies$deviation
  zombies$standing = constants$quality + influence(constants, percent_standing) > zombies$stubbornness
  zombies
}

experiment = function(constants, zombies, n){
  result = c()
  for(i in 1:n){
#  while(length(result) < 2 | result[length(result)] != result[length(result) - 1])
    zombies = rise(constants, zombies)
    result = c(result, length(zombies$standing[zombies$standing==TRUE]))
  }
  result
}

# Usage:
# zombies = make_zombies(50:99, 200)
# zombies = make_zombies(c(50:55, 94:99), 200)
# experiment(constants, zombies, 10)

# more likely to have ovation:
# Higher quality
# if deviation range is larger, but the ovation takes longer

# less likely to have ovation:
# Higher threshold
# if e is polarized distribution

# if deviation range is smaller, likely to have a fast outcome in either direction
