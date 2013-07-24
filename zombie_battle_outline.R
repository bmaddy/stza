## Setup
########

hConstants = data.frame(health = 3)
hConstants$aim = .8
hConstants$max_damage = 4

zConstants = data.frame(health = 5)
zConstants$aim = .5
zConstants$max_damage = 3

make_army = function(n, constants){
  army = data.frame(ndx = 1:n)
  army$health = constants$health
  army$aim = constants$aim
  army$max_damage = constants$max_damage
  army$target = NA ## ndx of an opponent
  army$hit = NA ## TRUE/FALSE
  army$damage = NA ## a number from 1 to max_damage
  army
}

## Transition
#############

# make a list of the targets still active
# each shooter picks an active target
# determine if the shooter hits
# determine damage inflicted on hit
# all of this goes on the shooter data.frame
aim_fire = function(shooters, targets){
  ...
}

# for each target, determine the total damage done
# decrease the target's health by that much
# all of this goes on the target data.frame
apply_damage = function(shooters, targets){
  ...
}

# aim_fire for both armies
# apply damage for both armies
# return a list of both armies
fight_round = function(shooters, targets){
  ...
}

## Iterate
##########

# Calls fight_round until one army has no survivors
# return a data.frame of populations
fight = function(army1, army2){
  ...
}

## Report
#########

pop_plot = function(results){
  ...
}

army_plot = function(army1, army2){
  ...
}

animate_battle = function(army1, army2){
  ...
}