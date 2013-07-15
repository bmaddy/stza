dropbox = '/Users/pkirwin/Dropbox/STZA_private/'
reload = function(file_name){
	source(paste(dropbox,"rwsb.R",sep=''))
	}
	
## Constants
############

zombie_constants = data.frame(health = 5)
zombie_constants$aim = .5
zombie_constants$max_damage = 3

human_constants = data.frame(health = 3)
human_constants$aim = .8
human_constants$max_damage = 4
	
## Setup
########

make_army = function(n, constants){
	army = data.frame(health = rep(constants$health, n))
	army$aim = constants$aim
	army$max_damage = constants$max_damage
	army
	}
	
## Transition
#############

aim_fire = function(shooters, targets){
	live_targets = (1:nrow(targets))[targets$health > 0]
	if(length(live_targets) == 1){shooters$target = live_targets}
	else{shooters$target = sample(live_targets, nrow(shooters), replace = TRUE)}
	shooters$hit = shooters$aim > runif(nrow(shooters))
	shooters$damage = 0
	for(i in 1:nrow(shooters)){
		if(shooters$hit[i] & shooters$health[i] > 0){shooters$damage[i] = sample(1:shooters$max_damage[i], 1)}
		}
	shooters
	}
	
apply_damage = function(shooters, targets){
	for(target in 1:nrow(targets)){
			targets$health[target] = targets$health[target] - sum(shooters$damage[shooters$target == target])
		}
	targets
	}

play_round = function(army1, army2){
	army1 = aim_fire(army1, army2)
	army2 = aim_fire(army2, army1)
	army1 = apply_damage(army2, army1)
	army2 = apply_damage(army1, army2)
	list(army1, army2)
	}
	
## Iterate
##########

fight = function(army1, army2, output = FALSE){
	pops = data.frame(army1 = nrow(army1), army2 = nrow(army2))
	step = 0
	if(output){army_plot(army1, army2, step)}
	while(nrow(army1[army1$health > 0,]) > 0 & nrow(army2[army2$health > 0,])){
		step = step + 1
		armies = play_round(army1, army2)
		army1 = armies[[1]]
		army2 = armies[[2]]
		if(output){army_plot(army1, army2, step)}
		pops = rbind(pops, c(nrow(army1[army1$health > 0,]), nrow(army2[army2$health > 0,])) )
		}
	pops
	}
	
## Reporting
############

pop_plot = function(results){
	plot(1:nrow(results), results$army1,
		main = "Army Populations", xlab = "turn", ylab = "surviving population",
		type = "b", ylim = c(0, max(results[1,]))
		)
	points(1:nrow(results), results$army2, col = "red", type = "b")
	legend("bottomleft",legend = c("army1", "army2"), pch = 1, col = c("black","red"))
	}
	
army_plot = function(army1, army2, step = 0){
	army1$ndx = 1:nrow(army1)
	army2$ndx = 1:nrow(army2)
	army1_alive = army1[army1$health > 0,]
	army1_dead = army1[army1$health <= 0,]
	army2_alive = army2[army2$health > 0,]
	army2_dead = army2[army2$health <= 0,]
	plot(rep(1,nrow(army1_alive)),army1_alive$ndx,
		main = "The field of battle", xlim = c(0, 3), ylim = c(0, 1 + max(nrow(army1), nrow(army2))),
		xlab = paste("The battle at step",step)
		)
	points(rep(1,nrow(army1_dead)), army1_dead$ndx, pch = 16, col = "red")
	points(rep(2,nrow(army2_alive)), army2_alive$ndx, pch = 2)
	points(rep(2,nrow(army2_dead)), army2_dead$ndx, col = "red", pch = 17)
	}
	
animate_battle = function(army1, army2){
	ani.start()
	fight(army1, army2, TRUE)
	ani.stop()
	}