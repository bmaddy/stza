running_speed = function(height){
  (height / 2) + 5
}
#running_speed(6)
#running_speed(5)

find_suspicious_IQs_a = function(set_of_IQs){
  zombie_IQs = c()
  for(i in set_of_IQs){
    if(i < 70 & ((i %% 2) == 0)){
      zombie_IQs = append(zombie_IQs, i)
    }
  }
  zombie_IQs
}

find_suspicious_IQs_b = function(set_of_IQs){
  set_of_IQs[set_of_IQs < 70 & ((set_of_IQs %% 2) == 0)]
}

#set_of_IQs = c(57, 72, 89, 50, 67, 76, 53, 78, 65, 58)
#find_suspicious_IQs_a(set_of_IQs)
#find_suspicious_IQs_b(set_of_IQs)

#set_of_IQs = c(82, 83, 80, 68, 55, 79, 60, 75, 72, 64)
#find_suspicious_IQs_a(set_of_IQs)
#find_suspicious_IQs_b(set_of_IQs)


zombies_created = function(town_size, existing_zombie_population, days_in_future){
  if(days_in_future == 0){
    existing_zombie_population
  }
  else{
    created_zombies = ceiling(100*existing_zombie_population/town_size)
    new_zombie_population = min(town_size, existing_zombie_population + created_zombies)
    zombies_created(town_size, new_zombie_population, days_in_future - 1)
  }
}
#zombies_created(100, 1, 0)
#zombies_created(100, 1, 1)
#zombies_created(100, 1, 2)
#zombies_created(15000, 800, 13)
#zombies_created(20000, 1500, 7)
#zombies_created(10000, 9300, 100)