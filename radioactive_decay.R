make_cup = function(n){
  sample(1:6, n, replace = TRUE)
}

remove = function(cup, n){
  cup[cup != n]
}

shake = function(cup){
  sample(1:6, length(cup), replace = TRUE)
}

experiment = function(cup){
  remaining = c()
  while(length(cup) > 0){
    remaining = append(remaining, length(cup))
    cup = shake(cup)
    cup = remove(cup, 1)
  }
  remaining
}