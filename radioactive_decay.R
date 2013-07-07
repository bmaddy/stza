make_cup = function(n){
  face = sample(1:6, n, replace = TRUE)
  f = as.data.frame(face)
  f$age = 0
  f
}

remove = function(cup, n){
  cup[cup$face != n,]
}

shake = function(cup){
  cup$face = sample(1:6, length(cup$face), replace = TRUE)
  cup$age = cup$age + 1
  cup
}

experiment = function(cup){
  remaining = cup()
  while(length(cup$face) > 0){
    remaining = append(remaining, length(cup$face))
    cup = shake(cup)
    cup = remove(cup, 1)
  }
  remaining
}