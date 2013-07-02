make_cup = function(n){
  face = sample(1:6, n, replace = TRUE)
  f = as.data.frame(face)
  f$age = 0
  f
}

remove = function(c, n){
  c[c$face != n,]
}

shake = function(c){
  c$face = sample(1:6, length(c$face), replace = TRUE)
  c$age = c$age + 1
  c
}

experiment = function(c){
  remaining = c()
  while(length(c$face) > 0){
    remaining = append(remaining, length(c$face))
    c = shake(c)
    c = remove(c, 1)
  }
  return(remaining)
}