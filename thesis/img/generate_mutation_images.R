library(mvtnorm)

myfun = function(v)
{
  -dmvnorm(v, mean=c(-1,-1), sigma=matrix(c(1.2,0.7,0.7,1),nrow=2))
}

derand = function(pop)
{
  pop_size = dim(pop)[1]
  selected = matrix(replicate(pop_size, sample(1:pop_size, size = 3, replace = F)), nrow=pop_size, byrow=T)
  t(apply(selected, 1, function(sel) { pop[sel[1],] + 0.9 * (pop[sel[2],] - pop[sel[3],]) }))
}

debest = function(pop,best)
{
  pop_size = dim(pop)[1]
  selected = matrix(replicate(pop_size, sample(1:pop_size, size = 2, replace = F)), nrow=pop_size, byrow=T)
  t(apply(selected, 1, function(sel) { best + 0.9 * (pop[sel[1],] - pop[sel[2],]) }))
}

demid = function(pop,mid)
{
  pop_size = dim(pop)[1]
  selected = matrix(replicate(pop_size, sample(1:pop_size, size = 2, replace = F)), nrow=pop_size, byrow=T)
  t(apply(selected, 1, function(sel) { mid + 0.9 * (pop[sel[1],] - pop[sel[2],]) }))
}

decurrentrand = function(pop)
{
  pop_size = dim(pop)[1]
  selected = matrix(replicate(pop_size, sample(1:pop_size, size = 3, replace = F)), nrow=pop_size, byrow=T)
  selected = cbind(1:pop_size, selected)
  t(apply(selected, 1, function(sel) { 0.5 * pop[sel[1],] + 0.5*pop[sel[2],] + 0.9 * (pop[sel[3],] - pop[sel[4],]) }))
}

decurrentbest = function(pop,best)
{
  pop_size = dim(pop)[1]
  selected = matrix(replicate(pop_size, sample(1:pop_size, size = 2, replace = F)), nrow=pop_size, byrow=T)
  selected = cbind(1:pop_size, selected)
  t(apply(selected, 1, function(sel) { 0.5 * pop[sel[1],] + 0.5*best + 0.9 * (pop[sel[2],] - pop[sel[3],]) }))
}

derandbest = function(pop,best)
{
  pop_size = dim(pop)[1]
  selected = matrix(replicate(pop_size, sample(1:pop_size, size = 3, replace = F)), nrow=pop_size, byrow=T)
  t(apply(selected, 1, function(sel) { 0.5 * pop[sel[1],] + 0.5*best + 0.9 * (pop[sel[2],] - pop[sel[3],]) }))
}

decurrentpbest = function(pop,p)
{
  pop_size = dim(pop)[1]
  p_size = pop_size*p
  values = myfun(pop)
  pop = pop[order(values),]

  selected = matrix(replicate(pop_size, sample(1:pop_size, size = 2, replace = F)), nrow=pop_size, byrow=T)
  selected = cbind(matrix(replicate(pop_size, sample(1:p_size, size = 1, replace = F)), nrow=pop_size, byrow=T), selected)
  selected = cbind(1:pop_size, selected)
  t(apply(selected, 1, function(sel) { pop[sel[1],] + 0.9 * (pop[sel[2],] - pop[sel[1],]) + 0.9 * (pop[sel[3],] - pop[sel[4],]) }))
}


x = seq(from = -3, to = 3, by=0.01)
y = seq(from = -3, to = 3, by=0.01)
v = expand.grid(x,y)
random_before = rmvnorm(2000, mean = c(1,1), sigma = matrix(c(0.3,0.3,0.3,0.8), nrow=2))

png("basic.png", width = 500, height=500)
contour(x,y, matrix(myfun(v),nrow=length(x)), main="Pierwotna populacja", xlab="x", ylab="y")
points(random_before[,1], random_before[,2], pch="*")
n_best = which.min(myfun(random_before))
points(random_before[n_best,1],random_before[n_best,2], pch="O", col="green")
points(-1,-1, pch="x", col="red")
dev.off()

png("rand1.png", width = 500, height=500)
contour(x,y, matrix(myfun(v),nrow=length(x)), main="rand/1; F = 0.9", xlab="x", ylab="y")
after = derand(random_before)
n_best_after = which.min(myfun(after))
points(after[,1], after[,2], pch="*", col="black")
points(after[n_best_after,1],after[n_best_after,2], pch="O", col="green")
points(-1,-1, pch="x", col="red")
dev.off()

png("best1.png", width = 500, height=500)
contour(x,y, matrix(myfun(v),nrow=length(x)), main="best/1; F = 0.9", xlab="x", ylab="y")
after = debest(random_before, matrix(random_before[n_best,], nrow=1))
n_best_after = which.min(myfun(after))
points(after[,1], after[,2], pch="*", col="black")
points(after[n_best_after,1],after[n_best_after,2], pch="O", col="green")
points(-1,-1, pch="x", col="red")
dev.off()

png("mid1.png", width = 500, height=500)
contour(x,y, matrix(myfun(v),nrow=length(x)), main="mid/1; F = 0.9", xlab="x", ylab="y")
after = demid(random_before, colMeans(random_before))
n_best_after = which.min(myfun(after))
points(after[,1], after[,2], pch="*", col="black")
points(after[n_best_after,1],after[n_best_after,2], pch="O", col="green")
points(-1,-1, pch="x", col="red")
dev.off()

png("current-to-rand.png", width = 500, height=500)
contour(x,y, matrix(myfun(v),nrow=length(x)), main="current-to-rand/1; F = 0.9; K = 0.5", xlab="x", ylab="y")
after = decurrentrand(random_before)
n_best_after = which.min(myfun(after))
points(after[,1], after[,2], pch="*", col="black")
points(after[n_best_after,1],after[n_best_after,2], pch="O", col="green")
points(-1,-1, pch="x", col="red")
dev.off()

png("current-to-best.png", width = 500, height=500)
contour(x,y, matrix(myfun(v),nrow=length(x)), main="current-to-best/1; F = 0.9; K = 0.5", xlab="x", ylab="y")
after = decurrentbest(random_before, matrix(random_before[n_best,], nrow=1))
n_best_after = which.min(myfun(after))
points(after[,1], after[,2], pch="*", col="black")
points(after[n_best_after,1],after[n_best_after,2], pch="O", col="green")
points(-1,-1, pch="x", col="red")
dev.off()

png("rand-to-best.png", width = 500, height=500)
contour(x,y, matrix(myfun(v),nrow=length(x)), main="rand-to-best/1; F = 0.9; K = 0.5", xlab="x", ylab="y")
after = derandbest(random_before, matrix(random_before[n_best,], nrow=1))
n_best_after = which.min(myfun(after))
points(after[,1], after[,2], pch="*", col="black")
points(after[n_best_after,1],after[n_best_after,2], pch="O", col="green")
points(-1,-1, pch="x", col="red")
dev.off()

png("current-to-pbest.png", width = 500, height=500)
contour(x,y, matrix(myfun(v),nrow=length(x)), main="current-to-pbest/1; F = 0.9; p = 0.01", xlab="x", ylab="y")
after = decurrentpbest(random_before, 0.01)
n_best_after = which.min(myfun(after))
points(after[,1], after[,2], pch="*", col="black")
points(after[n_best_after,1],after[n_best_after,2], pch="O", col="green")
points(-1,-1, pch="x", col="red")
dev.off()


