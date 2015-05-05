library("DEoptim")
miss = 0
x = DEoptim(mygoal,
            lower = rep(-100,30),
            upper=rep(100,30),
            control = DEoptim.control(F=0.9, CR=0.9, NP=300, itermax = 1000, strategy=1),
            fnMap=function(p){
              RF_TRUNCATE[[1]](p, c(-100,100)) })
print(miss)
mygoal = function(x)
{
  if (is.null(dim(x)) || dim(x)[1] == 1)
  {
    sum((x)^2)
  }
  else
  {
    apply(x,1,function(r) {sum((r)^2)})
  }
}

mygoal_chamsko = function(x)
{
  acc = 0
  for(i in 1:30)
    acc = acc + (x[i]*x[i])
  return(acc)
}

de2 = function(f, d, Fv, cr, np, iter, range)
{
  X = initialize_unif(np, d, range)
  off = 0
  swp = 0
  for (i in 1:iter)
  {
#    print(min(f(X)))
    trios = replicate(np, sample(np, 3, F))
    selected = X[trios[1,],]
    vectors = Fv * (X[trios[2,],] - X[trios[3,],])
    V = selected + vectors
    mod_cr = matrix(runif(np*d) < cr, nrow=np)
    swp = swp + sum(mod_cr)
    Z = V*mod_cr + X*(1-mod_cr)
    off = off + sum((Z > max(range)) + (Z < min(range)))
    Z = RF_TRUNCATE[[1]](Z, range)
    mod = f(Z) <= f(X)
    X = mod*Z + (1-mod)*X
  }
#  print(min(f(X)))
#  print(swp)
#  print(off)
  return(min(f(X)))
}

de2chamsko = function(f, d, Fv, cr, np, iter, range)
{
  X = c()
  bestv = 1e20
  besti = 0
  for (i in 1:np)
  {
    o = c()
    for(i in 1:d)
      o[i] = runif(1, range[1], range[2])
    X = rbind(X, o)
  }

  off = 0


  for (g in 1:iter)
  {
    for (i in 1:np)
    {
        E = c()
        r1 = sample(np, 1)
        while (r1 == i)
          r1 = sample(np, 1)
        r2 = sample(np, 1)
        while (r2 == i || r2 == r1)
          r2 = sample(np, 1)
        r3 = sample(1:np, 1)
        while (r3 == i || r3 == r1 || r3 == r2)
          r3 = sample(np, 1)
        for(j in 1:d) {
          if (runif(1) < cr)
          {
            E[j] = X[r1,j] + Fv * (X[r2,j] - X[r3,j])
          }
          else
          {
            E[j] = X[i,j]
          }
        }

        for (j in 1:d)
        {
          if (E[j] < range[1] || E[j] > range[2])
            off = off + 1
          if (E[j] < range[1])
            E[j] = range[1]
          if (E[j] > range[2])
            E[j] = range[2]
        }
        #print(E);
        if ((mygoal_chamsko(E)) < (mygoal_chamsko(X[i,])))
        {
          for(j in 1:d) {
            X[i,j] = E[j]
          }
          if (mygoal_chamsko(E) < bestv)
          {
            bestv = mygoal_chamsko(E)
            besti = i
          }
        }
    }
    print(bestv)
  }
  print(off)
}
