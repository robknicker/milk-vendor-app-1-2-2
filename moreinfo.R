mean = 7
sd = 2
cost = 1.5
sell = 2.2
undercost = sell - cost
overcost = cost
exp = 7
var = sd^2
meanexp = mean*exp
varexp = exp^2*var
sdexp = sqrt(varexp)
q = undercost/(overcost+undercost)
opt1 = qnorm(q,meanexp,sdexp)
opt=round(opt1)

yearlyprofit = (365/exp)*opt*(sell-cost)

probperf <- (pnorm(opt+.5, meanexp, sdexp)-pnorm(opt-.5, meanexp, sdexp))

probs <- c()
opportunitycosts <- matrix()
overstockcosts<- matrix()
allcosts<- matrix(nrow=2*opt,ncol=2*opt)

for (i in 0:(2*opt)){
  probs[i] <- pnorm(i+.5, meanexp, sdexp)-pnorm(i-.5, meanexp, sdexp)
}

for (i in 0:(2*opt)){
  for (j in 0:i) {
    allcosts[i,j] <- overcost*(i-j)
  }
  for (j in (i):(2*opt)) {
    allcosts[i,j] <- undercost*(j-i)
  }
}

costs <- c()

for (i in 0:(2*opt)){
  costs[i] = sum(allcosts[i,]*probs)
}

yearlycosts <- costs*365/exp
