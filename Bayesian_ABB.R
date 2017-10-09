no.of.draws = 200000

#generate random priors for A and B considering our data follows a uniform distribution
prior = data.frame(pA = runif(no.of.draws, 0, 1), pB = runif(no.of.draws, 0, 1))

#This function will generate 1 random number with size of 32 and with probabilities from above
#for both version A and B 
generative.model = function(pA, pB){
  conversionA = rbinom(1, 32, pA)
  conversionB = rbinom(1, 32, pB)
  c(conversionA = conversionA, conversionB = conversionB)
}

#call generative.model for 200000 draws and create a dataframe containing the randomly
#generated values for A and B
simulate.data = as.data.frame(t(sapply(1:no.of.draws, function(i){
  generative.model(prior$pA[i], prior$pB[i])
})))

#now we will generate the posterior probabilities based on the values that match the number of clicks for 
#version A and B
posterior = prior[simulate.data$conversionA == 8 & simulate.data$conversionB == 16, ]

hist(posterior$pB - posterior$pA)
abline(v = mean(posterior$pB - posterior$pA), col= "red", lwd = 3)



#Let us check based on the posterior which version will earn more profits
profitA = -100 + posterior$pA * 1000
profitB = -400 + posterior$pB * 1000

hist(profitA - profitB, breaks = 50)
expected.profit = mean(profitA - profitB)
abline(v = expected.profit, col= "red", lwd = 3)