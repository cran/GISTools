#Original code by Chris Brunsdon
#Edited by Binbin Lu

sdCuts <- function(x,n = 5,params = NA) 
{
	balance <- function (x) x - mean(x)
	grads = balance(1:(n - 1))
	res = mean(x)+grads*sd(x) 
	names(res) = paste(grads,"SD",sep="") 
	res 
}

