#Original code by Chris Brunsdon
#Edited by Binbin Lu

shading <- function(breaks,cols=brewer.pal(length(breaks),'Reds')) 
{
  res=list(breaks=breaks,cols=cols)
	class(res) = 'shading'
	res
}

