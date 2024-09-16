#Original code by Chris Brunsdon
#Edited by Binbin Lu

choropleth <-function(sp,v,shading,...) 
{	
	if(missing(v))
		   stop("v is not allowing to be missing for mapping")
	if(inherits(sp, "Spatial"))
	{
	    var.data <- sp[[v]]
		if(missing(shading))
		   shading = auto.shading(var.data)
		i = shading$cols[1+findInterval(sp@data[,v],shading$breaks)]
		plot(sp,col=i,...) 
	}
	else
	{
		var.data <- sp[[v]]
		if(missing(shading))
		   shading = auto.shading(var.data)
		i = shading$cols[1+findInterval(var.data,shading$breaks)]
	    plot(st_geometry(sp),col=i,...) 
	}
	
}