#Original code by Chris Brunsdon
#Edited by Binbin Lu

auto.shading <- function(x,digits=2,cutter=quantileCuts,n=5,params=NA,cols=brewer.pal(n,'Reds'))
{
	brk = cutter(x,n=n,params=params)
	if (!is.na(digits)) brk = signif(brk,digits=digits)
	brk = sort(brk)
	brk = brk[!duplicated(brk)]
	res=list(breaks=brk,cols=cols)
	class(res) = 'shading' 
	res
}

poly.outer <- function (exo.object, input.poly, extend = 0) 
{
    spdf <- FALSE
    if(inherits(exo.object, "Spatial"))
    {
        p4s <- CRS(proj4string(exo.object))
        exo.object <- st_as_sf(exo.object)
        spdf <- TRUE
    }
    if(inherits(input.poly, "Spatial"))
        input.poly <- st_as_sf(input.poly)
    box <- function(obj, extend) {
        xy = st_bbox(obj)
        x = xy[c(1,3)] + c(-extend, extend)
        y = xy[c(2,4)] + c(-extend, extend)
        st_polygon(list(cbind(x[c(1, 1, 2, 2,1)], y[c(1, 2, 2, 1,1)])))
    }
    choppo <- function(obj1, obj2, extend) {
        res = box(obj1, extend)
        res = st_difference(res, st_union(obj2))
    }
    res = choppo(exo.object, input.poly, extend)
    #proj4string(res) = 
    if(spdf)
    {
        res <- as(res, "Spatial")
        proj4string(res) = p4s
    }
    res
}

add.masking <- function (maskPoly, color = "white")
{
  plot(maskPoly, col = color, border = color, usePolypath=TRUE, add = TRUE)
}
 
