#' brick your nc file
#'
#' @description \code{\link{adt}} calculates ADT based on hourly traffic data. The
#' input traffic data is usually for morning rush hours.
#'
#' @param pc numeric vector for passenger cars
#' @param lcv numeric vector for light commercial vehicles
#' @param hgv numeric vector for heavy good vehicles or trucks
#' @param bus numeric vector for bus
#' @param mc numeric vector for motorcycles
#' @param p_pc data-frame profile for passenger cars, 24 hours only.
#' @param p_lcv data-frame profile for light commercial vehicles, 24 hours only.
#' @param p_hgv data-frame profile for heavy good vehicles or trucks, 24 hours only.
#' @param p_bus data-frame profile for bus, 24 hours only.
#' @param p_mc data-frame profile for motorcycles, 24 hours only.
#' @param expanded boolean argument for returning numeric vector or "Vehicles"
#' @return numeric vector of total volume of traffic per link, or data-frames
#' of expanded traffic
#' @importFrom units as_units
#' @export
#' @examples {
#' data(net)
#' }
bricknc <- function(nc, array = FALSE, var, z = 6,
                    num_param_coords = c(xmin = -150,
                                     xmax = 10,
                                     ymin = -60,
                                     ymax = 40,
                                     dx = 215,
                                     dy = 135),
                    char_param_coords,

                    verbose = TRUE){
  f <- ncdf4::nc_open(filename = nc)
  if(verbose){
    cat(paste("The file has",f$nvars,"variables:\n"))
    cat(names(f$var))
  }

  if(missing(var)){
    choice <- utils::menu(names(f$var), title="Choose var")
    nvar <- names(f$var)[choice]
    u <- ncdf4::ncvar_get(f, nvar)
    cat(paste0("\nThe '", nvar, "' array is ",
               format(object.size(u), units = "Mb"), "\n"))
  } else {
    u <- ncdf4::ncvar_get(f, var)
    if(verbose){
      cat(paste0("\nThe '", var, "' array is ",
                 format(object.size(u), units = "Mb"), "\n"))
    }
  }
  if(array){
    return(u)
  }
  a <- function(x) ifelse(x < 0, - 1, 1)
  lon <- seq(xmin, xmax, by = length( (xmin - a(xmin)):xmax)/dx)
  lat <- seq(ymin, ymax, by = length((ymin - a(ymin)):ymax)/dy)

  lista <- list()
  for (i in seq(1,dim(u)[4])) {
    lista[[i]] <- raster::raster(t(u[1:dim(u)[1],
                                     dim(u)[2]:1,
                                     z,
                                     i]),
                                 xmn = min(lon),xmx = max(lon),
                                 ymn = min(lat),ymx = max(lat),
                                 crs="+init=epsg:4326")
  }
  ru1 <- raster::flip(raster::brick(lista), direction = "y")
  if(verbose){
    cat(paste0("\nThis brick is ", format(object.size(ru1), units = "Mb")))
  }
  return(ru1)
}
