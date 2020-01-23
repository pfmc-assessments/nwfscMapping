#' Determine Distance Between Two Points
#' 
#' Calculate the distance between two points based on their latitude and
#' longitude.
#' 
#' @param long1 The longitude for point one.
#' @param lat1 The latitude for point one.
#' @param long2 The longitude for point two.
#' @param lat2 The latitude for point two.
#' @export
distance <-
function(long1, lat1, long2, lat2){
    sqrt(((long2 - long1) * .long.to.km((lat1 + lat2)/2))^2 + ((lat2 - lat1) * .lat.to.km((lat1 + lat2)/2))^2)
}
