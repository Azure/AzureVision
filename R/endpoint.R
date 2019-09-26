vision_endpoint <- function(url, key=NULL, aad_token=NULL, cognitive_token=NULL)
{
    obj <- cognitive_endpoint(url, service_type="ComputerVision", key=key, aad_token=aad_token,
                              cognitive_token=cognitive_token)
    class(object) <- c("vision_endpoint", class(obj))
    object
}


customvision_endpoint <- function(url, key=NULL, aad_token=NULL, cognitive_token=NULL)
{
    obj <- cognitive_endpoint(url, service_type="CustomVision", key=key, aad_token=aad_token,
                              cognitive_token=cognitive_token)
    class(object) <- c("customvision_endpoint", class(obj))
    object
}


is_any_uri <- function(string)
{
    uri <- httr::parse_url(string)
    !is.null(uri$scheme) && !is.null(uri$hostname)
}
