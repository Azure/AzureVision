vision_endpoint <- function(url, type, key=NULL, aad_token=NULL, cognitive_token=NULL)
{
    type <- normalize_cognitive_type(type)
    url <- httr::parse_url(url)
    url$path <- get_api_path(type)

    object <- list(url=url, key=key, aad_token=aad_token, cognitive_token=cognitive_token)
    class(object) <- c("vision_endpoint", "cognitive_endpoint")

    object
}

