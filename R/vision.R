analyze <- function(endpoint, image, domain=NULL, options=list(), ...)
{
    body <- image_to_body(image)

    op <- if(is.null(domain))
        "analyze"
    else file.path("models", domain, "analyze")

    res <- call_cognitive_endpoint(endpoint, op, body=body, options=options, ..., http_verb="POST")
    as_vision_response(res)
}


describe <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "describe", body=body, ..., http_verb="POST")
    as_vision_response(res)
}


detect_objects <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "detect", body=body, ..., http_verb="POST")
    as_vision_response(res)
}


detect_faces <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "analyze", body=body, options=list(visualFeatures="faces"), ...,
                                   http_verb="POST")
    as_vision_response(res)
}


area_of_interest <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "areaOfInterest", body=body, ..., http_verb="POST")
    as_vision_response(res)
}


tag <- function(endpoint, image, language="en", ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "tag", body=body, options=list(language=language), ..., http_verb="POST")
    tags <- as_vision_response(res)$tags
    do.call(rbind.data.frame, c(lapply(tags, function(x)
    {
        if(is.null(x$hint))
            x$hint <- NA_character_
        x
    }), stringsAsFactors=FALSE))
}


categorize <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "analyze", body=body, options=list(visualFeatures="categories"), ...,
                                   http_verb="POST")
    cats <- as_vision_response(res)$categories
    do.call(rbind.data.frame, c(cats, stringsAsFactors=FALSE))
}


read_text <- function(endpoint, image, detect_orientation=TRUE, language="en", ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "ocr", body=body,
                                   options=list(detectOrientation=detect_orientation, language=language), ...,
                                   http_verb="POST")
    as_vision_response(res)
}


list_models <- function(endpoint)
{
    res <- call_cognitive_endpoint(endpoint, "models")
    as_vision_response(res)
}


make_thumbnail <- function(endpoint, image, width, height, smart_crop=TRUE, ..., outfile=NULL)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "generateThumbnail", body=body,
                                   options=list(width=width, height=height, smartCropping=smart_crop), ...,
                                   http_verb="POST")
    if(!is.null(outfile))
        writeBin(outfile, res)
    else res
}


image_to_body <- function(image)
{
    if(is.raw(image))
        image
    else if(file.exists(image))
        readBin(image, "raw", file.info(image)$size)
    else if(is_any_uri(image))
        list(url=image)
    else stop("Could not find image", call.=FALSE)

}


as_vision_response <- function(res)
{
    structure(res, class="computer_vision_response")
}


print.computer_vision_response <- function(x, ...)
{
    nms <- names(x)
    meta_nms <- c("requestId", "metadata")
    meta <- nms %in% meta_nms

    print(x[!meta])
    invisible(x)
}
