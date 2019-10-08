#' Interface to Azure Computer Vision API
#'
#' @param endpoint A computer vision endpoint.
#' @param image An image to be sent to the endpoint. This can be either a filename, a publicly accessible URL, or a raw vector holding the file contents.
#' @param domain For `analyze`, which builtin model to use to analyze the image.
#' @param options For `analyze`, a list of optional queries to obtain a broader analysis.
#' @param language For `tag` and `read_text`, a 2-character code indicating the language to use. The default is `en`, for English.
#' @param detect_orientation For `read_text`, whether to automatically determine the image's orientation.
#' @param width,height For `make_thumbnail`, The dimensions for the returned thumbnail.
#' @param smart_crop For `make_thumbnail`, whether to automatically determine the best location to crop for the thumbnail. Useful when the aspect ratio of the original image and the thumbnail don't match.
#' @param outfile For `make_thumbnail`, an optional filename for the generated thumbnail. If not provided, the thumbnail is returned as a raw vector.
#' @param ... Arguments passed to lower-level functions, and ultimately to `call_cognitive_endpoint`.
#' @rdname computervision
#' @export
analyze <- function(endpoint, image, domain=NULL, options=list(), ...)
{
    body <- image_to_body(image)

    op <- if(is.null(domain))
        "analyze"
    else file.path("models", domain, "analyze")

    res <- call_cognitive_endpoint(endpoint, op, body=body, options=options, ..., http_verb="POST")
    as_vision_response(res)
}


#' @rdname computervision
#' @export
describe <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "describe", body=body, ..., http_verb="POST")
    as_vision_response(res)
}


#' @rdname computervision
#' @export
detect_objects <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "detect", body=body, ..., http_verb="POST")
    as_vision_response(res)
}


#' @rdname computervision
#' @export
detect_faces <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "analyze", body=body, options=list(visualFeatures="faces"), ...,
                                   http_verb="POST")
    as_vision_response(res)
}


#' @rdname computervision
#' @export
area_of_interest <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "areaOfInterest", body=body, ..., http_verb="POST")
    as_vision_response(res)
}


#' @rdname computervision
#' @export
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


#' @rdname computervision
#' @export
categorize <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "analyze", body=body, options=list(visualFeatures="categories"), ...,
                                   http_verb="POST")
    cats <- as_vision_response(res)$categories
    do.call(rbind.data.frame, c(cats, stringsAsFactors=FALSE))
}


#' @rdname computervision
#' @export
read_text <- function(endpoint, image, detect_orientation=TRUE, language="en", ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "ocr", body=body,
                                   options=list(detectOrientation=detect_orientation, language=language), ...,
                                   http_verb="POST")
    as_vision_response(res)
}


#' @rdname computervision
#' @export
list_builtin_models <- function(endpoint)
{
    res <- call_cognitive_endpoint(endpoint, "models")
    as_vision_response(res)
}


#' @rdname computervision
#' @export
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
    structure(res, class="computervision_response")
}


print.computervision_response <- function(x, ...)
{
    nms <- names(x)
    meta_nms <- c("requestId", "metadata")
    meta <- nms %in% meta_nms

    print(x[!meta])
    invisible(x)
}
