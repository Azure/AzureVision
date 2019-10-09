#' Interface to Azure Computer Vision API
#'
#' @param endpoint A computer vision endpoint.
#' @param image An image to be sent to the endpoint. This can be either a filename, a publicly accessible URL, or a raw vector holding the file contents.
#' @param domain For `analyze`, an optional domain-specific model to use to analyze the image. Can be "celebrities" or "landmarks".
#' @param feature_types For `analyze`, an optional character vector of more detailed features to return. This can be one or more of: "categories", "tags", "description", "faces", "imagetype", "color", "adult", "brands" and "objects".
#' @param language A 2-character code indicating the language to use for tags, feature labels and descriptions. The default is `en`, for English.
#' @param detect_orientation For `read_text`, whether to automatically determine the image's orientation.
#' @param width,height For `make_thumbnail`, the dimensions for the returned thumbnail.
#' @param smart_crop For `make_thumbnail`, whether to automatically determine the best location to crop for the thumbnail. Useful when the aspect ratios of the original image and the thumbnail don't match.
#' @param outfile For `make_thumbnail`, an optional filename for the generated thumbnail. If not provided, the thumbnail is returned as a raw vector.
#' @param ... Arguments passed to lower-level functions, and ultimately to `call_cognitive_endpoint`.
#' @details
#' `analyze` extracts visual features from the image. To obtain more detailed features, specify the `domain` and/or `feature_types` arguments as appropriate.
#'
#' `describe` returns a short text description of the image.
#'
#' `detect_objects` detects objects in the image. It returns a list of bounding boxes indicating the locations of the detected objects.
#'
#' `detect_faces` detects the presence or absence of a human face in the image.
#'
#' `area_of_interest` attempts to find the "interesting" part of an image, meaning the most likely location of the image's subject.
#'
#' `tag` returns a set of words that are relevant to the content of the image. Not to be confused with the [`add_tags`] or [`tag_uploaded_images`] functions that are part of the Custom Vision API.
#'
#' `categorize` attempts to place the image into a list of predefined categories.
#'
#' `read_text` performs optical character recognition (OCR) on the image.
#'
#' `list_domains` returns the predefined domain-specific models that can be queried by `analyze` for deeper analysis. Not to be confused with the domains available for training models with the Custom Vision API.
#'
#' `make_thumbnail` generates a thumbnail of the image, with the specified dimensions.
#' @return
#' `make_thumbnail` returns a raw vector holding the contents of the thumbnail, if the `outfile` argument is NULL. `tag` and `categorize` return a data frame. The others return an object of class `computervision_response`, which is a simple wrapper class for the response from the API endpoint to allow pretty-printing.
#' @seealso
#' [`computervision_endpoint`], [`AzureCognitive::call_cognitive_endpoint`]
#'
#' [Computer Vision documentation](https://docs.microsoft.com/en-us/azure/cognitive-services/Computer-vision/Home)
#' @aliases computervision
#' @rdname computervision
#' @export
analyze <- function(endpoint, image, domain=NULL, feature_types=NULL, language="en", ...)
{
    body <- image_to_body(image)
    if(!is_empty(feature_types))
        feature_types <- paste0(feature_types, collapse="")
    options <- list(detail=domain, language=language, visualFeatures=feature_types)

    res <- call_cognitive_endpoint(endpoint, "analyze", body=body, options=options, ..., http_verb="POST")
    as_vision_response(res)
}


#' @rdname computervision
#' @export
describe <- function(endpoint, image, language="en", ...)
{
    body <- image_to_body(image)
    options <- list(language=language)
    res <- call_cognitive_endpoint(endpoint, "describe", body=body, options=options, ..., http_verb="POST")
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
list_domains <- function(endpoint)
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
        readBin(image, "raw", file.size(image))
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
