#' Interface to Azure Computer Vision API
#'
#' @param endpoint A computer vision endpoint.
#' @param image An image to be sent to the endpoint. This can be either a filename, a publicly accessible URL, or a raw vector holding the file contents.
#' @param domain For `analyze`, an optional domain-specific model to use to analyze the image. Can be "celebrities" or "landmarks".
#' @param feature_types For `analyze`, an optional character vector of more detailed features to return. This can be one or more of: "categories", "tags", "description", "faces", "imagetype", "color", "adult", "brands" and "objects". If not supplied, defaults to "categories".
#' @param language A 2-character code indicating the language to use for tags, feature labels and descriptions. The default is `en`, for English.
#' @param detect_orientation For `read_text`, whether to automatically determine the image's orientation.
#' @param width,height For `make_thumbnail`, the dimensions for the returned thumbnail.
#' @param smart_crop For `make_thumbnail`, whether to automatically determine the best location to crop for the thumbnail. Useful when the aspect ratios of the original image and the thumbnail don't match.
#' @param outfile For `make_thumbnail`, the filename for the generated thumbnail. Alternatively, if this is NULL the thumbnail is returned as a raw vector.
#' @param ... Arguments passed to lower-level functions, and ultimately to `call_cognitive_endpoint`.
#' @details
#' `analyze` extracts visual features from the image. To obtain more detailed features, specify the `domain` and/or `feature_types` arguments as appropriate.
#'
#' `describe` attempts to provide a text description of the image.
#'
#' `detect_objects` detects objects in the image.
#'
#' `area_of_interest` attempts to find the "interesting" part of an image, meaning the most likely location of the image's subject.
#'
#' `tag` returns a set of words that are relevant to the content of the image. Not to be confused with the [`add_tags`] or [`add_image_tags`] functions that are part of the Custom Vision API.
#'
#' `categorize` attempts to place the image into a list of predefined categories.
#'
#' `read_text` performs optical character recognition (OCR) on the image.
#'
#' `list_domains` returns the predefined domain-specific models that can be queried by `analyze` for deeper analysis. Not to be confused with the domains available for training models with the Custom Vision API.
#'
#' `make_thumbnail` generates a thumbnail of the image, with the specified dimensions.
#' @return
#' `analyze` returns a list containing the results of the analysis. The components will vary depending on the domain and feature types requested.
#'
#' `describe` returns a list with two components: `tags`, a vector of text labels; and `captions`, a data frame of descriptive sentences.
#'
#' `detect_objects`  returns a dataframe giving the locations and types of the detected objects.
#'
#' `area_of_interest` returns a length-4 numeric vector, containing the top-left coordinates of the area of interest and its width and height.
#'
#' `tag` and `categorize` return a data frame of tag and category information, respectively.
#'
#' `read_text` returns the extracted text as a list with one component per region that contains text. Each component is a vector of character strings.
#'
#' `list_computervision_domains` returns a character vector of domain names.
#'
#' `make_thumbnail` returns a raw vector holding the contents of the thumbnail, if the `outfile` argument is NULL. Otherwise, the thumbnail is saved into `outfile`.
#'
#' @seealso
#' [`computervision_endpoint`], [`AzureCognitive::call_cognitive_endpoint`]
#'
#' [Computer Vision documentation](https://docs.microsoft.com/en-us/azure/cognitive-services/Computer-vision/Home)
#' @examples
#' \dontrun{
#'
#' vis <- computervision_endpoint(
#'     url="https://accountname.cognitiveservices.azure.com/",
#'     key="account_key"
#' )
#'
#' list_domains(vis)
#'
#' # analyze a local file
#' analyze(vis, "image.jpg")
#' # picture on the Internet
#' analyze(vis, "https://example.com/image.jpg")
#' # as a raw vector
#' analyze(vis, readBin("image.jpg", "raw", file.size("image.jpg")))
#'
#' # analyze has optional extras
#' analyze(vis, "image.jpg", feature_types=c("faces", "objects"))
#'
#' describe(vis, "image.jpg")
#' detect_objects(vis, "image.jpg")
#' area_of_interest(vis, "image.jpg")
#' tag(vis, "image.jpg")  # more reliable than analyze(*, feature_types="tags")
#' categorize(vis, "image.jpg")
#' read_text(vis, "scanned_text.jpg")
#'
#' }
#' @aliases computervision
#' @rdname computervision
#' @export
analyze <- function(endpoint, image, domain=NULL, feature_types=NULL, language="en", ...)
{
    body <- image_to_body(image)
    if(!is_empty(feature_types))
        feature_types <- paste0(feature_types, collapse=",")
    options <- list(details=domain, language=language, visualFeatures=feature_types)

    res <- call_cognitive_endpoint(endpoint, "analyze", body=body, options=options, ..., http_verb="POST",
                                   simplifyVector=TRUE)

    res[!(names(res) %in% c("requestId", "metadata"))]
}


#' @rdname computervision
#' @export
describe <- function(endpoint, image, language="en", ...)
{
    body <- image_to_body(image)
    options <- list(language=language)
    res <- call_cognitive_endpoint(endpoint, "describe", body=body, options=options, ..., http_verb="POST",
                                   simplifyVector=TRUE)
    res$description
}


#' @rdname computervision
#' @export
detect_objects <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "detect", body=body, ..., http_verb="POST", simplifyVector=TRUE)
    res$objects
}


#' @rdname computervision
#' @export
area_of_interest <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "areaOfInterest", body=body, ..., http_verb="POST", simplifyVector=TRUE)
    unlist(res$areaOfInterest)
}


#' @rdname computervision
#' @export
tag <- function(endpoint, image, language="en", ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "tag", body=body, options=list(language=language), ..., http_verb="POST")

    # hint not always present, so need to construct data frame manually
    do.call(rbind.data.frame, c(lapply(res$tags, function(x)
    {
        if(is.null(x$hint))
            x$hint <- NA_character_
        x
    }), stringsAsFactors=FALSE, make.row.names=FALSE))
}


#' @rdname computervision
#' @export
categorize <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "analyze", body=body, options=list(visualFeatures="categories"), ...,
                                   http_verb="POST", simplifyVector=TRUE)
    res$categories
}


#' @rdname computervision
#' @export
read_text <- function(endpoint, image, detect_orientation=TRUE, language="en", ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "ocr", body=body,
                                   options=list(detectOrientation=detect_orientation, language=language), ...,
                                   http_verb="POST")

    lapply(res$regions, function(region)
    {
        sapply(region$lines, function(line)
        {
            w <- sapply(line$words, `[[`, "text")
            paste(w, collapse=" ")
        })
    })
}


#' @rdname computervision
#' @export
list_computervision_domains <- function(endpoint, ...)
{
    res <- call_cognitive_endpoint(endpoint, "models", ..., simplifyVector=TRUE)
    res$models$name
}


#' @rdname computervision
#' @export
make_thumbnail <- function(endpoint, image, outfile, width=50, height=50, smart_crop=TRUE, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "generateThumbnail", body=body,
                                   options=list(width=width, height=height, smartCropping=smart_crop), ...,
                                   http_verb="POST")
    if(!is.null(outfile))
        writeBin(res, outfile)
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
