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


#' @export
describe <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "describe", body=body, ..., http_verb="POST")
    as_vision_response(res)
}


#' @export
detect_objects <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "detect", body=body, ..., http_verb="POST")
    as_vision_response(res)
}


#' @export
detect_faces <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "analyze", body=body, options=list(visualFeatures="faces"), ...,
                                   http_verb="POST")
    as_vision_response(res)
}


#' @export
area_of_interest <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "areaOfInterest", body=body, ..., http_verb="POST")
    as_vision_response(res)
}


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


#' @export
categorize <- function(endpoint, image, ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "analyze", body=body, options=list(visualFeatures="categories"), ...,
                                   http_verb="POST")
    cats <- as_vision_response(res)$categories
    do.call(rbind.data.frame, c(cats, stringsAsFactors=FALSE))
}


#' @export
read_text <- function(endpoint, image, detect_orientation=TRUE, language="en", ...)
{
    body <- image_to_body(image)
    res <- call_cognitive_endpoint(endpoint, "ocr", body=body,
                                   options=list(detectOrientation=detect_orientation, language=language), ...,
                                   http_verb="POST")
    as_vision_response(res)
}


#' @export
list_builtin_models <- function(endpoint)
{
    res <- call_cognitive_endpoint(endpoint, "models")
    as_vision_response(res)
}


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


# vectorised form of the above, checks that all images are raw/filename/URL
images_to_bodies <- function(images)
{
    type <- image_type(images)
    if(type == "raw" && is.raw(images))
        images <- list(images)

    # returned list will be named
    names(images) <- if(type == "raw") seq_along(images) else images

    switch(type,
        raw=mapply(
            function(conts, name) list(name=name, contents=conts),
            images,
            names(images),
            SIMPLIFY=FALSE
        ),

        files=mapply(
            function(f, size) list(name=f, contents=readBin(f, "raw", size)),
            images,
            file.size(images),
            SIMPLIFY=FALSE
        ),

        urls=lapply(images, function(f) list(url=f))
    )
}


image_type <- function(images)
{
    all_raw <- is.raw(images) || (is.list(images) && all(sapply(images, is.raw)))

    all_files <- if(!all_raw)
        all(file.exists(images))
    else FALSE

    all_urls <- if(!all_raw && !all_files)
        all(sapply(images, is_any_uri))
    else FALSE

    if(!all_files && !all_urls && !all_raw)
        stop("All image inputs must be of the same type: filenames, URLs or raw vectors", call.=FALSE)

    if(all_raw) "raw" else if(all_files) "files" else "urls"
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
