#' Add, list and remove images for a project
#'
#' @param project A Custom Vision project.
#' @param images For `add_images`, the images to add (upload) to the project.
#' @param image_ids For `remove_images`, the IDs of the images to remove from the project.
#' @param tags Optional tags to add to the images. Only for classification projects.
#' @param regions Optional list of regions in the images that contain objects. Only for object detection projects.
#' @param include For `list_images`, which images to include in the list: untagged, tagged, or both (the default).
#' @param as For `list_images`, the return value: a vector of image IDs, a data frame of image metadata, or a list of metadata.
#' @param iteration For `list_images`, the iteration ID (roughly, which model generation to use). Defaults to the latest iteration.
#' @param confirm For `remove_images`, whether to ask for confirmation first.
#' @param ... Arguments passed to lower-level functions.
#' @details
#' The images to be uploaded can be specified as:
#' - A vector of local filenames. JPG, PNG and GIF file formats are supported.
#' - A vector of publicly accessible URLs.
#' - A raw vector, or a list of raw vectors, holding the binary contents of the image files.
#'
#' Uploaded images can also have _tags_ added (for a classification project) or _regions_ (for an object detection project). Classification tags can be specified in the following ways:
#' - As a single character string. In this case, the tag will be applied to all image IDs.
#' - As a vector of strings, with length equal to the length of `image_ids`. The tags will be applied to the images in order.
#' - As a _list_ of vectors of strings, with the length of the list equal to the length of `image_ids`. Each vector in the list contains the tags to be assigned to the corresponding image.
#'
#' Object detection projects also have tags, but they are specified as part of the `regions` argument. The regions to add should be specified as a list of data frames, with one data frame per image. Each data frame should have one row per region, and the following columns:
#' - `left`, `top`, `width`, `height`: the location and dimensions of the region bounding box, normalised to be between 0 and 1.
#' - `tag`: the name of the tag to associate with the region.
#' Any other columns in the data frame will be ignored.
#'
#' Note that once uploaded, images are identified only by their ID; there is no general link back to the source filename or URL.
#' @return
#' For `add_images`, the vector of IDs of the uploaded images.
#'
#' For `list_images`, based on the value of the `as` argument. The default is a vector of image IDs; `as="list"` returns a (nested) list of image metadata with one component per image; and `as="dataframe"` returns the same metadata but reshaped into a data frame.
#'
#' For `remove_images`, NULL on successful removal.
#' @seealso
#' [`add_image_tags`] and [`add_image_regions`] to add tags and regions to images, if not done at upload time
#'
#' [`add_tags`], [`list_tags`], [`remove_tags`]
#'
#' [`customvision_project`]
#' @aliases customvision_images
#' @rdname customvision_images
#' @export
add_images <- function(project, ...)
{
    UseMethod("add_images")
}


#' @rdname customvision_images
#' @export
add_images.classification_project <- function(project, images, tags=NULL, ...)
{
    img_ids <- add_images_internal(project, images)
    if(!is_empty(tags))
        add_image_tags(project, img_ids, tags)
    img_ids
}


#' @rdname customvision_images
#' @export
add_images.object_detection_project <- function(project, images, regions=NULL, ...)
{
    img_ids <- add_images_internal(project, images)
    if(!is_empty(regions))
        add_image_regions(project, img_ids, regions)
    img_ids
}


add_images_internal <- function(project, images)
{
    bodies <- images_to_bodies(images)
    src_names <- names(bodies)
    op <- if(is.null(bodies[[1]]$contents)) "images/urls" else "images/files"

    lst <- list()
    while(!is_empty(bodies))
    {
        idx <- seq_len(min(length(bodies), 64))
        res <- do_training_op(project, op, body=list(images=unname(bodies[idx])), http_verb="POST")
        if(!res$isBatchSuccessful)
            stop("Not all images were successfully added", call.=FALSE)

        bodies <- bodies[-idx]
        lst <- c(lst, res$images)
    }

    # need to reorder uploading result to match original image vector
    srcs <- sapply(lst, `[[`, "sourceUrl")
    lst <- lapply(lst[match(src_names, srcs)], `[[`, "image")

    img_ids <- sapply(lst, function(x) x$id)
}


#' @rdname customvision_images
#' @export
list_images <- function(project, include=c("all", "tagged", "untagged"), as=c("ids", "dataframe", "list"),
                        iteration=NULL)
{
    get_paged_list <- function(op)
    {
        skip <- 0
        lst <- list()
        repeat
        {
            opts <- list(iterationId=iteration, take=256, skip=skip)
            res <- do_training_op(project, op, options=opts, simplifyVector=simplify)
            if(is_empty(res))
                break

            skip <- skip + 256
            lst <- if(as == "ids")
                c(lst, res$id)
            else c(lst, list(res))
        }
        switch(as,
            ids=unlist(lst),
            dataframe=do.call(rbind, lst),
            list=lst
        )
    }

    include <- match.arg(include)
    as <- match.arg(as)
    simplify <- as != "list"

    tagged_imgs <- if(include != "untagged") get_paged_list("images/tagged") else NULL
    untagged_imgs <- if(include != "tagged") get_paged_list("images/untagged") else NULL

    if(as == "ids")
        as.character(c(tagged_imgs, untagged_imgs))
    else if(as == "dataframe")
    {
        if(is.data.frame(untagged_imgs) && nrow(untagged_imgs) > 0)
        {
            untagged_imgs$tags <- NA
            if(!is.null(tagged_imgs$regions))
                untagged_imgs$regions <- NA
        }
        rbind.data.frame(tagged_imgs, untagged_imgs)
    }
    else c(tagged_imgs, untagged_imgs)
}


#' @rdname customvision_images
#' @export
remove_images <- function(project, image_ids=list_images(project, "untagged", as="ids"), confirm=TRUE)
{
    if(!confirm_delete("Are you sure you want to remove images from the project?", confirm))
        return(invisible(project))

    while(!is_empty(image_ids))
    {
        idx <- seq_len(min(length(image_ids), 256))
        image_batch <- paste0(image_ids[idx], collapse=",")
        do_training_op(project, "images", options=list(imageIds=image_batch), http_verb="DELETE")
        image_ids <- image_ids[-idx]
    }

    invisible(NULL)
}


#' View images uploaded to a Custom Vision project
#'
#' @param project A Custom Vision project.
#' @param img_ids The IDs of the images to view. You can use [`list_images`] to get the image IDs for this project.
#' @param which Which image to view: the resized version used for training (the default), the original uploaded image, or the thumbnail.
#' @param max_images The maximum number of images to display.
#' @param iteration The iteration ID (roughly, which model generation to use). Defaults to the latest iteration.
#' @details
#' Images in a Custom Vision project are stored in Azure Storage. This function gets the URLs for the uploaded images and displays them in your browser.
#' @seealso
#' [`list_images`]
#' @export
browse_images <- function(project, img_ids, which=c("resized", "original", "thumbnail"), max_images=20,
                          iteration=NULL)
{
    if(length(img_ids) > max_images)
    {
        warning("Only the first ", max_images, " images displayed", call.=FALSE)
        img_ids <- img_ids[seq_len(max_images)]
    }

    opts <- list(
        imageIds=paste0(img_ids, collapse=","),
        iterationId=iteration
    )
    res <- do_training_op(project, "images/id", options=opts, simplifyDataFrame=TRUE)

    img_urls <- switch(match.arg(which),
        resized=res$resizedImageUri,
        original=res$originalImageUri,
        thumbnail=res$thumbnailUri
    )

    lapply(img_urls, httr::BROWSE)
    invisible(NULL)
}


# vectorised form of image_to_body, checks that all images are raw/filename/URL
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
    if(is.raw(images) || (is.list(images) && all(sapply(images, is.raw))))
        return("raw")

    if(all(file.exists(images) & !dir.exists(images)))
        return("files")

    if(all(sapply(images, is_any_uri)))
        return("urls")

    stop("All image inputs must be of the same type: filenames, URLs or raw vectors", call.=FALSE)
}
