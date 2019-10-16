#' Add, list and remove images for a project
#'
#' @param project A Custom Vision project.
#' @param images For `add_images`, the images to add (upload) to the project.
#' @param image_ids For `remove_images`, the IDs of the images to remove from the project.
#' @param tags For `add_images.classification_project`, optional tags to add to the images.
#' @param regions For `add_images.object_detection_project`, an optional list of regions in the images that contain objects. Only used for object detection projects.
#' @param include For `list_images`, which images to include in the list: untagged, tagged, or both (the default).
#' @param as For `list_images`, the return value: a vector of image IDs, a data frame of image metadata, or a list of metadata.
#' @param iteration For `list_images`, the iteration ID (roughly, which model generation to use). Defaults to the latest iteration.
#' @param confirm For `remove_images`, whether to ask for confirmation first.
#' @param ... Arguments passed to lower-level functions.
#' @details
#' The images to be uploaded can be specified as:
#' - A vector of local filenames. All common image file formats are supported.
#' - A vector of publicly accessible URLs.
#' - A raw vector, or a list of raw vectors, holding the binary contents of the image files.
#'
#' Uploaded images can be _tagged_, which set the target labels for each image. To specify a single tag per image, set `tags` to be a vector. For multiple tags per image, `tags` should be a list, with each component being the vector of tags for the corresponding image. If `tags` has length 1, it is recycled to the length of `images` as a convenience feature for giving multiple images the same tag.
#'
#' Specifying regions is used for an object detection project. The `regions` argument should be a nested list, with each component specifying the regions for the corresponding image.
#'
#' Note that once uploaded, images are identified only by their ID; there is no general link back to the source filename or URL.
#'
#' @return
#' For `add_images`, the vector of IDs of the uploaded images.
#'
#' For `list_images`, based on the value of the `as` argument. The default is a vector of image IDs; `as="list"` returns a (nested) list of image metadata with one component per image; and `as="dataframe"` returns the same metadata but reshaped into a data frame.
#'
#' For `remove_images`, NULL on successful removal.
#' @seealso
#' [`add_image_tags`] to add tags to images if not done at upload time, [`remove_image_tags`]
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


#' View an image uploaded to a Custom Vision project
#'
#' @param project A Custom Vision project.
#' @param img_id The ID of the image. You can use [`list_images`] to get the image IDs for this project.
#' @param which Which image to view: the resized version used for training (the default), the original uploaded image, or the thumbnail.
#' @param iteration The iteration ID (roughly, which model generation to use). Defaults to the latest iteration.
#' @details
#' Images in a Custom Vision project are stored in Azure Storage. This function simply gets the URL for the uploaded image and displays it in your browser.
#' @export
browse_image <- function(project, img_id, which=c("resized", "original", "thumbnail"), iteration=NULL)
{
    opts <- list(
        imageIds=img_id,
        iterationId=iteration
    )
    res <- do_training_op(project, "images/id", options=opts)

    img_url <- switch(match.arg(which),
        resized=res[[1]]$resizedImageUri,
        original=res[[1]]$originalImageUri,
        thumbnail=res[[1]]$thumbnailUri
    )
    httr::BROWSE(img_url)
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
