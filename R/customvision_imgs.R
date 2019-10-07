#' Add, list and remove images for a project
#'
#' @param project A Custom Vision project.
#' @param images For `add_images`, the images to add (upload) to the project.
#' @param image_ids For `remove_images`, the IDs of the images to remove from the project.
#' @param tags For `add_images`, optional tags to add to the images. Used for classification projects.
#' @param regions For `add_images`, an optional list of regions in the images that contain objects. Used for object detection projects.
#' @param include For `list_images`, which images to include in the list: untagged, tagged, or both (the default).
#' @param as For `list_images`, the return value: a vector of image IDs, a data frame of image metadata, or a list of metadata.
#' @param iteration For `list_images`, the iteration ID (roughly, which model generation to use). Defaults to the latest iteration.
#' @param confirm For `remove_images`, whether to ask for confirmation first.
#' @details
#' The images to be uploaded can be specified as:
#' - A vector of local filenames. All common image file formats are supported.
#' - A vector of publicly accessible URLs.
#' - A raw vector, or a list of raw vectors, holding the binary contents of the image files.
#'
#' Uploaded images can be _tagged_ (for a classification project), which set the target labels for each image. To specify a single tag per image, set `tags` to be a vector. For multiple tags per image, `tags` should be a list, with each component being the vector of tags for the corresponding image. If `tags` has length 1, it is recycled to the length of `images` as a convenience feature for giving multiple images the same tag.
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
#' [`tag_uploaded_images`] to add tags to images if not done at upload time, [`untag_uploaded_images`]
#'
#' [`add_tags`], [`list_tags`], [`remove_tags`]
#'
#' [`create_customvision_project`], [`get_customvision_project`], [`delete_customvision_project`]
#' @rdname customvision_image
#' @export
add_images <- function(project, images, tags=NULL, regions=NULL)
{
    images <- images_to_bodies(images)
    op <- if(is.null(images[[1]]$contents)) "images/urls" else "images/files"

    res <- do_training_op(project, op, body=list(images=unname(images)), http_verb="POST")
    if(!res$isBatchSuccessful)
        stop("Not all images were successfully added", call.=FALSE)

    res <- res$images

    srcs <- sapply(res, `[[`, "sourceUrl")
    # need to reorder uploading result to match original image vector
    res <- lapply(res[match(names(images), srcs)], `[[`, "image")

    img_ids <- sapply(res, function(x) x$id)
    if(!is_empty(tags))
        tag_uploaded_images(project, tags, img_ids)

    img_ids
}


#' @rdname customvision_image
#' @export
list_images <- function(project, include=c("all", "tagged", "untagged"), as=c("ids", "dataframe", "list"),
                        iteration=NULL)
{
    include <- match.arg(include)
    as <- match.arg(as)
    simplify <- as != "list"

    tagged_imgs <- if(include != "untagged")
        do_training_op(project, "images/tagged", options=list(iterationId=iteration), simplifyVector=simplify)
    else NULL

    untagged_imgs <- if(include != "tagged")
        do_training_op(project, "images/untagged", options=list(iterationId=iteration), simplifyVector=simplify)
    else NULL

    if(as == "ids")
        as.character(c(tagged_imgs$id, untagged_imgs$id))
    else if(as == "dataframe")
    {
        if(is.data.frame(untagged_imgs) && nrow(untagged_imgs) > 0)
            untagged_imgs$tags <- NA
        rbind.data.frame(tagged_imgs, untagged_imgs)
    }
    else c(tagged_imgs, untagged_imgs)
}


#' @rdname customvision_image
#' @export
remove_images <- function(project, image_ids=list_images(project, "untagged", as="ids"), confirm=TRUE)
{
    if(!confirm_delete("Are you sure you want to remove images from the project?", confirm))
        return(invisible(project))

    image_ids <- paste0(image_ids, collapse=",")
    do_training_op(project, "images", options=list(imageIds=image_ids), http_verb="DELETE")
    invisible(NULL)
}


#' Add, retrieve and remove tags for a project
#'
#' @param project A Custom Vision project.
#' @param tags For `add_tags`, a vector of strings to treat as tags.
#' @param name,id For `get_tag`, the name (text string) for a tag, and its ID. Provide one or the other, but not both.
#' @param negative_name For `add_negative_tag`, the label to provide a negative tag. See 'Negative tags' below.
#' @param as For `list_tags`, the format in which to return results: a vector of tag names, a vector of tag IDs, a data frame of metadata, or a list of metadata.
#' @param iteration For `list_tags` and `get_tag`, the iteration ID (roughly, which model generation to use). Defaults to the latest iteration.
#' @param confirm For `remove_tags`, whether to ask for confirmation first.
#' @details
#' _Tags_ are the labels attached to images for use in classification projects. An image can have one or multiple tags associated with it; however, the latter only makes sense if the project is setup for multi-label classification.
#'
#' Tags form part of the metadata for a Custom Vision project, and have to be explicitly defined prior to use. Each tag has a corresponding ID which is used to manage it. In general, you can let AzureVision handle the details of managing tags and tag IDs.
#'
#' @section Negative tags:
#' A _negative tag_ is a special tag that represents the absence of any other tag. For example, if a project is classifying images into cats and dogs, an image that doesn't contain either a cat or dog should be given a negative tag. This can be distinguished from an _untagged_ image, where there is no information at all on what it contains.
#'
#' You can add a negative tag to a project with the `add_negative_tag` method. Once defined, a negative tag is treated like any other tag. A project can only have one negative tag defined.
#' @seealso
#' [`tag_uploaded_images`], [`untag_uploaded_images`]
#' @rdname customvision_tag
#' @export
add_tags <- function(project, tags)
{
    current_tags <- list_tags(project, as="names")
    tags <- unique_tags(tags)
    newtags <- setdiff(tags, current_tags)
    if(is_empty(newtags))
        return(NULL)

    res <- lapply(newtags, function(tag)
        do_training_op(project, "tags", options=list(name=tag), http_verb="POST"))

    do.call(rbind.data.frame, c(stringsAsFactors=FALSE,
        lapply(res, function(x) x[c("name", "id")]))
    )
}


#' @rdname customvision_tag
#' @export
add_negative_tag <- function(project, negative_name="_negative_")
{
    taglist <- list_tags(project, as="dataframe")

    if(any(taglist$type == "Negative"))
    {
        warning("Project already has a negative tag", call.=FALSE)
        return(NULL)
    }

    res <- if(negative_name %in% taglist$name)
    {
        tagid <- taglist$id[which(negative_name == taglist$name)]
        do_training_op(project, file.path("tags", tagid), body=list(type="Negative"), http_verb="PATCH")
    }
    else do_training_op(project, "tags", options=list(name=negative_name, type="Negative"), http_verb="POST")

    data.frame(res[c("name", "id")], stringsAsFactors=FALSE)
}


#' @rdname customvision_tag
#' @export
list_tags <- function(project, as=c("names", "ids", "dataframe", "list"), iteration=NULL)
{
    as <- match.arg(as)
    if(as == "list")
        return(do_training_op(project, "tags", options(iterationId=iteration)))

    tags <- do_training_op(project, "tags", options(iterationId=iteration), simplifyVector=TRUE)
    if(as == "names")
        tags$name
    else if(as == "ids")
        tags$id
    else as.data.frame(tags)
}


#' @rdname customvision_tag
#' @export
get_tag <- function(project, name=NULL, id=NULL, iteration=NULL)
{
    if(is.null(id))
    {
        taglist <- list_tags(project, iteration=iteration, as="list")
        tagnames <- sapply(taglist, `[[`, "name")
        i <- which(name == tagnames)
        if(is_empty(i))
            stop(sprintf("Image tag '%s' not found", name), call.=FALSE)

        taglist[[i]]
    }
    else do_training_op(project, file.path("tags", id), options(iterationid=iteration))
}


#' @rdname customvision_tag
#' @export
remove_tags <- function(project, tags, confirm=TRUE)
{
    tags <- unique_tags(tags)
    if(!confirm_delete("Are you sure you want to remove tags from the project?", confirm))
        return(invisible(project))

    lapply(get_tag_ids_from_names(tags, project), function(tag)
        do_training_op(project, file.path("tags", tag), http_verb="DELETE"))

    invisible(NULL)
}


#' Tag and untag images uploaded to a project
#'
#' @param project a Custom Vision project.
#' @param tags For `tag_uploaded_images`, the tag labels to add to the images. For `untag_uploaded_images`, the tags (either text labels or IDs) to remove from images. The default for untagging is to remove all assigned tags.
#' @param image_ids The IDs of the images to tag or untag.
#' @details
#' `tag_uploaded_images` is for tagging images that were uploaded previously, while `untag_uploaded_images` untags them. Adding tags does not remove previously assigned ones. Similarly, removing one tag from an image leaves any other tags intact.
#' @return
#' The vector of IDs for the images affected, invisibly.
#' @seealso
#' [`add_images`], [`list_tags`]
#' @rdname customvision_tag_image
#' @export
tag_uploaded_images <- function(project, tags, image_ids=list_images(project, "untagged", as="ids"))
{
    if(length(tags) != length(image_ids) && length(tags) != 1)
        stop("Must supply tags for each image", call.=FALSE)

    if(any(is_guid(tags)))
        warning("'tags' argument should be tag names (did you supply a list of image IDs?)", call.=FALSE)
    if(!all(is_guid(image_ids)))
        stop("Must provide GUIDs of images to be tagged", call.=FALSE)

    unique_tags <- unique_tags(tags)
    add_tags(project, unique_tags)
    tag_ids <- get_tag_ids_from_names(unique_tags, project)

    if(length(tags) == 1)
        tags <- rep(tags, length(image_ids))

    req_list <- lapply(seq_along(unique_tags), function(i)
    {
        this_set <- sapply(tags, function(tags_j) unique_tags[i] %in% tags_j)
        if(!is_empty(this_set))
            data.frame(imageId=image_ids[this_set], tagId=tag_ids[i], stringsAsFactors=FALSE)
        else NULL
    })

    do_training_op(project, "images/tags", body=list(tags=do.call(rbind, req_list)), http_verb="POST")
    invisible(image_ids)
}


#' @rdname customvision_tag_image
#' @export
untag_uploaded_images <- function(project, image_ids=list_images(project, "tagged", as="ids"),
                                  tags=list_tags(project, as="ids"))
{
    if(!all(is_guid(tags)))
    {
        taglist <- list_tags(project, as="dataframe")[c("name", "id")]
        tags <- taglist$id[match(tags, taglist$name)]
    }
    opts <- list(
        imageIds=paste0(image_ids, collapse=","),
        tagIds=paste0(tags, collapse=",")
    )
    do_training_op(project, "images/tags", options=opts, http_verb="DELETE")
    invisible(image_ids)
}


get_tag_ids_from_names <- function(tagnames, project)
{
    tagdf <- list_tags(project, as="dataframe")
    unname(structure(tagdf$id, names=tagdf$name)[tagnames])
}


unique_tags <- function(tags)
{
    if(is.list(tags)) unique(unlist(tags)) else unique(tags)
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


