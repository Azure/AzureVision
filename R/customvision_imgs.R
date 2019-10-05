#' @export
list_tags <- function(project, iteration=NULL, as=c("names", "ids", "dataframe", "list"))
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


#' @export
add_images <- function(project, images, tags=NULL, regions=NULL)
{
    type <- validate_images(images)

    imglist <- if(type == "files")
        add_image_files(project, images)
    else add_image_urls(project, images)

    if(!is_empty(tags))
    {
        imglist <- sapply(imglist, function(x) x$id)
        tag_uploaded_images(project, tags, imglist)
    }
    invisible(project)
}


#' @export
add_tags <- function(project, tags)
{
    current_tags <- list_tags(project, as="names")
    tags <- unique_tags(tags)
    newtags <- setdiff(tags, current_tags)
    if(!is_empty(newtags))
    {
        lapply(newtags, function(tag)
            do_training_op(project, "tags", options=list(name=tag), http_verb="POST"))
    }
    invisible(project)
}


#' @export
add_negative_tag <- function(project, negative_name="_negative_")
{
    taglist <- list_tags(project, as="dataframe")

    if(any(taglist$type == "Negative"))
    {
        warning("Project already has a negative tag", call.=FALSE)
        return(invisible(project))
    }

    if(negative_name %in% taglist$name)
    {
        tagid <- taglist$id[which(negative_name == taglist$name)]
        do_training_op(project, file.path("tags", tagid), body=list(type="Negative"), http_verb="PATCH")
    }
    else do_training_op(project, "tags", options=list(name=negative_name, type="Negative"), http_verb="POST")

    invisible(project)
}


#' @export
list_images <- function(project, include=c("both", "tagged", "untagged"), iteration=NULL,
                        as=c("ids", "dataframe", "list"))
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
        as.character(rbind.data.frame(tagged_imgs, untagged_imgs)$id)
    else if(as == "dataframe")
        rbind.data.frame(tagged_imgs, untagged_imgs)
    else c(tagged_imgs, untagged_imgs)
}


#' @export
tag_uploaded_images <- function(project, tags, images=list_images(project, "untagged", as="ids"))
{
    if(length(tags) != length(images) || length(tags) != 1)
        stop("Must supply tags for each image", call.=FALSE)

    unique_tags <- unique_tags(tags)
    add_tags(project, tags)
    tag_ids <- get_tag_ids_from_names(unique_tags, project)

    req_list <- lapply(seq_along(unique_tags), function(i)
    {
        this_set <- sapply(tags, function(tags_j) unique_tags[i] %in% tags_j)
        if(!is_empty(this_set))
            data.frame(imageId=images[this_set], tagId=tag_ids[i], stringsAsFactors=FALSE)
        else NULL
    })

    do_training_op(project, "images/tags", body=list(tags=do.call(rbind, req_list)), http_verb="POST")
    invisible(project)
}


#' @export
untag_uploaded_images <- function(project, images=list_images(project, "tagged", as="ids"),
                                  tags=list_tags(project, as="ids"))
{
    images <- paste0(images, collapse=",")
    tags <- paste0(tags, collapse=",")
    opts <- list(imageIds=images, tagIds=tags)

    do_training_op(project, "images/tags", options=opts, http_verb="DELETE")
    invisible(project)
}


#' @export
remove_images <- function(project, images=list_images(project, "untagged", as="ids"), confirm=TRUE)
{
    if(!confirm_delete("Are you sure you want to remove images from the project?", confirm))
        return(invisible(project))

    images <- paste0(images, collapse=",")
    do_training_op(project, "images", options=list(imageIds=images), http_verb="DELETE")
    invisible(project)
}


#' @export
remove_tags <- function(project, tags, confirm=TRUE)
{
    tags <- unique_tags(tags)
    if(!confirm_delete("Are you sure you want to remove tags from the project?", confirm))
        return(invisible(project))

    lapply(get_tag_ids_from_names(tags, project), function(tag)
        do_training_op(project, file.path("tags", tag), http_verb="DELETE"))

    invisible(project)
}


add_image_files <- function(project, images)
{
    imglist <- lapply(images, function(img)
        list(name=img, contents=readBin(img, "raw", file.info(img)$size))
    )
    res <- do_training_op(project, "images/files", body=list(images=imglist), http_verb="POST")
    if(!res$isBatchSuccessful)
        stop("Not all images were successfully added", call.=FALSE)

    res <- res$images

    # need to reorder uploading result to match original image vector
    srcs <- sapply(res, `[[`, "sourceUrl")
    lapply(res[match(images, srcs)], `[[`, "image")
}


add_image_urls <- function(project, images)
{
    imglist <- lapply(images, function(img)
        list(url=img)
    )
    res <- do_training_op(project, "images/urls", body=list(images=imglist), http_verb="POST")
    if(!res$isBatchSuccessful)
        stop("Not all images were successfully added", call.=FALSE)

    res <- res$images

    # need to reorder uploading result to match original image vector
    srcs <- sapply(res, `[[`, "sourceUrl")
    lapply(res[match(images, srcs)], `[[`, "image")
}


get_tag_ids_from_names <- function(tagnames, project)
{
    tagdf <- list_tags(project, as="dataframe")
    unname(structure(tagdf$id, names=tagdf$name)[tagnames])
}


unique_tags <- function(tags)
{
    if(is.list(tags)) unique(unlist(tags)) else tags
}


validate_images <- function(images)
{
    all_files <- all(file.exists(images))
    all_urls <- all(sapply(images, is_any_uri))
    if(!all_files && !all_urls)
        stop("Must supply either a vector of all file names or all URLs", call.=FALSE)

    if(all_files) "files" else "urls"
}
