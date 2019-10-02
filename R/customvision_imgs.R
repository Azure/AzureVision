list_tags <- function(project, iteration=NULL, as=c("names", "dataframe", "list"))
{
    as <- match.arg(as)
    if(as == "list")
        return(do_training_op(project, "tags", options(iterationId=iteration)))

    tags <- do_training_op(project, "tags", simplifyVector=TRUE)
    if(as == "names")
        tags$name
    else as.data.frame(tags)
}


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


add_images <- function(project, images, tags=NULL, regions=NULL)
{
    all_files <- all(sapply(images, file.exists))
    all_urls <- all(sapply(images, is_any_uri))
    if(!all_files && !all_urls)
        stop("Must supply either a vector of all file names or all URLs", call.=FALSE)

    if(all_files)
        add_image_files(project, images)
    else add_image_urls(project, images)

    if(!is_empty(tags))
    {
        add_tags(project, unique(as.character(tags)))
        tag_images(project, images, tags)
    }
    invisible(project)
}


add_tags <- function(project, tags)
{
    current_tags <- list_tags(project)
    newtags <- setdiff(tags, current_tags)
    if(!is_empty(newtags))
    {
        lapply(newtags, function(tag)
            do_training_op(project, "tags", options=list(name=tag), http_verb="POST"))
    }
    invisible(project)
}


add_negative_tag <- function(project, negative_name="_negative_")
{
    taglist <- list_tags(project, as="list")

    if(any(sapply(taglist, `[[`, "type") == "Negative"))
    {
        warning("Project already has a negative tag", call.=FALSE)
        return(invisible(project))
    }

    tagnames <- sapply(taglist, `[[`, "name")
    if(negative_name %in% tagnames)
    {
        tagid <- taglist[[which(negative_name == tagnames)]]$id
        do_training_op(project, file.path("tags", tagid), body=list(type="Negative"), http_verb="PATCH")
    }
    else do_training_op(project, "tags", options=list(name=negative_name, type="Negative"), http_verb="POST")

    invisible(project)
}


get_images <- function(project, include=c("tagged", "untagged", "both"), iteration=NULL,
                       as=c("id", "dataframe", "list"))
{
    include <- match.arg(include)
    as <- match.arg(as)
    simplify <- as != "list"

    tagged_imgs <- if(include %in% c("tagged", "both"))
        do_training_op(project, "images/tagged", options=list(iterationId=iteration), simplifyVector=simplify)
    else NULL

    untagged_imgs <- if(include %in% c("untagged", "both"))
        do_training_op(project, "images/untagged", options=list(iterationId=iteration), simplifyVector=simplify)
    else NULL

    if(as == "id")
        rbind(tagged_imgs, untagged_imgs)$id
    else if(as == "dataframe")
        rbind(tagged_imgs, untagged_imgs)
    else c(tagged_imgs, untagged_imgs)
}


tag_images <- function(project, images, tags)
{
}


add_image_files <- function(project, images)
{
    images <- lapply(images, function(img)
        list(name=img, contents=readBin(img, "raw", file.info(img)$size))
    )
    res <- do_training_op(project, "images/files", body=list(images=images), http_verb="POST")
    if(!res$isBatchSuccessful)
        warning("Not all images were successfully added", call.=FALSE)
}


add_image_urls <- function(project, images)
{
    images <- lapply(images, function(img)
        list(url=img)
    )
    res <- do_training_op(project, "images/urls", body=list(images=images), http_verb="POST")
    if(!res$isBatchSuccessful)
        warning("Not all images were successfully added", call.=FALSE)
}

