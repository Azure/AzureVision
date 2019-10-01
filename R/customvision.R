list_customvision_projects <- function(endpoint)
{
    lst <- call_cognitive_endpoint(endpoint, "training/projects")
    lapply(lst, function(obj)
    {
        class(obj) <- "customvision_project"
        obj
    })
}


get_customvision_project <- function(endpoint, name=NULL, id=NULL)
{
    if(is.null(id))
        id <- get_project_id_by_name(endpoint, name)

    obj <- call_cognitive_endpoint(endpoint, file.path("training/projects", id))
    class(obj) <- "customvision_project"
    obj
}


create_customvision_project <- function(endpoint, name,
                                        purpose=c("classification", "object_detection"),
                                        domain="general",
                                        multiple_labels=FALSE,
                                        description=NULL)
{
    purpose <- match.arg(purpose)
    domain_id <- get_domain_id(domain, purpose)
    type <- if(purpose == "object_detection")
        NULL
    else if(multiple_labels)
        "multilabel"
    else "multiclass"

    opts <- list(
        name=name,
        domainId=domain_id,
        classificationType=type,
        description=description
    )

    obj <- call_cognitive_endpoint(endpoint, "training/projects", options=opts, http_verb="POST")
    class(obj) <- "customvision_project"
    obj
}


delete_customvision_project <- function(endpoint, name=NULL, id=NULL, confirm=TRUE)
{
    if(is.null(id))
        id <- get_project_id_by_name(endpoint, name)

    msg <- sprintf("Are you sure you really want to delete the project '%s'?", if(!is.null(name)) name else id)
    if(!confirm_delete(msg, confirm))
        return(invisible(NULL))

    call_cognitive_endpoint(endpoint, file.path("training/projects", id), http_verb="DELETE")
    invisible(NULL)
}


update_customvision_project <- function(endpoint, name=NULL, id=NULL,
                                        domain="general",
                                        multiple_labels=FALSE,
                                        description=NULL,
                                        export_target=c("none", "basic", "VAIDK"))
{
    if(is.null(id))
        id <- get_project_id_by_name(endpoint, name)

    project <- get_customvision_project(endpoint, id=id)
    newbody <- list()

    if(!is.null(name) && name != project$name)
        newbody$name <- name

    if(!missing(description))
        newbody$description <- description

    newbody$settings <- project$settings
    purpose <- get_purpose_from_domain_id(project$settings$domainId)

    if(!missing(domain))
        newbody$settings$domainId <- get_domain_id(domain, purpose)

    if(!missing(multiple_labels))
        newbody$settings$classificationType <- if(multiple_labels) "Multilabel" else "Multiclass"

    obj <- call_cognitive_endpoint(endpoint, file.path("training/projects", id), body=newbody, http_verb="PATCH")
    class(obj) <- "customvision_project"
    obj
}


.domain_ids <- list(
    classification=c(
        general="ee85a74c-405e-4adc-bb47-ffa8ca0c9f31",
        food="c151d5b5-dd07-472a-acc8-15d29dea8518",
        landmarks="ca455789-012d-4b50-9fec-5bb63841c793",
        retail="b30a91ae-e3c1-4f73-a81e-c270bff27c39"
    ),
    object_detection=c(
        general="da2e3a8a-40a5-4171-82f4-58522f70fbc1",
        logo="1d8ffafe-ec40-4fb2-8f90-72b3b6cecea4"
    )
)

get_domain_id <- function(domain, purpose)
{
    ids <- .domain_ids[[purpose]]
    i <- which(domain == names(ids))
    if(is_empty(i))
        stop(sprintf("Domain '%s' not found", domain), call.=FALSE)
    ids[i]
}


get_purpose_from_domain_id <- function(id)
{
    i <- which(sapply(.domain_ids, function(domains) id %in% domains))
    names(.domain_ids)[i]
}


print.customvision_project <- function(x, ...)
{
    cat("Azure Custom Vision project '", x$name, "' (", x$id, ")\n", sep="")
    print(x$settings)
    invisible(x)
}


get_project_id_by_name <- function(endpoint, name=NULL)
{
    if(is.null(name))
        stop("Either name or ID must be supplied", call.=FALSE)

    lst <- list_customvision_projects(endpoint)
    i <- which(sapply(lst, function(obj) obj$name == name))
    if(is_empty(i))
        stop(sprintf("Project '%s' not found", name), call.=FALSE)

    lst[[i]]$id
}
