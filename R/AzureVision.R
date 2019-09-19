#' @import AzureRMR
NULL

globalVariables(c("self", "private"))

.onLoad <- function(libname, pkgname)
{
    add_methods()
}


add_methods <- function()
{
    az_resource_group$set("public", "create_vision_service", overwrite=TRUE,
    function(name, location=self$location, sku="S1", ...)
    {
        az_resource$new(self$token, self$subscription, self$name,
            type="Microsoft.CognitiveServices/accounts",
            name=name,
            location=location,
            kind="ComputerVision",
            sku=list(name=sku), ...)
    })

    az_resource_group$set("public", "get_vision_service", overwrite=TRUE,
    function(name)
    {
        az_resource$new(self$token, self$subscription, self$name,
            type="Microsoft.CognitiveServices/accounts",
            name=name)
    })

    az_resource_group$set("public", "delete_vision_service", overwrite=TRUE,
    function(name, confirm=TRUE, wait=FALSE)
    {
        az_resource$new(self$token, self$subscription, self$name,
            type="Microsoft.CognitiveServices/accounts",
            name=name,
            deployed_properties=list(NULL))$delete(confirm=confirm, wait=wait)
    })
}
