
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SQAPI

<!-- badges: start -->
<!-- badges: end -->

The goal of SQAPI is to allow user friendly interaction with SQUIDLE+
API through r.

## Installation

You can install the development version of SQAPI from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sajessop/SQAPI")
```

## Example

These are a few basic examples which shows you how to build a url and
send a request:

``` r
if (interactive()) {
  library(SQAPI)
  
  # Create instance of SQAPI with SQAPI$new()
  api <- SQAPI$new()
  
  ############## Example 0 - Get all annotation sets from a usergroup with id matching 92 ##############
  # Create filters
  my_filters <- query_filter(
    name = "annotation_set",
    op = "has",
    val = query_filter(
      name = "usergroups",
      op = "any",
      val = query_filter(name = "id", op = "eq", val = "92")
    )
  )
  # Create parameters
  # max limit is 200 000, combine limit and offset to download in chunks
  my_params <- query_params(limit = 200)
  # Send request
  # Note: use export() for export endpoints
  r <- export(
    api = api,
    endpoint = "api/annotation/export",
    query_filters = my_filters,
    query_parameters = my_params,
    template = "data.csv"
  )
  # parse
  p <- parse_api(r, filetype = "csv")
  
  
  # Check if needs review...
  another_filter <- query_filter(name = "needs_review", op = "eq", val = "True")
  # Note: you can add more filters by passing a list of your query filters
  second_request <- export(
    api,
    "api/annotation/export",
    query_filters = list(my_filters, another_filter),
    query_parameters = my_params
  )
  
  
  ############## Example 1 - Filtering by geom ##############
  ## 1.1 Get a list of deployments within a polygon
  # Define polygon
  polygon <- list(
    list(lat = -38.8, lon = 145.0),
    list(lat = -38.8, lon = 149.5),
    list(lat = -40.2, lon = 149.5),
    list(lat = -40.2, lon = 145.0),
    list(lat = -38.8, lon = 145.0)
  )
  # Create filters
  my_filters1.1 <- query_filter("geom_start", "geo_in_poly", polygon)
  
  # Send request (using "request" for non-export endpoint)
  r1.1 <- request("GET", api, "api/deployment", my_filters1.1)
  
  #parse
  p1.1 <- parse_api(r1.1)
  
  ## 1.2 Get all annotations within a polygon
  # Create nested filters for getting all annotations that contain media with poses within a polygon
  my_filters1.2 <- query_filter(
    name = "point",
    op = "has",
    val = query_filter(
      name = "media",
      op = "has",
      val = query_filter(
        name = "poses",
        op = "any",
        val = query_filter(name = "geom", op = "geo_in_poly", val = polygon)
      )
    )
  )
  
  # Send request to export endpoint and parse response
  r1.2 <- export(api = api,
                 endpoint = "api/annotation/export",
                 query_filters = my_filters1.2)
  p1.2 <- parse_api(r1.2)
  
  ############## Example 2 get a list of annotation sets matching a specified id and check if they are real science ##############
  # Create filters
  my_filters2 <- query_filter(name = "id", op = "eq", val = "5432")
  another_filter2 <- query_filter(name = "is_real_science", op = "eq", val = "true")
  
  # Send request
  r2 <- request(
    api = api,
    endpoint = "api/annotation_set",
    query_filters = list(my_filters2, another_filter2),
    verb = "GET"
  )
  # parse
  p2 <- parse_api(r2)
  
  ############## Example 4 - A simple query to Get annotations that match annotation_set_id = 5432 and specify pagination parameters ##############
  # Create filters
  my_filters4 <- query_filter(name = "annotation_set_id", op = "eq", val = "5432")
  # Create other parameters
  my_params4 <- query_params(page = 14, results_per_page = 56)
  # Append filters and parameters and send request
  # Note: use request() for non export endpoints
  r4 <- request("GET", api, "api/annotation", my_filters4, my_params4)
  # Parse
  p4 <- parse_api(r4)
  
  
  ############## Example 5 - Using translate and include columns options ##############
  # Create filters
  my_filters5 <- query_filter(
    name = "events",
    op = "any",
    val = query_filter(name = "id", op = "is_not_null")
  )
  
  # Specify translate
  t <- translate(target_label_scheme_id = 3, vocab_registry_keys = c("worms", "caab"))
  # Create other params
  my_params5 <- query_params(
    group_by = "pose.dep",
    include_columns = c(
      "id",
      "key",
      "path_best",
      "timestamp_start",
      "path_best_thm",
      "pose.timestamp",
      "pose.lat",
      "pose.lon",
      "pose.alt",
      "pose.dep",
      "pose.data",
      "pose.id",
      "deployment.key",
      "deployment.campaign.key",
      "deployment.id",
      "deployment.campaign.id",
      "event_log"
    )
  )
  # Send request
  r5 <- export(
    api = api,
    endpoint = "api/media_collection/13453/export",
    query_filters = my_filters5,
    query_parameters = my_params5,
    translate = t,
    template = "data.csv",
    metadata_filename = "my_metadata3.json"
  )
  
  # Parse
  p3 <- parse_api(r5)
  
  
  ############## Example 5 - A POST request workflow ##############
  # load library for use of %>% 
  library(magrittr)
  # Create a list to post
  empty_post <- list("name" = "Example df name", "description" = "Description of media collection")
  # Send request with body attached 
  # POST to endpoint = "api/media_collection" to create a media collection
  post <- request(
    verb = "POST",
    api = api,
    endpoint = "api/media_collection",
    body = empty_post
  )
  # This has created an empty media collection named "Example df name" with a description "Description of media collection"
  
  # Next we want to get the unique id of the media_collection we just created
  f <- query_filter("name", "eq", "Example df name")
  get <- request("GET",
                 api = api,
                 endpoint = "api/media_collection",
                 query_filters = f)
  parsed <- parse_api(get)
  id <- parsed$objects$id
  
  # Now we want to populate the media collection with images from SQUIDLE
  # Read in data file with list of media keys
  df <- read.csv("dummy_media.csv")
  # Collect unique keys from df
  key_list <- df$point.media.key %>% unique()
  
  # Build post body
  post_json <- toJSON(list(filters = list(list(
    name = "key", # or "id"
    op   = "in", val  = key_list
  ))), auto_unbox = TRUE)
  
  # Wrap post_json in a named list as form parameter
  post_me <- list(q = post_json)
  

  # POST request to the id of the empty media collection
  post2 <- request(
    verb = "POST",
    api = api,
    endpoint = "api/media_collection/14357/media", #from id object
    body = post_me
  )

}
```
