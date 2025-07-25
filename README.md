
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
if (interactive()){
library(SQAPI)
api <- SQAPI$new()
# Example 0 - Get all annotation sets from a usergroup with id matching 92
{
  # Create filters
  my_filters <- query_filter(
    name = "annotation_set",
    op = "has",
    val = query_filter(
      name = "usergroups",
      op = "any",
      val = query_filter(
        name = "id", 
        op = "eq", 
        val = "92")
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
  another_filter <- query_filter(name = "needs_review", 
                                 op = "eq", 
                                 val = "True")
   # Note: you can add more filters by passing a list of your query filters
   second_request <- export(
    api,
    "api/annotation/export",
    query_filters = list(my_filters, another_filter),
    query_parameters = my_params
  )

   
}

# Example 1 - Get a list of deployments within a polygon defined by lat long coords
  # Define polygon
  polygon <- list(
  list(lat = -38.8, lon = 145.0),
  list(lat = -38.8, lon = 149.5),
  list(lat = -40.2, lon = 149.5),
  list(lat = -40.2, lon = 145.0),
  list(lat = -38.8, lon = 145.0)
)
  # Create filters
  my_filters1 <- query_filter("geom_start", "geo_in_poly", polygon)
  
  # Send request (using "request" for non-export endpoint)
  r1 <- request("GET", api, "api/deployment", my_filters1)
  
  #parse
  p1 <- parse_api(r1)
  
# Example 2 get a list of annotation sets matching a specified id and check if they are real science
  
  # Create filters
  my_filters2 <- query_filter(name = "id", op = "eq", val = "5432")
  another_filter2 <- query_filter(name = "is_real_science", op = "eq", val = "true")
  
  # Send request
  r2 <- request(api = api, endpoint = "api/annotation_set", query_filters = list(my_filters2,another_filter2), verb = "GET")
  # parse
  p2 <- parse_api(r2)

# Example 4 - A simple query to Get annotations that match annotation_set_id = 5432 and specify pagination parameters
{
  # Create filters
  my_filters_4 <- query_filter(name = "annotation_set_id", op = "eq", val = "5432")
  # Create other parameters
  my_params_4 <- query_params(page = 14, results_per_page = 56)
  # Append filters and parameters and send request
  # Note: use request() for non export endpoints
  r4 <- request("GET", api, "api/annotation", my_filters_4, my_params_4)
  # Parse
  p4 <- parse_api(r4)

}
}
```
