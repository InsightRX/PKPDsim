# Read model definition from JSON

Does some substitution of escaped characters in strings in the JSON
file, then converts to a list with
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)

## Usage

``` r
read_model_json(path)
```

## Arguments

- path:

  Path to JSON file

## Value

List containing contents of original JSON file
