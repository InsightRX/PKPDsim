# Check if package number is different from currently installed, and provide some messaging.

Technically it only checks if a package version is different, not
necessarily a higher version number.

## Usage

``` r
is_newer_package(package, new_version)
```

## Arguments

- package:

  R package

- new_version:

  new version number
