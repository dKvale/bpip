---  
dev version: 0.0.0.9000  
---  


bpip
=======

This R package provides a data frame interface for EPA's air dispersion downwash model BPIP.

## Install 

```r
install.packages("remotes")

remotes::install_github("dKvale/bpip")
```

## Use

Create a 10x10x10 building that is rotated 45 degrees and located 20 meters West of the air emissions source.

## Create new bpip table
```r 
library(bpip)

builds <- new_bpip()

builds
```

## Adjust building parameters
```r 
builds$bld_rotation      <- 45
builds$length_x          <- 10
builds$angle_from_source <- 270 

builds
```

## Plot building
```r 
plot_bpip(builds)
```

## Install BPIP to a new EPA folder in working directory
```r 
library(installEPA)
install_epa("bpip", dir = "EPA")
```

## Create bpip input file
```r 
write_bpip(builds, "rotated_building.bpi")
```

## Run bpip
```r 
run_bpip("rotated_building.bpi", "rotated_results", exe_folder = "EPA/")
```

## Read results
```r 
results <- read_bpip_out(file = "rotated_results.out", as_text = FALSE)

results
```
