# Experiment 50min
Manifest50min <- read_csv("data-raw/manifest-50min.csv")
use_data(Manifest50min, overwrite = TRUE)

# Experiment SelfOther
ManifestSelfOther <- read_csv("data-raw/manifest-selfother.csv")
use_data(ManifestSelfOther, overwrite = TRUE)

# Experiment Scalability
ManifestScalability <- read_csv("data-raw/manifest-scalability.csv")
use_data(ManifestScalability, overwrite = TRUE)
