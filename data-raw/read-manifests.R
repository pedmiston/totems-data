# Experiment 1 ----
Exp1Manifest <- read_csv("data-raw/manifest-exp1.csv")
use_data(Exp1Manifest, overwrite = TRUE)

# Experiment 2 ----
Exp2Manifest <- read_csv("data-raw/manifest-exp2.csv")
use_data(Exp2Manifest, overwrite = TRUE)

# Experiment 3 ----
Exp3Manifest <- read_csv("data-raw/manifest-exp3.csv")
use_data(Exp3Manifest, overwrite = TRUE)

# Experiment 4 ----
Exp4Manifest <- read_csv("data-raw/manifest-exp4.csv")
use_data(Exp4Manifest, overwrite = TRUE)
