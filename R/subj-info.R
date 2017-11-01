get_subj_info <- function() {
  subj_info <- gs_title("totems-subj-info")
  gs_download(subj_info, ws = "TOT/TOI", to = "data-raw/subj-info/tot-toi.csv")
  gs_download(subj_info, ws = "TOM", to = "data-raw/subj-info/tom.csv")
}
