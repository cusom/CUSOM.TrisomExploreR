library(fgsea)
library(here)

GSEA_hallmarks <- fgsea::gmtPathways(gmt.file = here("data-raw/h.all.v7.4.symbols.gmt"))

usethis::use_data(GSEA_hallmarks)

#usethis::use_data(GSEA_hallmarks, internal = TRUE, overwrite = TRUE)
