make.unique.vouchers <-
function(metadata, voucherFormula = c("Primary.collector.last.name", "Collector.number", "isolate", "CollectionNumber", "Collection")) {
      vouchers.ln.cn <- apply(metadata[voucherFormula], 1, function(x) paste(tidyName(x[!is.na(x)]), collapse = ''))
      names(vouchers.ln.cn) <- metadata$NCBI_voucher # note that NCBI_accession is not actually the accession number used in NCBI! The preferred accession number is primary_accession
      return(vouchers.ln.cn)
      }
