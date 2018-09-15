

## Required packages
library(data.table)

## Function that takes in 
## dataframe with weights
## from a consortium and 
## creates three output 
## files in the correct format
## for use in plink
## (e.g., no headers or rownames)
## 1. a file with SNPs only
## 2. a file with SNPS, reference alleles
## 3. a file with SNPS, reference alleles, and weights

## takes in following arguments
## @df_allinfo: dataframe with complete weights info (needs to at least 
## have snp, ref allele, and weights columns)
## @nameof_SNPcol: string with name of column that refers to the rs# in above df
## @nameof_refallelecol: string with name of col that contains allele1/refallele/risk allele
## @nameof_effectsizecol: string with name of col containing effect size (e.g, beta or z-score)
## @nameof_phenotype: string with name of phenotype--used to name output files
## @dirname_storeoutput: string with name of directory where you want to store output

format_PGSfiles_forplink <- function(df_allinfo,
                                     nameof_SNPcol,
                                     nameof_refallelecol,
                                     nameof_effectsizecol,
                                     nameof_phenotype,
                                     dirname_storeoutput){
  
  ## preprocess data to capitalize ref alleles column
  data_cleaned = as.data.table(df_allinfo)
  data_cleaned[, nameof_refallelecol] = sapply(data_cleaned[, .SD, 
                                                            .SDcols = nameof_refallelecol], 
                                               toupper) 

  ## first create and write snp file
  snp_df = data_cleaned[, .SD, .SDcols = nameof_SNPcol]
  file_plus_dir = sprintf("%s%s", 
                          dirname_storeoutput,
                          sprintf("%s_%s", nameof_phenotype,
                                  "snplist.txt"))
  write.table(snp_df, file_plus_dir, 
              row.names = FALSE, col.names = FALSE, quote = FALSE, 
              sep = "\t")
  
  print('wrote SNP list to directory')
  
  ## then create and write snp + allele file
  ## has step to convert all reference alleles to uppercase
  snpallele_df = data_cleaned[, .SD, .SDcols = c(nameof_SNPcol,nameof_refallelecol)]
  
  file_plus_dir = sprintf("%s%s", 
                          dirname_storeoutput,
                          sprintf("%s_%s", nameof_phenotype,
                                  "refallele.txt"))
  write.table(snpallele_df, file_plus_dir, 
              row.names = FALSE, col.names = FALSE, quote = FALSE, 
              sep = "\t")
  
  print('wrote SNP + ref alleles to directory')
  
  ## finally, create and write snp+ allele + weights files
  snpweights_df = data_cleaned[, .SD, .SDcols = c(nameof_SNPcol,
                                                  nameof_refallelecol,
                                                  nameof_effectsizecol)]
  file_plus_dir = sprintf("%s%s", 
                          dirname_storeoutput,
                          sprintf("%s_%s", nameof_phenotype,
                                  "beta.txt"))
  write.table(snpweights_df, file_plus_dir, 
              row.names = FALSE, col.names = FALSE, quote = FALSE, 
              sep = "\t")
  
  print('wrote SNP + ref alleles + weights to directory')
}


## example application:
## create_PGS_files(df_allinfo = height_weights_data,
## nameof_SNPcol = 'MarkerName',
## nameof_refallelecol = 'Allele1',
## nameof_effectsizecol = 'b',
## nameof_phenotype = 'varheight',
## dirname_storeoutput = 'Users/fakedir/')

