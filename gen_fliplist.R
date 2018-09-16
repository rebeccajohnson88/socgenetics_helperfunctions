
# packages
library(dplyr)


gen_fliplist <- function(pathto_predfile, 
						pathto_writesnps){


	snps_notscored_df = read.table(pathto_predfile, 
                            sep = "\t")
	colnames(snps_notscored_df) = c('reason_notscored', 
                             'snp')

	print(sprintf('%s snps not scored because snps not present on chip',
              nrow(snps_notscored_df %>% filter(reason_notscored == 'NOSNP'))))
	print(sprintf('%s snps not scored because snps present on chip but not scored due to allele mismatch',
              nrow(snps_notscored_df %>% filter(reason_notscored == 'NOALLELE'))))


	# Subset to SNPs not scored because snp
	# was present but no alleles in genetic df
	# matched ref allele from consortium
	# (contrast to the # not scored because of no snp)
	snps_notscored_noallele = snps_notscored_df %>% filter(reason_notscored == "NOALLELE")

	# Get names of snps (pulling out the rs#)
	snps_notscored_names = regmatches(snps_notscored_noallele$snp, 
                            regexpr("rs[0-9]+", snps_notscored_noallele$snp))

	# Write snps whose alleles should be flipped to a file
	write.table(snps_notscored_names, 
			pathto_writesnps, 
            col.names = FALSE, row.names = FALSE, quote = FALSE, 
            sep = "\t")

	print('wrote text file with flipped snps')

}