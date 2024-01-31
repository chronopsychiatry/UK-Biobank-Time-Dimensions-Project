##### UK Biobank Time sign off variables ######

### Details on variables provided here: https://biobank.ndph.ox.ac.uk/showcase/label.cgi?id=129
### Order of data collection: https://biobank.ctsu.ox.ac.uk/ukb/ukb/docs/Orderofdatacollection.pdf
# Order: Reception (21811) -> Touchscreen cognitive (21825) -> Touchscreen (21822) -> verbal interview (21831) -> biometrics (21834) -> sample collection (21842)

## Get column number corresponding to ukb variable ids in ukb673864.tab, a bulk data file with numberous ukb field ids as columns and samples as rows
awk -v RS='\t' '/f.21821.0.0/{print NR; exit}' ukb673864.tab
awk -v RS='\t' '/f.21834.0.0/{print NR; exit}' ukb673864.tab

## Print columns of interest
awk -F"\t" '{print $1 "\t" $455 "\t" $463 "\t" $469 "\t" $473 "\t" $484}' ukb673864.tab > /home/lfahey/ukb/time_variables_test.txt

## get sample numbers with NA
awk -F"\t" '{print $5}' /home/lfahey/ukb/time_variables_test.txt | grep "NA" | wc -l
# take this number away from total sample number to get number of samples with time data
