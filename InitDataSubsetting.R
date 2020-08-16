
X2019publicTX_allColumns <- subset(X2019publiclarcsv, state_code== 'TX')
write.csv(X2019publicTX_allColumns, "X2019publicTX_allColumns.csv", row.names = TRUE)
remove(X2019publicTX_allColumns)

X2019publicTN_allColumns <- subset(X2019publiclarcsv, state_code== 'TN')
write.csv(X2019publicTN_allColumns, "X2019publicTN_allColumns.csv", row.names = TRUE)
remove(X2019publicTN_allColumns)

X2019publicLA_allColumns <- subset(X2019publiclarcsv, state_code== 'LA')
write.csv(X2019publicLA_allColumns, "X2019publicLA_allColumns.csv", row.names = TRUE)
remove(X2019publicLA_allColumns)

X2019publicNY_allColumns <- subset(X2019publiclarcsv, state_code== 'NY')
write.csv(X2019publicNY_allColumns, "X2019publicNY_allColumns.csv", row.names = TRUE)
remove(X2019publicNY_allColumns)