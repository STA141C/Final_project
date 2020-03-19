## read tinydata of glass for classification
## set data colnames, set colclasses
glass <- as.data.frame(read.table('tinydata/glass.data', header = FALSE, sep = ",",
                                  col.names = c('Id', 'RI', 'Na', 'Mg',
                                                'Al', 'Si', 'K', 'Ca',
                                                'Ba', 'Fe', 'Type'),
                                  colClasses = c("integer", "double", "double", "double",
                                                 "double", "double", "double", "double",
                                                 "double", "double", "factor")))

glass <- subset(glass, select = -Id)
## split the data into train set (75%) and test set (25%)
train_size <- floor(0.75 * nrow(glass))
set.seed(123)
train_ind <- sample(seq_len(nrow(glass)), size = train_size)
train_glass_sample <- glass[train_ind, ]
test_glass_sample <- glass[-train_ind,]

## save train and test sample for test
save(train_glass_sample, file= "tinydata/train_glass_sample.Rda")
save(test_glass_sample, file='tinydata/test_glass_sample.Rda')


## read tiny data of mortality for regression
mortality <-as.data.frame(readxl::read_excel("tinydata/mortality.xls")[,1:7])
train_size <- floor(0.75 * nrow(mortality))
set.seed(123)
train_ind <- sample(seq_len(nrow(mortality)), size = train_size)
train_mortality_sample <- mortality[train_ind, ]
test_mortality_sample <- mortality[-train_ind,]
save(train_mortality_sample, file= "tinydata/train_mortality_sample.Rda")
save(test_mortality_sample, file='tinydata/test_mortality_sample.Rda')

