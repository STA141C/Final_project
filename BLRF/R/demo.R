## read tinydata of glass
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
train_sample <- glass[train_ind, ]
test_sample <- glass[-train_ind,]

## test PACKAGE
implement_BLRF(Type~., data = train_sample, gamma = 0.5, b = NULL, s = 10, r= 100, n_var =3, core = 1)
