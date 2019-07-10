library(mice)

# 从csv文件读取newdata数据
newdata <- read.csv('newdata20190710.csv')

#根据newdata各属性值是否为NA构造0-1矩阵x。0表示值非NA,1表示值为NA
x <- as.data.frame(abs(is.na(newdata)))

# 对x每一列进行求和，求和结果大于0代表该属性中有NA值。筛选包含NA值的属性列构造0-1矩阵y
y <- x[which(apply(x,2,sum)>0)]


# 包含NA值的属性名
incomplete_columns <- colnames(y)

print(incomplete_columns)

# 计算包含NA值的各属性之间的相关性,并保存在csv文件中
write.csv(cor(y), 'cor_y_y.csv') 

# 计算矩阵y中各属性的缺失值与其他所有属性之间的相关性，并保存在csv文件中
cor_y_all <- cor(newdata, y, use="pairwise.complete.obs")
write.csv(cor_y_all, 'cor_y_all.csv')

imp <- mice(newdata)

fit <- with(imp, glm(income ~ 1))

pooled <- pool(fit)


