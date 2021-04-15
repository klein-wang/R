library(dplyr)
data(mtcars)
str(mtcars)

#1.按行筛选:filter()，按给定的逻辑判断筛选出符合要求的子数据集, 类似于 subset() 函数，如：
filter(mtcars, mpg>=22)

#2.按列筛选:select(), 用列名作参数来选择子数据集。dplyr包中提供了些特殊功能的函数与select函数结合使用, 如：
data(iris)
iris = tbl_df(iris)
#选取变量名前缀包含Petal的列
select(iris, starts_with("Petal"))
#选取变量名前缀不包含Petal的列
select(iris, -starts_with("Petal"))

#3.mutate变量变形: mutate(), 可以对数据框中已有的变量进行操作或者增加变量
#新增列
mtcars %>% mutate(cyl2 = cyl * 2,cyl4 = cyl2 * 2)

#删除列
mtcars %>% mutate(mpg = NULL,disp = disp * 0.0163871)

#4.排序函数: arrange(), 这里需要注意排序与排名的区别，如：
#按给定的列名依次对行进行排序
arrange(mtcars, mpg)
#多个列名
arrange(mtcars, mpg,disp)
