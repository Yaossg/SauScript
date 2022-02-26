# 版本记录

BUG 修复不列入（除非仅包含 BUG 修复）

## v1.0

初始版本

### v1.0.1

加入 for 循环

移除逗号运算符

### v1.0.2

加入浮点数支持

### v1.0.3

`nan`和`inf`

### v1.1-alpha

重写表达式解析

### v1.1-beta

throw input print 不再是运算符而是语句

加入三目运算符 ?:

## v1.1

加入函数

### v1.1.1

void 类型

RefNode 缓存优化

更多外部函数和常量

`__LINE__`

### v1.1.2

跳转优化

补充外部函数

### v1.2-alpha

删除 del

优化外部函数

代码块风格改为花括号样式

repeat until 改为 do while

elif 改为 else if

## v1.2

let 变为 :=

return throw input print 变为运算符

break continue function if-else try-catch while do-while for {} 变为表达式

void() 变为 {}

加入 yield 运算符

### v1.2.1

加入重现脚本和绘制语法树支持

优化字符串化和代码结构

### v1.2.2

_BUG 修复_

### v1.2.3

重写了求值

函数体现在必须用 = 引导

### v1.2.4

支持了十六进制实数

REPL 的作用域扩大

### v1.3-alpha

删除 input 运算符

加入 readInt 和 readReal 函数

加入无符号右移

加入列表

### v1.3-beta

加入函数重载

:tada: SauScript 代码破 2000 行啦

## v1.3

any 类型，用于函数参数和返回值

点运算符——成员函数语法糖

加入常用列表操作

### v1.3.1

关键字 function 改为 fn

函数参数和返回值类型缺省为 any

加入 <=> 运算符

移除 print 运算符

加入 print 和 println 函数

加入 map 和 filter 函数
