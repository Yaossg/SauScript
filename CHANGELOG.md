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

### v1.3.2

删除 yield 运算符 和 do-while 循环

break 和 continue 变为 yielding 运算符

循环现在默认返回一个列表

for 循环的初始化和迭代子句可以用逗号间隔为多个分句

加入 for-each 循环

加入无参 print 和 println

### v1.3.3

修改了 yielding 的机制

改善了输入函数

### v1.4-alpha

加入字符支持

比较运算符支持所有类型

加入更多列表操作

### v1.4-beta

更好的字面量

typeid 函数

### v1.4-gamma

重写错误提示信息

现在任意类型都可以隐式转换为 void

### v1.4-delta

改进错误提示信息

允许表达式中部分多余的换行符

generate 和 reduce 函数

### v1.4-epsilon

加入可变性锁，在 for-each 期间列表不可变

addAll 被 concat 取代，generate 被 iota 取代，indexOf 被 find 取代

删除 lastIndexOf 加入 flat、find_if、无 init 的 reduce

## v1.4

加入字符串

重置 I/O 函数，调整部分函数，加入更多函数

暂时删除自定义 err

### v1.4.1

更好的异常

重新加入自定义 err

for 循环现在只能 for-each

更好的比较运算

### v1.4.2

加入字典

更多函数

细化字符错误信息

