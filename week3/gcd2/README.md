# gcd2
一个列表中数字的最大公约数

## 超时解法
```
solution xs
  | length xs == 1 = head xs
  | otherwise = gcd (head xs) (solution $ tail xs) 
```

原因:length超时。

## 简洁解法
```
solution = foldl1 gcd

```

foldl1作用：对列表按照函数做折叠。
