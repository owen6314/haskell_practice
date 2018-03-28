# listcontain
判断两个列表之间是否存在包含关系

## 简洁解法
solution xs ys = subset xs ys || subset ys xs
  where subset = all . flip elem


