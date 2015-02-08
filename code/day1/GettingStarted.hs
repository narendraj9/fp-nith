alist = [1,2,3,4,5]
takeThree = take 3

anotherList = [10..20]
takeFive = take 5


second xs = head (tail xs)
third xs = head (tail (tail xs))

cube xeta = xeta * xeta * xeta

swap (x, y) = (y, x)

pair x y = (x, y)
