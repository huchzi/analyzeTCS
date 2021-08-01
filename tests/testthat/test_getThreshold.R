testFile <- parseTCS(system.file("tests", "testdata", "01_OD_2020-01-29_090720.txt", package = "analyzeTCS"))

print(getwd())

expect_equal(getThreshold("both", "outer", testFile),
             c(red = 1.663, green = 3.647, blue = 5.956, cyan = 6.875),
             tolerance = 0.001)
