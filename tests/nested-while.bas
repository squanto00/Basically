DIM outer AS INTEGER
DIM inner AS INTEGER

outer = 1

WHILE outer <= 3
    inner = 1

    WHILE inner <= 2
        PRINT outer
        PRINT inner
        inner = inner + 1
    WEND

    outer = outer + 1
WEND

PRINT "Nested loops finished!"
