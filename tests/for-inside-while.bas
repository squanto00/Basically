DIM I AS INTEGER
DIM J AS INTEGER

I = 1

WHILE I <= 2
    FOR J = 1 TO 3
        PRINT I
        PRINT J
    NEXT J

    I = I + 1
WEND

PRINT "Mixed loops finished!"
