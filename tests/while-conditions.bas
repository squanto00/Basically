DIM count AS INTEGER

count = 1

WHILE count <= 4
    IF count = 3 THEN
        PRINT "Found three!"
    ELSE
        PRINT count
    END IF

    count = count + 1
WEND

PRINT "Conditions inside loops work!"
