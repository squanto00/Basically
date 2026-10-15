' BASICALLY 0.6 nested conditionals test

DIM playerHealth AS INTEGER
DIM playerGold AS INTEGER

playerHealth = 100
playerGold = 0

IF playerHealth > 0 THEN
    PRINT "Player is alive!"

    IF playerGold > 0 THEN
        PRINT "Player is wealthy!"
    ELSE
        PRINT "Player is broke!"
    END IF
ELSE
    PRINT "Player is dead!"
END IF

PRINT "Nested test finished!"
