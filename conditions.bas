' BASICALLY 0.5 conditional logic test

DIM playerHealth AS INTEGER
DIM playerGold AS INTEGER

playerHealth = 75
playerGold = 0

IF playerHealth > 0 THEN
    PRINT "Player is alive!"
END IF

IF playerGold > 0 THEN
    PRINT "Player has gold!"
END IF

PRINT "Conditional test finished!"
