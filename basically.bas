' BASICALLY 0.4
' Typed variables and arithmetic expressions

CONST MAX_VARIABLES = 100
CONST TYPE_STRING = 1
CONST TYPE_INTEGER = 2

DIM SHARED variableNames(0 TO MAX_VARIABLES - 1) AS STRING
DIM SHARED variableTypes(0 TO MAX_VARIABLES - 1) AS INTEGER
DIM SHARED stringValues(0 TO MAX_VARIABLES - 1) AS STRING
DIM SHARED numericValues(0 TO MAX_VARIABLES - 1) AS LONGINT
DIM SHARED variableCount AS INTEGER

DIM SHARED expressionText AS STRING
DIM SHARED expressionPosition AS INTEGER
DIM SHARED parseError AS INTEGER

DIM AS STRING filename
DIM AS STRING sourceLine
DIM AS STRING statement
DIM AS STRING outputText
DIM AS STRING variableName
DIM AS STRING upperStatement

DIM AS INTEGER fileNumber
DIM AS INTEGER i
DIM AS INTEGER foundVariable
DIM AS INTEGER declarationPosition
DIM AS INTEGER declaredType

DIM AS DOUBLE expressionValue

DECLARE FUNCTION ParseExpression() AS DOUBLE
DECLARE FUNCTION ParseTerm() AS DOUBLE
DECLARE FUNCTION ParsePrimary() AS DOUBLE
DECLARE FUNCTION FindVariable(nameToFind AS STRING) AS INTEGER
DECLARE SUB SkipExpressionSpaces()

IF COMMAND(1) = "" THEN
    PRINT "BASICALLY 0.4"
    PRINT "Usage: basically <file.bas>"
    END 1
END IF

filename = COMMAND(1)
fileNumber = FREEFILE

IF OPEN(filename FOR INPUT AS #fileNumber) <> 0 THEN
    PRINT "Error: Could not open "; filename
    END 1
END IF

variableCount = 0

WHILE NOT EOF(fileNumber)

    LINE INPUT #fileNumber, sourceLine
    statement = LTRIM(RTRIM(sourceLine))

    ' Ignore empty lines and apostrophe comments.
    IF LEN(statement) > 0 THEN
        IF LEFT(statement, 1) <> "'" THEN

            upperStatement = UCASE(statement)

            ' Handle PRINT statements.
            IF UCASE(LEFT(statement, 5)) = "PRINT" THEN

                outputText = LTRIM(MID(statement, 6))

                ' Print a string literal.
                IF LEN(outputText) >= 2 THEN
                    IF LEFT(outputText, 1) = CHR(34) AND RIGHT(outputText, 1) = CHR(34) THEN

                        PRINT MID(outputText, 2, LEN(outputText) - 2)

                    ELSE

                        ' Check whether this is a variable name.
                        foundVariable = FindVariable(outputText)

                        IF foundVariable >= 0 THEN

                            IF variableTypes(foundVariable) = TYPE_STRING THEN
                                PRINT stringValues(foundVariable)
                            ELSE
                                PRINT numericValues(foundVariable)
                            END IF

                        ELSE

                            ' Otherwise, evaluate a numeric expression.
                            expressionText = outputText
                            expressionPosition = 1
                            parseError = 0

                            expressionValue = ParseExpression()
                            SkipExpressionSpaces()

                            IF parseError <> 0 OR expressionPosition <= LEN(expressionText) THEN
                                PRINT "Error: Invalid PRINT expression: "; outputText
                                CLOSE #fileNumber
                                END 1
                            ELSE
                                PRINT FIX(expressionValue)
                            END IF

                        END IF
                    END IF
                ELSE
                    PRINT ""
                END IF

            ELSE

                ' Handle variable declarations.
                IF UCASE(LEFT(statement, 4)) = "DIM " THEN

                    declaredType = 0
                    declarationPosition = INSTR(upperStatement, " AS STRING")

                    IF declarationPosition > 0 THEN
                        declaredType = TYPE_STRING
                    ELSE
                        declarationPosition = INSTR(upperStatement, " AS INTEGER")

                        IF declarationPosition > 0 THEN
                            declaredType = TYPE_INTEGER
                        END IF
                    END IF

                    IF declarationPosition = 0 THEN
                        PRINT "Error: Expected DIM name AS STRING or DIM name AS INTEGER"
                        CLOSE #fileNumber
                        END 1
                    END IF

                    variableName = LCASE(TRIM(MID(statement, 5, declarationPosition - 5)))

                    IF LEN(variableName) = 0 THEN
                        PRINT "Error: Missing variable name"
                        CLOSE #fileNumber
                        END 1
                    END IF

                    ' Check for duplicate declarations.
                    foundVariable = FindVariable(variableName)

                    IF foundVariable >= 0 THEN
                        PRINT "Error: Variable already declared: "; variableName
                        CLOSE #fileNumber
                        END 1
                    END IF

                    IF variableCount >= MAX_VARIABLES THEN
                        PRINT "Error: Too many variables"
                        CLOSE #fileNumber
                        END 1
                    END IF

                    variableNames(variableCount) = variableName
                    variableTypes(variableCount) = declaredType
                    stringValues(variableCount) = ""
                    numericValues(variableCount) = 0
                    variableCount = variableCount + 1

                ELSE

                    ' Handle assignments.
                    declarationPosition = INSTR(statement, "=")

                    IF declarationPosition > 1 THEN

                        variableName = LCASE(TRIM(LEFT(statement, declarationPosition - 1)))
                        outputText = TRIM(MID(statement, declarationPosition + 1))

                        foundVariable = FindVariable(variableName)

                        IF foundVariable < 0 THEN
                            PRINT "Error: Unknown variable "; variableName
                            CLOSE #fileNumber
                            END 1
                        END IF

                        IF variableTypes(foundVariable) = TYPE_STRING THEN

                            ' String variables accept quoted strings or other string variables.
                            IF LEN(outputText) >= 2 THEN
                                IF LEFT(outputText, 1) = CHR(34) AND RIGHT(outputText, 1) = CHR(34) THEN

                                    stringValues(foundVariable) = MID(outputText, 2, LEN(outputText) - 2)

                                ELSE

                                    i = FindVariable(outputText)

                                    IF i >= 0 THEN
                                        IF variableTypes(i) = TYPE_STRING THEN
                                            stringValues(foundVariable) = stringValues(i)
                                        ELSE
                                            PRINT "Error: Cannot assign an INTEGER to a STRING"
                                            CLOSE #fileNumber
                                            END 1
                                        END IF
                                    ELSE
                                        PRINT "Error: STRING values must be quoted or use a STRING variable"
                                        CLOSE #fileNumber
                                        END 1
                                    END IF

                                END IF
                            ELSE
                                stringValues(foundVariable) = outputText
                            END IF

                        ELSE

                            ' Evaluate the numeric expression on the right-hand side.
                            expressionText = outputText
                            expressionPosition = 1
                            parseError = 0

                            expressionValue = ParseExpression()
                            SkipExpressionSpaces()

                            IF parseError <> 0 OR expressionPosition <= LEN(expressionText) THEN
                                PRINT "Error: Invalid INTEGER expression: "; outputText
                                CLOSE #fileNumber
                                END 1
                            ELSE
                                ' INTEGER assignments discard fractional parts.
                                numericValues(foundVariable) = FIX(expressionValue)
                            END IF

                        END IF

                    ELSE
                        PRINT "Error: Unrecognized statement: "; statement
                        CLOSE #fileNumber
                        END 1
                    END IF

                END IF
            END IF
        END IF
    END IF

WEND

CLOSE #fileNumber
PRINT "Program finished."


' Find a declared variable by name.
FUNCTION FindVariable(nameToFind AS STRING) AS INTEGER

    DIM AS INTEGER i

    FindVariable = -1

    FOR i = 0 TO variableCount - 1
        IF variableNames(i) = LCASE(TRIM(nameToFind)) THEN
            FindVariable = i
            EXIT FUNCTION
        END IF
    NEXT i

END FUNCTION


' Skip whitespace in a numeric expression.
SUB SkipExpressionSpaces()

    DO WHILE expressionPosition <= LEN(expressionText)

        IF MID(expressionText, expressionPosition, 1) <> " " AND _
           MID(expressionText, expressionPosition, 1) <> CHR(9) THEN
            EXIT DO
        END IF

        expressionPosition = expressionPosition + 1

    LOOP

END SUB


' Parse addition and subtraction.
FUNCTION ParseExpression() AS DOUBLE

    DIM AS DOUBLE result
    DIM AS DOUBLE rightValue
    DIM AS STRING operation

    result = ParseTerm()

    DO

        SkipExpressionSpaces()

        IF expressionPosition > LEN(expressionText) THEN EXIT DO

        operation = MID(expressionText, expressionPosition, 1)

        IF operation <> "+" AND operation <> "-" THEN EXIT DO

        expressionPosition = expressionPosition + 1
        rightValue = ParseTerm()

        IF operation = "+" THEN
            result = result + rightValue
        ELSE
            result = result - rightValue
        END IF

    LOOP

    ParseExpression = result

END FUNCTION


' Parse multiplication and division.
FUNCTION ParseTerm() AS DOUBLE

    DIM AS DOUBLE result
    DIM AS DOUBLE rightValue
    DIM AS STRING operation

    result = ParsePrimary()

    DO

        SkipExpressionSpaces()

        IF expressionPosition > LEN(expressionText) THEN EXIT DO

        operation = MID(expressionText, expressionPosition, 1)

        IF operation <> "*" AND operation <> "/" THEN EXIT DO

        expressionPosition = expressionPosition + 1
        rightValue = ParsePrimary()

        IF operation = "*" THEN

            result = result * rightValue

        ELSE

            IF rightValue = 0 THEN
                parseError = 1
                result = 0
            ELSE
                result = result / rightValue
            END IF

        END IF

    LOOP

    ParseTerm = result

END FUNCTION


' Parse numbers, numeric variables, unary signs, and parentheses.
FUNCTION ParsePrimary() AS DOUBLE

    DIM AS STRING ch
    DIM AS STRING variableIdentifier
    DIM AS STRING numberText

    DIM AS INTEGER startPosition
    DIM AS INTEGER found

    ParsePrimary = 0

    SkipExpressionSpaces()

    IF expressionPosition > LEN(expressionText) THEN
        parseError = 1
        EXIT FUNCTION
    END IF

    ch = MID(expressionText, expressionPosition, 1)

    ' Unary minus.
    IF ch = "-" THEN
        expressionPosition = expressionPosition + 1
        ParsePrimary = -ParsePrimary()
        EXIT FUNCTION
    END IF

    ' Unary plus.
    IF ch = "+" THEN
        expressionPosition = expressionPosition + 1
        ParsePrimary = ParsePrimary()
        EXIT FUNCTION
    END IF

    ' Parenthesized expression.
    IF ch = "(" THEN

        expressionPosition = expressionPosition + 1
        ParsePrimary = ParseExpression()

        SkipExpressionSpaces()

        IF expressionPosition > LEN(expressionText) THEN
            parseError = 1
        ELSEIF MID(expressionText, expressionPosition, 1) <> ")" THEN
            parseError = 1
        ELSE
            expressionPosition = expressionPosition + 1
        END IF

        EXIT FUNCTION

    END IF

    ' Integer literal.
    IF ch >= "0" AND ch <= "9" THEN

        startPosition = expressionPosition

        DO WHILE expressionPosition <= LEN(expressionText)

            ch = MID(expressionText, expressionPosition, 1)

            IF ch < "0" OR ch > "9" THEN EXIT DO

            expressionPosition = expressionPosition + 1

        LOOP

        numberText = MID(expressionText, startPosition, expressionPosition - startPosition)
        ParsePrimary = VAL(numberText)
        EXIT FUNCTION

    END IF

    ' Variable name.
    IF (ch >= "A" AND ch <= "Z") OR _
       (ch >= "a" AND ch <= "z") OR ch = "_" THEN

        startPosition = expressionPosition

        DO WHILE expressionPosition <= LEN(expressionText)

            ch = MID(expressionText, expressionPosition, 1)

            IF NOT ((ch >= "A" AND ch <= "Z") OR _
                    (ch >= "a" AND ch <= "z") OR _
                    (ch >= "0" AND ch <= "9") OR ch = "_") THEN
                EXIT DO
            END IF

            expressionPosition = expressionPosition + 1

        LOOP

        variableIdentifier = LCASE(MID(expressionText, startPosition, expressionPosition - startPosition))
        found = FindVariable(variableIdentifier)

        IF found < 0 THEN
            parseError = 1
        ELSEIF variableTypes(found) <> TYPE_INTEGER THEN
            parseError = 1
        ELSE
            ParsePrimary = numericValues(found)
        END IF

        EXIT FUNCTION

    END IF

    ' Anything else is invalid.
    parseError = 1

END FUNCTION
