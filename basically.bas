' BASICALLY 0.6
' Typed variables, arithmetic expressions, and conditional logic

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
DIM SHARED sourceLines(0 TO 9999) AS STRING
DIM SHARED sourceLineCount AS INTEGER
DIM SHARED programCounter AS INTEGER

DIM AS STRING filename
DIM AS STRING sourceLine
DIM AS STRING statement
DIM AS STRING outputText
DIM AS STRING variableName
DIM AS STRING upperStatement
DIM AS STRING blockLine

DIM AS INTEGER fileNumber
DIM AS INTEGER i
DIM AS INTEGER foundVariable
DIM AS INTEGER declarationPosition
DIM AS INTEGER declaredType
DIM AS INTEGER thenPosition
DIM AS INTEGER endIfPosition
DIM AS INTEGER nestingDepth
DIM AS INTEGER conditionValue

DIM AS DOUBLE expressionValue

DECLARE FUNCTION ParseExpression() AS DOUBLE
DECLARE FUNCTION ParseTerm() AS DOUBLE
DECLARE FUNCTION ParsePrimary() AS DOUBLE
DECLARE FUNCTION FindVariable(nameToFind AS STRING) AS INTEGER
DECLARE FUNCTION EvaluateCondition(conditionText AS STRING) AS INTEGER
DECLARE SUB SkipExpressionSpaces()

IF COMMAND(1) = "" THEN
    PRINT "BASICALLY 0.6"
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
sourceLineCount = 0

' Load the source before execution so IF blocks can skip lines.
WHILE NOT EOF(fileNumber)
    LINE INPUT #fileNumber, sourceLine

    IF sourceLineCount >= 10000 THEN
        PRINT "Error: Program exceeds the 10000-line limit"
        CLOSE #fileNumber
        END 1
    END IF

    sourceLines(sourceLineCount) = sourceLine
    sourceLineCount = sourceLineCount + 1
WEND

programCounter = 0

WHILE programCounter < sourceLineCount

    sourceLine = sourceLines(programCounter)
    statement = LTRIM(RTRIM(sourceLine))

    ' Ignore empty lines and apostrophe comments.
    IF LEN(statement) > 0 THEN
        IF LEFT(statement, 1) <> "'" THEN

            upperStatement = UCASE(statement)

            ' Handle IF blocks.
            IF UCASE(LEFT(statement, 3)) = "IF " THEN

                thenPosition = INSTR(upperStatement, " THEN")

                IF thenPosition = 0 THEN
                    PRINT "Error: Expected IF condition THEN"
                    CLOSE #fileNumber
                    END 1
                END IF

                outputText = TRIM(MID(statement, 4, thenPosition - 4))
                conditionValue = EvaluateCondition(outputText)

                IF conditionValue < -1 THEN
                    PRINT "Error: Invalid IF condition: "; outputText
                    CLOSE #fileNumber
                    END 1
                END IF

                IF conditionValue = 0 THEN

                    ' Find the next eligible ELSEIF, ELSE, or END IF.
                    ' Ignore clauses belonging to nested IF blocks.
                    nestingDepth = 0
                    endIfPosition = -1

                    FOR i = programCounter + 1 TO sourceLineCount - 1
                        blockLine = UCASE(LTRIM(RTRIM(sourceLines(i))))

                        IF LEN(blockLine) > 0 THEN
                            IF LEFT(blockLine, 1) <> "'" THEN

                                IF LEFT(blockLine, 3) = "IF " THEN
                                    nestingDepth = nestingDepth + 1

                                ELSEIF blockLine = "END IF" THEN
                                    IF nestingDepth = 0 THEN
                                        endIfPosition = i
                                        EXIT FOR
                                    ELSE
                                        nestingDepth = nestingDepth - 1
                                    END IF

                                ELSEIF nestingDepth = 0 AND LEFT(blockLine, 7) = "ELSEIF " THEN

                                    thenPosition = INSTR(blockLine, " THEN")

                                    IF thenPosition = 0 THEN
                                        PRINT "Error: Expected ELSEIF condition THEN"
                                        CLOSE #fileNumber
                                        END 1
                                    END IF

                                    outputText = TRIM(MID(blockLine, 8, thenPosition - 8))
                                    conditionValue = EvaluateCondition(outputText)

                                    IF conditionValue < -1 THEN
                                        PRINT "Error: Invalid ELSEIF condition: "; outputText
                                        CLOSE #fileNumber
                                        END 1
                                    END IF

                                    IF conditionValue <> 0 THEN
                                        endIfPosition = i
                                        EXIT FOR
                                    END IF

                                ELSEIF nestingDepth = 0 AND blockLine = "ELSE" THEN
                                    endIfPosition = i
                                    EXIT FOR
                                END IF

                            END IF
                        END IF
                    NEXT i

                    IF endIfPosition < 0 THEN
                        PRINT "Error: IF without matching END IF"
                        CLOSE #fileNumber
                        END 1
                    END IF

                    ' The main loop increment moves to the selected branch.
                    programCounter = endIfPosition

                END IF

            ELSEIF LEFT(upperStatement, 7) = "ELSEIF " OR upperStatement = "ELSE" THEN

                ' A preceding branch was true.
                ' Skip the remaining branches and find this IF's END IF.
                nestingDepth = 0
                endIfPosition = -1

                FOR i = programCounter + 1 TO sourceLineCount - 1
                    blockLine = UCASE(LTRIM(RTRIM(sourceLines(i))))

                    IF LEN(blockLine) > 0 THEN
                        IF LEFT(blockLine, 1) <> "'" THEN
                            IF LEFT(blockLine, 3) = "IF " THEN
                                nestingDepth = nestingDepth + 1

                            ELSEIF blockLine = "END IF" THEN
                                IF nestingDepth = 0 THEN
                                    endIfPosition = i
                                    EXIT FOR
                                ELSE
                                    nestingDepth = nestingDepth - 1
                                END IF
                            END IF
                        END IF
                    END IF
                NEXT i

                IF endIfPosition < 0 THEN
                    PRINT "Error: IF without matching END IF"
                    CLOSE #fileNumber
                    END 1
                END IF

                programCounter = endIfPosition

            ELSEIF upperStatement = "END IF" THEN

                ' A selected branch reaches END IF normally.

            ' Handle PRINT statements.
            ELSEIF UCASE(LEFT(statement, 5)) = "PRINT" THEN

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

    programCounter = programCounter + 1
WEND

CLOSE #fileNumber
PRINT "Program finished."


' Evaluate a numeric comparison.
' Returns 1 for true, 0 for false, and -1 for an invalid condition.
FUNCTION EvaluateCondition(conditionText AS STRING) AS INTEGER

    DIM AS INTEGER i
    DIM AS INTEGER operatorLength
    DIM AS DOUBLE leftValue
    DIM AS DOUBLE rightValue
    DIM AS STRING comparisonOperator
    DIM AS STRING leftText
    DIM AS STRING rightText
    DIM AS STRING currentChar
    DIM AS STRING nextChar

    EvaluateCondition = -2
    comparisonOperator = ""
    operatorLength = 0

    ' Locate the comparison operator between two arithmetic expressions.
    FOR i = 1 TO LEN(conditionText)

        currentChar = MID(conditionText, i, 1)
        nextChar = ""

        IF i < LEN(conditionText) THEN
            nextChar = MID(conditionText, i + 1, 1)
        END IF

        IF currentChar = "<" OR currentChar = ">" THEN
            IF nextChar = "=" OR (currentChar = "<" AND nextChar = ">") THEN
                comparisonOperator = currentChar + nextChar
                operatorLength = 2
                EXIT FOR
            ELSE
                comparisonOperator = currentChar
                operatorLength = 1
                EXIT FOR
            END IF
        ELSEIF currentChar = "=" THEN
            comparisonOperator = "="
            operatorLength = 1
            EXIT FOR
        END IF

    NEXT i

    IF operatorLength = 0 THEN EXIT FUNCTION

    leftText = TRIM(LEFT(conditionText, i - 1))
    rightText = TRIM(MID(conditionText, i + operatorLength))

    IF LEN(leftText) = 0 OR LEN(rightText) = 0 THEN
        EXIT FUNCTION
    END IF

    ' Evaluate the left-hand expression.
    expressionText = leftText
    expressionPosition = 1
    parseError = 0
    leftValue = ParseExpression()
    SkipExpressionSpaces()

    IF parseError <> 0 OR expressionPosition <= LEN(expressionText) THEN
        EXIT FUNCTION
    END IF

    ' Evaluate the right-hand expression.
    expressionText = rightText
    expressionPosition = 1
    parseError = 0
    rightValue = ParseExpression()
    SkipExpressionSpaces()

    IF parseError <> 0 OR expressionPosition <= LEN(expressionText) THEN
        EXIT FUNCTION
    END IF

    SELECT CASE comparisonOperator
        CASE "="
            EvaluateCondition = (leftValue = rightValue)
        CASE "<>"
            EvaluateCondition = (leftValue <> rightValue)
        CASE "<"
            EvaluateCondition = (leftValue < rightValue)
        CASE ">"
            EvaluateCondition = (leftValue > rightValue)
        CASE "<="
            EvaluateCondition = (leftValue <= rightValue)
        CASE ">="
            EvaluateCondition = (leftValue >= rightValue)
        CASE ELSE
            EvaluateCondition = -2
    END SELECT

END FUNCTION


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
