' BASICALLY 0.7
' Typed variables, arithmetic expressions, conditional logic, and loops

CONST MAX_VARIABLES = 100
CONST MAX_SOURCE_LINES = 10000
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

DIM SHARED sourceLines(0 TO MAX_SOURCE_LINES - 1) AS STRING
DIM SHARED sourceLineCount AS INTEGER
DIM SHARED programCounter AS INTEGER

' Loop matching tables.
DIM SHARED loopStartForEnd(0 TO MAX_SOURCE_LINES - 1) AS INTEGER

' FOR/NEXT matching and runtime state.
DIM SHARED forStartForEnd(0 TO MAX_SOURCE_LINES - 1) AS INTEGER
DIM SHARED forEndForStart(0 TO MAX_SOURCE_LINES - 1) AS INTEGER
DIM SHARED forLoopVariable(0 TO MAX_SOURCE_LINES - 1) AS INTEGER
DIM SHARED forLoopLimit(0 TO MAX_SOURCE_LINES - 1) AS LONGINT
DIM SHARED forLoopStep(0 TO MAX_SOURCE_LINES - 1) AS LONGINT
DIM SHARED loopEndForStart(0 TO MAX_SOURCE_LINES - 1) AS INTEGER

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

' Loop prepass variables.
DIM AS INTEGER blockDepth
DIM AS INTEGER blockStart
DIM AS INTEGER blockEnd
DIM AS INTEGER blockStack(0 TO MAX_SOURCE_LINES - 1)

' FOR/NEXT parsing state.
DIM AS INTEGER forStepPosition
DIM AS INTEGER forStartVariable
DIM AS STRING forRest
DIM AS STRING forStartText
DIM AS STRING forLimitText
DIM AS STRING forStepText
DIM AS DOUBLE forStartValue
DIM AS DOUBLE forLimitValue
DIM AS DOUBLE forStepValue

DIM AS DOUBLE expressionValue

DECLARE FUNCTION ParseExpression() AS DOUBLE
DECLARE FUNCTION ParseTerm() AS DOUBLE
DECLARE FUNCTION ParsePrimary() AS DOUBLE
DECLARE FUNCTION FindVariable(nameToFind AS STRING) AS INTEGER
DECLARE FUNCTION EvaluateCondition(conditionText AS STRING) AS INTEGER
DECLARE FUNCTION EvaluateNumericExpression(expressionToEvaluate AS STRING) AS DOUBLE
DECLARE SUB SkipExpressionSpaces()

IF COMMAND(1) = "" THEN
    PRINT "BASICALLY 0.7"
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

' Load the entire source before execution.
WHILE NOT EOF(fileNumber)

    LINE INPUT #fileNumber, sourceLine

    IF sourceLineCount >= MAX_SOURCE_LINES THEN
        PRINT "Error: Program exceeds the 10000-line limit"
        CLOSE #fileNumber
        END 1
    END IF

    sourceLines(sourceLineCount) = sourceLine
    sourceLineCount = sourceLineCount + 1

WEND

' Match WHILE and WEND statements before execution.
' This allows nested loops and detects unmatched loop statements.
blockDepth = 0

FOR i = 0 TO sourceLineCount - 1

    statement = LTRIM(RTRIM(sourceLines(i)))
    upperStatement = UCASE(statement)

    loopStartForEnd(i) = -1
    loopEndForStart(i) = -1

    IF LEN(statement) > 0 THEN
        IF LEFT(statement, 1) <> "'" THEN

            IF LEFT(upperStatement, 6) = "WHILE " THEN

                IF blockDepth >= MAX_SOURCE_LINES THEN
                    PRINT "Error: Too many nested WHILE loops"
                    CLOSE #fileNumber
                    END 1
                END IF

                blockStack(blockDepth) = i
                blockDepth = blockDepth + 1

            ELSEIF upperStatement = "WEND" THEN

                IF blockDepth = 0 THEN
                    PRINT "Error: WEND without matching WHILE at line "; i + 1
                    CLOSE #fileNumber
                    END 1
                END IF

                blockDepth = blockDepth - 1
                blockStart = blockStack(blockDepth)
                blockEnd = i

                loopEndForStart(blockStart) = blockEnd
                loopStartForEnd(blockEnd) = blockStart

            END IF

        END IF
    END IF

NEXT i

IF blockDepth > 0 THEN
    PRINT "Error: WHILE without matching WEND at line "; _
        blockStack(blockDepth - 1) + 1
    CLOSE #fileNumber
    END 1
END IF

' Match FOR and NEXT independently of WHILE/WEND.
' Separate tables allow the two loop types to be nested together.
blockDepth = 0

FOR i = 0 TO sourceLineCount - 1

    statement = LTRIM(RTRIM(sourceLines(i)))
    upperStatement = UCASE(statement)

    forStartForEnd(i) = -1
    forEndForStart(i) = -1

    IF LEN(statement) > 0 THEN
        IF LEFT(statement, 1) <> "'" THEN

            IF LEFT(upperStatement, 4) = "FOR " THEN

                IF blockDepth >= MAX_SOURCE_LINES THEN
                    PRINT "Error: Too many nested FOR loops"
                    CLOSE #fileNumber
                    END 1
                END IF

                blockStack(blockDepth) = i
                blockDepth = blockDepth + 1

            ELSEIF upperStatement = "NEXT" OR _
                LEFT(upperStatement, 5) = "NEXT " THEN

                IF blockDepth = 0 THEN
                    PRINT "Error: NEXT without matching FOR at line "; i + 1
                    CLOSE #fileNumber
                    END 1
                END IF

                blockDepth = blockDepth - 1
                blockStart = blockStack(blockDepth)

                forEndForStart(blockStart) = i
                forStartForEnd(i) = blockStart

            END IF

        END IF
    END IF

NEXT i

IF blockDepth > 0 THEN
    PRINT "Error: FOR without matching NEXT at line "; _
        blockStack(blockDepth - 1) + 1
    CLOSE #fileNumber
    END 1
END IF

programCounter = 0

' Main interpreter loop.
WHILE programCounter < sourceLineCount

    sourceLine = sourceLines(programCounter)
    statement = LTRIM(RTRIM(sourceLine))
    upperStatement = UCASE(statement)


    ' Ignore blank lines and apostrophe comments.
    IF LEN(statement) > 0 THEN
        IF LEFT(statement, 1) <> "'" THEN

            ' Handle FOR loops.
            IF LEFT(upperStatement, 4) = "FOR " THEN

                outputText = TRIM(MID(statement, 5))
                declarationPosition = INSTR(outputText, "=")

                IF declarationPosition <= 1 THEN
                    PRINT "Error: Expected FOR variable = start TO limit"
                    CLOSE #fileNumber
                    END 1
                END IF

                variableName = LCASE(TRIM(LEFT(outputText, declarationPosition - 1)))
                forStartVariable = FindVariable(variableName)

                IF forStartVariable < 0 THEN
                    PRINT "Error: Unknown FOR variable "; variableName
                    CLOSE #fileNumber
                    END 1
                END IF

                IF variableTypes(forStartVariable) <> TYPE_INTEGER THEN
                    PRINT "Error: FOR variable must be INTEGER: "; variableName
                    CLOSE #fileNumber
                    END 1
                END IF

                outputText = TRIM(MID(outputText, declarationPosition + 1))
                thenPosition = INSTR(UCASE(outputText), " TO ")

                IF thenPosition = 0 THEN
                    PRINT "Error: Expected TO in FOR statement"
                    CLOSE #fileNumber
                    END 1
                END IF

                forStartText = TRIM(LEFT(outputText, thenPosition - 1))
                forRest = TRIM(MID(outputText, thenPosition + 4))
                forStepPosition = INSTR(UCASE(forRest), " STEP ")

                IF forStepPosition > 0 THEN
                    forLimitText = TRIM(LEFT(forRest, forStepPosition - 1))
                    forStepText = TRIM(MID(forRest, forStepPosition + 6))
                ELSE
                    forLimitText = forRest
                    forStepText = "1"
                END IF

                IF LEN(forStartText) = 0 OR LEN(forLimitText) = 0 OR _
                    LEN(forStepText) = 0 THEN
                    PRINT "Error: Invalid FOR expression"
                    CLOSE #fileNumber
                    END 1
                END IF

                forStartValue = EvaluateNumericExpression(forStartText)

                IF parseError <> 0 THEN
                    PRINT "Error: Invalid FOR start expression: "; forStartText
                    CLOSE #fileNumber
                    END 1
                END IF

                forLimitValue = EvaluateNumericExpression(forLimitText)

                IF parseError <> 0 THEN
                    PRINT "Error: Invalid FOR limit expression: "; forLimitText
                    CLOSE #fileNumber
                    END 1
                END IF

                forStepValue = EvaluateNumericExpression(forStepText)

                IF parseError <> 0 THEN
                    PRINT "Error: Invalid FOR STEP expression: "; forStepText
                    CLOSE #fileNumber
                    END 1
                END IF

                IF forStartValue <> FIX(forStartValue) OR _
                   forLimitValue <> FIX(forLimitValue) OR _
                   forStepValue <> FIX(forStepValue) THEN
                   PRINT "Error: FOR start, limit, and STEP must be integers"
                   CLOSE #fileNumber
                   END 1
               END IF

               numericValues(forStartVariable) = FIX(forStartValue)
                forLoopVariable(programCounter) = forStartVariable
                forLoopLimit(programCounter) = FIX(forLimitValue)
                forLoopStep(programCounter) = FIX(forStepValue)

                IF forLoopStep(programCounter) = 0 THEN
                    PRINT "Error: FOR STEP cannot be zero"
                    CLOSE #fileNumber
                    END 1
                END IF

                ' Skip the body if the initial value is already past the limit.
                IF (forLoopStep(programCounter) > 0 AND _
                    numericValues(forStartVariable) > forLoopLimit(programCounter)) OR _
                    (forLoopStep(programCounter) < 0 AND _
                    numericValues(forStartVariable) < forLoopLimit(programCounter)) THEN

                    programCounter = forEndForStart(programCounter)

                END IF

            ELSEIF upperStatement = "NEXT" OR _
                LEFT(upperStatement, 5) = "NEXT " THEN

                IF forStartForEnd(programCounter) < 0 THEN
                    PRINT "Error: NEXT without matching FOR at line "; _
                        programCounter + 1
                    CLOSE #fileNumber
                    END 1
                END IF

                blockStart = forStartForEnd(programCounter)
                forStartVariable = forLoopVariable(blockStart)

                ' If NEXT names a variable, it must match its FOR.
                IF LEN(TRIM(statement)) > 4 THEN

                    variableName = LCASE(TRIM(MID(statement, 6)))

                    IF variableName <> variableNames(forStartVariable) THEN
                        PRINT "Error: NEXT variable does not match FOR at line "; _
                            programCounter + 1
                        CLOSE #fileNumber
                        END 1
                    END IF

                END IF

                numericValues(forStartVariable) = _
                    numericValues(forStartVariable) + forLoopStep(blockStart)

                ' Return to the body while the counter remains in range.
                IF (forLoopStep(blockStart) > 0 AND _
                    numericValues(forStartVariable) <= forLoopLimit(blockStart)) OR _
                    (forLoopStep(blockStart) < 0 AND _
                    numericValues(forStartVariable) >= forLoopLimit(blockStart)) THEN

                    ' Continue at the first statement inside the loop.
                    ' The main interpreter increments the counter afterward.
                    programCounter = blockStart

                END IF

            ' Handle WHILE loops.
            ELSEIF LEFT(upperStatement, 6) = "WHILE " THEN

                outputText = TRIM(MID(statement, 7))
                conditionValue = EvaluateCondition(outputText)

                IF conditionValue = -2 THEN
                    PRINT "Error: Invalid WHILE condition: "; outputText
                    CLOSE #fileNumber
                    END 1
                END IF

                IF conditionValue = 0 THEN

                    ' False condition: skip to the matching WEND.
                    IF loopEndForStart(programCounter) < 0 THEN
                        PRINT "Error: WHILE without matching WEND at line "; _
                            programCounter + 1
                        CLOSE #fileNumber
                        END 1
                    END IF

                    programCounter = loopEndForStart(programCounter)

                END IF

            ELSEIF upperStatement = "WEND" THEN

                ' Return to the matching WHILE.
                IF loopStartForEnd(programCounter) < 0 THEN
                    PRINT "Error: WEND without matching WHILE at line "; _
                        programCounter + 1
                    CLOSE #fileNumber
                    END 1
                END IF

                ' The main loop increments the counter after this block.
                ' Subtracting one makes execution return to WHILE.
                programCounter = loopStartForEnd(programCounter) - 1

            ' Handle IF blocks.
            ELSEIF LEFT(upperStatement, 3) = "IF " THEN

                thenPosition = INSTR(upperStatement, " THEN")

                IF thenPosition = 0 THEN
                    PRINT "Error: Expected IF condition THEN"
                    CLOSE #fileNumber
                    END 1
                END IF

                outputText = TRIM(MID(statement, 4, thenPosition - 4))
                conditionValue = EvaluateCondition(outputText)

                IF conditionValue = -2 THEN
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

                                ELSEIF nestingDepth = 0 AND _
                                    LEFT(blockLine, 7) = "ELSEIF " THEN

                                    thenPosition = INSTR(blockLine, " THEN")

                                    IF thenPosition = 0 THEN
                                        PRINT "Error: Expected ELSEIF condition THEN"
                                        CLOSE #fileNumber
                                        END 1
                                    END IF

                                    outputText = TRIM(MID(blockLine, 8, thenPosition - 8))
                                    conditionValue = EvaluateCondition(outputText)

                                    IF conditionValue = -2 THEN
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

                    programCounter = endIfPosition

                END IF

            ELSEIF LEFT(upperStatement, 7) = "ELSEIF " OR _
                upperStatement = "ELSE" THEN

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
            ELSEIF LEFT(upperStatement, 5) = "PRINT" THEN

                outputText = LTRIM(MID(statement, 6))

                IF LEN(outputText) > 0 THEN

                    ' Print a quoted string.
                    IF LEFT(outputText, 1) = CHR(34) AND _
                        RIGHT(outputText, 1) = CHR(34) THEN

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

                            IF parseError <> 0 OR _
                                expressionPosition <= LEN(expressionText) THEN

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
                IF LEFT(upperStatement, 4) = "DIM " THEN

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

                                IF LEFT(outputText, 1) = CHR(34) AND _
                                    RIGHT(outputText, 1) = CHR(34) THEN

                                    stringValues(foundVariable) = _
                                        MID(outputText, 2, LEN(outputText) - 2)

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

                            IF parseError <> 0 OR _
                                expressionPosition <= LEN(expressionText) THEN

                                PRINT "Error: Invalid INTEGER expression: "; outputText
                                CLOSE #fileNumber
                                END 1

                            ELSE
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


' Evaluate a numeric expression and report parsing errors through parseError.
FUNCTION EvaluateNumericExpression(expressionToEvaluate AS STRING) AS DOUBLE

    expressionText = expressionToEvaluate
    expressionPosition = 1
    parseError = 0

    EvaluateNumericExpression = ParseExpression()
    SkipExpressionSpaces()

    IF expressionPosition <= LEN(expressionText) THEN
        parseError = 1
    END IF

END FUNCTION


' Evaluate a numeric comparison.
' Returns -1 for true, 0 for false, and -2 for an invalid condition.
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

            IF nextChar = "=" OR _
                (currentChar = "<" AND nextChar = ">") THEN

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

        numberText = MID(expressionText, startPosition, _
            expressionPosition - startPosition)

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

        variableIdentifier = LCASE(MID(expressionText, startPosition, _
            expressionPosition - startPosition))

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
