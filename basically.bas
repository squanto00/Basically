' BASICALLY 0.3
' String variables and PRINT

CONST MAX_VARIABLES = 100

DIM AS STRING filename
DIM AS STRING sourceLine
DIM AS STRING trimmedLine
DIM AS STRING statement
DIM AS STRING outputText

DIM AS STRING variableNames(MAX_VARIABLES - 1)
DIM AS STRING variableValues(MAX_VARIABLES - 1)

DIM AS INTEGER fileNumber
DIM AS INTEGER variableCount
DIM AS INTEGER i
DIM AS INTEGER equalsPosition
DIM AS INTEGER foundVariable

IF COMMAND(1) = "" THEN
    PRINT "BASICALLY 0.3"
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

    trimmedLine = LTRIM(RTRIM(sourceLine))
    statement = trimmedLine

    ' Ignore empty lines.
    IF LEN(statement) > 0 THEN

        ' Handle PRINT statements.
        IF UCASE(LEFT(statement, 5)) = "PRINT" THEN

            outputText = LTRIM(MID(statement, 6))

            ' Remove surrounding quotation marks from strings.
            IF LEN(outputText) >= 2 THEN
                IF LEFT(outputText, 1) = CHR(34) AND RIGHT(outputText, 1) = CHR(34) THEN
                    outputText = MID(outputText, 2, LEN(outputText) - 2)
                ELSE
                    ' Otherwise, treat the expression as a variable name.
                    foundVariable = -1

                    FOR i = 0 TO variableCount - 1
                        IF variableNames(i) = LCASE(outputText) THEN
                            foundVariable = i
                            EXIT FOR
                        END IF
                    NEXT i

                    IF foundVariable >= 0 THEN
                        outputText = variableValues(foundVariable)
                    ELSE
                        PRINT "Error: Unknown variable "; outputText
                        CLOSE #fileNumber
                        END 1
                    END IF
                END IF
            END IF

            PRINT outputText

        ELSE

            ' Handle variable declarations.
            IF UCASE(LEFT(statement, 4)) = "DIM " THEN

                ' This first version accepts declarations of the form:
                ' DIM name AS STRING
                equalsPosition = INSTR(statement, " AS STRING")

                IF equalsPosition > 5 THEN
                    outputText = LCASE(TRIM(MID(statement, 5, equalsPosition - 5)))

                    IF variableCount < MAX_VARIABLES THEN
                        variableNames(variableCount) = outputText
                        variableValues(variableCount) = ""
                        variableCount = variableCount + 1
                    ELSE
                        PRINT "Error: Too many variables"
                        CLOSE #fileNumber
                        END 1
                    END IF
                ELSE
                    PRINT "Error: Expected DIM name AS STRING"
                    CLOSE #fileNumber
                    END 1
                END IF

            ELSE

                ' Handle assignments such as:
                ' name = "BASICALLY"
                equalsPosition = INSTR(statement, "=")

                IF equalsPosition > 1 THEN
                    outputText = LCASE(TRIM(LEFT(statement, equalsPosition - 1)))
                    statement = TRIM(MID(statement, equalsPosition + 1))

                    ' Find the variable being assigned.
                    foundVariable = -1

                    FOR i = 0 TO variableCount - 1
                        IF variableNames(i) = outputText THEN
                            foundVariable = i
                            EXIT FOR
                        END IF
                    NEXT i

                    IF foundVariable < 0 THEN
                        PRINT "Error: Unknown variable "; outputText
                        CLOSE #fileNumber
                        END 1
                    END IF

                    ' Store the assigned string.
                    IF LEN(statement) >= 2 THEN
                        IF LEFT(statement, 1) = CHR(34) AND RIGHT(statement, 1) = CHR(34) THEN
                            statement = MID(statement, 2, LEN(statement) - 2)
                        END IF
                    END IF

                    variableValues(foundVariable) = statement

                ELSE
                    PRINT "Error: Unrecognized statement: "; statement
                    CLOSE #fileNumber
                    END 1
                END IF

            END IF
        END IF
    END IF
WEND

CLOSE #fileNumber
PRINT "Program finished."
