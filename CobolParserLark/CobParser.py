from lark import Lark 

cobol_parser = Lark(r"""
            source_program: ( "IDENTIFICATION" | "ID" ) "DIVISION" "." program-id-cobol-source-program 
                            [ procedure-division ] 
                            [ { nested-cobol-source-program }* "." ]
            program-id-cobol-source-program:	"PROGRAM-ID" [ "." ] program-name [ "." ]

            procedure-division:	"PROCEDURE" "DIVISION" "."
                                    [ { paragraphs } "." ]

            paragraphs:	"[" { sentence }* { paragraph }* "]"

            paragraph:	"[" paragraph-name "." { sentence }* "["

            paragraph-name:	user-defined-word

            sentence: statement-list "."

            statement-list:	{ statement }+

            statement: display-statement

            display-statement:	"DISPLAY" { ( literal ) }

            literal = SIGNED_NUMBER

            user-defined-word = cobol-word

            cobol-word = [A-Za-z0-9]+ ([\-]+ [A-Za-z0-9]+)*

            %import common.ESCAPED_STRING
            %import common.SIGNED_NUMBER
            %import common.WS
            %ignore WS

            """, start= 'source_program')

text = "IDENTIFICATION DIVISION. PROGRAM-ID. TEST01. PROCEDURE DIVISON. DISPLAY 01"
print(cobol_parser.parse(text).pretty())