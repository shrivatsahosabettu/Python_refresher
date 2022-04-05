from lark import Lark
import re

cobol_parser = Lark(r"""
            statement: display_statement  

            display_statement:	"DISPLAY"  identi | literal

            literal: SIGNED_NUMBER
            identi: string
            string: /[a-z0-9]([_a-z0-9\-]*[a-z0-9]+)?/            
           
            %import common.SIGNED_NUMBER
            %import common.ESCAPED_STRING
            %import common.WS
            %ignore WS

            """, start= 'statement')

text = "DISPLAY TEST."
print(cobol_parser.parse(text).pretty())


