from lark import Lark
from lark import Transformer


class TreeToJson(Transformer):
    def string(self, s):
        (s,) = s
        return s[1:-1]

    def number(selfself, n):
        (n,) = n
        return float(n)

    list = list
    pair = tuple
    dict = dict

    null = lambda self, _: None
    true = lambda self, _: True
    false = lambda self, _: False

class MyTransformer(Transformer):

    def list(self, items):
        return list(items)

    def pair(self, key_value):
        k, v = key_value
        return k, v

    def dict(self, items):
        return dict(items)


json_parser = Lark(r"""
            ?value: dict
                |   list
                |   string
                |   SIGNED_NUMBER       -> number
                |   "true"              -> true
                |   "false"             -> false
                |   "null"              -> null
            
            list: "[" (value ("," value)*) "]"
                
            dict: "{" [pair ("," pair)*] "}"
            pair: string ":" value
            string: ESCAPED_STRING
            
            %import common.ESCAPED_STRING
            %import common.SIGNED_NUMBER
            %import common.WS
            %ignore WS    
              """, start='value')


text = '{"key": ["item0", "item1", 3.14, true]}'
print(json_parser.parse(text).pretty())
# tree = json_parser.parse(text)
# print(MyTransformer().transform(tree))
tree = json_parser.parse(text)
print(TreeToJson().transform(tree))


