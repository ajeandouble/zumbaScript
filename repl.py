from interpreter.lexer import Lexer
from interpreter.parser import ASTParser

class Interpreter():
    def __init__(self, tokens):
        self._tokens = tokens

    def _error(self, msg):
        raise Exception(msg)


def main():
    input = """
        function main(a,b,c) {
            f(1);
        }
    """
    lexer = Lexer(input)
    tokens = lexer.get_tokens()
    parser = ASTParser(tokens)
    node = parser.program()
    print(node)

print(__name__)
if __name__ == "__main__":
    main()
