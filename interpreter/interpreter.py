from typing import Union, Type, List, Any
from abc import ABC, abstractmethod
from interpreter.tokenizer import TokenTypes as TT
from interpreter.ast import (
    AST,
    Program,
    Num,
    Var,
    BinOp,
    UnaryOp,
    NoOp,
    Assign,
    Function,
    FunctionCall,
    ReturnVal,
    String
)
from interpreter.exceptions import InterpreterError
from copy import copy


class FunctionFrame:
    def __init__(self, func: Function):
        self.func: Function = func
        self.locals: dict = {}

    def __repr__(self):
        return f"{FunctionFrame.__name__}({self.func})"

    def __str__(self):
        return str(self)


class ASTVisitor:
    def __init__(self, program_node: Program):
        self._program: Program = program_node
        self._main_function = None
        self._globals: dict = {}
        self._call_stack: List[FunctionFrame] = []
        self._ret = None

    @property
    def globals(self):
        return self._globals

    @property
    def scope(self):
        if self._call_stack:
            return self._call_stack[-1].locals
        else:
            return self._globals

    def _resolve_var(self, var_name):
        if var_name in self.scope:
            return self.scope[var_name]

        if var_name in self._globals:
            return self._globals[var_name]

        raise InterpreterError(f"no such variable {var_name}")

    @property
    def program(self):
        return self._program

    @property
    def main_function(self):
        return self._main_function

    @property
    def global_scope(self):
        return self._globals

    def visit_String(self, node: Num):
        return node.token.value

    def visit_String(self, node: String):
        return node.token.value

    def visit_BinOp(self, node: BinOp):
        if node._op.type == TT.PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node._op.type == TT.MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node._op.type == TT.MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node._op.type == TT.DIV:
            return self.visit(node.left) / self.visit(node.right)

    def visit_UnaryOp(self, node: UnaryOp):
        op = node.token.type
        if op == TT.PLUS:
            return +self.visit(node.expr)
        elif op == TT.MINUS:
            return -self.visit(node.expr)

    def visit_Var(self, node: Var):
        return self._resolve_var(node.value)

    def visit_Assign(self, node: Assign):
        var_name = node.left.value
        if var_name in self._globals:
            scope = self._globals
        else:
            scope = self.scope
        scope[var_name] = self.visit(node.right)

    def visit_Function(self, node: Function):
        for stmt in node.statements:
            visited_node = self.visit(stmt)
            if type(visited_node) == ReturnVal:
                break
        _ = self._call_stack.pop()

    def visit_Main(self, node: Function):
        # TODO: parse stdin args
        self._call_stack.append(FunctionFrame(node))
        self.visit_Function(node)
        pass

    def visit_FunctionCall(self, node: FunctionCall):
        builtins = "print"
        if node.func.value in builtins:
            for arg in node.args:
                if type(arg) == Num:
                    print(arg.token.value, end=" ")
                elif type(arg) == Var:
                    print(self.scope[arg.token.value], end=" ")
            return
        locals = {}
        func_token = node.func
        callee_func = self._globals[func_token.value]
        for i, callee_arg in enumerate(callee_func.args):
            callee_arg_name = callee_arg.value
            locals[callee_arg_name] = self.visit(node.args[i])
            pass

        self._call_stack.append(FunctionFrame(callee_func))
        self._call_stack[-1].locals = locals
        self.visit_Function(callee_func)
        return self._ret

    def visit_ReturnVal(self, node: ReturnVal):
        self._ret = self.visit(node._return_val)
        print("_ret=", self._ret)
        return node

    def visit(self, node: AST) -> Any:
        if type(node) is NoOp:
            return
        elif type(node) is Function:
            return self.visit_Function(node)
        elif type(node) is FunctionCall:
            return self.visit_FunctionCall(node)
        elif type(node) is BinOp:
            return self.visit_BinOp(node)
        elif type(node) is UnaryOp:
            return self.visit_UnaryOp(node)
        elif type(node) is Assign:
            return self.visit_Assign(node)
        elif type(node) is Var:
            return self.visit_Var(node)
        elif type(node) is Num:
            return self.visit_String(node)
        elif type(node) is String:
            return self.visit_String(node)
        elif type(node) is ReturnVal:
            return self.visit_ReturnVal(node)

    def visit_Program(self):
        # check if duplicate function names
        func_names = [f.id.value for f in self.program.functions]
        if len(func_names) != len(set(func_names)):
            raise InterpreterError(
                "duplicate function names"
            )  # TODO: explicit duplicates

        # check entrypoint 'main'
        main_func = list(filter(lambda x: x.id.value == "main", self.program.functions))
        if not main_func:
            raise InterpreterError("main function not found")
        self._main_function = main_func[0]

        for func in self.program.functions:
            if func.id.value == "main":
                continue
            scope = self.scope
            self._globals[func.id.value] = func

        # execute global code
        for stmt in self._program.statements:
            if type(stmt) == NoOp:
                continue
            if type(stmt) == FunctionCall:
                raise InterpreterError("no function call allowed in global scope")
            self.visit(stmt)

        # execute main()
        self.visit_Main(self._main_function)
