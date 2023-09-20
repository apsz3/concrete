import click

from prompt_toolkit import PromptSession
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory
from prompt_toolkit.completion import WordCompleter
from prompt_toolkit.history import FileHistory

from concrete.lexer import RESERVED

from concrete.concrete import Concrete
from concrete.lexer import CCRLexer
from concrete.parser import CCRParser

import traceback
def i(ccr, debug):
    # Create a completer with some example words
    completer = WordCompleter(RESERVED)

    # Create a history object to store previous inputs
    history = FileHistory('.ccr_history')

    # Create a prompt session with autocompletion and history support
    session = PromptSession(
        history=history,
        auto_suggest=AutoSuggestFromHistory(),
        completer=completer,
    )
    while True:
        try:
            # Read input from the user
            inp = session.prompt('> ')
            if not inp:
                continue
            stack, _ = ccr.run(inp, debug)
            if len(stack) == 0:
                click.echo(">")
            else:
                click.echo(stack.pop())
        except EOFError:
            # Exit gracefully on Ctrl+D/EOT
            break
        except Exception as e:
            print(e)
DEBUG = False

@click.command()
@click.option('-i', '--interactive', is_flag=True, help='Run in interactive mode')
@click.option('-s', '--string', default='', help='Interpret a string')
@click.argument('file', type=click.Path(exists=True, file_okay=True, dir_okay=False), required=False)
@click.option('-h', '--help', 'display_help', is_flag=True, help='Display help information')
@click.option('-d', '--debug', is_flag=True, help='Enable debug mode')
@click.option('-p', '--parse', is_flag=True, help='Enable parse mode')
@click.option('-l', '--lex', is_flag=True, help='Enable lex mode')
def cli(interactive, string, file, display_help, debug, parse, lex):
    if display_help:
        click.echo(click.get_current_context().get_help())
        return

    modes = []
    if parse or lex:
        if parse:
            modes.append('parse')
        if lex:
            modes.append('lex')

    if interactive and not file and not string:
        i(Concrete())
        return

    if file:
        with open(file, "r") as fp:
            src = fp.read()
    elif string:
        src = string

    if modes:
        do_modes(modes, src)
        return
    ccr = Concrete()
    ccr.run(src, debug)

    if interactive:
        i(ccr, debug)


def do_modes(modes, src):
    for m in modes:
        if m == "lex":
            print(CCRLexer(src).tokenize(src)) # TODO: Ugly, override super()
        if m == "parse":
            print(CCRParser(src).parse(src))