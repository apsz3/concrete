from pprint import pprint

import click

from prompt_toolkit import PromptSession
from prompt_toolkit.auto_suggest import AutoSuggestFromHistory
from prompt_toolkit.completion import WordCompleter
from prompt_toolkit.history import FileHistory


from concrete.compiler import walk
from concrete.concrete import Concrete
from concrete.parser import parse, Reserved
from concrete.exceptions import CCRException
import traceback


def i(ccr, debug):
    # Create a completer with some example words
    completer = WordCompleter(Reserved)

    # Create a history object to store previous inputs
    history = FileHistory(".ccr_history")

    # Create a prompt session with autocompletion and history support
    session = PromptSession(
        history=history,
        auto_suggest=AutoSuggestFromHistory(),
        completer=completer,
    )
    while True:
        try:
            # Read input from the user
            inp = session.prompt("> ")
            if not inp:
                continue
            stack, _ = ccr.run(inp, debug)
            if len(stack) == 0:
                pass
            else:
                click.echo(stack.pop())
        except EOFError:
            # Exit gracefully on Ctrl+D/EOT
            break
        except CCRException as e:
            print(e, end="")
        except Exception as e:
            import traceback

            traceback.print_exc()


DEBUG = False


@click.command()
@click.option("-i", "--interactive", is_flag=True, help="Run in interactive mode")
@click.option("-s", "--string", default="", help="Interpret a string")
@click.argument(
    "file", type=click.Path(exists=True, file_okay=True, dir_okay=False), required=False
)
@click.option(
    "-h", "--help", "display_help", is_flag=True, help="Display help information"
)
@click.option("-d", "--debug", is_flag=True, help="Enable debug mode")
@click.option("-p", "--parse", is_flag=True, help="Enable parse mode")
@click.option("-c", "--compile", is_flag=True, help="Enable compiler mode")
@click.option("-l", "--lex", is_flag=True, help="Enable lex mode")
def cli(interactive, string, file, display_help, debug, parse, lex, compile):
    if display_help:
        click.echo(click.get_current_context().get_help())
        return

    modes = []
    if parse or lex or compile:
        if parse:
            modes.append("parse")
        if lex:
            modes.append("lex")
        if compile:
            modes.append("compile")

    if interactive and not file and not string:
        i(Concrete(), debug)
        return

    src = None
    if file:
        with open(file, "r") as fp:
            src = fp.read()
    elif string:
        src = string

    if modes:
        do_modes(modes, src)
        return

    if src:
        ccr = Concrete()
        ccr.run(src, debug)
        return

    if interactive:
        i(ccr, debug)
        return

    click.echo(click.get_current_context().get_help())


def do_modes(modes, src):
    # TODO: Ovveride the file/tokenize bits so the
    # instance doesn't need to pass the src around to both the
    # constructor and the tokenizer method
    for m in modes:
        #        if m == "lex":
        #            pprint([_ for _ in CCRLexer(src).tokenize(src)])
        if m == "parse":
            pprint(parse(src))
        if m == "compile":
            Concrete().bytecode(src)
