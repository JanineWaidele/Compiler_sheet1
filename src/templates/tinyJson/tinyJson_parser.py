from parsers.common import *

type Json = str | int | dict[str, Json]

def ruleJson(toks: TokenStream) -> Json:
    """
    Parses a JSON object, a JSON string, or a JSON number.
    """
    return {} # TODO

def ruleEntryList(toks: TokenStream) -> dict[str, Json]:
    """
    Parses the content of a JSON object.
    """
    return {} # TODO

def parse(code: str) -> Json:
    parser = mkLexer("./src/parsers/tinyJson/tinyJson_grammar.lark")
    tokens = list(parser.lex(code))
    log.info(f'Tokens: {tokens}')
    toks = TokenStream(tokens)
    res = ruleJson(toks)
    toks.ensureEof(code)
    return res
