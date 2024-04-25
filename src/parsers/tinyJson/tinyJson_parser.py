from parsers.common import *

type Json = str | int | dict[str, Json]

def ruleJson(toks: TokenStream) -> Json:
    """
    Parses a JSON object, a JSON string, or a JSON number.
    """
    obj: dict[str, Json] = {}
    isDict = False
    key_tmp = ""
    
    while(toks.lookahead() != TokenStream.eof):
        tNext = toks.next()
        
        if (tNext.type == "STRING"):
            if not isDict:
                # just a string was passed
                return tNext.value.replace('"',"")
            # string is key in dict
            key_tmp = tNext.value.replace('"',"")

        elif (tNext.type in ["DIGIT", "INT"]):
            if not isDict:
                # just an int was passed
                return int(tNext.value)

        elif (tNext.type == "LBRACE"):
            # { indicates JSON object
            isDict = True
            
        elif (tNext.type == "COLON"):
            # assign value to key
            if(toks.lookahead().type == "STRING"):
                obj[key_tmp] = toks.next().value.replace('"',"")
            elif(toks.lookahead().type in ["DIGIT", "INT"]):
                obj[key_tmp] = int(toks.next().value)
            elif(toks.lookahead().type == "LBRACE"):
                toks.next()
                if (toks.lookahead() == "RBRACE"):
                    # value is {}
                    obj[key_tmp] = {}
                else:
                    # value is another JSON object
                    obj[key_tmp] = ruleEntryList(toks)

        elif(tNext.type == "RBRACE"):
            # marks end of JSON object
            toks.next()
            key_tmp = ""
            isDict = False
            if(toks.lookahead().type in ["RBRACE","$EOF"]):
                return obj
    return obj

def ruleEntryList(toks: TokenStream) -> dict[str, Json]:
    """
    Parses the content of a JSON object.
    """
    obj: dict[str, Json] = {}
    key_tmp = ""
    
    while(toks.lookahead() != TokenStream.eof):
        tNext = toks.next()
        
        if (tNext.type == "STRING"):
            if key_tmp == "":
                key_tmp = tNext.value.replace('"',"")

        elif (tNext.type == "COMMA"):
            key_tmp = ""
            
        elif (tNext.type == "COLON"):
            # assign value to key
            if(toks.lookahead().type == "STRING"):
                obj[key_tmp] = toks.next().value.replace('"',"")
            elif(toks.lookahead().type in ["DIGIT", "INT"]):
                obj[key_tmp] = int(toks.next().value)
    return obj 

def parse(code: str) -> Json:
    parser = mkLexer("./src/parsers/tinyJson/tinyJson_grammar.lark")
    tokens = list(parser.lex(code))
    log.info(f'Tokens: {tokens}')
    toks = TokenStream(tokens)
    res = ruleJson(toks)
    toks.ensureEof(code)
    return res
