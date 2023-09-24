class CCRException(Exception):
    pass


class NotWellTypedException(CCRException):
    pass


class HasLexerErrorException(CCRException):
    pass


class HasParserErrorException(CCRException):
    pass
