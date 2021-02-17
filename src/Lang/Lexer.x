{
{-# OPTIONS -w  #-}
module Lang.Lexer
  ( alexMonadScan
  , alexError
  , runAlex
  , runAlex'
  , Alex(..)
  , Token(..)
  )
where

import Control.Applicative as App (Applicative (..))
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits
import Control.Monad ( liftM )
import Lang.Tokens
}


@comment_line      = "//" ( [^\n\/]* [^\n]* )?
@comment_start     = "/*"
@comment_end       = "*/"

@escape            = \\ [nrt\\'"0] 
@decimal_suffix    = \. [0-9][0-9_]*
@double            = [0-9][0-9_]* @decimal_suffix?
@string            = \" ( @escape | $printable # \")* \"
@id                = [A-Za-z][A-Za-z0-9'_]*
@char              = \' ( @escape | [^\\'\n\t\r] ) \'
tokens :-
    $white+			;
    "if"                   { tok' TIf }
    "elif"                 { tok' TElIf }
    "else"                 { tok' TElse }
    "while"                { tok' TWhile }
    "do"                   { tok' TDo }
    "load"                 { tok' TLoad }
    "+"                    { tok' TPlus }
    "-"                    { tok' TMinus }
    "*"                    { tok' TStar }
    "/"                    { tok' TSlash }
    "%"                    { tok' TMod }
    "=>"                   { tok' TFatArr }
    "="                    { tok' TAssign }
    "^"                    { tok' TDash }
    "<"                    { tok' TLt }
    ">"                    { tok' TGt }
    "=="                   { tok' TEq }
    "!="                   { tok' TNEq }
    "<>"                   { tok' TLtGt }
    "><"                   { tok' TGtLt }
    "-<"                   { tok' TRFork }
    ">-"                   { tok' TLFork }
    "~"                    { tok' TNot }
    "&&"                   { tok' TAnd }
    "||"                   { tok' TOr }
    ")"                    { tok' TRParen }
    "("                    { tok' TLParen }
    "}"                    { tok' TRBrace }
    "{"                    { tok' TLBrace }
    "]"                    { tok' TRBracket }
    "["                    { tok' TLBracket }
    ","                    { tok' TComma }
    ";"                    { tok' TSemi }
    "true"                 { tok' TTrue }
    "false"                { tok' TFalse }
    "null"                 { tok' TNull }
    @double                { tok (TNum . read) }
    @char                  { tok lexChar }
    @string                { tok (lexString []) }
    @id                    { tok TSym}
    @comment_line.*                   ;
    @comment_start(.*\n)*@comment_end ;

{
-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = uncurry (:) . utf8Encode'

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (ord c) of
  (x, xs) -> (fromIntegral x, map fromIntegral xs)
 where
  go oc
    | oc <= 0x7f
    = (oc, [])
    | oc <= 0x7ff
    = (0xc0 + (oc `Data.Bits.shiftR` 6), [0x80 + oc Data.Bits..&. 0x3f])
    | oc <= 0xffff
    = ( 0xe0 + (oc `Data.Bits.shiftR` 12)
      , [ 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
        , 0x80 + oc Data.Bits..&. 0x3f
        ]
      )
    | otherwise
    = ( 0xf0 + (oc `Data.Bits.shiftR` 18)
      , [ 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
        , 0x80 + oc Data.Bits..&. 0x3f
        ]
      )


type Byte = Word8

type AlexInput
  = (AlexPosn,     -- current position,
               Char,         -- previous char
                     [Byte],       -- pending bytes on current char
                             String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p, c, _ps, s) = (p, c, [], s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p, c, _bs, _s) = c

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (p, c, (b : bs), s ) = Just (b, (p, c, bs, s))
alexGetByte (_, _, []      , []) = Nothing
alexGetByte (p, _, [], (c : s)) =
  let p' = alexMove p c
  in  case utf8Encode' c of
        (b, bs) -> p' `seq` Just (b, (p', c, bs, s))


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' =
  AlexPn (a + 1) l (c + alex_tab_size - ((c - 1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a + 1) (l + 1) 1
alexMove (AlexPn a l c) _    = AlexPn (a + 1) l (c + 1)

data AlexState = AlexState {
        alex_pos :: !AlexPosn,  -- position at current input location
        alex_inp :: String,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int,        -- the current startcode
        alex_fp :: FilePath      -- the current file path
    }

-- Compile with -funbox-strict-fields for best results!


runAlex :: String -> Alex a -> Either String a
runAlex input__ (Alex f) =
  case
      f
        (AlexState { alex_bytes = []
                   , alex_pos   = alexStartPos
                   , alex_inp   = input__
                   , alex_chr   = '\n'
                   , alex_scd   = 0
                   , alex_fp    = "PiG"
                   }
        )
    of
      Left  msg    -> Left msg
      Right (_, a) -> Right a

newtype Alex a = Alex { unAlex :: AlexState -> Either String (AlexState, a) }

instance Functor Alex where
  fmap f a = Alex $ \s -> case unAlex a s of
    Left  msg      -> Left msg
    Right (s', a') -> Right (s', f a')

instance Applicative Alex where
  pure a = Alex $ \s -> Right (s, a)
  fa <*> a = Alex $ \s -> case unAlex fa s of
    Left  msg     -> Left msg
    Right (s', f) -> case unAlex a s' of
      Left  msg      -> Left msg
      Right (s'', b) -> Right (s'', f b)

instance Monad Alex where
  m >>= k = Alex $ \s -> case unAlex m s of
    Left  msg     -> Left msg
    Right (s', a) -> unAlex (k a) s'
  return = App.pure

alexGetInput :: Alex AlexInput
alexGetInput =
  Alex
    $ \s@AlexState { alex_pos = pos, alex_chr = c, alex_bytes = bs, alex_inp = inp__ } ->
        Right (s, (pos, c, bs, inp__))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos, c, bs, inp__) = Alex $ \s ->
  case s { alex_pos = pos, alex_chr = c, alex_bytes = bs, alex_inp = inp__ } of
    state__@(AlexState{}) -> Right (state__, ())

alexError :: AlexPosn -> String -> Alex a
alexError (AlexPn _ l c) msg = do
  fp <- alexGetFilePath
  Alex $ const $ Left (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState { alex_scd = sc } -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s { alex_scd = sc }, ())

alexGetFilePath :: Alex FilePath
alexGetFilePath = Alex $ \s@AlexState { alex_fp = fp } -> Right (s, fp)

alexSetFilePath :: FilePath -> Alex ()
alexSetFilePath fp = Alex $ \s -> Right (s { alex_fp = fp }, ())

alexMonadScan :: Alex Token
alexMonadScan = do
  inp <- alexGetInput
  sc  <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
      alexError p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip inp' len -> do
      alexSetInput inp'
      alexMonadScan
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) len


type AlexAction result = AlexInput -> Int -> Alex result


-- just ignore this token and scan another one
skip :: AlexAction Token
skip _input _len = alexMonadScan

-- ignore this token, but set the start code to a new value
begin :: Int -> AlexAction Token
begin code _input _len = do
  alexSetStartCode code
  alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input__ len = do
  alexSetStartCode code
  action input__ len


token :: (AlexInput -> Int -> token) -> AlexAction token
token t input__ len = return (t input__ len)

data Token = Token AlexPosn TokenType
  deriving ( Show )

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TEOF

tok :: (String -> TokenType) -> AlexAction Token
tok f = \(p, _, _, s) i -> return $ Token p (f (take i s))

tok' :: TokenType -> AlexAction Token
tok' = tok . const

escape :: Char -> Char
escape c = case c of
  'n' -> '\n'
  't' -> '\t'
  'r' -> '\r'
  '0' -> '\0'
  o   -> o

lexString :: String -> String -> TokenType
lexString acc []             = TStr (reverse acc)
lexString acc ('"'      : s) = lexString acc s
lexString acc ('\\' : c : s) = lexString (ec : acc) s where ec = escape c
lexString acc (c        : s) = lexString (c : acc) s

lexChar :: String -> TokenType
lexChar ('\''        : c : '\'' : _) = TChar c
lexChar ('\'' : '\\' : c : '\'' : _) = TChar (escape c)

runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (alexSetFilePath fp >> a)

}


