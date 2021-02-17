{
{-# OPTIONS -w -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings #-}

module Lang.Lexer
  ( alexMonadScan
  , alexError
  , runAlex
  , runAlex'
  , Alex(..)
  , Token(..)
  )
where

import           Control.Applicative           as App
                                                ( Applicative(..) )
import           Data.Word                      ( Word8 )
import           Data.Char                      ( ord )
import qualified Data.Bits
import qualified Data.Text.Lazy                as TL
import           Control.Monad                  ( liftM )
import           Lang.Tokens
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
    @double                { tok (TNum . read . TL.unpack) }
    @char                  { tok lexChar }
    @string                { tok (lexString TL.empty) }
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
                             TL.Text)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p, c, _ps, s) = (p, c, [], s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p, c, _bs, _s) = c

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte (p, c, (b : bs), s ) = Just (b, (p, c, bs, s))
alexGetByte (_, _, []      , s ) | TL.null s = Nothing
alexGetByte (p, _, [], text) =
  let c  = TL.head text
      s  = TL.tail text
      p' = alexMove p c
  in  case utf8Encode' c of
        (b, bs) -> p' `seq` Just (b, (p', c, bs, s))

-- AlexPosn
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
        alex_inp :: TL.Text,     -- the current input
        alex_chr :: !Char,      -- the character before the input
        alex_bytes :: [Byte],
        alex_scd :: !Int,        -- the current startcode
        alex_fp :: FilePath      -- the current file path
    }

-- Alex monad
runAlex :: TL.Text -> Alex a -> Either TL.Text a
runAlex input (Alex f) =
  case
      f
        (AlexState { alex_bytes = []
                   , alex_pos   = alexStartPos
                   , alex_inp   = input
                   , alex_chr   = '\n'
                   , alex_scd   = 0
                   , alex_fp    = "PiG"
                   }
        )
    of
      Left  msg    -> Left msg
      Right (_, a) -> Right a

runAlex' :: Alex a -> FilePath -> TL.Text -> Either TL.Text a
runAlex' a fp input = runAlex input (alexSetFilePath fp >> a)

newtype Alex a = Alex { unAlex :: AlexState -> Either TL.Text (AlexState, a) }

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

alexError :: AlexPosn -> TL.Text -> Alex a
alexError (AlexPn _ l c) msg = do
  fp <- alexGetFilePath
  Alex $ const $ Left
    (  TL.pack fp
    <> ":"
    <> TL.pack (show l)
    <> ":"
    <> TL.pack (show c)
    <> ": "
    <> msg
    )

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
      alexError p ("lexical error at character '" <> TL.take 1 s <> "'")
    AlexSkip inp' len -> do
      alexSetInput inp'
      alexMonadScan
    AlexToken inp' len action -> do
      alexSetInput inp'
      action (ignorePendingBytes inp) len


type AlexAction result = AlexInput -> Int -> Alex result
 
data Token = Token AlexPosn TokenType
  deriving ( Show )

alexEOF :: Alex Token
alexEOF = do
  (p, _, _, _) <- alexGetInput
  return $ Token p TEOF

tok :: (TL.Text -> TokenType) -> AlexAction Token
tok f = \(p, _, _, s) i -> return $ Token p (f (TL.take (fromIntegral i) s))

tok' :: TokenType -> AlexAction Token
tok' = tok . const

escape :: Char -> Char
escape c = case c of
  'n' -> '\n'
  't' -> '\t'
  'r' -> '\r'
  '0' -> '\0'
  o   -> o

lexString :: TL.Text -> TL.Text -> TokenType
lexString acc str | TL.null str         = TStr (TL.reverse acc)
                  | TL.head str == '"'  = lexString acc s
                  | TL.head str == '\\' = lexString (ec `TL.cons` acc) $ TL.tail s
                  | otherwise           = lexString (c `TL.cons` acc) s
 where
  ec = escape . TL.head $ TL.tail str
  c  = TL.head str
  s  = TL.tail str


lexChar :: TL.Text -> TokenType
lexChar str | TL.head str == '\'' = lexChar $ TL.tail str
            | TL.head str == '\\' = TChar (escape c)
            | otherwise           = TChar c
  where c = TL.head str

}


