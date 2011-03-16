{-# LANGUAGE Rank2Types, FlexibleContexts #-}
{-
Copyright (c)2011, Markus Barenhoff

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Markus Barenhoff nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

module Data.Bittorrent.Parser (parseBEncodedFile
                              ,bencodedParser)  where

import qualified Data.Map as M

import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Data.Bittorrent.Intern
import Data.Functor.Identity (Identity)
import Data.List (sort)

type P t = (Stream s Identity Char) => ParsecT s u Identity t

-- | parses a 'BEncodedT' from file
parseBEncodedFile :: FilePath -> IO (Either ParseError BEncodedT)
parseBEncodedFile = parseFromFile bencodedParser

-- | parses a 'BEncodedT'
bencodedParser :: P BEncodedT
bencodedParser = (try stringP <|> 
                  try integerP <|>
                  try listP <|>
                  try dictP )

-- | Integers are represented by an 'i' followed by the number in base 10 
-- followed by an 'e'. For example i3e corresponds to 3 and i-3e corresponds 
-- to -3. Integers have no size limitation. i-0e is invalid. All encodings 
-- with a leading zero, such as i03e, are invalid, other than i0e, which of 
-- course corresponds to 0.   
integerP :: P BEncodedT
integerP = fmap BInteger $ between (char 'i') (char 'e') number
                 `label` "bencoded integer"

-- | Strings are length-prefixed base ten followed by a colon and the string.
-- For example 4:spam corresponds to 'spam'.  
stringP :: P BEncodedT
stringP = do 
    { len <- fmap read $ many1 digit
    ; char ':'
    ; fmap BString $ count len anyChar
    } `label` "bencoded string"
          
-- | Lists are encoded as an 'l' followed by their elements (also bencoded)
-- followed by an 'e'. 
-- For example l4:spam4:eggse corresponds to ['spam', 'eggs'].  
listP :: P BEncodedT         
listP = fmap BList $ between (char 'l') (char 'e') (many bencodedParser)
        `label` "bencoded list"
        
-- | Dictionaries are encoded as a 'd' followed by a list of alternating keys
-- and their corresponding values followed by an 'e'. For example, 
-- d3:cow3:moo4:spam4:eggse corresponds to {'cow': 'moo', 'spam': 'eggs'} and 
-- d4:spaml1:a1:bee corresponds to {'spam': ['a', 'b']}. Keys must be strings 
-- and appear in sorted order (sorted as raw strings, not alphanumerics).  
dictP :: P BEncodedT
dictP = do 
  d <- dictP'
  let ks = M.keys $ unpackBDict d
  if (ks == sort ks) 
    then parserReturn d
    else fail "keys in parsed dictionary did not appear in order"

-- | a less strict version of 'dictP' which is not validating the dicts keys to be in order
dictP' :: P BEncodedT
dictP' = fmap BDict $ fmap M.fromList $ between (char 'd') (char 'e') (many dictPairP)
         `label` "bencoded dictionary"
  where dictPairP = do 
          { k <- fmap unpackBString stringP
          ; v <- bencodedParser
          ; return (k,v)
          }

number :: P Integer
number = fmap read $ (try neg <|> try zero <|> try pos) 
         `label` "number"
  where digitNonZero = oneOf "123456789"
        zero = do 
          char '0'
          return "0"
        pos = do
          d <- digitNonZero
          ds <- many digit
          return $ d : ds
        neg = do
          s <- char '-'
          ds <- pos
          return $ s : ds
          
