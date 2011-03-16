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

module Data.Bittorrent.Get (getBEncodedT) where

import qualified Data.Map as M

import Data.Char (chr)
import Data.Binary
import Data.Binary.Get
import Data.Bittorrent.Intern
import Data.Encoding (decodeLazyByteString)
import Data.Encoding.ASCII


getBEncodedT :: Get BEncodedT
getBEncodedT = do
  c <- lookAhead $ getChr
  case (c) of
    'i' -> getBInteger
    'l' -> getBList
    'd' -> getBDict
    otherwise -> getBString
    
    
getBInteger :: Get BEncodedT
getBInteger = do
  getOneOf "i"
  i <- getNumber
  getOneOf "e"
  return $ BInteger i

getBList :: Get BEncodedT
getBList = do
  getOneOf "l"
  l <- whileNot 'e' getBEncodedT
  getOneOf "e"
  return $ BList l

getBString :: Get BEncodedT
getBString = do
  l <- fmap read (whileNot ':' getDigit)
  skip 1
  fmap BString $ getLazyByteString l

getBDict :: Get BEncodedT
getBDict = do
  getOneOf "d"
  ds <- whileNot 'e' getBDictPair
  getOneOf "e"
  return $ BDict $ M.fromList ds
  
getBDictPair :: Get (String, BEncodedT)
getBDictPair = do
  k <- fmap (unpackBString ASCII) getBString
  v <- getBEncodedT
  return (k,v)

getChr :: Get Char
getChr = fmap (chr.fromInteger.toInteger) getWord8


getOneOf :: [Char] -> Get Char
getOneOf cs = do
  c <- getChr
  if (elem c cs) then return c
    else fail $ "expected oneOf '" ++ cs  ++ "' - read '" ++ [c] ++ "'"  

getDigit :: Get Char
getDigit = getOneOf ['0' .. '9']
getPosDigit = getOneOf ['1'..'9']

whileNot :: Char -> Get a -> Get [a]
whileNot s p = do
  c <- lookAhead getChr 
  if (c == s) then return []
    else do a <- p
            as <- whileNot s p
            return (a:as)

count :: Integer -> Get a -> Get [a]
count l p
  | l <= 0 = do return []
  | otherwise = do a <- p
                   as <- count (l-1) p
                   return (a:as)

getNumber :: Get Integer
getNumber = do
  c <- lookAhead $ getOneOf $ ['0'..'9'] ++ ['-']
  case (c) of
    '0' -> do skip 1; return 0
    '-' -> getNegNumber
    otherwise -> getPosNumber
    
getNegNumber = do
  getOneOf "-"
  getPosNumber
               

getPosNumber = do
  d <- getPosDigit
  ds <- whileNot 'e' getDigit
  return $ read (d:ds)
