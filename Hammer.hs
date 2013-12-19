{- HHammer: Haskell bindings to Hammer.
   Copyright 2013 Nikita Karetnikov <nikita@karetnikov.org>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, version 2.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Hammer where
import Foreign.Ptr
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Foreign.Marshal.Unsafe
import Data.Functor
import Data.Int
import Data.Word

import Allocator
{- typedef int bool; -}
type C'bool = CInt

{- typedef struct HParseState_ HParseState; -}
data C'HParseState_ = C'HParseState_

type C'HParseState = C'HParseState_

{- typedef enum HParserBackend_ {
            PB_MIN = 0,
            PB_PACKRAT = PB_MIN,
            PB_REGULAR,
            PB_LLk,
            PB_LALR,
            PB_GLR,
            PB_MAX = PB_GLR
        } HParserBackend; -}
type C'HParserBackend = CUInt

c'PB_MIN = 0
c'PB_MIN :: (Num a) => a

c'PB_PACKRAT = 0
c'PB_PACKRAT :: (Num a) => a

c'PB_REGULAR = 1
c'PB_REGULAR :: (Num a) => a

c'PB_LLk = 2
c'PB_LLk :: (Num a) => a

c'PB_LALR = 3
c'PB_LALR :: (Num a) => a

c'PB_GLR = 4
c'PB_GLR :: (Num a) => a

c'PB_MAX = 4
c'PB_MAX :: (Num a) => a

-- #synonym_t HParserBackend , <HParserBackend_>
{- typedef enum HTokenType_ {
            TT_NONE = 1,
            TT_BYTES = 2,
            TT_SINT = 4,
            TT_UINT = 8,
            TT_SEQUENCE = 16,
            TT_RESERVED_1,
            TT_ERR = 32,
            TT_USER = 64,
            TT_MAX
        } HTokenType; -}
type C'HTokenType = CUInt

c'TT_NONE = 1
c'TT_NONE :: (Num a) => a

c'TT_BYTES = 2
c'TT_BYTES :: (Num a) => a

c'TT_SINT = 4
c'TT_SINT :: (Num a) => a

c'TT_UINT = 8
c'TT_UINT :: (Num a) => a

c'TT_SEQUENCE = 16
c'TT_SEQUENCE :: (Num a) => a

c'TT_RESERVED_1 = 17
c'TT_RESERVED_1 :: (Num a) => a

c'TT_ERR = 32
c'TT_ERR :: (Num a) => a

c'TT_USER = 64
c'TT_USER :: (Num a) => a

c'TT_MAX = 65
c'TT_MAX :: (Num a) => a

-- #synonym_t HTokenType , <HTokenType_>
{- typedef struct HCountedArray_ {
            size_t capacity;
            size_t used;
            HArena * arena;
            struct HParsedToken_ * * elements;
        } HCountedArray; -}
data C'HCountedArray = C'HCountedArray{
  c'HCountedArray'capacity :: CSize,
  c'HCountedArray'used :: CSize,
  c'HCountedArray'arena :: Ptr C'HArena,
  c'HCountedArray'elements :: Ptr (Ptr C'HParsedToken)
} deriving (Eq,Show)
p'HCountedArray'capacity p = plusPtr p 0
p'HCountedArray'capacity :: Ptr (C'HCountedArray) -> Ptr (CSize)
p'HCountedArray'used p = plusPtr p 4
p'HCountedArray'used :: Ptr (C'HCountedArray) -> Ptr (CSize)
p'HCountedArray'arena p = plusPtr p 8
p'HCountedArray'arena :: Ptr (C'HCountedArray) -> Ptr (Ptr C'HArena)
p'HCountedArray'elements p = plusPtr p 12
p'HCountedArray'elements :: Ptr (C'HCountedArray) -> Ptr (Ptr (Ptr C'HParsedToken))
instance Storable C'HCountedArray where
  sizeOf _ = 16
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    v2 <- peekByteOff p 8
    v3 <- peekByteOff p 12
    return $ C'HCountedArray v0 v1 v2 v3
  poke p (C'HCountedArray v0 v1 v2 v3) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    pokeByteOff p 8 v2
    pokeByteOff p 12 v3
    return ()

-- #synonym_t HCountedArray , <HCountedArray_>
{- typedef struct HBytes_ {
            const uint8_t * token; size_t len;
        } HBytes; -}

data C'HBytes = C'HBytes{
  c'HBytes'token :: Ptr CUChar,
  c'HBytes'len :: CSize
} deriving (Eq,Show)
p'HBytes'token p = plusPtr p 0
p'HBytes'token :: Ptr (C'HBytes) -> Ptr (Ptr CUChar)
p'HBytes'len p = plusPtr p 4
p'HBytes'len :: Ptr (C'HBytes) -> Ptr (CSize)
instance Storable C'HBytes where
  sizeOf _ = 8
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    return $ C'HBytes v0 v1
  poke p (C'HBytes v0 v1) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    return ()

-- #synonym_t HBytes , <HBytes_>
{- typedef struct HParsedToken_ {
            HTokenType token_type;
            union {
                HBytes bytes;
                int64_t sint;
                uint64_t uint;
                double dbl;
                float flt;
                HCountedArray * seq;
                void * user;
            };
            size_t index;
            char bit_offset;
        } HParsedToken; -}

-- TODO: Support sint, uint, etc.
data C'HTokenData = Bytes C'HBytes
                  -- | Seq C'HCountedArray
                  deriving (Eq,Show)

data C'HParsedToken = C'HParsedToken{
  c'HParsedToken'token_type :: C'HTokenType,
  c'HParsedToken'token_data :: C'HTokenData,
  c'HParsedToken'index :: CSize,
  c'HParsedToken'bit_offset :: CChar
} deriving (Eq,Show)
p'HParsedToken'token_type p = plusPtr p 0
p'HParsedToken'token_type :: Ptr (C'HParsedToken) -> Ptr (C'HTokenType)
p'HParsedToken'token_data p = plusPtr p 4
p'HParsedToken'token_data :: Ptr C'HParsedToken -> Ptr C'HTokenData
p'HParsedToken'index p = plusPtr p 12
p'HParsedToken'index :: Ptr (C'HParsedToken) -> Ptr (CSize)
p'HParsedToken'bit_offset p = plusPtr p 16
p'HParsedToken'bit_offset :: Ptr (C'HParsedToken) -> Ptr (CChar)
instance Storable C'HParsedToken where
  sizeOf _ = 20
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- case () of
      _ | v0 == c'TT_BYTES    -> Bytes <$> peekByteOff p 4
        -- TODO: Support sint, uint, etc.
        -- | v0 == c'TT_SEQUENCE -> Seq <$> peekByteOff p 4
    v2 <- peekByteOff p 12
    v3 <- peekByteOff p 16
    return $ C'HParsedToken v0 v1 v2 v3
  poke p (C'HParsedToken v0 v1 v2 v3) = do
    pokeByteOff p 0 v0
    case v1 of
      Bytes v1' -> pokeByteOff p 4 v1'
      -- TODO: Support sint, uint, etc.
      -- Seq v1'   -> pokeByteOff p 4 v1'
    pokeByteOff p 12 v2
    pokeByteOff p 16 v3

-- #synonym_t HParsedToken , <HParsedToken_>
{- typedef struct HParseResult_ {
            const HParsedToken * ast; int64_t bit_length; HArena * arena;
        } HParseResult; -}
data C'HParseResult = C'HParseResult{
  c'HParseResult'ast :: Ptr C'HParsedToken,
  c'HParseResult'bit_length :: CLong,
  c'HParseResult'arena :: Ptr C'HArena
} deriving (Eq,Show)
p'HParseResult'ast p = plusPtr p 0
p'HParseResult'ast :: Ptr (C'HParseResult) -> Ptr (Ptr C'HParsedToken)
p'HParseResult'bit_length p = plusPtr p 4
p'HParseResult'bit_length :: Ptr (C'HParseResult) -> Ptr (CLong)
p'HParseResult'arena p = plusPtr p 12
p'HParseResult'arena :: Ptr (C'HParseResult) -> Ptr (Ptr C'HArena)
instance Storable C'HParseResult where
  sizeOf _ = 16
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    v2 <- peekByteOff p 12
    return $ C'HParseResult v0 v1 v2
  poke p (C'HParseResult v0 v1 v2) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    pokeByteOff p 12 v2
    return ()

-- #synonym_t HParseResult , <HParseResult_>
-- {- typedef struct HBitWriter_ HBitWriter; -}
-- #opaque_t HBitWriter_
-- #synonym_t HBitWriter , <HBitWriter_>
-- #callback HAction , Ptr <HParseResult_> -> Ptr () -> IO (Ptr <HParsedToken_>)
-- #callback HPredicate , Ptr <HParseResult_> -> Ptr () -> IO CInt
{- typedef struct HCFChoice_ HCFChoice; -}
data C'HCFChoice_ = C'HCFChoice_

-- #synonym_t HCFChoice , <HCFChoice_>
{- typedef struct HRVMProg_ HRVMProg; -}
data C'HRVMProg_ = C'HRVMProg_

-- #synonym_t HRVMProg , <HRVMProg_>
{- typedef struct HParserVtable_ HParserVtable; -}
data C'HParserVtable_ = C'HParserVtable_

-- #synonym_t HParserVtable , <HParserVtable_>
{- typedef struct HParser_ {
            const HParserVtable * vtable;
            HParserBackend backend;
            void * backend_data;
            void * env;
            HCFChoice * desugared;
        } HParser; -}
data C'HParser = C'HParser{
  c'HParser'vtable :: Ptr C'HParserVtable_,
  c'HParser'backend :: C'HParserBackend,
  c'HParser'backend_data :: Ptr (),
  c'HParser'env :: Ptr (),
  c'HParser'desugared :: Ptr C'HCFChoice_
} deriving (Eq,Show)
p'HParser'vtable p = plusPtr p 0
p'HParser'vtable :: Ptr (C'HParser) -> Ptr (Ptr C'HParserVtable_)
p'HParser'backend p = plusPtr p 4
p'HParser'backend :: Ptr (C'HParser) -> Ptr (C'HParserBackend)
p'HParser'backend_data p = plusPtr p 8
p'HParser'backend_data :: Ptr (C'HParser) -> Ptr (Ptr ())
p'HParser'env p = plusPtr p 12
p'HParser'env :: Ptr (C'HParser) -> Ptr (Ptr ())
p'HParser'desugared p = plusPtr p 16
p'HParser'desugared :: Ptr (C'HParser) -> Ptr (Ptr C'HCFChoice_)
instance Storable C'HParser where
  sizeOf _ = 20
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    v2 <- peekByteOff p 8
    v3 <- peekByteOff p 12
    v4 <- peekByteOff p 16
    return $ C'HParser v0 v1 v2 v3 v4
  poke p (C'HParser v0 v1 v2 v3 v4) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    pokeByteOff p 8 v2
    pokeByteOff p 12 v3
    pokeByteOff p 16 v4
    return ()

-- #synonym_t HParser , <HParser_>
{- typedef struct HParserTestcase_ {
            unsigned char * input; size_t length; char * output_unambiguous;
        } HParserTestcase; -}
data C'HParserTestcase = C'HParserTestcase{
  c'HParserTestcase'input :: Ptr CUChar,
  c'HParserTestcase'length :: CSize,
  c'HParserTestcase'output_unambiguous :: CString
} deriving (Eq,Show)
p'HParserTestcase'input p = plusPtr p 0
p'HParserTestcase'input :: Ptr (C'HParserTestcase) -> Ptr (Ptr CUChar)
p'HParserTestcase'length p = plusPtr p 4
p'HParserTestcase'length :: Ptr (C'HParserTestcase) -> Ptr (CSize)
p'HParserTestcase'output_unambiguous p = plusPtr p 8
p'HParserTestcase'output_unambiguous :: Ptr (C'HParserTestcase) -> Ptr (CString)
instance Storable C'HParserTestcase where
  sizeOf _ = 12
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    v2 <- peekByteOff p 8
    return $ C'HParserTestcase v0 v1 v2
  poke p (C'HParserTestcase v0 v1 v2) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    pokeByteOff p 8 v2
    return ()

-- #synonym_t HParserTestcase , <HParserTestcase_>
{- typedef struct HCaseResult_ {
            bool success;
            union {
                const char * actual_results; size_t parse_time;
            };
        } HCaseResult; -}
data C'HCaseResult = C'HCaseResult{
  c'HCaseResult'success :: CInt
} deriving (Eq,Show)
p'HCaseResult'success p = plusPtr p 0
p'HCaseResult'success :: Ptr (C'HCaseResult) -> Ptr (CInt)
instance Storable C'HCaseResult where
  sizeOf _ = 8
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    return $ C'HCaseResult v0
  poke p (C'HCaseResult v0) = do
    pokeByteOff p 0 v0
    return ()

-- #synonym_t HCaseResult , <HCaseResult_>
{- typedef struct HBackendResults_ {
            HParserBackend backend;
            bool compile_success;
            size_t n_testcases;
            size_t failed_testcases;
            HCaseResult * cases;
        } HBackendResults; -}
data C'HBackendResults = C'HBackendResults{
  c'HBackendResults'backend :: C'HParserBackend,
  c'HBackendResults'compile_success :: CInt,
  c'HBackendResults'n_testcases :: CSize,
  c'HBackendResults'failed_testcases :: CSize,
  c'HBackendResults'cases :: Ptr C'HCaseResult
} deriving (Eq,Show)
p'HBackendResults'backend p = plusPtr p 0
p'HBackendResults'backend :: Ptr (C'HBackendResults) -> Ptr (C'HParserBackend)
p'HBackendResults'compile_success p = plusPtr p 4
p'HBackendResults'compile_success :: Ptr (C'HBackendResults) -> Ptr (CInt)
p'HBackendResults'n_testcases p = plusPtr p 8
p'HBackendResults'n_testcases :: Ptr (C'HBackendResults) -> Ptr (CSize)
p'HBackendResults'failed_testcases p = plusPtr p 12
p'HBackendResults'failed_testcases :: Ptr (C'HBackendResults) -> Ptr (CSize)
p'HBackendResults'cases p = plusPtr p 16
p'HBackendResults'cases :: Ptr (C'HBackendResults) -> Ptr (Ptr C'HCaseResult)
instance Storable C'HBackendResults where
  sizeOf _ = 20
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    v2 <- peekByteOff p 8
    v3 <- peekByteOff p 12
    v4 <- peekByteOff p 16
    return $ C'HBackendResults v0 v1 v2 v3 v4
  poke p (C'HBackendResults v0 v1 v2 v3 v4) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    pokeByteOff p 8 v2
    pokeByteOff p 12 v3
    pokeByteOff p 16 v4
    return ()

-- #synonym_t HBackendResults , <HBackendResults_>
{- typedef struct HBenchmarkResults_ {
            size_t len; HBackendResults * results;
        } HBenchmarkResults; -}
data C'HBenchmarkResults = C'HBenchmarkResults{
  c'HBenchmarkResults'len :: CSize,
  c'HBenchmarkResults'results :: Ptr C'HBackendResults
} deriving (Eq,Show)
p'HBenchmarkResults'len p = plusPtr p 0
p'HBenchmarkResults'len :: Ptr (C'HBenchmarkResults) -> Ptr (CSize)
p'HBenchmarkResults'results p = plusPtr p 4
p'HBenchmarkResults'results :: Ptr (C'HBenchmarkResults) -> Ptr (Ptr C'HBackendResults)
instance Storable C'HBenchmarkResults where
  sizeOf _ = 8
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    return $ C'HBenchmarkResults v0 v1
  poke p (C'HBenchmarkResults v0 v1) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    return ()

-- #synonym_t HBenchmarkResults , <HBenchmarkResults_>
-- #ccall h_parse , Ptr <HParser_> -> Ptr CUChar -> CSize -> IO (Ptr <HParseResult_>)
foreign import ccall "hammer.h h_parse" h_parse
  :: Ptr C'HParser -> CString -> CSize -> IO (Ptr C'HParseResult)

parse :: Ptr C'HParser -> String -> Maybe C'HParseResult
parse p s = unsafeLocalState $ do
  let len = fromIntegral $ length s
  res <- withCString s $ \s' ->
    h_parse p s' len
  if res == nullPtr
  then return Nothing
  else Just <$> peek res

-- #ccall h_parse__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr CUChar -> CSize -> IO (Ptr <HParseResult_>)
-- #ccall h_token , Ptr CUChar -> CSize -> IO (Ptr <HParser_>)
foreign import ccall "hammer.h h_token" h_token
  :: CString -> CSize -> IO (Ptr C'HParser)

token :: String -> Ptr C'HParser
token s = unsafeLocalState $ do
  let len = fromIntegral $ length s
  withCString s $ \s' ->
    h_token s' len

-- #ccall h_token__m , Ptr <HAllocator_> -> Ptr CUChar -> CSize -> IO (Ptr <HParser_>)
-- #ccall h_ch , CUChar -> IO (Ptr <HParser_>)
foreign import ccall "hammer.h h_ch" h_ch
  :: CUChar -> IO (Ptr C'HParser)

ch :: Char -> Ptr C'HParser
ch = unsafeLocalState . h_ch . castCharToCUChar

-- #ccall h_ch__m , Ptr <HAllocator_> -> CUChar -> IO (Ptr <HParser_>)
-- #ccall h_ch_range , CUChar -> CUChar -> IO (Ptr <HParser_>)
foreign import ccall "hammer.h h_ch_range" h_ch_range
  :: CUChar -> CUChar -> IO (Ptr C'HParser)

ch_range :: Char -> Char -> Ptr C'HParser
ch_range x y =
  unsafeLocalState $ h_ch_range (castCharToCUChar x) (castCharToCUChar y)

-- #ccall h_ch_range__m , Ptr <HAllocator_> -> CUChar -> CUChar -> IO (Ptr <HParser_>)
-- #ccall h_int_range , Ptr <HParser_> -> CLong -> CLong -> IO (Ptr <HParser_>)
-- #ccall h_int_range__m , Ptr <HAllocator_> -> Ptr <HParser_> -> CLong -> CLong -> IO (Ptr <HParser_>)
-- #ccall h_bits , CSize -> CInt -> IO (Ptr <HParser_>)
-- #ccall h_bits__m , Ptr <HAllocator_> -> CSize -> CInt -> IO (Ptr <HParser_>)
-- #ccall h_int64 , IO (Ptr <HParser_>)
foreign import ccall "hammer.h h_int64" h_int64
  :: IO (Ptr C'HParser)

int64 :: Ptr C'HParser
int64 = unsafeLocalState h_int64

-- #ccall h_int64__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_int32 , IO (Ptr <HParser_>)
foreign import ccall "hammer.h h_int32" h_int32
  :: IO (Ptr C'HParser)

int32 :: Ptr C'HParser
int32 = unsafeLocalState h_int32

-- #ccall h_int32__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_int16 , IO (Ptr <HParser_>)
foreign import ccall "hammer.h h_int16" h_int16
  :: IO (Ptr C'HParser)

int16 :: Ptr C'HParser
int16 = unsafeLocalState h_int16

-- #ccall h_int16__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_int8 , IO (Ptr <HParser_>)
foreign import ccall "hammer.h h_int8" h_int8
  :: IO (Ptr C'HParser)

int8 :: Ptr C'HParser
int8 = unsafeLocalState h_int8

-- #ccall h_int8__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_uint64 , IO (Ptr <HParser_>)
-- #ccall h_uint64__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_uint32 , IO (Ptr <HParser_>)
-- #ccall h_uint32__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_uint16 , IO (Ptr <HParser_>)
-- #ccall h_uint16__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_uint8 , IO (Ptr <HParser_>)
-- #ccall h_uint8__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_whitespace , Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_whitespace__m , Ptr <HAllocator_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_left , Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_left__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_right , Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_right__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_middle , Ptr <HParser_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_middle__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_action , Ptr <HParser_> -> <HAction> -> Ptr () -> IO (Ptr <HParser_>)
-- #ccall h_action__m , Ptr <HAllocator_> -> Ptr <HParser_> -> <HAction> -> Ptr () -> IO (Ptr <HParser_>)
-- #ccall h_in , Ptr CUChar -> CSize -> IO (Ptr <HParser_>)
-- #ccall h_in__m , Ptr <HAllocator_> -> Ptr CUChar -> CSize -> IO (Ptr <HParser_>)
-- #ccall h_not_in , Ptr CUChar -> CSize -> IO (Ptr <HParser_>)
-- #ccall h_not_in__m , Ptr <HAllocator_> -> Ptr CUChar -> CSize -> IO (Ptr <HParser_>)
-- #ccall h_end_p , IO (Ptr <HParser_>)
-- #ccall h_end_p__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_nothing_p , IO (Ptr <HParser_>)
-- #ccall h_nothing_p__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_sequence , Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_sequence__m , Ptr <HAllocator_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_sequence__mv , Ptr <HAllocator_> -> Ptr <HParser_> -> <__builtin_va_list> -> IO (Ptr <HParser_>)
-- #ccall h_sequence__v , Ptr <HParser_> -> <__builtin_va_list> -> IO (Ptr <HParser_>)
-- #ccall h_sequence__a , Ptr (Ptr ()) -> IO (Ptr <HParser_>)
-- #ccall h_sequence__ma , Ptr <HAllocator_> -> Ptr (Ptr ()) -> IO (Ptr <HParser_>)
-- #ccall h_choice , Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_choice__m , Ptr <HAllocator_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_choice__mv , Ptr <HAllocator_> -> Ptr <HParser_> -> <__builtin_va_list> -> IO (Ptr <HParser_>)
-- #ccall h_choice__v , Ptr <HParser_> -> <__builtin_va_list> -> IO (Ptr <HParser_>)
-- #ccall h_choice__a , Ptr (Ptr ()) -> IO (Ptr <HParser_>)
-- #ccall h_choice__ma , Ptr <HAllocator_> -> Ptr (Ptr ()) -> IO (Ptr <HParser_>)
-- #ccall h_butnot , Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_butnot__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_difference , Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_difference__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_xor , Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_xor__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_many , Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_many__m , Ptr <HAllocator_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_many1 , Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_many1__m , Ptr <HAllocator_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_repeat_n , Ptr <HParser_> -> CSize -> IO (Ptr <HParser_>)
-- #ccall h_repeat_n__m , Ptr <HAllocator_> -> Ptr <HParser_> -> CSize -> IO (Ptr <HParser_>)
-- #ccall h_optional , Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_optional__m , Ptr <HAllocator_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_ignore , Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_ignore__m , Ptr <HAllocator_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_sepBy , Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_sepBy__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_sepBy1 , Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_sepBy1__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_epsilon_p , IO (Ptr <HParser_>)
-- #ccall h_epsilon_p__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_length_value , Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_length_value__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_attr_bool , Ptr <HParser_> -> <HPredicate> -> Ptr () -> IO (Ptr <HParser_>)
-- #ccall h_attr_bool__m , Ptr <HAllocator_> -> Ptr <HParser_> -> <HPredicate> -> Ptr () -> IO (Ptr <HParser_>)
-- #ccall h_and , Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_and__m , Ptr <HAllocator_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_not , Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_not__m , Ptr <HAllocator_> -> Ptr <HParser_> -> IO (Ptr <HParser_>)
-- #ccall h_indirect , IO (Ptr <HParser_>)
-- #ccall h_indirect__m , Ptr <HAllocator_> -> IO (Ptr <HParser_>)
-- #ccall h_bind_indirect , Ptr <HParser_> -> Ptr <HParser_> -> IO ()
-- #ccall h_bind_indirect__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParser_> -> IO ()
-- #ccall h_parse_result_free , Ptr <HParseResult_> -> IO ()
-- #ccall h_parse_result_free__m , Ptr <HAllocator_> -> Ptr <HParseResult_> -> IO ()
-- #ccall h_write_result_unamb , Ptr <HParsedToken_> -> IO CString
-- #ccall h_pprint , Ptr <_IO_FILE> -> Ptr <HParsedToken_> -> CInt -> CInt -> IO ()
-- #ccall h_compile , Ptr <HParser_> -> <HParserBackend_> -> Ptr () -> IO CInt
-- #ccall h_compile__m , Ptr <HAllocator_> -> Ptr <HParser_> -> <HParserBackend_> -> Ptr () -> IO CInt
-- #ccall h_bit_writer_new , Ptr <HAllocator_> -> IO (Ptr <HBitWriter_>)
-- #ccall h_bit_writer_put , Ptr <HBitWriter_> -> CULong -> CSize -> IO ()
-- #ccall h_bit_writer_get_buffer , Ptr <HBitWriter_> -> Ptr CSize -> IO (Ptr CUChar)
-- #ccall h_bit_writer_free , Ptr <HBitWriter_> -> IO ()
-- #ccall h_act_first , Ptr <HParseResult_> -> Ptr () -> IO (Ptr <HParsedToken_>)
-- #ccall h_act_second , Ptr <HParseResult_> -> Ptr () -> IO (Ptr <HParsedToken_>)
-- #ccall h_act_last , Ptr <HParseResult_> -> Ptr () -> IO (Ptr <HParsedToken_>)
-- #ccall h_act_flatten , Ptr <HParseResult_> -> Ptr () -> IO (Ptr <HParsedToken_>)
-- #ccall h_act_ignore , Ptr <HParseResult_> -> Ptr () -> IO (Ptr <HParsedToken_>)
-- #ccall h_benchmark , Ptr <HParser_> -> Ptr <HParserTestcase_> -> IO (Ptr <HBenchmarkResults_>)
-- #ccall h_benchmark__m , Ptr <HAllocator_> -> Ptr <HParser_> -> Ptr <HParserTestcase_> -> IO (Ptr <HBenchmarkResults_>)
-- #ccall h_benchmark_report , Ptr <_IO_FILE> -> Ptr <HBenchmarkResults_> -> IO ()
-- #ccall h_allocate_token_type , CString -> IO CInt
-- #ccall h_get_token_type_number , CString -> IO CInt
-- #ccall h_get_token_type_name , CInt -> IO CString
