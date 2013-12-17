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

module Allocator where
import Foreign.Ptr
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{- typedef struct HAllocator_ {
            void * (* alloc)(struct HAllocator_ * allocator, size_t size);
            void * (* realloc)(struct HAllocator_ * allocator,
                               void * ptr,
                               size_t size);
            void (* free)(struct HAllocator_ * allocator, void * ptr);
        } HAllocator; -}
data C'HAllocator = C'HAllocator{
  c'HAllocator'alloc :: FunPtr (Ptr C'HAllocator -> CSize -> Ptr ()),
  c'HAllocator'realloc :: FunPtr (Ptr C'HAllocator -> Ptr () -> CSize -> Ptr ()),
  c'HAllocator'free :: FunPtr (Ptr C'HAllocator -> Ptr () -> IO ())
} deriving (Eq,Show)
p'HAllocator'alloc p = plusPtr p 0
p'HAllocator'alloc :: Ptr (C'HAllocator) -> Ptr (FunPtr (Ptr C'HAllocator -> CSize -> Ptr ()))
p'HAllocator'realloc p = plusPtr p 4
p'HAllocator'realloc :: Ptr (C'HAllocator) -> Ptr (FunPtr (Ptr C'HAllocator -> Ptr () -> CSize -> Ptr ()))
p'HAllocator'free p = plusPtr p 8
p'HAllocator'free :: Ptr (C'HAllocator) -> Ptr (FunPtr (Ptr C'HAllocator -> Ptr () -> IO ()))
instance Storable C'HAllocator where
  sizeOf _ = 12
  alignment _ = 4
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 4
    v2 <- peekByteOff p 8
    return $ C'HAllocator v0 v1 v2
  poke p (C'HAllocator v0 v1 v2) = do
    pokeByteOff p 0 v0
    pokeByteOff p 4 v1
    pokeByteOff p 8 v2
    return ()

-- {- typedef struct HArena_ HArena; -}
data C'HArena = C'HArena

-- #ccall h_new_arena , Ptr <HAllocator> -> CSize -> IO (Ptr <HArena_>)
-- #ccall h_arena_malloc , Ptr <HArena_> -> CSize -> IO (Ptr ())
-- #ccall h_arena_free , Ptr <HArena_> -> Ptr () -> IO ()
-- #ccall h_delete_arena , Ptr <HArena_> -> IO ()
-- {- typedef struct {
--             size_t used; size_t wasted;
--         } HArenaStats; -}
-- #starttype HArenaStats
-- #field used , CSize
-- #field wasted , CSize
-- #stoptype
-- #ccall h_allocator_stats , Ptr <HArena_> -> Ptr <HArenaStats> -> IO ()
