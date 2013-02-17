
module Data.Yarr.IO.Image (
    -- * The Image array type 
      Image (..)

    -- * Image IO 
    , readImage, writeImage, readRGB, readRGBVectors
    ) where

import Control.Applicative (Applicative, (<$>))
import Control.Monad (when)

import Data.Int
import Data.Word

import Foreign.C.String (CString, withCString)
import Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import Foreign.Marshal.Utils (with)

import Data.Yarr

#include "IL/il.h"

type ILuint    = #type ILuint
type ILsizei   = #type ILsizei
type ILboolean = #type ILboolean
type ILenum    = #type ILenum
type ILint     = #type ILint
type ILubyte   = #type ILubyte

-- DevIL uses unsigned integers as names for each image in processing.
newtype ImageName = ImageName ILuint 
    deriving (Show)


data Image = RGBA (UArray F L Dim2 (VecList N4 Word8))
           | RGB (UArray F L Dim2 (VecList N3 Word8))
           | BGRA (UArray F L Dim2 (VecList N4 Word8))
           | BGR (UArray F L Dim2 (VecList N3 Word8))
           | Grey (UArray F L Dim2 Word8)

-- | @readRGBVectors = 'readRGB' 'construct'@
readRGBVectors
    :: (Vector v Word8, Dim v ~ N3)
    => Image -> UArray D L Dim2 (v Word8)
readRGBVectors = readRGB construct

-- | Fuction to uniformly import images of any type.
--
-- Example:
--
-- @
-- anyImage <- 'readImage' \"lena.png\"
-- let image = readRGB (\\r g b -> ...) anyImage
-- @
readRGB
    :: (Fun N3 Word8 a) -- ^ Passed red, green, blue component in @0-255@ range
    -> Image            -- ^ Image to import
    -> UArray D L Dim2 a
readRGB fmapRGB@(Fun mapRGB) image =
    case image of
        (RGBA arr) ->
            dmap (\v -> inspect v $ Fun $ \r g b a -> mapRGB r g b) arr
        (RGB arr)  ->
            dmap (\v -> inspect v fmapRGB) arr
        (BGRA arr) ->
            dmap (\v -> inspect v $ Fun $ \b g r a -> mapRGB r g b) arr
        (BGR arr)  ->
            dmap (\v -> inspect v $ Fun $ \b g r -> mapRGB r g b) arr
        (Grey arr) ->
            dmap (\l -> mapRGB l l l) arr

-- | Reads 'Image' from file.
readImage :: FilePath -> IO Image
readImage f = do
    ilInit
    name <- ilGenImageName
    ilBindImage name

    success <- ilLoadImage f
    when (not success) $
       error "Unable to load the image."

    toYarr name

-- | Writes 'Image' to file.
writeImage :: FilePath -> Image -> IO ()
writeImage f i = do
    ilInit
    name <- ilGenImageName
    ilBindImage name

    successCopy <- fromYarr i
    when (not successCopy) $
        error "Unable to copy the image to the DevIL buffer."

    successSave <- ilSaveImage f
    when (not successSave) $
        error "Unable to the save the image to the file."

    ilDeleteImage name

-- ----------------------------------------------------------------------

foreign import ccall unsafe "ilInit" ilInitC :: IO ()
foreign import ccall unsafe "ilOriginFunc" ilOriginFuncC :: ILenum -> IO ILboolean
foreign import ccall unsafe "ilEnable" ilEnableC :: ILenum -> IO ILboolean

-- | Initialize the library.
ilInit :: IO ()
ilInit = do
    ilInitC
    -- By default, origin is undefined and depends on the image type
    _ <- ilOriginFuncC (#const IL_ORIGIN_LOWER_LEFT)
    _ <- ilEnableC (#const IL_ORIGIN_SET)
    return ()
{-# INLINE ilInit #-}

foreign import ccall unsafe "ilGenImages" ilGenImagesC
  :: ILsizei -> Ptr ILuint -> IO ()

-- | Allocates a new image name.
ilGenImageName :: IO ImageName
ilGenImageName = do
    alloca $ \pName -> do
        ilGenImagesC 1 pName
        name <- peek pName
        return $! ImageName name
{-# INLINE ilGenImageName #-}

foreign import ccall unsafe "ilBindImage" ilBindImageC :: ILuint -> IO ()

-- | Sets the image name as the current image for processing.
ilBindImage :: ImageName -> IO ()
ilBindImage (ImageName name) = ilBindImageC name
{-# INLINE ilBindImage #-}

foreign import ccall unsafe "ilLoadImage" ilLoadImageC :: CString -> IO ILboolean

-- | Loads the image as the current DevIL image name.
ilLoadImage :: FilePath -> IO Bool
ilLoadImage f = (0 /=) <$> withCString f ilLoadImageC
{-# INLINE ilLoadImage #-}

foreign import ccall unsafe "ilGetInteger" ilGetIntegerC :: ILenum -> IO ILint

il_RGB, il_RGBA, il_BGR, il_BGRA, il_LUMINANCE :: ILenum
il_RGB = (#const IL_RGB)
il_RGBA = (#const IL_RGBA)
il_BGR = (#const IL_BGR)
il_BGRA = (#const IL_BGRA)
il_LUMINANCE = (#const IL_LUMINANCE)

il_IMAGE_HEIGHT, il_IMAGE_WIDTH :: ILenum
il_IMAGE_FORMAT, il_IMAGE_TYPE :: ILenum
il_UNSIGNED_BYTE :: ILenum
il_IMAGE_HEIGHT = (#const IL_IMAGE_HEIGHT)
il_IMAGE_WIDTH = (#const IL_IMAGE_WIDTH)
il_IMAGE_FORMAT = (#const IL_IMAGE_FORMAT)
il_IMAGE_TYPE = (#const IL_IMAGE_TYPE)
il_UNSIGNED_BYTE = (#const IL_UNSIGNED_BYTE)

foreign import ccall unsafe "ilConvertImage" ilConvertImageC
    :: ILenum -> ILenum -> IO ILboolean
foreign import ccall unsafe "ilGetData" ilGetDataC :: IO (Ptr ILubyte)

-- | Puts the current image inside a yarr array.
toYarr :: ImageName -> IO Image
toYarr name = do
    width' <- ilGetIntegerC il_IMAGE_WIDTH
    height' <- ilGetIntegerC il_IMAGE_HEIGHT
    let (width, height) = (fromIntegral width', fromIntegral height')
    format <- ilGetIntegerC il_IMAGE_FORMAT
    pixelType <- fromIntegral <$> ilGetIntegerC il_IMAGE_TYPE

    case fromIntegral format :: ILenum of
        (#const IL_RGB) -> do
            convert il_RGB pixelType
            RGB <$> pixelsToArray (height, width)
        (#const IL_RGBA) -> do
            convert il_RGBA pixelType
            RGBA <$> pixelsToArray (height, width)
        (#const IL_BGR) -> do
            convert il_BGR pixelType
            BGR <$> pixelsToArray (height, width)
        (#const IL_BGRA) -> do
            convert il_BGRA pixelType
            BGRA <$> pixelsToArray (height, width)
        (#const IL_LUMINANCE) -> do
            convert il_LUMINANCE pixelType
            Grey <$> pixelsToArray (height, width)
        _ -> do
            ilConvertImage il_RGBA il_UNSIGNED_BYTE
            RGBA <$> pixelsToArray (height, width)
  where
    -- Converts the image to the given format if the pixel type isn't Word8.
    convert format pixelType
        | pixelType == il_UNSIGNED_BYTE = return ()
        | otherwise = ilConvertImage format il_UNSIGNED_BYTE

    -- Converts the C vector of unsigned bytes to a garbage collected repa 
    -- array.
    pixelsToArray :: Dim2 -> IO (UArray F L Dim2 a)
    pixelsToArray dstExtent = do
        pixels <- ilGetDataC
        managedPixels <- newForeignPtr pixels (ilDeleteImage name)
        arr <- unsafeFromForeignPtr dstExtent (castForeignPtr managedPixels)
        arr `deepseq` return ()
        return arr

    ilConvertImage format pixelType = do
        success <- (0 /=) <$> ilConvertImageC format pixelType
        when (not success) $
                error "Unable to convert the image to a supported format."

foreign import ccall unsafe "ilTexImage" ilTexImageC
    :: ILuint -> ILuint -> ILuint   -- w h depth
    -> ILubyte -> ILenum -> ILenum  -- numberOfChannels format type
    -> Ptr ()                       -- data (copy from this pointer)
    -> IO ILboolean

-- | Copies the repa array to the current image buffer.
fromYarr :: Image -> IO Bool
fromYarr (RGB i)  =
    let (h, w) = extent i
    in (0 /=) <$> (withForeignPtr (toForeignPtr i) $ \p ->
            ilTexImageC (fromIntegral w) (fromIntegral h) 1 3
                        (fromIntegral il_RGB) il_UNSIGNED_BYTE (castPtr p))
fromYarr (RGBA i) =
    let (h, w) = extent i
    in (0 /=) <$> (withForeignPtr (toForeignPtr i) $ \p ->
            ilTexImageC (fromIntegral w) (fromIntegral h) 1 4
                        (fromIntegral il_RGBA) il_UNSIGNED_BYTE (castPtr p))
fromYarr (BGR i)  =
    let (h, w) = extent i
    in (0 /=) <$> (withForeignPtr (toForeignPtr i) $ \p ->
            ilTexImageC (fromIntegral w) (fromIntegral h) 1 3
                        (fromIntegral il_BGR) il_UNSIGNED_BYTE (castPtr p))
fromYarr (BGRA i) =
    let (h, w) = extent i
    in (0 /=) <$> (withForeignPtr (toForeignPtr i) $ \p ->
            ilTexImageC (fromIntegral w) (fromIntegral h) 1 4
                        (fromIntegral il_BGRA) il_UNSIGNED_BYTE (castPtr p))
fromYarr (Grey i) =
    let (h, w) = extent i
    in (0 /=) <$> (withForeignPtr (toForeignPtr i) $ \p ->
            ilTexImageC (fromIntegral w) (fromIntegral h) 1 1
                        (fromIntegral il_LUMINANCE) il_UNSIGNED_BYTE 
                        (castPtr p))

foreign import ccall unsafe "ilSaveImage" ilSaveImageC :: CString -> IO ILboolean

-- | Saves the current image.
ilSaveImage :: FilePath -> IO Bool
ilSaveImage file = do
    (0 /=) <$> withCString file ilSaveImageC
{-# INLINE ilSaveImage #-}

foreign import ccall unsafe "ilDeleteImages" ilDeleteImagesC
    :: ILsizei -> Ptr ILuint -> IO ()

-- | Releases an image with its name.
ilDeleteImage :: ImageName -> IO ()
ilDeleteImage (ImageName name) =
    with name $ \pName ->
        ilDeleteImagesC 1 pName
{-# INLINE ilDeleteImage #-}