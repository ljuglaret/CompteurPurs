module Web.HTML.Event.DataTransfer
  ( DataTransfer
  , files
  , types
  , getData
  , setData
  , DropEffect(..)
  , dropEffect
  , setDropEffect
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.MediaType (MediaType(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Web.File.FileList (FileList)

foreign import data DataTransfer :: Type

-- | Contains a list of all the local files available on the data transfer.
-- | Empty if the drag operation doesn't involve dragging files.
-- |
-- | It's possible that a drag operation may have null files, instead of an
-- | empty file list. In these cases Nothing is returned.
files :: DataTransfer -> Maybe FileList
files = toMaybe <$> _files

foreign import _files :: DataTransfer -> Nullable FileList

-- | Returns an array of data formats used in the drag operation.
-- | If the drag operation included no data, then the array is empty.
foreign import types :: DataTransfer -> Array String

foreign import _getData
  :: String
  -> DataTransfer
  -> Effect String

-- | Retrieves the data for a given media type, or an empty string if data for
-- | that type does not exist or the data transfer object contains no data.
getData :: MediaType -> DataTransfer -> Effect String
getData (MediaType format) dt = _getData format dt

foreign import _setData
  :: String
  -> String
  -> DataTransfer
  -> Effect Unit

-- | Sets the data transfer object's data for a given media format.
setData
  :: MediaType
  -> String
  -> DataTransfer
  -> Effect Unit
setData (MediaType format) dat dt = _setData format dat dt

foreign import _dropEffect :: DataTransfer -> Effect String

data DropEffect = Copy | Link | Move | None

derive instance eqDropEffect :: Eq DropEffect
derive instance ordDropEffect :: Ord DropEffect

-- | Gets the data transfer object's drop effect.
dropEffect :: DataTransfer -> Effect DropEffect
dropEffect dt =
  _dropEffect dt <#> case _ of
    "copy" -> Copy
    "link" -> Link
    "move" -> Move
    "none" -> None
    _ -> None

foreign import _setDropEffect :: String -> DataTransfer -> Effect Unit

-- | Sets the data transfer object's drop effect.
setDropEffect :: DropEffect -> DataTransfer -> Effect Unit
setDropEffect de = _setDropEffect case de of
  Copy -> "copy"
  Link -> "link"
  Move -> "move"
  None -> "none"
