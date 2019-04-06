module internal InternalPGTypes

open InternalColumn
module HPQ = InternalPrimQuery

type STextEncoding = System.Text.Encoding
type SByteString = FSharpx.Collections.ByteString
//import qualified Data.ByteString.Lazy as LByteString
//import qualified Data.Time.Locale.Compat as Locale

/// FIXME: SQLite requires temporal types to have the type "TEXT" which
/// may cause problems elsewhere.
//let unsafePgFormatTime (typeName: Time.FormatTime t => HPQ.Name -> String -> t -> Column c
//unsafePgFormatTime _typeName formatString = castToType "TEXT" . format
//  where format = Time.formatTime Locale.defaultTimeLocale formatString

let literalColumn (lit : HPQ.Literal) : Column<'a> =
  lit |> HPQ.ConstExpr |> Column

let castToType (typeName: HPQ.Name) (str: string) : Column<'c> =
  Column <| HPQ.CastExpr (typeName,  HPQ.ConstExpr ^ HPQ.OtherLit ^ str)

//let strictDecodeUtf8 : SByteString -> string =
//  SText.unpack . STextEncoding.decodeUtf8
//
//lazyDecodeUtf8 :: LByteString.ByteString -> String
//lazyDecodeUtf8 = LText.unpack . LTextEncoding.decodeUtf8
