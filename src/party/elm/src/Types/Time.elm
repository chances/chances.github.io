module Types.Time exposing (Time)

import Time.DateTime exposing (DateTime, fromISO8601)

-- Data.Aeson UTC time encoder in Haskell
-- https://github.com/bos/aeson/blob/88ef704e2f6cdb4b9c60990637fb7890f286a2fd/Data/Aeson/Encoding/Builder.hs#L253

type alias Time =
    { utcTime = DateTime
    -- , localTime = DateTime
    }
