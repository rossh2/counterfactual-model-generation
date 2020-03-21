module Time
    ( Time
    ) where

data Time = Past | PastOrPresent | Present | Future | DeltaInDays Int -- Delta from Present, e.g. -1 for yesterday, +1 for tomorrow
    -- in LTL: Past = \exists t < t_0, PastOrPresent = \exists t <= t_0, Present = t_0, Future = \exists t > t_0
    deriving (Eq) -- Ord, Show declared below

instance Show Time where
    show Past = "Past"
    show PastOrPresent = "Past or Present"
    show Present = "Present"
    show Future = "Future"
    show (DeltaInDays 1) = "Tomorrow"
    show (DeltaInDays (-1)) = "Yesterday"
    show (DeltaInDays n) = if n > 0 then (dayOrDays n) ++ " from today" else (dayOrDays (-n)) ++ " ago"

dayOrDays :: Int -> String
dayOrDays n = if n == 1 then "1 day" else (show n) ++ " days"

instance Ord Time where
    (<=) Past            t               = True
    -- It's not clear where PastOrPresent should sit, but for the purpose of this ordering imagine that it sits a tiny delta of time before the present
    -- So any definitely past time is earlier than it, but the (definitely) present is later than it, and no definite time is equal to it
    (<=) PastOrPresent   Past            = False
    (<=) PastOrPresent   PastOrPresent   = True
    (<=) PastOrPresent   Present         = True
    (<=) PastOrPresent   Future          = True
    (<=) PastOrPresent   (DeltaInDays n) = n >= 0
    (<=) Present         Past            = False
    (<=) Present         PastOrPresent   = False
    (<=) Present         Present         = True
    (<=) Present         Future          = True
    (<=) Present         (DeltaInDays n) = n >= 0
    (<=) Future          Future          = True
    (<=) Future          t               = False
    (<=) (DeltaInDays n) Past            = False
    (<=) (DeltaInDays n) PastOrPresent   = n < 0
    (<=) (DeltaInDays n) Present         = n <= 0
    (<=) (DeltaInDays n) Future          = True