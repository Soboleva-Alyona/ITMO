module HW1.T1(
    Day(..),
    isWeekend,
    nextDay,
    daysToParty,
    afterDays,
    getDay
) where

import Numeric.Natural

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving(Show)

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays n Monday = getDay (mod (0 + n) 7)
afterDays n Tuesday = getDay (mod (1 + n) 7)
afterDays n Wednesday = getDay (mod (2 + n) 7)
afterDays n Thursday = getDay (mod (3 + n) 7)
afterDays n Friday = getDay (mod (4 + n) 7)
afterDays n Saturday = getDay (mod (5 + n) 7)
afterDays n Sunday = getDay (mod (6 + n) 7)

getDay :: Natural -> Day
getDay 0 = Monday
getDay 1 = Tuesday
getDay 2 = Wednesday
getDay 3 = Thursday
getDay 4 = Friday
getDay 5 = Saturday
getDay 6 = Sunday
getDay _ = undefined

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend Saturday  = True
isWeekend Sunday = True 
isWeekend _ = False

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
daysToParty Friday = 0
daysToParty Saturday = 6
daysToParty Sunday = 5
daysToParty Monday = 4
daysToParty Tuesday = 3
daysToParty Wednesday = 2
daysToParty Thursday = 1