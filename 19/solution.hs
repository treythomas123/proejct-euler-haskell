-- Solution to Project Euler problem 19
-- By Trey Thomas
--
-- You are given the following information, but you may prefer to do some
-- research for yourself.
--
-- 1 Jan 1900 was a Monday.
-- Thirty days has September,
-- April, June and November.
-- All the rest have thirty-one,
-- Saving February alone,
-- Which has twenty-eight, rain or shine.
-- And on leap years, twenty-nine.
-- A leap year occurs on any year evenly divisible by 4, but not on a century
-- unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth century
-- (1 Jan 1901 to 31 Dec 2000)?

monday = 1
sunday = 7

january = 1
february = 2
march = 3
april = 4
may = 5
june = 6
july = 7
august = 8
september = 9
october = 10
november = 11
december = 12

daysInMonth year month
    | elem month [september,april,june,november] = 30
    | month == february = if leapYear then 29 else 28
    | otherwise = 31
    where leapYear = mod year 400 == 0 || (mod year 4 == 0 && mod year 100 > 0)

datesFrom1900 = zip [ (year, month, day) |
                        year <- [1900..],
                        month <- [january..december],
                        day <- [1..(daysInMonth year month)] ]
                    $ cycle [monday..sunday]

main = print $ length $ filter sundayOnFirstDayOfMonth $ dates
    where
        sundayOnFirstDayOfMonth ((_,_,d),wd) = d == 1 && wd == sunday
        year ((y,_,_),_) = y
        dates = takeWhile ((<2001) . year) $ dropWhile ((<1901) . year) datesFrom1900
