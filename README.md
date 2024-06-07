# CALENDAR-TIMES

CALENDAR-TIMES is a calendar time library implemented on top of LOCAL-TIME library.

It features zoned calendar times and calculations.

## Functions
### caltime->universal-time

```lisp
(caltime)
```

Convert CALTIME to UNIVERSAL-TIME.




### caltime-adjust

```lisp
(caltime &rest changes)
```


### caltime<

```lisp
(t1 t2)
```


### caltime<=

```lisp
(t1 t2)
```


### caltime=

```lisp
(t1 t2)
```

Returns T when the caltimes represent the same point in time.




### caltime>

```lisp
(t1 t2)
```


### caltime>=

```lisp
(t1 t2)
```


### caltimes-compose

```lisp
(t1 t2 &rest more)
```

Compose caltimes.



For example, a date + a time = datetime; a date-time + timezone = zoned-datetime..
### datetime-date

```lisp
(datetime)
```

Returns the DATE of DATETIME




### datetime-time

```lisp
(datetime)
```

Returns the WALLTIME of DATETIME.




### day-of-week

```lisp
(caltime &optional (format :number))
```

Return day of week of CALTIME.
FORMAT can be either :NUMBER (default) or :NAME.




### make-date

```lisp
(day month year)
```

Create a date object from DAY, MONTH and YEAR.




### make-datetime

```lisp
(seconds minutes hour day month year)
```

Create a date and time object.




### make-time

```lisp
(seconds minutes hour)
```

Create a time object.




### make-zoned-date

```lisp
(day month year &optional (timezone local-time:*default-timezone*))
```

Create a date with a timezone.




### make-zoned-datetime

```lisp
(seconds minutes hour day month year &optional
 (timezone local-time:*default-timezone*))
```

Create a datetime with a timezone.




### now

```lisp
(&optional timezone)
```

The ZONED-DATETIME now.




### time-now

```lisp
(&optional timezone)
```

The WALLTIME now.




### today

```lisp
(&optional timezone)
```

Returns DATE today.




## Generic-Functions
### caltime+

```lisp
(caltime amount unit &rest more)
```


### caltime-

```lisp
(caltime amount unit &rest more)
```

Return a new caltime from CALTIME reduced in AMOUNT UNITs.
Example:
(caltime- (now) 2 :day)




### caltime->local-time

```lisp
(caltime)
```

Generic caltime to local-time conversion.




### caltime-coerce

```lisp
(caltime class &rest args)
```

Convert between different classes of time types.




### caltime-difference

```lisp
(t1 t2 &optional unit)
```

Difference between caltimes, in UNITs.




### caltime-equalp

```lisp
(t1 t2)
```

Compare caltimes for equality.
This is a structural equality comparison. So, two caltimes that represent
the same point in time, but differ in one of its elements (for instance, its timezone), are considered different. Use CALTIME= for equality for caltimes that
represent the same point in time.




### clone-caltime

```lisp
(caltime &rest args)
```


### decode-caltime

```lisp
(caltime)
```

Decode a CALTIME parts and return them with VALUES.
The order of the list of values is the same as passed to the constructor functions.




### format-caltime

```lisp
(destination caltime &optional format &rest args)
```

Format CALTIME.
Destination can be T, then timestring is written to *STANDARD-OUTPUT*;
can be NIL, then a string is returned;
or can be a stream.




### parse-timestring

```lisp
(timestring class &rest args)
```

Parse TIMESTRING and return an instance of CLASS.
CLASS should be the class name of one of the subclasses of CALTIME.




## Slot-Accessors
### day-of
### hour-of
### minutes-of
### month-of
### seconds-of
### timezone-of
Timezone can be a LOCAL-TIME::TIMEZONE object, or an offset.

### year-of
## Classes
### caltime
Abstract caltime class

### date
A date like 2024-01-01

### datetime
A datetime like 2024-01-01T00:00:00

### walltime
Represents a 'wall' time. Like 01:01:22

### zoned-date
A date with a timezone.

### zoned-datetime
A datetime with a timezone.

## Constants
### +days-per-week+
### +hours-per-day+
### +minutes-per-day+
### +minutes-per-hour+
### +months-per-year+
### +seconds-per-day+
### +seconds-per-hour+
### +seconds-per-minute+
