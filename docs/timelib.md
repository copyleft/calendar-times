# TIMELIB

TIMELIB is a calendar time library implemented on top of LOCAL-TIME library.

It features zoned timestamps and calculations.

## Functions
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
(timestamp &optional (format :number))
```

Return day of week of TIMESTAMP.
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




### timestamp->universal-time

```lisp
(timestamp)
```

Convert TIMESTAMP to UNIVERSAL-TIME.




### timestamp<

```lisp
(t1 t2)
```


### timestamp<=

```lisp
(t1 t2)
```


### timestamp=

```lisp
(t1 t2)
```

Returns T when the timestamps represent the same point in time.




### timestamp>

```lisp
(t1 t2)
```


### timestamp>=

```lisp
(t1 t2)
```


### timestamps-compose

```lisp
(t1 t2 &rest more)
```

Compose timestamps.



For example, a date + a time = datetime; a date-time + timezone = zoned-datetime..
### today

```lisp
(&optional timezone)
```

Returns DATE today.




## Generic-Functions
### clone-timestamp

```lisp
(timestamp &rest args)
```


### decode-timestamp

```lisp
(timestamp)
```

Decode a TIMESTAMP parts and return them with VALUES.
The order of the list of values is the same as passed to the constructor functions.




### format-timestamp

```lisp
(destination timestamp &rest args)
```

Format TIMESTAMP.
Destination can be T, then timestring is written to *STANDARD-OUTPUT*;
can be NIL, then a string is returned;
or can be a stream.




### parse-timestring

```lisp
(timestring class &rest args)
```

Parse TIMESTRING and return an instance of CLASS.
CLASS should be the class name of one of the subclasses of TIMESTAMP.




### timestamp+

```lisp
(timestamp amount unit &rest more)
```


### timestamp-

```lisp
(timestamp amount unit &rest more)
```


### timestamp->local-time

```lisp
(timestamp)
```

Generic timestamp to local-time conversion.




### timestamp-coerce

```lisp
(timestamp class &rest args)
```

Convert between different classes of time types.




### timestamp-difference

```lisp
(t1 t2 &optional unit)
```

Difference between timestamps, in UNITs.




### timestamp-equalp

```lisp
(t1 t2)
```

Compare timestamps for equality.
This is a structural equality comparison. So, two timestamps that represent
the same point in time, but differ in one of its elements (for instance, its timezone), are considered different. Use TIMESTAMP= for equality for timestamps that
represent the same point in time.




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
### date
A date like 2024-01-01

### datetime
A datetime like 2024-01-01T00:00:00

### timestamp
Abstract timestamp class

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
