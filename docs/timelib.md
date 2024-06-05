# TIMELIB

TIMELIB is a calendar time library implemented on top of LOCAL-TIME library.

It features zoned time-entities and calculations.

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
(time-entity &optional (format :number))
```

Return day of week of TIME-ENTITY.
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




### time-entities-compose

```lisp
(t1 t2 &rest more)
```

Compose time-entities.



For example, a date + a time = datetime; a date-time + timezone = zoned-datetime..
### time-entity->universal-time

```lisp
(time-entity)
```

Convert TIME-ENTITY to UNIVERSAL-TIME.




### time-entity-adjust

```lisp
(time-entity &rest changes)
```


### time-entity<

```lisp
(t1 t2)
```


### time-entity<=

```lisp
(t1 t2)
```


### time-entity=

```lisp
(t1 t2)
```

Returns T when the time-entities represent the same point in time.




### time-entity>

```lisp
(t1 t2)
```


### time-entity>=

```lisp
(t1 t2)
```


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
### clone-time-entity

```lisp
(time-entity &rest args)
```


### decode-time-entity

```lisp
(time-entity)
```

Decode a TIME-ENTITY parts and return them with VALUES.
The order of the list of values is the same as passed to the constructor functions.




### format-time-entity

```lisp
(destination time-entity &optional format &rest args)
```

Format TIME-ENTITY.
Destination can be T, then timestring is written to *STANDARD-OUTPUT*;
can be NIL, then a string is returned;
or can be a stream.




### parse-timestring

```lisp
(timestring class &rest args)
```

Parse TIMESTRING and return an instance of CLASS.
CLASS should be the class name of one of the subclasses of TIME-ENTITY.




### time-entity+

```lisp
(time-entity amount unit &rest more)
```


### time-entity-

```lisp
(time-entity amount unit &rest more)
```

Return a new time-entity from TIME-ENTITY reduced in AMOUNT UNITs.
Example:
(time-entity- (now) 2 :day)




### time-entity->local-time

```lisp
(time-entity)
```

Generic time-entity to local-time conversion.




### time-entity-coerce

```lisp
(time-entity class &rest args)
```

Convert between different classes of time types.




### time-entity-difference

```lisp
(t1 t2 &optional unit)
```

Difference between time-entities, in UNITs.




### time-entity-equalp

```lisp
(t1 t2)
```

Compare time-entities for equality.
This is a structural equality comparison. So, two time-entities that represent
the same point in time, but differ in one of its elements (for instance, its timezone), are considered different. Use TIME-ENTITY= for equality for time-entities that
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

### time-entity
Abstract time-entity class

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
