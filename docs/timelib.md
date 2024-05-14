# TIMELIB

## Functions
## Generic-Functions
### format-timestamp
Format TIMESTAMP.
Destination can be T, then timestring is written to \*STANDARD-OUTPUT\*;
can be NIL, then a string is returned;
or can be a stream.

### timestamp->local-time
Generic timestamp to local-time conversion.

### timestamp-convert
Convert between different classes of time types.

### timestamp=
Compare timestamps for equality

## Slot-Accessors
### timezone-of
Timezone can be a LOCAL-TIME::TIMEZONE object, or an offset.

## Classs
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
