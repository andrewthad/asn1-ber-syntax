# Notes

Just some thought about how I would change the design of
this library. Currently, we have:

    data Value = Value
      { tagClass :: !Class
      , tagNumber :: !Word32
      , contents :: !Contents
      }
    data Contents
      = Boolean !Bool -- ^ Tag number: @0x01@
      | Integer !Int64 -- ^ Tag number: @0x02@
      | Utf8String !TS.ShortText
      ...
      | Constructed !(SmallArray Value)
      | Unresolved !Bytes

This works, but something is unsettling. We parse by
looking at the class and then primitive/constructed bit
and considering these cases:

1. Universal class, primitive type: parse the field all
   the way to something like `Integer` or `Utf8String`.
2. Constructed type: Decode to `Constructed`.
3. Primitive type, non-universal class: decode to `Unresolved`.

Having the two cases (1 and 3) for primitive types is odd. We could
kick this out and say that it's a concern for a higher level. To do
that, we would change `Contents` to

    data Contents
      = Constructed !(SmallArray Value)
      | Primitive !Bytes

All those extra data constructors would be replaced by `Bytes` parsers
that high-level decode function would use dependending on what types
were expected.
