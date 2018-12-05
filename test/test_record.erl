-module(test_record).

-record(r, {a,
            b = 42,
            c :: string(),
            d = 57 :: integer()}).
