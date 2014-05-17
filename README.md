Vom - A tiny logging library for Common Lisp
============================================

Vom is a logging library for lisp. It's goal is to be useful and small. It does
not provide a lot of features as other loggers do, but has a small codebase
that's easy to understand and use.

## Documentation

Logging is done by calling one of the logging macros:

- emerg
- alert
- crit
- error
- warn
- notice
- info
- debug
- debug1
- debug2
- debug3
- debug4

Each of these is a macro defined as such:

```lisp
(defmacro notice (format-str &rest args) ...)
```

They are used almost exactly like `format` (but without specifying the stream):

```lisp
(vom:error "there was a problem setting up your database: ~a" error)
```

### Configuration

You can set a global logging level:

```lisp
;; set the default loglevel such that only errors (or higher) get logged
(vom:config t :error)
```

or you can set per-package loglevels:

```lisp
(vom:config :my-package :notice)
```

In the above examples, any unconfigured package will have the loglevel of
`:error`, but the package `my-package` will log anything that's a `:notice` or
above.

### Output stream

You can change the stream that vom logs to via `vom:*log-stream*`. This defaults
to `t` (ie standard output).

## License

MIT. Do what you want with it. Just give me credit. Or I'll come to your house
for two weeks and eat your food and sleep on your couch and use your toothbrush.

