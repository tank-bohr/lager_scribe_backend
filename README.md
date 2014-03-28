Lager Scribe backend
====================

Lager backend for [facebook's scribe](https://github.com/facebook/scribe). This is a backend for the [Lager Erlang logging framework](https://github.com/basho/lager).

### Configuration

You can pass the backend the following configuration (shown are the defaults):

```erlang
    {lager, [
      {handlers, [
        {lager_scribe_backend, [
          {level,            debug},
          {formatter,        lager_default_formatter},
          {formatter_config, []},
          {category,         undefined}
          {scribe_host,      "localhost"},
          {scribe_port,      1463},
          {pool_size,        [{size, 5}, {max_overflow, 10}]}
        ]}
      ]}
    ]}
```

### Scribe log entry category

To define custom scribe category for message one should implement own `category` module like this

```erlang
-module(myapp_lagger_scribe_category).

-behaviour(lager_scribe_category).
-export([category/1]).

category(Message) ->
    Severity = lager_msg:severity(Message),
    log_category(Severity).

log_category(error) -> "myapp-error";
log_category(_)     -> "myapp-info".
```

by defalt category would be `lager.<lager severity>`

### Poolboy integration

`lager_scribe_backend` uses process pool to send log messages to scribe. You can tune pool size in handler config. To know more about `max_overflow` option see [poolboy documentation](https://github.com/devinus/poolboy/blob/master/README.md#options)
