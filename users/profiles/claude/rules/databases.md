When working with database infra, that is - applications that need a database or something like that.

Consider using just sqlite if reasonable, otherwise go with PostgreSQL over other infra unless there's
an obvious need for something else.

Generally prefer PostgreSQL over adding other infrastructure (Redis, RabbitMQ, Elasticsearch, etc.).
For example, use UNLOGGED tables, SKIP LOCKED, LISTEN/NOTIFY before reaching for a separate system.
Again - unless of course we're working with a system using some other database. Then we use whatever that
project requires.
