PROJECT = mnesia_cache
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0
PROJECT_APP_EXTRA_KEYS = {included_applications, ['mnesia']} # OR {mnesia, load} add to relx.config

include erlang.mk
