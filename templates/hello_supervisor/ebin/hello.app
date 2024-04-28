% More on this here: https://www.erlang.org/doc/man/app.html
{application, hello,
  [{description, "A hello server"},
    {vsn, "0.1.0"},
    {modules, [hello_app, hello_sup, hello_server]},
    {registered, [hello_sup, hello_server]},
    {applications, [
      kernel,
      stdlib
    ]},
    {mod, {hello_app, []}},
    {env, []}
]}.
