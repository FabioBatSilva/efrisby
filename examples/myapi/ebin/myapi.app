{application,myapi,
             [{description,"Cowboy REST API."},
              {vsn,"1.0.0"},
              {modules,[myapi_app,myapi_handler,myapi_sup]},
              {registered,[myapi_sup]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{myapi_app,[]}},
              {env,[]}]}.
