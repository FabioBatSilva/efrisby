{application,rest_api,
             [{description,"Cowboy REST API."},
              {vsn,"1.0.0"},
              {modules,[rest_api_app,rest_api_handler,rest_api_sup]},
              {registered,[rest_api_sup]},
              {applications,[kernel,stdlib,cowboy]},
              {mod,{rest_api_app,[]}},
              {env,[]}]}.
