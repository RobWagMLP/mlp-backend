{application,dbconnect,
             [{description,"db-connect-enginge"},
              {vsn,"0.1.0"},
              {applications,[kernel,stdlib,ssl,epgsql,poolboy,varpool]},
              {registered,[dbconnect,dbconnect_worker]},
              {mod,{dbconnect,[]}},
              {modules,[dbconnect,dbconnect_worker]},
              {registered,[rest_server]}]}.
