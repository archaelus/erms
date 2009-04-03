{application, erms,
 [{description, "Erlang SMS Gateway"},
  {vsn, "0.9"},
  {applications, [kernel,
                  stdlib,
                  sasl,
                  inets,
                  mnesia,
                  ibrowse,
                  proc_reg,
                  erlmail]},
  {mod, {erms_app, []}},
  {modules, [erms
             ,erms_app
             ,erms_auth
             ,erms_config
             ,erms_connection
             ,erms_connection_mgr
             ,erms_connection_sup
             ,erms_db
             ,erms_debug_connection
             ,erms_esme_connection
             ,erms_esme_listener
             ,erms_http_connection
             ,erms_inject
             ,erms_inject_mo
             ,erms_inject_mt
             ,erms_ipms
             ,erms_ipms_cache
             ,erms_log
             ,erms_mnp
             ,erms_mnp_cache
             ,erms_msg
             ,erms_msg_archiver
             ,erms_msg_queue
             ,erms_msg_queue_processor
             ,erms_numbers
             ,erms_random_debug_connection
             ,erms_rb
             ,erms_reports
             ,erms_rpc
             ,erms_shortcode
             ,erms_shortcode_lib
             ,erms_shortcode_sup
             ,erms_smpp
             ,erms_smsc
             ,erms_stats
             ,erms_sup
             ,erms_test_esme
             ,erms_uuid
             ,esvc_csv
             ,esvc_mnp_mo
             ,esvc_voucher
             ,esvc_voucher_db
             ,esvc_voucher_export
             ,esvc_voucher_import
             ,healthcheck_simulator
            ]},
  {registered, [erms_sup,
                erms_connection_sup,
                erms_shortcode_sup,
                erms_rb,
                erms_msg_archiver,
                erms_msg_queue,
                erms_stats,
                erms_mnp_cache,
                erms_test_esme,
                erms_ipms_cache]},
  {start_phases, [{recover, []},
                  {go, []}]}
 ]}.
