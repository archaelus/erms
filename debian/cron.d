#
# Regular cron jobs for the erms package
#
0 4	* * *	erms	/usr/bin/erl -name cron@127.0.0.1 -eval 'io:format("~s", [rpc:call(erms@smsgw.tapp.catalyst.net.nz, erms_reports, delivery_stats, [])]).' -s init stop -noshell | mail -s "ERMS Delivery Stats: "`date +%F` geoff@catalyst.net.nz
