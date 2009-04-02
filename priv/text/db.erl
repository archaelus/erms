{tables,[{user,[{record_name,user},
                {attributes,[name,password,realname,email]},
                {type, set},
                {disc_copies, ['erms@127.0.0.1']}]},
         {shortcode,[{record_name,shortcode},
                     {attributes,[name,description,mt_rules,mo_rules]},
                     {type, set},
                     {disc_copies, ['erms@127.0.0.1']}]},
         {connection,[{record_name,connection},
                      {attributes,[name,mod,args,supervised,rules]},
                      {type, set},
                      {disc_copies, ['erms@127.0.0.1']}]},
         {login,[{record_name,login},
                 {attributes,[type,username,password,shortcode_name,connection_name]},
                 {type, bag},
                 {disc_copies, ['erms@127.0.0.1']}]},
         {msg,[{record_name,msg},
               {attributes,[id,from,to,text]},
               {type, set},
               {disc_copies, ['erms@127.0.0.1']}]},
         {msg_status,
          [{record_name,msg_status},
           {attributes,[msg_id,from_connection,direction,shortcode,targets,state,delivery_time,expiry_time]},
           {type, set},
           {disc_copies, ['erms@127.0.0.1']}]},
         {delivery_failure,[{record_name,delivery_failure},
                            {attributes,[orig_msg_id, msg_id, destination, time, reason]},
                            {type, bag},
                            {disc_copies, ['erms@127.0.0.1']}]},
         {delivery_success,[{record_name,delivery_success},
                            {attributes,[orig_msg_id,msg_id,destination,time]},
                            {type, bag},
                            {disc_copies, ['erms@127.0.0.1']}]},
         {report_history,[{record_name, report_history},
                          {attributes,[date,report,data]},
                          {type, bag},
                          {disc_copies, ['erms@127.0.0.1']}]},
         {archived_msg,[{record_name, archived_msg},
                        {attributes, [msg_id,archive_date,msg_status,msgs]},
                        {type, set},
                        {disc_only_copies, ['erms@127.0.0.1']}]},
         {counter, [{record_name, counter},
                    {attributes, [key, count]},
                    {type, set},
                    {disc_copies, ['erms@127.0.0.1']}]},
        {campaign,[{record_name,campaign},
                    {attributes,[name, triggers, ad]},
                    {type, set},
                    {disc_copies, ['erms@127.0.0.1']}]}
        ]}.

{shortcode,"Debug",
 "Debugging shortcode",
 [{connection, "Debug-Log"}],
 [{connection, "Debug-Log"}]}.

{login,yaws,"debug-telecom","foobar","Debug","Debug-Log"}.
{login,yaws,"debug-vodafone","foobar","Debug","Debug-Log"}.

{connection, "Debug-Log", erms_debug_connection, [], true, []}.
{connection, "Debug-Vodafone", erms_random_debug_connection, [], true, []}.
{connection, "Debug-Telecom", erms_random_debug_connection, [], true, []}.
{connection, "Received MNP-MO", erms_debug_connection, [], true, []}.
{connection, "RandomDrop", erms_random_debug_connection, [], true, []}.
