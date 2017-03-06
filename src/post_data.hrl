-record(timer, {
          start_time,
          duration,
          done_events
         }).

-record(timer_seed, {
          duration,
          done_events
         }).

-record(post_slack_event, {
          path,
          content 
         }).

-record(complete_work_event, {
          user_id,
          property,
          option
         }).

-record(session, {
          user_id,
          session_id,
          timers
         }).

-record(post_data, {
          url,
          content
         }).
