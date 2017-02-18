-record(timer, {
          start_date_time,
          duration
         }).

-record(session, {
          user_id,
          session_id,
          timers
         })

-record(post_data, {
          url,
          content
         }).
