#!/bin/sh

# curl -w '\n' 'http://localhost:8080/createItem' --data 'name=sample&mode=hudson.model.FreeStyleProject&Submit=OK' -XPOST
# curl -H 'Content-Type:application/json' -H 'User-Agent:iPhone' -H 'Accept-Encoding:gzip,deflate' -d "{"key":"val","key2":",val2"}" http://~~~~~~~~~~
curl -v -X POST -H "Content-Type: application/json" -d '
{
  "start_work_hock": {
    "url" : "your slack url",
    "content" : {
      "text": "ポモドーロをがんばってくださいね！", 
      "icon_emoji": ":tomato:"
    }
  },

  "start_rest_hock": {
    "url" : "your slack hock",
    "content" : {
      "text": "ポモドーロが終わりましたよ！", 
      "icon_emoji": ":tomato:"
    }
  },

  "session_completion_hock": {
    "url" : "your slack hock",
    "content" : {
      "text": "ふたたび頑張るときです！", 
      "icon_emoji": ":tomato:"
    }
  }
}
' http://localhost:8080/slack_me
