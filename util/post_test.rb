require 'net/http'
require 'json'

res = Net::HTTP.post_form(URI.parse('http://localhost:8080/user/create'), 'Content-Type' => 'application/json')
res = JSON.parse(res.body)

user_id = res["user_id"]

uri = URI("http://localhost:8080/pomodoro/start/#{user_id}")
req = Net::HTTP::Post.new(uri, 'Content-Type' => 'application/json')
req.body = <<JSON
{
  "property": {
    "work_duration": 1,
    "rest_duration": 1
  },
  "event": {
    "start": [], 
    "work_done": [],
    "rest_done": []
  }
}
JSON

res = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(req)
end

p res

uri = URI("http://localhost:8080/session/#{user_id}")
p uri
sleep(1)

res = Net::HTTP.get(uri)
