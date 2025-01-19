
api_url<-mwiki
mwiki

api_url <- paste0(mwiki,"api.php")
#username <- "your-username"
#password <- "your-password"

login_response <- POST(api_url, body = list(
  action = "login",
  lgname = username,
  lgpassword = password,
  format = "json"
))

login_token <- content(login_response)$login$token

login_confirm_response <- POST(api_url, body = list(
  action = "login",
  lgname = username,
  lgpassword = password,
  lgtoken = login_token,
  format = "json"
))

token_response <- GET(api_url, query = list(
  action = "query",
  meta = "tokens",
  type = "csrf",
  format = "json"
))

csrf_token <- content(token_response)$query$tokens$csrftoken

edit_response <- POST(api_url, body = list(
  action = "edit",
  # title = page.x$ns,
  #text = page.x$content,
  title = "Testpage",
  

    text = "[[new page]] to testing API, time: 25/01/14-10:43",
  token = csrf_token,
  format = "json"
))

edit_result <- content(edit_response)
print(edit_result)
