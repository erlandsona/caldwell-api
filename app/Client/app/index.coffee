require 'font-awesome/css/font-awesome'
require('smoothscroll-polyfill').polyfill()
require('fastclick').attach document.body
{ database, auth } = require './firebase'

# Source
require './assets/fonts'
require './Stylesheets'


now = new Date().getTime()
shows = try JSON.parse localStorage.getItem('shows')
      

# Attach port handlers to app instance.
# app : Initializer -> Program (Maybe Value) Model Msg
app = require('./Main').Site.fullscreen { shows, now }

# Firebase Stuff
# app.ports.

# Smooth Scrolling between Nav Links
app.ports.easeIntoView.subscribe (id) ->
  document
    .getElementById id
    .scrollIntoView
      behavior: "smooth"
      block: "start"

app.ports.snapIntoView.subscribe (id) ->
  document
    .getElementById id
    .scrollIntoView()
