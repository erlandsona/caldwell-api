# Libs
require 'font-awesome/css/font-awesome'
require('smoothscroll-polyfill').polyfill()
require('fastclick').attach document.body

# Source
require './assets/fonts'
require './Stylesheets'


now = new Date().getTime()
cachedGigs = try JSON.parse localStorage.getItem('gigs')

scroll = window.pageYOffset || document.body.scrollTop

# Attach port handlers to app instance.
# app : Initializer -> Program (Maybe Value) Model Msg
app = require('./Main').Site.fullscreen { cachedGigs, now }


# Smooth Scrolling between Nav Links
app.ports.easeIntoView.subscribe (className) ->
  document
    .querySelector(className)
    .scrollIntoView
      behavior: "smooth"
      block: "start"

app.ports.snapIntoView.subscribe (className) ->
  document
    .querySelector(className)
    .scrollIntoView()

window.onscroll = ->
  newScroll = window.pageYOffset || document.body.scrollTop
  app.ports.scroll.send [scroll, newScroll]
  scroll = newScroll
