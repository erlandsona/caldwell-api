# Libs
require './assets/icons/font-awesome/css/fontello'
require('smoothscroll-polyfill').polyfill()
require('fastclick').attach document.body
throttle = require('lodash.throttle')

# Source
require './assets/fonts'
require './Stylesheets'

class App
  constructor: (@ports) ->
    @ports.postInit.subscribe (classNames) =>
      @setScrolling()
      @initializeScrollTargets classNames
      @setupScrollSubscription()

    # Smooth Scrolling between Nav Links
    @ports.easeIntoView.subscribe (className) =>
      @setScrolling()
      document
        .querySelector(className)
        .scrollIntoView
          behavior: "smooth"
          block: "start"

    @ports.snapIntoView.subscribe (className) =>
      target = document.querySelector className
      document.documentElement.scrollTop =
        target.offsetTop - @getCurrentScrollPosition()

  setScrolling: =>
    setTimeout =>
      @ports.scrollStart.send null
    , 1000

  initializeScrollTargets: (classNames) =>
    elems = classNames.map (className) =>
      document.querySelector className
    bottoms = elems.map (e) =>
      # 250 px from the bottom to update routes.
      (e.offsetTop + e.offsetHeight) - 250
    @ports.scrollTargets.send bottoms

  setupScrollSubscription: =>
    scroll = @getCurrentScrollPosition()

    window.onscroll = throttle =>
      newScroll =  @getCurrentScrollPosition()
      @ports.scroll.send [scroll, newScroll]
      scroll = newScroll
    , 30

  getCurrentScrollPosition: =>
    window.pageYOffset or
      (document.documentElement or
       document.parentNode or
       document.body).scrollTop
# Attach port handlers to app instance.
# app : Initializer -> Program (Maybe Value) Model Msg
now = new Date().getTime()
cachedGigs = try JSON.parse(localStorage.getItem 'gigs')
elm = require('./Client').Site.fullscreen { cachedGigs, now }
new App(elm.ports)
