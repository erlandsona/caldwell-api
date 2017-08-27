firebase = require 'firebase'

# Initialize Firebase
firebase.initializeApp
  apiKey: 'AIzaSyBKuRiMO3rhBv9cAzhAZJHYf6yyEVCnp5I'
  authDomain: 'caldwell-band.firebaseapp.com'
  databaseURL: 'https://caldwell-band.firebaseio.com'
  projectId: 'caldwell-band'
  storageBucket: 'caldwell-band.appspot.com'
  messagingSenderId: '567749644974'

[database, auth] = [firebase.database(), firebase.auth()]

module.exports = { firebase, database, auth }
