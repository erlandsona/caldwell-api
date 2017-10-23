path = require 'path'
webpack = require 'webpack'
merge = require 'webpack-merge'
HtmlWebpackPlugin = require 'html-webpack-plugin'
autoprefixer = require 'autoprefixer'
ExtractTextPlugin = require 'extract-text-webpack-plugin'
CopyWebpackPlugin = require 'copy-webpack-plugin'

entryFile = path.resolve __dirname, 'app/index.coffee'
outputPath = path.resolve __dirname, 'public'

# determine build env
TARGET_ENV =
  if process.env.npm_lifecycle_event is 'build'
  then 'production' else 'development'

console.log 'WEBPACKing with ENV:', TARGET_ENV

outputFilename = if TARGET_ENV is 'production' then '[name]-[hash].js' else '[name].js'

cssLoader =
  loader: 'css-loader'
  options:
    minimize:
      convertValues: false

postcssLoader =
  loader: 'postcss-loader'
  options:
    plugins: [
      require('autoprefixer') browsers: ['last 2 versions']
    ]

# common webpack config
commonConfig =
  output:
    path: outputPath
    filename: outputFilename
    publicPath: '/'

  resolve:
    extensions: [
      '.js'
      '.elm'
      '.css'
      '.coffee'
    ]

  module:
    rules: [
      test: /\.(eot|svg|ttf|woff(2)?)(\?v=\d+\.\d+\.\d+)?/
      loader: 'url-loader?limit=10000'
    ,
      test: /\.coffee$/
      loader: 'coffee-loader'
    ]

  plugins: [
    new HtmlWebpackPlugin
      template: 'app/index.html'

    new CopyWebpackPlugin [
      from: 'app/assets/images/'
      to:   'images/'
    ,
      from: 'app/assets/icons'
      flatten: true
    , { from: 'app/assets/CNAME' }
    , { from: 'app/assets/manifest.json' }
    , { from: 'app/assets/browserconfig.xml' }
    ]
  ]

# additional webpack settings for local env (when invoked by 'npm start')
if TARGET_ENV is 'development'
  module.exports = merge commonConfig,
    entry: [
      # 'webpack-dev-server/app?http://localhost:7777'
      entryFile
    ]

    devServer:
      contentBase: outputPath
      historyApiFallback: true
      inline: true
      port: 7777

    module:
      rules: [
        test: /\.elm$/
        exclude: [/elm-stuff/, /node_modules/, /app\/Stylesheets\.elm$/]
        use: [
          'elm-hot-loader'
          'elm-webpack-loader?verbose=true&warn=true&debug=true'
        ]
      ,
        test: /app\/Stylesheets\.elm$/
        use: [
          'style-loader'
          'css-loader'
          postcssLoader
          'elm-css-webpack-loader'
        ]
      ,
        test: /\.css$/
        use: [
          'style-loader'
          'css-loader'
          postcssLoader
        ]
      ]

# additional webpack settings for prod env (when invoked via 'npm run build')
if TARGET_ENV is 'production'
  module.exports = merge commonConfig,
    entry: entryFile

    module:
      rules: [
        test:    /\.elm$/
        exclude: [/elm-stuff/, /node_modules/, /app\/Stylesheets\.elm$/]
        use:     'elm-webpack-loader'
      ,
        test: /app\/Stylesheets\.elm$/
        use: ExtractTextPlugin.extract
          fallback: "style-loader"
          use: [
            cssLoader
            postcssLoader
            'elm-css-webpack-loader'
          ]
      ,
        test: /\.css$/
        use: ExtractTextPlugin.extract
          fallback: 'style-loader'
          use: [
            cssLoader
            postcssLoader
          ]
      ]

    plugins: [
      new webpack.optimize.OccurrenceOrderPlugin()

      # extract CSS into a separate file
      new ExtractTextPlugin '[name]-[hash].css'

      new HtmlWebpackPlugin
        template: 'app/index.html'
        # Hack github to serve elm app at all routes.
        filename: '404.html'

      # minify & mangle JS
      new webpack.optimize.UglifyJsPlugin
        minimize:   true
        compressor: warnings: false
        comments: false
    ]
