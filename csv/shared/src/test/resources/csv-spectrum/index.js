var fs = require('fs')
var path = require('path')
module.exports = function(cb) {
  absoluteFilePaths('csvs', function(err, csvs) {
    if (err) return cb(err)
    absoluteFilePaths('json', function(err, json) {
      var items = []
      for (var i = 0; i < csvs.length; i++) {
        var item = {
          csv: fs.readFileSync(csvs[i].path),
          json: fs.readFileSync(json[i].path),
          name: path.basename(csvs[i].name, path.extname(csvs[i].name))
        }
        items.push(item)
      }
      cb(err, items)
    })
  })
}

function absoluteFilePaths(folder, cb) {
  fs.readdir(path.join(__dirname, folder), function(err, files) {
    if (err) return cb(err)
    cb(null, files.map(function(f) {
      return {
        name: f,
        path: path.join(__dirname, folder, f)
      }
    }))
  })
}