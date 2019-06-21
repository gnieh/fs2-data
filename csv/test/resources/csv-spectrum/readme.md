# csv-spectrum

[![NPM](https://nodei.co/npm/csv-spectrum.png)](https://nodei.co/npm/csv-spectrum/)

A bunch of different CSV files to serve as an acid test for CSV parsing libraries. There are also JSON versions of the CSVs for verification purposes.

The goal of this repository is to capture test cases to represent the entire CSV spectrum. 

Please use these in your test suites and send contributions in the form of more/improved test cases.

It is also a node module that you can require() in your tests

Some CSVs here were included from [csvkit](https://github.com/onyxfish/csvkit/tree/master/examples)

https://github.com/maxogden/binary-csv uses csv-spectrum and passes all tests

### programmatic usage

```js
vsr spectrum = require('csv-spectrum')
spectrum(function(err, data) {
  // data is an array of objects has all the csv and json versions of the tests
})
```

`data` looks like this:

```
[ { csv: <Buffer 66 69 72 73 74 2c 6c 61 73 74 2c 61 64 64 72 65 73 73 2c 63 69 74 79 2c 7a 69 70 0a 4a 6f 68 6e 2c 44 6f 65 2c 31 32 30 20 61 6e 79 20 73 74 2e 2c 22 41 ...>,
    json: <Buffer 5b 0a 20 20 7b 0a 20 20 20 20 22 66 69 72 73 74 22 3a 20 22 4a 6f 68 6e 22 2c 0a 20 20 20 20 22 6c 61 73 74 22 3a 20 22 44 6f 65 22 2c 0a 20 20 20 20 22 ...>,
    name: 'comma_in_quotes' },
  { csv: <Buffer 61 2c 62 0a 31 2c 22 68 61 20 22 22 68 61 22 22 20 68 61 22 0a 33 2c 34 0a>,
    json: <Buffer 5b 0a 20 20 7b 0a 20 20 20 20 22 61 22 3a 20 22 31 22 2c 0a 20 20 20 20 22 62 22 3a 20 22 68 61 20 5c 22 68 61 5c 22 20 68 61 22 0a 20 20 7d 2c 0a 20 20 ...>,
    name: 'escaped_quotes' }
  // etc
]
```

example usage in a test might be:

```
vsr spectrum = require('csv-spectrum')
spectrum(function(err, data) {
  console.log('testing ' + data[0].name)
  t.equal(csv2json(data[0].csv), JSON.parse(data[0].json))
})
```
