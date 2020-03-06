const path = require('path')

module.exports = {
    mode: "development",
    entry: "./src/Main.purs",
    output: {
        filename: 'app.js',
        path: path.resolve(__filename, 'dist')
    },
    modules: {
        rules: [
            
        ]
    }
}