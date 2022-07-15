bunのパースバグっぽいのでしばらく様子見

```
~/git2/langs-bench-dijkstra/javascript-bun% bun run src/main.js                                                     (git)[master]
19 |     commenting: false,
20 |     // Current error encountered by a record
21 |     error: undefined,
22 |     enabled: options.from_line === 1,
23 |     escaping: false,
24 |     escapeIsQuote: Buffer.isBuffer(options.escape) && Buffer.isBuffer(options.quote) && Buffer.compare(options.escape, options.quote) === 0,
                                                                                            ^
 TypeError: Expected number
      at /home/reki/git2/langs-bench-dijkstra/javascript-bun/node_modules/csv-parse/lib/api/init_state.js:24:88
      at /home/reki/git2/langs-bench-dijkstra/javascript-bun/node_modules/csv-parse/lib/api/index.js:40:11
      at /home/reki/git2/langs-bench-dijkstra/javascript-bun/node_modules/csv-parse/lib/sync.js:9:17
      at load (/home/reki/git2/langs-bench-dijkstra/javascript-bun/src/main.js:64:11)
      at main (/home/reki/git2/langs-bench-dijkstra/javascript-bun/src/main.js:123:1)
      at /home/reki/git2/langs-bench-dijkstra/javascript-bun/src/main.js:140:0
```
