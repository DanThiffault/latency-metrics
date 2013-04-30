# latency-metrics

A Clojure library designed to parse ping log files and generate metrics.

## Usage

Run the following command to start logging data: 

```ping <gateway-ip-address> | while read pong; do echo "$(date): $pong"; done >> ping.log```

after you've collected data you can process it with the following:

```
lein repl
(use 'latency-metrics.core)

; view a chart of ping requests over 100ms
(view-chart-split input-file-name 100)

; save the same chart
(save-chart-split input-file-name 100 "chart.jpg")

; export metrics to a csv. Make your own charts in Excel
(def plot-data (generate-plot-data (generate-results input-file-name)))
(plot-data-to-csv output-file-name plot-data)
```

## License

Copyright © 2013 119 Labs LLC

Distributed under the Eclipse Public License, the same as Clojure.
