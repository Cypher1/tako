cargo test -- --test-threads 1 --report-time -Z unstable-options | sed "s/\(.*\)<\(.*\)>/\2  \1/" | sort -n
