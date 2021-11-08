# rgtmx

## Context

This is a convenience wrapper for the GTmetrix API 2.0.

I whipped up a small library to talk to GTmetrix via R. There's some basic sanity checking baked in, but obviously this is still work in progress and there are (potentially critical) bugs. Feel free to check it out, though. If you need help, take a look at the manuals. A good point to start is `?start_test`. 

If you have feedback and/or points for improvement, feel free to open an issue / PR here on Github. I would love to hear from you!

Roman

## Usage

```
# Install and load library.
devtools::install_github("RomanAbashin/rgtmx")
library(rgtmx)
```
### Start test (and get results)
```
# Minimal example #1.
# Returns the final report after checking test status roughly every 3 seconds. 
result <- start_test("google.com", "[API_KEY]")
```
This will start a test and wait for the report to be generated, returning the result as data.frame. Optionally, you can just simply return the test ID and other meta data via the parameter `wait_for_completion = FALSE`.

```
# Minimal example #2.
# Returns just the test ID and some meta data.
result <- start_test("google.com", "[API_KEY]", wait_for_completion = FALSE)
```

Other optional parameters: `location`,
`browser`,
`report`,
`retention`,
`httpauth_username`,
`httpauth_password`,
`adblock`,
`cookies`,
`video`,
`stop_onload`,
`throttle`,
`allow_url`,
`block_url`,
`dns`,
`simulate_device`,
`user_agent`,
`browser_width`,
`browser_height`,
`browser_dppx`,
`browser_rotate`.

### Show available browsers
```
show_available_browsers("[API_KEY]")
```
### Show available locations
```
show_available_locations("[API_KEY]")
```
### Get specific test
```
get_test("[TEST_ID]", "[API_KEY]")
```
### Get specific report
```
get_report("[REPORT_ID]", "[API_KEY]")
```
### Get all tests
```
get_all_tests("[API_KEY]")
```
### Get account status
```
get_account_status("[API_KEY]")
```
