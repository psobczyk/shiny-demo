
# Shiny app example


## How to run inside a docker container?

### Building

```bash
docker build -t shiny-demo .
```

### Running

```bash
docker run -p 3838:3838 shiny-demo
```


## Adding unit tests

Use the packages `usethis`.

```bash
usethis::use_testthat()  # just once
usethis::use_test("TestName")
```
