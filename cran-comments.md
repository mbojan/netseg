## Test environments

- local Ubuntu 18.04.5 LTS install, R 4.0.3
- GitHub Actions (ubuntu 20.04), release
- GitHub Actions (ubuntu 20.04), devel
- GitHub Actions (Mac OS X 10.15.7), release
- GitHub Actions (MS Windows Server 2019), release


## R CMD check results

```
Found the following (possibly) invalid URLs:
     URL: doi:10.1016/j.socnet.2014.04.001
       From: man/freeman.Rd
             man/netseg-package.Rd
       Message: Invalid URI scheme (use \doi for DOIs in Rd markup)
```

```
     URL: http://cranlogs.r-pkg.org/ (moved to
https://cranlogs.r-pkg.org:443/)
       From: README.md
       Status: 200
       Message: OK
```

Please change http --> https, add trailing slashes, or follow moved
content as appropriate.
