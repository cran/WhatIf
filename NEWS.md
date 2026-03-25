All changes to WhatIf are documented here. GitHub issue numbers are given after
each change note when relevant. See <https://github.com/IQSS/WhatIf/issues>.
External contributors are referenced with their GitHub usernames when
applicable.

# WhatIf 1.5-11

## Changes

-   Replaced deprecated `with_mock()` usage in tests with modern testthat
    approach, fixing CRAN check failures.

-   Added structured `Authors@R` field to DESCRIPTION.

-   Updated minimum R version to 3.5.0.

-   New maintainer: Katalina Toth.

# WhatIf 1.5-9

## Major changes

-   Convex hull test now run in parallel. The number of cores can be
    specified in the `whatif` call with the `mc.cores` argument.

## Minor changes

-   Additional tests added.

-   Progress bar added to `whatif` convex hull test to indicate progress.

-   Travis CI added for continuous integration testing.

# WhatIf 1.5-8

## Major changes

-   Returns Zelig integration.
