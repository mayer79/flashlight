# mrIML

<details>

* Version: 2.1.0
* GitHub: https://github.com/nickfountainjones/mrIML
* Source code: https://github.com/cran/mrIML
* Date/Publication: 2025-07-28 18:40:02 UTC
* Number of recursive dependencies: 219

Run `revdepcheck::revdep_details(, "mrIML")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in 'mrIML-Ex.R' failed
    The error most likely occurred in:
    
    > ### Name: mrCovar
    > ### Title: Investigate partial dependencies of a covariate for mrIML JSDMs
    > ###   (Joint Species Distribution Models)
    > ### Aliases: mrCovar
    > 
    > ### ** Examples
    > 
    ...
        ▆
     1. ├─mrIML_rf %>% mrCovar(var = "scale.prop.zos", sdthresh = 0.05)
     2. ├─mrIML::mrCovar(., var = "scale.prop.zos", sdthresh = 0.05)
     3. │ └─... %>% ...
     4. ├─dplyr::mutate(...)
     5. ├─dplyr::group_by(., .data$label)
     6. └─dplyr:::group_by.data.frame(., .data$label)
     7.   └─dplyr::group_by_prepare(.data, ..., .add = .add, error_call = current_env())
     8.     └─rlang::abort(bullets, call = error_call)
    Execution halted
    ```

*   checking running R code from vignettes ...
    ```
      'mrIML.Rmd' using 'UTF-8'... failed
     ERROR
    Errors in running code in vignettes:
    when running code in 'mrIML.Rmd'
      ...
    +     v = "scale.prop.zos") %>% plot() + ggtitle("Effect of scale.prop.zos on Microfilaria") + 
    + .... [TRUNCATED] 
    
    > PD_scale.prop.zos_rf <- mrIML_rf %>% mrCovar(var = "scale.prop.zos", 
    +     sdthresh = 0)
    
      When sourcing 'mrIML.R':
    Error: Must group by variables found in `.data`.
    ✖ Column `label` is not found.
    Execution halted
    ```

