check_packages = function(names)
{
    for(name in names)
    {
        if (!(name %in% installed.packages(lib="C:/Users/dipesh/Documents/R/win-library/3.2")))
            install.packages(name, repos="http://cran.us.r-project.org", lib="C:/Users/dipesh/Documents/R/win-library/3.2")
    
        library(name, character.only=TRUE, lib.loc = "C:/Users/dipesh/Documents/R/win-library/3.2")
    }
}
