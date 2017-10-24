-	Download at https://sourceforge.net/projects/rportable/
-	Add to the bottom of R-Portable/App/R-Portable/etc/Rprofile.site:
.First = function(){
    .libPaths(.Library)
}
Note: This will force R-Portable to only use its local library (specified in the hidden global variable .Library) for installing/loading packages.
