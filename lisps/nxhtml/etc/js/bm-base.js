// This is the bookmarklet template for the bookmarklet framework.
// The js file template is in bm-base-file.js
//
// To make code for a bookmarklet fill in the values below.
// Then remove comments and new lines. This can be done for example here:
//
//   http://closure-compiler.appspot.com/home
// 
// (You can do all this with the command jsut-mk-bookmarklet in the
// Emacs elips package nXhtml.)
////////////////////////////////////////////////////////////////////

(function(){

    ////////////////////////////////////////////////////////////////////
    /// Your values (you must use '' instead of ""):
    var myURL='URL';
    var myArgs=null;
    var myNamespace = 'NAMESPACE'; // For this bookmarklet, same as in .js file!
    ////////////////////////////////////////////////////////////////////

    window[myNamespace+'-myArgs'] = myArgs;

    // Has the .js file already been loaded?
    if (window[myNamespace])
        window[myNamespace]();
    else {
        var elt=document.getElementsByTagName('head')[0]
            || document.getElementsByTagName('body')[0];
        if (elt) {
            var script=document.createElement('script');
            script.type='text/javascript';
            script.src=myURL;
            elt.appendChild(script);
        } else
            alert('You must be on a html page for this to work');
        /* testing a
           comment */
    }
})();
// void(0)
