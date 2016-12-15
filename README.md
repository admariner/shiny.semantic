
<link href="http://fonts.googleapis.com/css?family=Lato:300,700,300italic|Inconsolata" rel="stylesheet" type="text/css">

<link href='docs/style.css' rel='stylesheet' type='text/css'>

shiny.semantic
==============

Semantic UI wrapper for Shiny

With this library it's easy to wrap Shiny with Semantic UI components. Add few simple CSS classes to your components and achieve amazing boost look of your user interface.

<!-- TODO Filip better example -->
<h2>
Before
</h2>
<!--html_preserve-->
<a>Client's info</a>
<p>
</p>
<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Thu Dec 15 13:07:35 2016 -->
<table class="table shiny-table table- spacing-s" style="width:auto;">
<thead>
<tr>
<th style="text-align: left;">
Name
</th>
<th style="text-align: left;">
City
</th>
<th style="text-align: left;">
Revenue
</th>
</tr>
</thead>
<tbody>
<tr>
<td>
John Smith
</td>
<td>
Warsaw, Poland
</td>
<td>
$210.50
</td>
</tr>
<tr>
<td>
Lindsay More
</td>
<td>
SF, United States
</td>
<td>
$172.78
</td>
</tr>
</tbody>
</table>

<!--/html_preserve-->

<h2>
After
</h2>
<!--html_preserve-->
<body style="min-height: 611px;">
<a class="ui green ribbon label">Client's info</a>
<p>
</p>
<!-- html table generated in R 3.3.2 by xtable 1.8-2 package -->
<!-- Thu Dec 15 13:07:36 2016 -->
<table class="ui very basic collapsing celled table">
<tr>
<th>
Name
</th>
<th>
City
</th>
<th>
Revenue
</th>
</tr>
<tr>
<td>
John Smith
</td>
<td>
Warsaw, Poland
</td>
<td>
$210.50
</td>
</tr>
<tr>
<td>
Lindsay More
</td>
<td>
SF, United States
</td>
<td>
$172.78
</td>
</tr>
</table>

</body>
<!--/html_preserve-->

<!-- #Basic tutorial article is available on [Appsilon Data Science blog](your_future_art_link). -->
<!-- Live demo link below -->
<!-- TODO Analogy to http://shiny.rstudio.com/gallery/widget-gallery.html -->
<p style="text-align: center; font-size: x-large; clear: both">
<a href="http://demo.appsilondatascience.com/shiny.semantic/components">Components live demo</a>
</p>
For better understanding it's good to check [Semantic UI documentation.](http://semantic-ui.com/introduction/getting-started.html)

Source code
-----------

This library source code can be found on [Appsilon Data Science's](http://appsilondatascience.com) Github: <br> <https://github.com/Appsilon/shiny.semantic>

<script>
document.write('<div class="logo"><a href="http://appsilondatascience.com"><img alt="Appsilon Data Science" src="https://cdn.rawgit.com/Appsilon/website-cdn/gh-pages/logo-white.png"/></a></div>')
</script>
How to install?
---------------

**Note! This library is still in its infancy. Api might change in the future.**

At the moment it's possible to install this library through [devtools](https://github.com/hadley/devtools).

    devtools::install_github("Appsilon/shiny.semantic")

To install [previous version]() you can run:

    devtools::install_github("Appsilon/shiny.semantic", ref = "0.1.0")

How to use it?
--------------

TODO Chris - write how to initialse all necessary things in shinyServer and ui. <br> TODO Chris - write about invoking specific components JS, found in Semantic UI documentation.

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png) <!--html_preserve-->
<pre>
<code class="r">div(class = "ui button", uiicon("user"), "Icon button")</code>
</pre>

<!--/html_preserve-->
![](README_files/figure-markdown_github/unnamed-chunk-6-1.png) <!--html_preserve-->
<pre>
<code class="r">div(class = "ui raised segment", div(class = "ui relaxed divided list", c("Apples", 
    "Pears", "Oranges") %&gt;% purrr::map(~div(class = "item", uiicon("large github middle aligned"), 
    div(class = "content", a(class = "header", "Hello"), div(class = "description", 
        .))))))</code>
</pre>

<!--/html_preserve-->
![](README_files/figure-markdown_github/unnamed-chunk-8-1.png) <!--html_preserve-->
<pre>
<code class="r">div(class = "ui card", div(class = "content", div(class = "right floated meta", "14h"), 
    img(class = "ui avatar image", src = "http://semantic-ui.com/images/avatar/large/elliot.jpg"), 
    "Elliot"), div(class = "image", img(src = "http://semantic-ui.com/images/wireframe/image.png")), 
    div(class = "content", span(class = "right floated", uiicon("heart outline like"), 
        "17 likes"), uiicon("comment"), "3 comments"), div(class = "extra content", 
        div(class = "ui large transparent left icon input", uiicon("heart ouline"), 
            tags$input(type = "text", placeholder = "Add Comment..."))))</code>
</pre>

<!--/html_preserve-->
**More examples**

The source code for the live demo you were able to go to in the bigging is located in **/examples** folder. To run it locally you will have to install:

-   [**highlighter**](https://github.com/Appsilon/highlighter)

        devtools::install_github("Appsilon/highlighter")

Check out our dashboard examples:

1.  [Churn analytics](http://demo.appsilondatascience.com/shiny.semantic/churn)
2.  [Fraud detection](demo.appsilondatascience.com/shiny.semantic/frauds)

All dashboards source code can be found in **/demo** folder. You will have to follow the according README files to install all necessary dependencies for those projects.

How to contribute?
------------------

If you want to contribute to this project please submit a regular PR, once you're done with new feature or bug fix.<br>

**Changes in documentation**

Both repository **README.md** file and an official documentation page are generated with Rmarkdown, so if there is a need to update them, please modify accordingly a **README.Rmd** file and run a **build\_readme.R** script to compile it.

Troubleshooting
---------------

We used the latest versions of dependencies for this library, so please update your R environment before installation.

However, if you encounter any problems, try the following:

1.  Up-to-date R language environment
2.  Installing specific dependent libraries versions
    -   shiny

            install.packages("shiny", version='0.14.2.9001')

3.  Some bugs may be realted directly to Semantic UI. In that case please try to check issues on its [repository.](https://github.com/Semantic-Org/Semantic-UI)

Future enhacements
------------------

-   create all update functions for input components to mimic shiny as close as possible
-   add some glue code in dsl.R to make using this package smoother
-   CRAN release

Appsilon Data Science
=====================

<script>
document.write('<div class="subheader"> We Provide End-to-End Data Science Solutions </div>  <div class="logo"><a href="http://appsilondatascience.com"><img alt="Appsilon Data Science" src="https://cdn.rawgit.com/Appsilon/website-cdn/gh-pages/logo-white.png" /></a></div>');
</script>
Get in touch [dev@appsilondatascience.com](dev@appsilondatascience.com)

<script>
document.write('<a href="https://github.com/Appsilon/shiny.semantic"><img style="position: absolute; margin: 0; top: 0; right: 0; border: 0;" src="https://camo.githubusercontent.com/38ef81f8aca64bb9a64448d0d70f1308ef5341ab/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f6461726b626c75655f3132313632312e706e67" alt="Fork me on GitHub" data-canonical-src="https://s3.amazonaws.com/github/ribbons/forkme_right_darkblue_121621.png"></a>')
</script>
