Breakdown of most common section clicked on per visit on the Wikipedia Portal
=======

In [the clickthroughs tab](http://discovery.wmflabs.org/portal/#clickthrough_rate) we look at the actual rate of clickthroughs and in the [action breakdown tab](http://discovery.wmflabs.org/portal/#action_breakdown) we can see that clickthrough broken down into the "last action taken".
*This* graph shows the most common clicked section per visit. Our analyses showed that any one session can have multiple visits and each visit can have multiple clicks associated with it. This shows us (for each section) the proportion of visits in which that section was the most commonly clicked on. The options are:

1. **No action**: Not taking any action on the page.
2. **Other projects**: clicking the links to Wiktionary, Wikisource, etc. on the bottom of the page.
3. **Primary links**: the prominent links to languages around the globe.
4. **Search**: using the Search box and button.
5. **Secondary links**: the less-prominent plaintext links to languages.

The values are expressed as percentages - so if a feature has the value "61.3" it is being used 61.3% of the time.

Outages and notes
-------

* '__A__' (2016-09-13): Added event logging of language-switching, causing some events to flow into old table and some events to flow into the new table. See [T143149](https://phabricator.wikimedia.org/T143149) for more details.
* '__R__': on 2017-01-01 we started calculating all of Discovery's metrics using a new version of [our data retrieval and processing codebase](https://phabricator.wikimedia.org/diffusion/WDGO/) that we migrated to [Wikimedia Analytics](https://www.mediawiki.org/wiki/Analytics)' [Reportupdater infrastructure](https://wikitech.wikimedia.org/wiki/Analytics/Reportupdater). See [T150915](https://phabricator.wikimedia.org/T150915) for more details.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question) or [Chelsy](mailto:cxie@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small;">
  <strong>Link to this dashboard:</strong> <a href="https://discovery.wmflabs.org/portal/#most_common">https://discovery.wmflabs.org/portal/#most_common</a>
  | Page is available under <a href="https://creativecommons.org/licenses/by-sa/3.0/" title="Creative Commons Attribution-ShareAlike License">CC-BY-SA 3.0</a>
  | <a href="https://phabricator.wikimedia.org/diffusion/WDPR/" title="Wikipedia.org Portal Dashboard source code repository">Code</a> is licensed under <a href="https://phabricator.wikimedia.org/diffusion/WDPR/browse/master/LICENSE.md" title="MIT License">MIT</a>
  | Part of <a href="https://discovery.wmflabs.org/">Discovery Dashboards</a>
</p>
