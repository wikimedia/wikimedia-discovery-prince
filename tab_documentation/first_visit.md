Breakdown of Wikipedia Portal visitors' first visit
=======

In [the clickthroughs tab](http://discovery.wmflabs.org/portal/#clickthrough_rate) we look at the actual rate of clickthroughs. In the [actions breakdown tab](http://discovery.wmflabs.org/portal/#breakdown) we look at the proportions of sessions performing various types of actions. *This* graph shows the actions breakdown at first visit and ignores any subsequent visits the user may have made. The options are:

1. **Language search**: the "find a language" box.
2. **No action**: Not taking any action on the page.
3. **Other projects**: clicking the links to Wiktionary, Wikisource, etc. on the bottom of the page.
4. **Primary links**: the prominent links to languages around the globe.
5. **Search**: using the Search box and button.
6. **Other languages**: using the "Other languages" links near the bottom of the page which take you to a list of Wikipedias.
7. **Secondary links**: the less-prominent plaintext links to languages.

The values are expressed as percentages - so if a feature has the value "61.3" it is being used 61.3% of the time.

In the future, we might include data such as the average time to first clickthrough from initial page landing.

Outages and notes
-------

* '__A__' (2016-09-13): Added event logging of language-switching, causing some events to flow into old table and some events to flow into the new table. See [T143149](https://phabricator.wikimedia.org/T143149) for more details.
* '__R__': on 2017-01-01 we started calculating all of Discovery's metrics using a new version of [our data retrieval and processing codebase](https://phabricator.wikimedia.org/diffusion/WDGO/) that we migrated to [Wikimedia Analytics](https://www.mediawiki.org/wiki/Analytics)' [Reportupdater infrastructure](https://wikitech.wikimedia.org/wiki/Analytics/Reportupdater). See [T150915](https://phabricator.wikimedia.org/T150915) for more details.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question) or [Chelsy](mailto:cxie@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small;">
  <strong>Link to this dashboard:</strong> <a href="https://discovery.wmflabs.org/portal/#first_visit">https://discovery.wmflabs.org/portal/#first_visit</a>
  | Page is available under <a href="https://creativecommons.org/licenses/by-sa/3.0/" title="Creative Commons Attribution-ShareAlike License">CC-BY-SA 3.0</a>
  | <a href="https://phabricator.wikimedia.org/diffusion/WDPR/" title="Wikipedia.org Portal Dashboard source code repository">Code</a> is licensed under <a href="https://phabricator.wikimedia.org/diffusion/WDPR/browse/master/LICENSE.md" title="MIT License">MIT</a>
  | Part of <a href="https://discovery.wmflabs.org/">Discovery Dashboards</a>
</p>
