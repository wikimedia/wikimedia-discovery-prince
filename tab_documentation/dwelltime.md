Dwell-time on the Wikipedia portal
=======

This measures "dwell-time" on the Wikipedia portal: the amount of time people spend between arriving at the page and leaving. As "leaving" is defined as clicking on something for data collection purposes, this only covers the population that actually clicks through to pages or using search, not the population that simply closes their tab or browser, or goes to a different website altogether.

Outages and inaccuracies
------

* '__A__' (2015-12-07): The sampling changed to exclude a broader range of browsers, resulting in alterations to things like clickthrough rate and dwell time. We expect this to resolve itself on 4 January when a new schema version is launched.
* '__B__' (2016-09-13): Added event logging of language-switching, causing some events to flow into old table and some events to flow into the new table. See [T143149](https://phabricator.wikimedia.org/T143149) for more details.
* '__R__': on 2017-01-01 we started calculating all of Discovery's metrics using a new version of [our data retrieval and processing codebase](https://phabricator.wikimedia.org/diffusion/WDGO/) that we migrated to [Wikimedia Analytics](https://www.mediawiki.org/wiki/Analytics)' [Reportupdater infrastructure](https://wikitech.wikimedia.org/wiki/Analytics/Reportupdater). See [T150915](https://phabricator.wikimedia.org/T150915) for more details.

Broadly-speaking, it's worth noting that (as with all data based on JavaScript logging) the code that gathers this information requires a certain amount of browser capabilities to function. It's probably not going to work on 10 year old Nokia brick phones, and so the data will be biased against users using those kinds of devices.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small; color: gray;">
  <strong>Link to this dashboard:</strong>
  <a href="http://discovery.wmflabs.org/portal/#dwell_data">
    http://discovery.wmflabs.org/portal/#dwell_data
  </a>
</p>
