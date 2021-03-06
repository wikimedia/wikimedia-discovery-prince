Where users come from to the Wikipedia portal
=======

This measures where portal users come from each day, displaying the proportion of users from each top-10 country and the proportion from other countries, as a percentage.

Notes
------

* Broadly-speaking, it's worth noting that (as with all data based on JavaScript logging) the code that gathers this information requires a certain amount of browser capabilities to function. It's probably not going to work on 10 year old Nokia brick phones, and so the data will be biased against users using those kinds of devices.
* '__A__': on 2016-06-28 our Event Logging system started recording a finer view of U.S. traffic, breaking it down into 5 regions:
    - **Northeast Region**
        - New England Division: Connecticut, Maine, Massachusetts, New Hampshire, Rhode Island and Vermont
        - Middle Atlantic Division: New Jersey, New York and Pennsylvania
    - **Midwest Region**
        - East North Central Division: Illinois, Indiana, Michigan, Ohio and Wisconsin
        - West North Central Division: Iowa, Kansas, Minnesota, Missouri, Nebraska, North Dakota and South Dakota
    - **South Region**
        - South Atlantic Division: Delaware, District of Columbia, Florida, Georgia, Maryland, North Carolina, South Carolina, Virginia and West Virginia
        - East South Central Division: Alabama, Kentucky, Mississippi and Tennessee
        - West South Central Division: Arkansas, Louisiana, Oklahoma and Texas
    - **West Region**
        - Mountain Division: Arizona, Colorado, Idaho, Montana, Nevada, New Mexico, Utah and Wyoming
    - **Pacific Region**
        - Alaska, California, Hawaii, Oregon and Washington
* '__B__' (2016-09-13): Added event logging of language-switching, causing some events to flow into old table and some events to flow into the new table. See [T143149](https://phabricator.wikimedia.org/T143149) for more details.
* '__R__': on 2017-01-01 we started calculating all of Discovery's metrics using a new version of [our data retrieval and processing codebase](https://phabricator.wikimedia.org/diffusion/WDGO/) that we migrated to [Wikimedia Analytics](https://www.mediawiki.org/wiki/Analytics)' [Reportupdater infrastructure](https://wikitech.wikimedia.org/wiki/Analytics/Reportupdater). See [T150915](https://phabricator.wikimedia.org/T150915) for more details.

See [T136257](https://phabricator.wikimedia.org/T136257) for more details.

Outages and inaccuracies
------

- **A** (13 September 2016): Added event logging of language-switching, causing some events to flow into old table and some events to flow into the new table. See [T143149](https://phabricator.wikimedia.org/T143149) for more details.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question) or [Chelsy](mailto:cxie@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small;">
  <strong>Link to this dashboard:</strong> <a href="https://discovery.wmflabs.org/portal/#country_breakdown">https://discovery.wmflabs.org/portal/#country_breakdown</a>
  | Page is available under <a href="https://creativecommons.org/licenses/by-sa/3.0/" title="Creative Commons Attribution-ShareAlike License">CC-BY-SA 3.0</a>
  | <a href="https://phabricator.wikimedia.org/diffusion/WDPR/" title="Wikipedia.org Portal Dashboard source code repository">Code</a> is licensed under <a href="https://phabricator.wikimedia.org/diffusion/WDPR/browse/master/LICENSE.md" title="MIT License">MIT</a>
  | Part of <a href="https://discovery.wmflabs.org/">Discovery Dashboards</a>
</p>
