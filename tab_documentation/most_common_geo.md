Breakdown of Most Common Section Clicked on per Visit on the Wikipedia Portal by Geography
=======

This dashboard shows the most common clicked section per visit. Our analyses showed that any one session can have multiple visits and each visit can have multiple clicks associated with it. This shows us (for each section) the number/proportion of visits in which that section was the most commonly clicked on. The sections are:

1. **No action**: Not taking any action on the page.
2. **Other projects**: clicking the links to Wiktionary, Wikisource, etc. on the bottom of the page.
3. **Primary links**: the prominent links to languages around the globe.
4. **Search**: using the Search box and button.
5. **Other languages**: using the "Other languages" links near the bottom of the page which take you to a list of Wikipedias.
6. **Secondary links**: the less-prominent plaintext links to languages.

The values are expressed as numbers (sampled at a 1:200 rate) and as percentages - so if a country has the value "61.3" for search section, then 61.3% of all visits (in which search section was the most commonly clicked on) are from users in this country.

Notes
------

* This dashboard is aim to expand the "other" countries display in the current [Portal Dashboard](http://discovery.wmflabs.org/portal/#country_breakdown) to reflect more data that we're interested in seeing. See [T138107](https://phabricator.wikimedia.org/T138107) for more details.
* Broadly-speaking, it's worth noting that (as with all data based on JavaScript logging) the code that gathers this information requires a certain amount of browser capabilities to function. It's probably not going to work on 10 year old Nokia brick phones, and so the data will be biased against users using those kinds of devices.
* On 28 June 2016 our Event Logging system started recording a finer view of U.S. traffic, breaking it down into 5 regions:
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
    - **Other**
        - American Samoa, Guam, Northern Mariana Islands, Puerto Rico and Virgin Islands, U.S.

Outages and inaccuracies
------

- 13 September 2016: Added event logging of language-switching, causing some events to flow into old table and some events to flow into the new table. See [T143149](https://phabricator.wikimedia.org/T143149) for more details.

Questions, bug reports, and feature suggestions
------
For technical, non-bug questions, [email Mikhail](mailto:mpopov@wikimedia.org?subject=Dashboard%20Question) or [Chelsy](mailto:cxie@wikimedia.org?subject=Dashboard%20Question). If you experience a bug or notice something wrong or have a suggestion, [open a ticket in Phabricator](https://phabricator.wikimedia.org/maniphest/task/create/?projects=Discovery) in the Discovery board or [email Deb](mailto:deb@wikimedia.org?subject=Dashboard%20Question).

<hr style="border-color: gray;">
<p style="font-size: small;">
  <strong>Link to this dashboard:</strong> <a href="https://discovery.wmflabs.org/portal/#most_common_by_country">https://discovery.wmflabs.org/portal/#most_common_by_country</a>
  | Page is available under <a href="https://creativecommons.org/licenses/by-sa/3.0/" title="Creative Commons Attribution-ShareAlike License">CC-BY-SA 3.0</a>
  | <a href="https://phabricator.wikimedia.org/diffusion/WDPR/" title="Wikipedia.org Portal Dashboard source code repository">Code</a> is licensed under <a href="https://phabricator.wikimedia.org/diffusion/WDPR/browse/master/LICENSE.md" title="MIT License">MIT</a>
  | Part of <a href="https://discovery.wmflabs.org/">Discovery Dashboards</a>
</p>
