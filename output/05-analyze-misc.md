There's a lot to analyze with the Texas high school academic UIL data
set. Maybe I find it more interesting than others due to my personal
experiences with these competitions.

Now, after examining some of the biggest topics associated with this
data--including competitions, individuals, and schools--in a broad
manner, there are some other things that don't necessarily fall into
these categories that I think are worth investigating.

Siblings
--------

Let's look at the performance of siblings. Maybe this topic only came to
mind for me because I have brothers, on of who is my twin, but I think
anyone can find something interesting on this matter.

### Sibling Participation

So, let's start with something easy--which siblings competed together
the most?

<include> As in the analysis of individuals and schools, I'll include
myself and my school in the tables and figures where applicable.
</include>

<table>
<thead>
<tr class="header">
<th align="right">rnk</th>
<th align="left">name_last</th>
<th align="left">name_first_pair</th>
<th align="right">n</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">Zhang</td>
<td align="left">Jim &amp; Mark</td>
<td align="right">24</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">Ballard</td>
<td align="left">Chance &amp; Rance</td>
<td align="right">22</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">Garcia</td>
<td align="left">Javier &amp; Juan</td>
<td align="right">20</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">Walter</td>
<td align="left">Collin &amp; Lowell</td>
<td align="right">20</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">Bass</td>
<td align="left">Brian &amp; Michael</td>
<td align="right">19</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">Fabre</td>
<td align="left">Guadalupe &amp; Maria</td>
<td align="right">19</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">Priest</td>
<td align="left">Alex &amp; Chandler</td>
<td align="right">18</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">Vicuna</td>
<td align="left">Bianca &amp; Daniel</td>
<td align="right">17</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">Gee</td>
<td align="left">Grace &amp; John</td>
<td align="right">16</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">Morris</td>
<td align="left">Jason &amp; Ty</td>
<td align="right">16</td>
</tr>
</tbody>
</table>

Admittedly, I am a bit disappointed to find that my twin brother and I
are not at the very top of this list. Nonetheless, we are fairly near
the top, so I can take some satisfaction in that. [1]

I should note that the scraped data does not distinguish siblings, so I
had to define criteria to do so. To be specific, the table above
enforces the criteria that two people have the same last name, school,
and city, and that they compete in the exact same competition--that is,
a competition occurring in a given year and being of a same competition
type and same competition level (as well as the same conference and
competition area, if applicable). The numbers are inflated when not
enforcing the criteria that the two people must have competed in the
same competition type and level (nor conference and competition area),
and even more so when throwing out the criteria for same year.

### Sibling Performance

Participation in competitions is one thing, but what about sibling
performance? Let's use the same metric used elsewhere for ranking
performance--percent rank of scores summed across all records
(`prnk_sum`)--and see which sibling pairs show up among the top.

<table>
<thead>
<tr class="header">
<th align="right">rnk</th>
<th align="left">name_last</th>
<th align="left">name_first_pair</th>
<th align="left">n_bycomp</th>
<th align="left">n_defeat</th>
<th align="left">n_state</th>
<th align="right">prnk</th>
<th align="right">rnk_max</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">Priest</td>
<td align="left">Alex &amp; Chandler</td>
<td align="left">1,222</td>
<td align="left">1,022</td>
<td align="left">11</td>
<td align="right">31.73</td>
<td align="right">72</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">Fabre</td>
<td align="left">Guadalupe &amp; Maria</td>
<td align="left">1,348</td>
<td align="left">1,078</td>
<td align="left">14</td>
<td align="right">30.31</td>
<td align="right">76</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">Walter</td>
<td align="left">Collin &amp; Lowell</td>
<td align="left">1,074</td>
<td align="left">768</td>
<td align="left">16</td>
<td align="right">29.99</td>
<td align="right">80</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">Bass</td>
<td align="left">Brian &amp; Michael</td>
<td align="left">1,138</td>
<td align="left">889</td>
<td align="left">12</td>
<td align="right">29.62</td>
<td align="right">76</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">Gee</td>
<td align="left">Grace &amp; John</td>
<td align="left">896</td>
<td align="left">711</td>
<td align="left">10</td>
<td align="right">26.28</td>
<td align="right">64</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">Morris</td>
<td align="left">Jason &amp; Ty</td>
<td align="left">852</td>
<td align="left">625</td>
<td align="left">11</td>
<td align="right">24.13</td>
<td align="right">64</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">Patterson</td>
<td align="left">Ben &amp; Jeremy</td>
<td align="left">994</td>
<td align="left">708</td>
<td align="left">9</td>
<td align="right">22.30</td>
<td align="right">62</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">Alsup</td>
<td align="left">Jon &amp; Mason</td>
<td align="left">886</td>
<td align="left">667</td>
<td align="left">9</td>
<td align="right">22.18</td>
<td align="right">56</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">Vicuna</td>
<td align="left">Bianca &amp; Daniel</td>
<td align="left">1,056</td>
<td align="left">653</td>
<td align="left">3</td>
<td align="right">21.39</td>
<td align="right">68</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">Beavers</td>
<td align="left">Clay &amp; Cody</td>
<td align="left">902</td>
<td align="left">696</td>
<td align="left">8</td>
<td align="right">20.71</td>
<td align="right">52</td>
</tr>
</tbody>
</table>

It looks like the pairs at the top of these rankings based on score are
fairly similar to the list of pairs competing most frequently. (This is
not too surprising given that my choice of metric of ranking is based on
a summed value that "rewards" volume of participation rather than
per-competition performance.) Again, my twin brother and I appear near
the top.

My High School
--------------

Even though I highlighted my high school ("CLEMENS") in my examination
of schools and looked at individual scores elsewhere, I did not look at
other individuals that have gone to my school. Perhaps it is a bit
egotistical, but I am interested in knowing how I compare with others
that have attended my school (either before, with, or after me).

<table>
<thead>
<tr class="header">
<th align="right">rnk</th>
<th align="left">name</th>
<th align="right">n</th>
<th align="right">prnk_sum</th>
<th align="right">prnk_mean</th>
<th align="right">n_defeat_sum</th>
<th align="right">n_defeat_mean</th>
<th align="right">n_advanced_sum</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="right">1</td>
<td align="left">Land, Noah</td>
<td align="right">17</td>
<td align="right">13.67</td>
<td align="right">0.80</td>
<td align="right">447</td>
<td align="right">26.29</td>
<td align="right">14</td>
</tr>
<tr class="even">
<td align="right">2</td>
<td align="left">Fulton, Chris</td>
<td align="right">18</td>
<td align="right">12.66</td>
<td align="right">0.70</td>
<td align="right">351</td>
<td align="right">19.50</td>
<td align="right">16</td>
</tr>
<tr class="odd">
<td align="right">3</td>
<td align="left">Gonzales, Gavyn</td>
<td align="right">17</td>
<td align="right">12.26</td>
<td align="right">0.72</td>
<td align="right">371</td>
<td align="right">21.82</td>
<td align="right">15</td>
</tr>
<tr class="even">
<td align="right">4</td>
<td align="left">Elhabr, Andrew</td>
<td align="right">15</td>
<td align="right">9.87</td>
<td align="right">0.66</td>
<td align="right">296</td>
<td align="right">19.73</td>
<td align="right">11</td>
</tr>
<tr class="odd">
<td align="right">5</td>
<td align="left">Perry, Robert</td>
<td align="right">15</td>
<td align="right">8.75</td>
<td align="right">0.58</td>
<td align="right">249</td>
<td align="right">16.60</td>
<td align="right">10</td>
</tr>
<tr class="even">
<td align="right">6</td>
<td align="left">Garcia, Jon</td>
<td align="right">9</td>
<td align="right">7.94</td>
<td align="right">0.88</td>
<td align="right">259</td>
<td align="right">28.78</td>
<td align="right">6</td>
</tr>
<tr class="odd">
<td align="right">7</td>
<td align="left">Nesser, Austin</td>
<td align="right">17</td>
<td align="right">7.93</td>
<td align="right">0.47</td>
<td align="right">231</td>
<td align="right">13.59</td>
<td align="right">15</td>
</tr>
<tr class="even">
<td align="right">8</td>
<td align="left">Elhabr, Anthony</td>
<td align="right">13</td>
<td align="right">7.76</td>
<td align="right">0.60</td>
<td align="right">216</td>
<td align="right">16.62</td>
<td align="right">10</td>
</tr>
<tr class="odd">
<td align="right">9</td>
<td align="left">Guyott, David</td>
<td align="right">9</td>
<td align="right">5.37</td>
<td align="right">0.60</td>
<td align="right">157</td>
<td align="right">17.44</td>
<td align="right">7</td>
</tr>
<tr class="even">
<td align="right">10</td>
<td align="left">Baker, Ian</td>
<td align="right">10</td>
<td align="right">5.32</td>
<td align="right">0.53</td>
<td align="right">185</td>
<td align="right">18.50</td>
<td align="right">8</td>
</tr>
</tbody>
</table>

Alas, although my twin brother and I did not rank among the very top of
the siblings by participation and performance, we do appear among the
top when evaluating only people from my high school. In my opinion, the
sample size isn't so small that this achievement is trivial.

Wrap-Up
-------

I think all I've done here is more investigation of my personal
performance, so I'll spare the reader any more of my egotistical
exporation. And, with that said, I think this is a good point to bring
an end to my investigation of Texas high school academic UIL
competitions.

[1] I don't explicitly try to filter for twins only, but it's reasonable
to believe that many, if not most, are twins.
