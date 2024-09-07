Forza Horizon 4 Wheelspins
==========================

Wheelspins is Forza Horizon's version of lootboxes.
To the best of my knowledge,
the odds are not published anywhere,
so I simply recorded a bunch of spins.

The data is in [`wheelspin.md`](wheelspin.md).
The file format is described in [`processing.lisp`](processing.lisp),
which also contains tools for parsing and processing the data.

Prior Work
==========

I was able to find a few Reddit posts with similar goals.

- <https://www.reddit.com/r/forza/comments/9lcjlc/> has 140 (presumably regular) early-game wheelspins.
- <https://www.reddit.com/r/forza/comments/9kp0ip/> has another 100.
- <https://www.reddit.com/r/forza/comments/azhf35/> has another 1069.
- <https://www.reddit.com/r/ForzaHorizon/comments/s2xgww> has data for Forza Horizon 5.
    This specific post asks whether skipping the animation affects the distribution
    (compared to letting the wheelspin animation play out fully).
    Answer: no.
- <https://www.reddit.com/r/ForzaHorizon/comments/r155sg> has 407 wheelspins,
    also for Forza Horizon 5.

Analysis
========

I recorded 1809 entries in `wheelspin.md`.
I recorded each entry in the same order in which I spun the wheelspins
(i.e. opened the lootboxes)
This means that,
roughly speaking,
I spun 700 regular wheelspins,
then I supn over 250 super wheelspins
(generating 750 entries),
then finally I spun another 300 regular wheelspins.

How Cosmetics Affect the Prize Distribution
-------------------------------------------

Wheelspins sometimes gives cosmetic character customization options
(clothing and horns).
Each cosmetic can only be unlocked once,
so once a cosmetic is obtained
(whether from wheelspins or otherwise)
it is removed from the pool of possible wheelspin prizes.

As far as I'm aware,
there is only thing that influence the odds in wheelspins in Forza Horizon 4.
But how is the prize distribution shifted when cosmetics are removed from the prize pool?

I analyzed the regular wheelspins separately.
I separated them in groups of 100 wheelspins,
and counted the number of cosmetics, credit rewards, and cars in each group.

![Evolution of the prize type distribution as cosmetics are removed from the prize pool](cosmetics-evolution-type.png)

The number of cars stayed about the same,
but the number of credit rewards increased.
Therefore,
it is clear that cosmetics are simply replaced with credit rewards.

What about the rarities?
The same process gives the following graph.

![Evolution of the prize rarity distribution as cosmetics are removed from the prize pool](cosmetics-evolution-rarity.png)

There seems to be a small shift towards more common prizes,
but given the high overal variance,
this may very well be due to random chance;
i.e. I was just somewhat unlucky on the last 300 wheelspins.

For the average value of the wheelspin outcomes,
the answer is more nuanced.
The chart below splits between the credit rewards and the cars.

![Evolution of the average prize value as cosmetics are removed from the prize pool](cosmetics-evolution-value.png)

The credit rewards sooms to be trending downwards.
This trend is actually slightly masked by two outliers in the 600 and 700 wheelspins marks
(corresponding, respectively,
to wheelspins 501 to 600,
and 601 to 700):
I got one legendary one-million-credits wheelspin in each case.
If I remove the two outliers,
the two marks sitting slightly above 55k drops to 42194 and 40836,
respectively.

For the cars,
the big spike at 800 is also due to an outlier:
the 1939 Mercedes-Benz W154,
which is worth 10 million credits.
Overall,
I rolled six cars worth over one million credits on regular wheelspins:
- regular wheelspin #9, worth 2.1 million credits, the 1965 Shelby Cobra 427 S/C;
- regular wheelspin #112, worth 3.1 million credits, the 1984 Ferrari 288 GTO;
- regular wheelspin #294, worth 1.2 million credits, the 1954 Mercedes-Benz 300 SL Coupé;
- regular wheelspin #590, worth 2.3 million credits, the 2016 Lamborghini Centenario LP 770-4;
- regular wheelspin #791, worth 10 million credits, the 1939 Mercedes-Benz W154; and
- regular wheelspin #928, worth 2.0 million credits, the 1995 Ferrari F50.

Removing the outliers gives the following (combined) graph:

![Evolution of the average prize value as cosmetics are removed from the prize pool](cosmetics-evolution-value-no-outliers.png)

The average value of the credit rewards can be clearly seen to trend downwards.
I'm not sure why this happens.
My conjecture is that,
since I mostly obtained common-rarity cosmetics from those regular wheelspins,
they are replaced mostly with common credit rewards,
who pull the average down.
That,
or cosmetics are simply replaced with low-value credit rewards.

The average value of the cars still has significant variance,
but does not seem to be trending upwards nor downwards.

### More details about the dataset

I already had unlocked a significant fraction of the wheelspin-obtainable cosmetics
before I started recording wheelspins.
I unlocked the remaining ones in the first batch of 700 regular wheelspins,
so I got no cosmetics in the following 250 super wheelspins.
So the distribution of the 250 super wheelspins and the last 300 regular wheelspins stayed constant.
This means that the order between these two batches of wheelspins does not matter,
so I can consider that those 1000 regular wheelspins happened back-to-back.

This does mean that I could not use this dataset
to estimate,
for example,
the probability of getting a cosmetic from a super wheelspin;
hence the restriction to regular wheelspins.
