module Bio.View exposing (template)

-- Libraries

import Html exposing (Html)
import Markdown exposing (toHtml)


template : List (Html a)
template =
    toHtml Nothing """
It started on a whim one evening in 2014 when Austin, almost jokingly, turns to me and asks, “wanna write a song?” I kinda laughed, but we began writing and in no time we had a (really) rough draft of our first song. For the next couple months we wrote almost every night and some of the songs from that batch are still some of our favorites. At the time, we didn’t really know what we were doing. We just wrote songs and had a blast building our relationship. Then one night in December, I just looked at Austin and said, “we should play an open mic tonight.” That led us to booking more shows, and just having fun together playing around Nashville.

What are we up to now? Well, Em pursues her passion for songwriting, so she spends most of her time writing in Nashville. Caldwell is an outlet for Austin to hone his skills in production and performance. It’s the songs that we write that we want to share with the world. There’s a song about when we broke up that one time, and songs about falling in love, and songs that make us feel at home.

This year, we’re working on getting together a band and putting out an EP. So come hang out with us at one of our shows or in our living room. I love holiday parties and Austin will make you a mean cup of coffee.

P.S.

For everyone who has followed us while we went by A & Em, we decided to change our name to Caldwell because it’s where we began; it’s our roots. We met in the hallway on a snowy December morning on Caldwell Avenue. That’s a time we’ll never forget. We always want our music to be organic. To be about our relationship and life experiences.
"""
