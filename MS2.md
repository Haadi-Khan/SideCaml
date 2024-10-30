# MS 2 Progress

Our final project generates upvote-optimized posts for the Cornell Sidechat. In order to generate the text for posts, we need to wrangle data from sidechat. Thankfully, we can retrieve data from sidechat using the curl command:
```bash
curl -H 'accept: /' -H 'content-type: application/json' -H 'authorization: bearer eyJhbGciOiJIUzI1NiJ9.OTA3NjRhMDEtZThhYy00NjJlLWE1OWItZmJmMTQ4ZWM1YmZk.2rFdh885edRFF06kRuqsMNo6ms4i743FMRwWuztg32M' -H 'app-version: 5.4.15' -H 'accept-language: en-US;q=1.0, ar-US;q=0.9, zh-Hans-US;q=0.8, es-US;q=0.7, ja-US;q=0.6' -H 'user-agent: sidechat/5.4.15 (com.flowerave.sidechat; build:2; iOS 18.0.1) Alamofire/5.9.1' --compressed -X POST https://api.sidechat.lol/v1/posts/home -d '{"school_group_id":"73466f01-1f5c-4163-b8f7-c96292eeec67","type":"recent"}'
```

Each curl yields ~20 posts. The ending json can be modified with a "cursor" field, yielded by the output of the prior curl command. As an example, it would look like:
```bash
curl -H 'accept: /' -H 'content-type: application/json' -H 'authorization: bearer eyJhbGciOiJIUzI1NiJ9.OTA3NjRhMDEtZThhYy00NjJlLWE1OWItZmJmMTQ4ZWM1YmZk.2rFdh885edRFF06kRuqsMNo6ms4i743FMRwWuztg32M' -H 'app-version: 5.4.15' -H 'accept-language: en-US;q=1.0, ar-US;q=0.9, zh-Hans-US;q=0.8, es-US;q=0.7, ja-US;q=0.6' -H 'user-agent: sidechat/5.4.15 (com.flowerave.sidechat; build:2; iOS 18.0.1) Alamofire/5.9.1' --compressed -X POST https://api.sidechat.lol/v1/posts/home -d '{"school_group_id":"73466f01-1f5c-4163-b8f7-c96292eeec67","type":"recent", "cursor":"persisted~8672cdf2-3dcf-487b-962b-c913685f61b1"}'
```

Recursively calling this yields ~20x our call amount.

The (distilled) output is structured as:
```json
{
    "feed_items": [
        {
            "post": {
                "text": "",
                "vote_total": 0,
                "comment_count": 0,
                "time_created": ""
            /* ... */
            }
            /* ... */
        }
    ],
    "cursor": ""
}
```

For the purposes of training our model, we will focus on text and vote total, but we may utilize comment count as well in future iterations. The remainder of the metadata can be disregarded.

Our wrangling effort will grab the three highlighted fields from the json and build a CSV table for data storage. We created a collection of ~87k posts.

Our checkmark has a driver program to generate a CSV with a prompt asking for the total # of API requests desired (yields a CSV with 20 * # + 1 rows). After this, the program prints out a random post from our collection.