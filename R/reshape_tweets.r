#' reshape_tweets
#'
#' Reshape twitter data for coordination detection
#' 
#' @param tweets a pre-processed list of twitter data (output of preprocess_twitter())
#' @param intent the desired intent for analysis.
#' 
#' @return a reshaped data.table
#' 
#' @import data.table
#'
#' @export
#'


reshape_tweets <- function(
    tweets,
    intent = c("retweets", "hashtags", "urls")) {
    if (!inherits(tweets, "list")) {
        stop("Provided data probably not preprocessed yet.")
    }

    required_elements <- c("tweets", "referenced", "urls", "mentions", "hashtags")

    for (el in required_elements) {
        if (!el %in% names(tweets)) {
            stop(
                paste("Provided data does not have the right structure. Please ensure the list contains:", el)
            )
        }
    }

    output_cols <- c("object_id", "id_user", "content_id", "timestamp_share")

    if (intent == "retweets") {
        # Mapping overview
        # referenced_tweet_id -> object_id
        # author_id -> id_user
        # tweet_id -> content_id:
        # created_timestamp -> timestamp_share

        # filter only mentions that start at position 3
        # these are direct retweets:
        # "RT @username"

        candidates <- tweets$mentions[start == 3, tweet_id]
        retweets <- tweets$referenced[tweet_id %in% candidates]
        retweets <- retweets[type == "retweeted"]

        # join meta data with referenced tweets
        retweets <- tweets$tweets[retweets, on = "tweet_id"]

        # attach original tweets, we need the timestamps

        filt <- tweets$tweets$tweet_id %in% retweets$referenced_tweet_id

        original_tweets <- tweets$tweets[filt]
        original_tweets[, referenced_tweet_id := tweet_id]

        tweet_cols <- c("referenced_tweet_id", "author_id", "tweet_id", "created_timestamp")
        retweets <- rbind(retweets[, ..tweet_cols], original_tweets[, ..tweet_cols])

        data.table::setnames(retweets, tweet_cols, output_cols)
        data.table::setindex(retweets, object_id, id_user)

        return(retweets)
    } else {
        .NotYetImplemented()
    }
}
