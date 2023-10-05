import random

from .contextual import Contextual
from .recommender import Recommender


class MyRecommender(Recommender):
    def __init__(self, tracks_redis, tracks_to_users_redis, recommendations_redis, catalog):
        self.fallback = Contextual(tracks_redis, catalog)
        self.tracks_redis = tracks_redis
        self.tracks_to_users_redis = tracks_to_users_redis
        self.recommendations_redis = recommendations_redis
        self.catalog = catalog
        self.liked_tracks = [] # понравившиеся треки определяем как те, у которых >= 0.6

    def recommend_next(self, user: int, prev_track: int, prev_track_time: float) -> int:
        if len(self.liked_tracks) == 0:
            self.liked_tracks.append(prev_track)
        elif prev_track_time >= 0.6:
            self.liked_tracks.append(prev_track)

        random_liked_track = self.liked_tracks[random.randint(0, len(self.liked_tracks) - 1)]

        similar_users = self.tracks_to_users_redis.get(random_liked_track)
        if similar_users is None:
            return self.fallback.recommend_next(user, prev_track, prev_track_time)

        similar_users_list = list(self.catalog.from_bytes(similar_users))
        if len(similar_users_list) == 0:
            return self.fallback.recommend_next(user, prev_track, prev_track_time)

        random_similar_user = similar_users_list[random.randint(0, len(similar_users_list) - 1)]

        recommendations = self.recommendations_redis.get(random_similar_user)
        if recommendations is not None:
            shuffled = list(self.catalog.from_bytes(recommendations))
            random.shuffle(shuffled)
            return shuffled[0]
        else:
            return self.fallback.recommend_next(user, prev_track, prev_track_time)

