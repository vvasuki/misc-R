create table tblRetention(Age, Gender, City, State, Country, Email, NumStumbles, NumThumbdowns, NumThumbups, Adult, Xrated, NumComments, NumHits, Interests, NumFriends, NumShares, HasPic, SignupBrowser, S1Length, S1NumStumbles, S1NumTopics, S1NumThumbups, S1NumThumbdowns, S1FirstRating, S1FirstThumbup, S1FirstThumbdown, InstallDevice, AvgUrlScore20, NumTopics20, NumThumbdowns20, NumThumbups20, NumRatings20, HasTwoRatings20, HasVideo20, NumVideos20, NumDomains20, AvgUrlScore10, NumTopics10, NumThumbdowns10, NumThumbups10, NumRatings10, HasTwoRatings10, HasVideo10, NumVideos10, NumDomains10, AvgUrlScore5, NumTopics5, NumThumbdowns5, NumThumbups5, NumRatings5, HasTwoRatings5, HasVideo5, NumVideos5, NumDomains5, Url1, Url2, Url3, Url4, Url5, Retained);
.mode csv
.import retention_data.csv tblRetention

select count(*) from tblRetention;
