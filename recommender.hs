import System.Random
import System.IO.Unsafe
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))

users = ["user1","user2","user3","user4"]
items = ["item1","item2","item3","item4","item5","item6"]
purchasesHistory = [("user1",[["item1","item2","item3"],["item1","item2","item4"]]),
                    ("user2",[["item2","item5"],["item4","item5"]]),
                    ("user3",[["item3","item2"]]),
                    ("user4",[])]

--(1function-->createEmptyFreqList)
createEmptyFreqList :: [a]->[(a,[b])] 
createEmptyFreqList itemsList = [(currentItem,[]) | currentItem<-itemsList] 

--(1helper-->userPurchasesHelper)
userPurchasesHelper :: String -> [[String]]
userPurchasesHelper userSearched = [oneItem|(user,allItems)<-purchasesHistory,userSearched==user,oneItem<-allItems]

--(2helper-->findOccurenceListsHelper)
findOccurenceListsHelper :: String ->[[String]] ->[[String]]
findOccurenceListsHelper currentItem purchaseList = [occurenceList | occurenceList<-purchaseList, elem currentItem occurenceList]

--(3helper-->totalSum)
totalSum :: [(String,Int)] ->Int
totalSum rest = sum [x | (_,x)<-rest]

--(4helper-->statisticsHelper)
statisticsHelper :: String -> [[String]]->[(String,Int)]
statisticsHelper currentItem occurenceCur = [(otherItem, length (findOccurenceListsHelper otherItem occurenceCur))| otherItem<-items, otherItem/=currentItem,findOccurenceListsHelper otherItem occurenceCur/=[]]

statisticsHelper1 :: String -> [(String,[(String,Int)])]
statisticsHelper1 userSearched = [(currentItem, statisticsHelper currentItem (findOccurenceListsHelper currentItem (userPurchasesHelper userSearched))) | currentItem<-items]

--(5helper-->frequencyHelper)
frequencyHelper :: [(String,[(String,Int)])] -> [(String,Int)]
frequencyHelper statisticsList = [(item,totalSum restOfData)|(item,restOfData)<-statisticsList,totalSum restOfData/=0]

--(2function-->getAllUserStats)
getAllUserStats :: [(String,[[String]])]->[(String,[(String,[(String,Int)])])]
getAllUserStats purchasesHistory = [(user,statisticsHelper1 user)|(user,_)<-purchasesHistory]

--(3function-->freqListItems)
freqListItems :: String -> [(String,Int)]
freqListItems userSearched = [wantedList|(userName,statisticsList)<-getAllUserStats purchasesHistory,userName==userSearched,wantedList<-frequencyHelper statisticsList] 

--(freqListCart1-->first helper to filter the intersection)
freqListCart1 :: String -> [String] -> [[(String,Int)]]
freqListCart1 userSearched cart = 
    [statisticsList1| (userName,statisticsList)<-getAllUserStats purchasesHistory,userSearched==userName,(itemName,statisticsList1)<-statisticsList,statisticsList1/=[],elem itemName cart]

--(sumForCartUsersHelper1-->second helper to loop over each individual item and find their existence)
sumForCartUsersHelper1 :: [[(String,Int)]] -> [(String,Int)]
sumForCartUsersHelper1 mixedList = [(item,sumForCartUsersHelper mixedList item) | item<-items, sumForCartUsersHelper mixedList item/=0]

--(sumForCartUsersHelper-->third helper to increment all occurences
sumForCartUsersHelper :: [[(String,Int)]] -> String -> Int
sumForCartUsersHelper mixedList item = sum [n|currentList<-mixedList,(itemName,n)<-currentList,itemName==item]

--(4function-->freqListCart)
freqListCart :: String -> [String] -> [(String,Int)]
freqListCart userSearched cart = sumForCartUsersHelper1 (freqListCart1 userSearched cart) 

--(5function-->freqListCartAndItems)
freqListCartAndItems :: String -> [String] -> [(String,Int)]
freqListCartAndItems userSearched cart = intersection (freqListItems userSearched) (freqListCart userSearched cart)

--(testpurchaseshelper that gets intersection between certain user and other users)
testPurchases :: String->[[(String,[(String,Int)])]]
testPurchases user = purchasesIntersection (statisticsHelper1 user) [(userName,dataUsers)|(userName,dataUsers)<-getAllUserStats purchasesHistory,userName/=user]

--(6function-->purchasesIntersection)
purchasesIntersection :: Eq a => [(a,[(a,Int)])]->[(a,[(a,[(a,Int)])])]->[[(a,[(a,Int)])]]
purchasesIntersection userRequired otherUsers = [[(itemName,intersection itemData itemData1)|(itemName,itemData)<-userStatistics,(itemName1,itemData1)<-userRequired,itemData/=[],itemData1/=[],itemName==itemName1]| (user,userStatistics)<-otherUsers]

--(intersection helper for users to loop over the first list relating to first user)
intersection :: Eq a=> [(a,Int)]->[(a,Int)]->[(a,Int)]
intersection ys [] = ys
intersection [] ys = ys
intersection ((x,n):xs) (y:ys)|intersection1 (x,n) (y:ys) 0/=0 = (x,intersection1 (x,n) (y:ys) 0 + n):intersection xs (delete x (y:ys))
                              |otherwise = (x,intersection1 (x,n) (y:ys) 0 + n):intersection xs (y:ys)
 
--(intersection helper part to loop the head of first element over the second list)
intersection1 :: Eq a=>(a,Int)->[(a,Int)]->Int->Int
intersection1 _ [] n = n
intersection1 ((x,n)) ((y,n1):ys) count |x==y = intersection1 ((x,n)) ys (count+n1)
                                        |otherwise = intersection1 ((x,n)) ys count

--(delete helper to delete matching occurences)
delete ::Eq a => a->[(a,Int)]->[(a,Int)]
delete _ [] = []
delete x ((y,n):ys)|x==y = delete x ys
                   |otherwise=  (y,n):delete x ys

      
--(freqListUsersHelper to collect all inner intersections in one list)
freqListUsersHelper :: String->[[(String,Int)]]              
freqListUsersHelper userName = [detailsItems|otherUser<-testPurchases userName,(itemName,detailsItems)<-otherUser]

--(7function-->freqListUsers)
freqListUsers :: String->[(String,Int)]
freqListUsers userName  = [(item,find item (freqListUsersHelper userName))|item<-items,find item (freqListUsersHelper userName)/=0]  

--(helper find to loop over list of lists of tuples)
find :: String->[[(String,Int)]]->Int
find _ [] = 0
find x (y:ys)  = find1 x y 0 + find x ys 

--(helper find1 to find number certain item in a list of tuples)
find1 :: String ->[(String,Int)]->Int->Int
find1 _ [] count = count
find1 x ((y,n):ys) count |x==y = n+count
                         |otherwise = find1 x ys count

--(8function-->recommendEmptyCart)
recommendEmptyCart :: String->String
recommendEmptyCart userName = checkOccurence (rangeCalculator (freqListItems userName) 0) (randomZeroToX (totalSum (freqListItems userName)-1))

--(rangeCalculator helper to generate sets that correspond to the probability)
rangeCalculator [] _  = []
rangeCalculator ((x,n):xs) count = (x,[count..count+n-1]):rangeCalculator xs (count+n) 
 

--(checkOccurence helper to return the item with range that contains the random number)
checkOccurence [] _ = []
checkOccurence ((x,range):xs) randomNumber| elem randomNumber range = x
                                          | otherwise = checkOccurence xs randomNumber

--(9function-->recommendBasedOnItemsIncart)
recommendBasedOnItemsInCart :: String->[String]->String
recommendBasedOnItemsInCart userName cart = checkOccurence (rangeCalculator (freqListCartAndItems userName cart) 0) (randomZeroToX (totalSum (freqListCartAndItems userName cart)-1))

--(10function-->recommendBasedOnUsers)
recommendBasedOnUsers :: String -> String
recommendBasedOnUsers userName = checkOccurence (rangeCalculator (freqListUsers userName) 0) (randomZeroToX (totalSum (freqListUsers userName)-1))

--(recommend1 helper to do the recommend action without checking if the recommendation is empty or not)
recommend1 userName cart|cart==[]=[recommendEmptyCart userName,recommendBasedOnUsers userName]!!randomZeroToX 1
                        |otherwise = [recommendBasedOnItemsInCart userName cart,recommendBasedOnUsers userName]!!randomZeroToX 1

--(11function-->recommend) 
recommend :: String->[String]->String               
recommend userName cart  = if recommend1 userName cart=="" then items!!randomZeroToX (length items-1) else recommend1 userName cart                 