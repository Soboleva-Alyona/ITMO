# blockchain-hw-1

### 1 task git diff result (changes in function  executeTransaction(uint transactionId)):
```
@@ -91,6 +91,11 @@ contract MultiSigWallet {
         _;
     }

+    modifier validTransactionValue(uint transactionId) {
+        require(transactions[transactionId].value <= 66 ether);
+        _;
+    }
+
     
@@ -226,6 +231,7 @@ contract MultiSigWallet {
         ownerExists(msg.sender)
         confirmed(transactionId, msg.sender)
         notExecuted(transactionId)
+        validTransactionValue(transactionId)
     {
         if (isConfirmed(transactionId)) {
             Transaction storage txn = transactions[transactionId];
```


### 2 task git diff result (changes in function _beforeTokenTransfer):
```
@@ -365,7 +365,11 @@ contract ERC20 is Context, IERC20, IERC20Metadata {
         address from,
         address to,
         uint256 amount
-    ) internal virtual {}
+    ) internal virtual {
+        uint256 currentTime = block.timestamp;
+        uint256 dayInSeconds = 86400;
+        require((currentTime / dayInSeconds + 4) % 7 != 6, "Token can't be transfered on Saturday");
+    }
```



### 3 task git diff result:
```
@@ -37,7 +37,7 @@ contract DividendToken is StandardToken, Ownable {
         }));
     }

-    function() external payable {
+    function sendEthWithComment(bytes32 _comment) external payable {
         if (msg.value > 0) {
             emit Deposit(msg.sender, msg.value);
             m_totalDividends = m_totalDividends.add(msg.value);
```
