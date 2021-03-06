<?php
//MIDTERM ADDITIONS - DATABASE CONNECTION
// Create Database connection
$con=mysqli_connect("db536766613.db.1and1.com","dbo536766613","IMCsql!s05","db536766613");

// Check Database connection
if (mysqli_connect_errno()) {
  echo "Failed to connect to MySQL: " . mysqli_connect_error();
 }
 
	if(isset($_POST['name'])) {

	
//MIDTERM ADDITIONS - EXPERT TIP - AVOID POSTING LOOP


	 if(isset($_POST['cookie'])) {
	  $COOKIELOAD=1; }
	  
		 $CARTCOUNT = 0;
	     $UNAME = ($_POST['name']);
		 $GREETING = 'Welcome back '. $UNAME.'.';
		 
//MIDTERM ADDITIONS - SQL SELECT TO GET USER DETAILS	
//Final Addition: Extract customer data based on the user name he/she enters. All info is
//in array format.
		 $search1 = mysqli_query($con,"SELECT * FROM `Customer` WHERE UID = '". $UNAME ."'");
		 if(mysqli_num_rows($search1) > 0){
		 while($row = mysqli_fetch_array($search1)) {
		  $CARTCOUNT = $row[CartItems];
//Final Addition - Pref is read here and will be used later for cookie
		  $PREF = $row[Pref];
		  $LATEST = $row[LastCart];
//Final Addition - Read customer score and the other three fields used in the model
		  $SCORE = $row[Score];
		  $PURCHDAYS = $row[PurchDays];
          $VISITDAYS = $row[VisitDays];
          $PURCHNUM = $row[PurchNum];
//Final Addition - Modelling to predict customer value score. Explanation sees the documentation.       
      if($VISITDAYS > 3.8) {$VISITDAYSSCORE = 1;} else {$VISITDAYSSCORE = 0;}
      if($PURCHDAYS > 6) {$PURCHDAYSSCORE = 1;} else {$PURCHDAYSSCORE = 0;}
      if($PURCHNUM > 2.8) {$PURCHNUMSCORE = 1;} else {$PURCHNUMSCORE = 0;}
      if($CARTCOUNT > 1.5) {$CARTCOUNTSCORE = 1;} else {$CARTCOUNTSCORE = 0;}
      $VALUESCORE = $VISITDAYSSCORE  + 2*$CARTCOUNTSCORE + 3*$PURCHNUMSCORE - $PURCHDAYSSCORE;
		  }
		  
		  
//MIDTERM ADDITIONS - LOGIC TO SET BOOKS
//Final Addition: Search2 is aimed to extract book data based on the book genre related to the customer. All info is
//in array format.
//Final Addition: Search3 is aimed to extract information of the book last placed in the cart. All info is
//in array format.

	      $search2 = mysqli_query($con,"SELECT * FROM `Bookdetails` WHERE CatID = '". $PREF ."'");
		  if($LATEST != 0) {
		   $n=2;
		   $search3 = mysqli_query($con,"SELECT * FROM `Bookdetails` WHERE bid = '". $LATEST ."'");
	       $BOOKID1=$LATEST;
		   while($row = mysqli_fetch_array($search3)) {
		   ${"BOOKPIC1"} = $row[Image];
		   ${"BOOKTITLE1"} = $row[Title];
		   ${"BOOKAUTH1"} = $row[Author];
		   ${"BOOKDESC1"} = $row[Description];
		   ${"BOOKPRICE1"} = $row[Price];
		   }
		  } else 
		  { $n=1; 
		  }
		  while($row = mysqli_fetch_array($search2)) {
		  if($row[bid] != $LATEST){
//Final Addition: Information of all the books in the specific genre the user may be interested in, if 
//nothing has been added to the cart. 
	       ${"BOOKID$n"} = $row[bid];
		   ${"BOOKPIC$n"} = $row[Image];
		   ${"BOOKTITLE$n"} = $row[Title];
		   ${"BOOKAUTH$n"} = $row[Author];
		   ${"BOOKDESC$n"} = $row[Description];
		   ${"BOOKPRICE$n"} = $row[Price];
		   $n++;
		   }
		    }
		   } else {
//Final Addition: Default setting for new customers/logged out customers
		    $n=1;
		    $search4 = mysqli_query($con,"SELECT * FROM `Bookdetails` WHERE bid in (100,200,300,400)");
           while($row = mysqli_fetch_array($search4)) {
		   ${"BOOKID$n"} = $row[bid];
		   ${"BOOKPIC$n"} = $row[Image];
		   ${"BOOKTITLE$n"} = $row[Title];
		   ${"BOOKAUTH$n"} = $row[Author];
		   ${"BOOKDESC$n"} = $row[Description];
		   ${"BOOKPRICE$n"} = $row[Price];	
           $n++;
		   }		   
      }
     }	  else { 
//Final Addition: Default setting for new customers/logged out customers
		 $GREETING = 'Welcome Guest. <a href="#" class="my_popup_open">Log on</a> for recommendations.';
		 
//MIDTERM ADDITIONS - SET BOOKS FOR LOGGED OUT VISITORS
	
		 $n=1;
		 $search4 = mysqli_query($con,"SELECT * FROM `Bookdetails` WHERE bid in (100,200,300,400)");
           while($row = mysqli_fetch_array($search4)) {
		   ${"BOOKID$n"} = $row[bid];
		   ${"BOOKPIC$n"} = $row[Image];
		   ${"BOOKTITLE$n"} = $row[Title];
		   ${"BOOKAUTH$n"} = $row[Author];
		   ${"BOOKDESC$n"} = $row[Description];
		   ${"BOOKPRICE$n"} = $row[Price];	
           $n++;
		   }		   

		 }
		 
		 
?>

<html>

 <head>
 


 
 <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.11.2/jquery.min.js"></script>
 <script src="http://www.imcanalytics.com/js/jquery.popupoverlay.js"></script>
 <style>
 section {
    width: 90%;
    height: 200px;
    margin: auto;
    padding: 10px;
}

#one {
  float:left; 
  margin-right:20px;
  width:40%;
  border:1px solid;
  min-height:220px;
}

#two { 
  overflow:hidden;
  width:40%;
  border:1px solid;
  min-height:220px;
}

#three {
  float:left; 
  margin-top:10px;
  margin-right:20px;
  width:40%;
  border:1px solid;
  min-height:220px;
}

#four { 
  overflow:hidden;
  margin-top:10px;
  width:40%;
  border:1px solid;
  min-height:220px;
}

@media screen and (max-width: 400px) {
   #one { 
    float: none;
	margin-right:0;
    margin-bottom:10px;
    width:auto;
  }
  
  #two { 
  background-color: white;
  overflow:hidden;
  width:auto;
  margin-bottom:10px;
  min-height:170px;
}

   #three { 
    float: none;
	margin-right:0;
    margin-bottom:10px;
    width:auto;
  }
  
  #four { 
  background-color: white;
  overflow:hidden;
  width:auto;
  min-height:170px;
}

}
</style>

<script>
    
    $(document).ready(function() {

      // Initialize the plugin
	 
      $('#my_popup').popup({  
	   transition: 'all 0.3s',
       scrolllock: true // optional
   });

      $('#bookdeets').popup({  
	   transition: 'all 0.3s',
       scrolllock: true // optional
   });
   
});

   $.fn.DeetsBox = function(bid) {
        if(bid == '1'){	

//MIDTERM ADDITIONS - NEW VARIABLES AND CONDITIONS
//Final Addition - Pop-up Window Control
//Show book name and book price on the premium position of bookshelf.
		var bookname = $( "#book1" ).val();
		var bookprice = $( "#book1price" ).val();
		$("#showbookdeets").html(bookname + "<p>" + bookprice); 
		$("#bookshelf").val('1'); 
//Final Addition - Show "purchase" if the book has been added to the cart
		 var fromcart = $( "#iscart" ).val();
		 if(fromcart != 0){
		 
		 $("#deetcta").text('Purchase'); }
		}
		else if(bid == '2'){
		var bookname = $( "#book2" ).val();
		var bookprice = $( "#book2price" ).val();
		$("#showbookdeets").html(bookname + "<p>" + bookprice); 
		$("#bookshelf").val('2'); 
	// Student Comment: Pop-Up for book2
		 var fromcart = $( "#iscart" ).val();
		 if(fromcart != 0){
		 
		 $("#deetcta").text('Purchase'); }
		
		}
			else if(bid == '3'){
		var bookname = $( "#book3" ).val();
		var bookprice = $( "#book3price" ).val();
		$("#showbookdeets").html(bookname + "<p>" + bookprice); 
		$("#bookshelf").val('3'); 


		 var fromcart = $( "#iscart" ).val();
		 if(fromcart != 0){
		 
		 $("#deetcta").text('Purchase'); }
		}
			else if(bid == '4'){
		var bookname = $( "#book4" ).val();
		var bookprice = $( "#book4price" ).val();
		$("#showbookdeets").html(bookname + "<p>" + bookprice); 
		$("#bookshelf").val('4'); 
	// Student Comment: Pop-Up for book4
		 var fromcart = $( "#iscart" ).val();
		 if(fromcart != 0){
		 
		 $("#deetcta").text('Purchase'); }
		}
		$('#bookdeets').popup('show');
    };
	


</script>

<script language="JavaScript">

//TWO FUNCTIONS TO SET THE COOKIE
//Final Addition
//In the midterm file, bakeCookie messed up because we didn't specify the value of cvalue2.
//It made the function take 365 as the value of cvalue2, which should be expire day instead.
//Here we created a new variable pref, which tell us the category info related to the consumer.

function mixCookie() {

 	    var name = document.forms["form1"]["name"].value;
 	    var pref = document.getElementById('isthepref').value;

        bakeCookie("readuser", name, pref, 365);
			
   }
   

function bakeCookie(cname, cvalue1, cvalue2, exdays) {
    var d = new Date();
    d.setTime(d.getTime() + (exdays*24*60*60*1000));
    var expires = "expires="+d.toGMTString();
    document.cookie = cname + "=" + cvalue1 + "%-" + cvalue2 + ";" + expires;
}

//TWO FUNCTIONS TO GET THE COOKIE

function checkCookie() {
    var userdeets = getCookie("readuser");
    
//MIDTERM ADDITIONS - 'CHECKFIRST' VARIABLE - FOR 'IF' BELOW

	var checkfirst = document.getElementById('firstload').value;
	
    if (userdeets != "") {
	    var deets = userdeets.split("%-");
		var user = deets[0];
		
//MIDTERM ADDITIONS - NEW NESTED 'IF' LOGIC TO POST USERNAME TO PHP TO CHECK FOR DETAILS THROUGH SQL	

		 if(checkfirst != 1){
		  
		  post('index.php',{name:user,cookie:yes});
		  
		 } else { greeting.innerHTML = 'Welcome ' + user; }
	     
  } else { return "";}
}



function getCookie(cname) {

    var name = cname + "=";
    var ca = document.cookie.split(';');
//Final Addition: Define delimiter to be semicolon 
    for(var i=0; i<ca.length; i++) {
//Final Addition: Take out extra space
        var c = ca[i].trim();
        if (c.indexOf(name) == 0) return c.substring(name.length, c.length);
    }
    return "";
}

<!--MIDTERM ADDITIONS - FUNCTION TO DELETE COOKIE-->


function drop_cookie(name) {
  document.cookie = name + '=; expires=Thu, 01 Jan 1970 00:00:01 GMT;';
  window.location.href = window.location.pathname;
}

<!--MIDTERM ADDITIONS - FUNCTION TO POST FROM JS -->

function post(path, params, method) {
    method = method || "post"; // Set method to post by default if not specified.

    var form = document.createElement("form");
    form.setAttribute("method", method);
    form.setAttribute("action", path);

    for(var key in params) {
        if(params.hasOwnProperty(key)) {
            var hiddenField = document.createElement("input");
            hiddenField.setAttribute("type", "hidden");
            hiddenField.setAttribute("name", key);
            hiddenField.setAttribute("value", params[key]);

            form.appendChild(hiddenField);
         }
    }

    document.body.appendChild(form);
    form.submit();
}


</script>

<!-- Google Analytics Tracking Code -->

 <script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

  ga('create', 'UA-68559037-1', 'auto');
  ga('send', 'pageview');

</script>



 </head>
 
 
 <body  onload="checkCookie()">
<!-- Header Formatting -->
 
 <div style="width:100%; height:25%; background-color:#57585A;">
 <img src="img/ic1.jpg" style="max-height: 100%;">
 
<!--MIDTERM ADDITIONS - LOG-OUT LINK & LOGIC FOR VISITOR LOGGED STATE. CART NOW A LINK.--> 

<?php if(isset($_POST['name'])) { ?>
<!-- Drop Cookie -->	

    <div style="float:right; margin-right:50px;margin-top:10px; color:white;"> <a href="#" style="color:white;" onclick="drop_cookie('readuser');">Log Out</a> </div>
	
	<div style="float:right; margin-right:75px;margin-top:10px; color:white;"> 
	

	 <a href="#" style="color:white;" onClick="ga('send', 'event', 'convert', 'cart_click', '<?php echo $UNAME ?>');">Cart: <?php echo $CARTCOUNT ?></a>
	 </div>
 <?php } ?>
 </div>
 <div style="margin-top:10px; margin-bottom:10px; font-size: 130%; color:#57585A;">
 <strong>Icculus Media: For All Your Fictional Needs</strong>
 </div>
 
<!-- Final Addition: Call the greeting message defined on the top-->
 <div id="greeting"> <?php echo $GREETING ?> </div>
 
 <!--MIDTERM ADDITIONS - NEW HIDDEN FIELD - USED IN NEW CHECKCOOKIE LOGIC -->

 
 <input type="hidden" id="firstload" value="<?php echo $COOKIELOAD ?>">
 
  <!--MIDTERM ADDITIONS - NEW HIDDEN FIELD - USED FOR BOOK1 CTA -->
 <input type="hidden" id="iscart" value="<?php echo $LATEST ?>">
 
  <!--Final Addition - NEW HIDDEN FIELD - USED FOR PREF -->
 <input type="hidden" id="isthepref" value="<?php echo $PREF ?>">
 
 
 <div id="cta1"> Please browse our options:</div>
 <section>

 <div id="one" style="padding:10px;"> 
	<?php echo $BOOK1; ?>
	
	<img src="img/<?php echo $BOOKPIC1 ?>" style="float:left; margin-right:6px; height: 100px;">
	
	
<!-- MIDTERM ADDITIONS - ADDED BOOKPRICE. MADE BOOK DYNAMIC of Book1 -->
    <input type="hidden" id="book1" value="<?php echo $BOOKTITLE1 ?>">
	<input type="hidden" id="book1price" value="<?php echo $BOOKPRICE1 ?>">
<!--Final Addition: Extract $BOOKTITLE from DB and assign it to "Book1". Same with Book Price, Author
	and Desc-->
	<strong><?php echo $BOOKTITLE1 ?></strong><p>
	by <?php echo $BOOKAUTH1 ?> <p>
	<?php echo $BOOKDESC1 ?>
	<p>
	<?php if($LATEST != 0){ ?>
<!--Final Addition: If there is at least one book put in the cart. show the "purchase" button
					Otherwise, show "learn more". -->
	<input type="button" value="Purchase" id="book1button" onClick="ga('send', 'event', 'convert', 'purchase', document.getElementById('book1').value); $(this).DeetsBox(1);">
	<?php } else { ?>
	<input type="button" value="Learn More" id="book1button" onClick="ga('send', 'event', 'browse', 'learn_more_home', document.getElementById('book1').value); $(this).DeetsBox(1);">
	<?php } ?>
	
	
	
	
	</div>
	
<!-- SLOT2 -->
<!-- Final Addition -->
<!-- Nothing changes when customer score is lower than 50-->

<?php if ($SCORE < 50) { ?>

<div id="two" style="padding:10px;">
  <?php echo $BOOK2; ?>
    <img src="img/<?php echo $BOOKPIC2 ?>" style="float:left; margin-right:6px; height: 100px;">
    <input type="hidden" id="book2" value="<?php echo $BOOKTITLE2 ?>">
    <input type="hidden" id="book2price" value="<?php echo $BOOKPRICE2 ?>">
 
  <strong> <?php echo $BOOKTITLE2 ?></strong><p>
  by <?php echo $BOOKAUTH2 ?><p>
  <?php echo $BOOKDESC2 ?>
  <p>
  <input type="button" value="Learn More" id="book2button" onClick="ga('send', 'event', 'engagement', 'learn_more_home', document.getElementById('book2').value); $(this).DeetsBox(2)";>
 </div> <?php } else if ($SCORE <75) { ?>
 
 <div id="two" style="padding:10px;">
 <!-- If customer score is 50 to 75 show them “special offer book” -->
 
 <?php echo $BOOK2; ?>
    <img src="img/<?php echo $BOOKPIC2 ?>" style="float:left; margin-right:6px; height: 100px;">
    <input type="hidden" id="book2" value="<?php echo $BOOKTITLE2 ?>">
    <input type="hidden" id="book2price" value="< m?php echo $BOOKPRICE2 ?>">
<!-- Put "Special Offer Book" right next to the title to remind customers of the promotion-->
	<strong> Special Offer Book: <?php echo $BOOKTITLE2 ?></strong><p>
  	by <?php echo $BOOKAUTH2 ?><p>
  	<?php echo $BOOKDESC2 ?>
  	<p>
<!-- Label learn more button as "learn_more_special" to differentiate it from the ordinary one, which will Icculus to understand
	 whether mentioning the message in the text will increase response-->  	
  	<input type="button" value="Learn More" id="book2button" onClick="ga('send', 'event', 'engagement', 'learn_more_special', document.getElementById('book2').value); $(this).DeetsBox(2)";>
 </div> <?php } else if ($VALUESCORE < 5) { ?>
 
 
 <div id="two" style="padding:10px;">
 <!-- Free shipping for all customers value score lower than 5. Please see the rationale in the documentation. 
 	  Also free shipping is labeled differently to track the response -->
 <a href="#" onClick="ga('send', 'event', 'engagement', 'free_shipping', '<?php echo $UNAME ?>');"> Free Shipping for All Orders! </a> 
 </div> <?php } else {?>
 
 <div id="two" style="padding:10px;">
<!-- Half price for all customers value score higher than or equals to 5. Please see the rationale in the documentation. 
 	  Also it is labeled differently to track the response -->
 <a href="#" onClick="ga('send', 'event', 'engagement', 'half_price', '<?php echo $UNAME ?>');"> Buy one get other half price! </a> 
 </div> <?php }  ?>


	
	
	
	
	
	
<!-- SLOT3 -->	
 <div id="three" style="padding:10px;">

	<?php echo $BOOK2; ?>
	<?php echo $BOOK3; ?>

	<img src="img/<?php echo $BOOKPIC3 ?>" style="float:left; margin-right:6px; height: 100px;">

    <input type="hidden" id="book3" value="<?php echo $BOOKTITLE3 ?>">
    <input type="hidden" id="book3price" value="<?php echo $BOOKPRICE3 ?>">
    

	<strong><?php echo $BOOKTITLE3 ?></strong><p>

	by <?php echo $BOOKAUTH3 ?><p>

	<?php echo $BOOKDESC3 ?>
	<p>

	<input type="button" value="Learn More" id="book3button" onClick="ga('send', 'event', 'browse', 'learn_more_home', document.getElementById('book3').value); $(this).DeetsBox(3);">
	</div>
    
    
    
    
<!-- MIDTERM ADDITIONS - PHP SO THAT DISPLAY DEPENDS ON CART OR NOT -->	
<?php 
if($n > 4){ ?>
 <div id="four" style="padding:10px;">
	<?php echo $BOOK4; ?>
	<img src="img/<?php echo $BOOKPIC4 ?>" style="float:left; margin-right:6px; height: 100px;">
<!-- ASSIGNMENT 2 ADDITIONS - CREATED hidden input WITH UNIQUE ID -->
    <input type="hidden" id="book4" value="<?php echo $BOOKTITLE4 ?>">
    <input type="hidden" id="book4price" value="<?php echo $BOOKPRICE4 ?>">

	<strong><?php echo $BOOKTITLE4 ?></strong><p>

	by <?php echo $BOOKAUTH4 ?><p>

	<?php echo $BOOKDESC4 ?>
	<p>

	<input type="button" value="Learn More" id="book4button" onClick="ga('send', 'event', 'browse', 'learn_more_home', document.getElementById('book4').value); $(this).DeetsBox(4);">
	</div>
	<?php } else { ?>
	<div id="four" style="padding:10px;"></div>
	<?php } ?>
	
</section>

	<div id="my_popup" style = "background-color: white; display: none; padding: 20px;">
    <form name="form1" action="#" method="post">
	     <div>Please enter your name:</div>
	
    <input name="name" id="uname" type="text" /><p>
	<input type="submit" onclick="mixCookie();" value="Log In"/> <p>
	</form>
	</div>
	

	<div id="bookdeets" style = "background-color: white; display: none; padding: 20px;">
	<div id="showbookdeets"></div>
    <input type="hidden" id="bookshelf"  value="0">
	
<!--MIDTERM ADDITIONS - CHANGED TO BUTTON TO CLOSE-->


	<button id="deetcta" class="bookdeets_close"  onClick="ga('send', 'event', 'convert', 'cart_add', document.getElementById('bookshelf').value)";/>Add to Cart</button> <p>
	</div>

 </body>
 </html>
