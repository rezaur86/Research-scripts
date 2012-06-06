<?php
require_once "./config/config.php";

$cfname = sprintf("config/page_%s.txt", $user);
if (!($configFilePtr = fopen($cfname, "r")))
{
	$pages = 0;
}
else
{
	fscanf($configFilePtr, "%s", $pages);
	if (!strcmp($pages, '0'))
		$pages = 0;
}
if ($configFilePtr) fclose($configFilePtr);

if(!($configFilePtr = fopen($cfname, "w")))
{
	return;
}

//fprintf($configFilePtr, "%d", $pages);
//fflush($configFilePtr);
//fclose($configFilePtr);
if ($pages == 0){
	if (!($fbIDFilePtr = fopen('config/fbid.txt', 'r'))){
		echo "No Valid FB page identifier\n";
		return;
	}
	fscanf($fbIDFilePtr, "%s\n", $fbGroupID);
	fscanf($fbIDFilePtr, "%s\n", $fbGroupName);
	fclose($fbIDFilePtr);
	
	$lines = file_get_contents('config/fbid.txt');
	
	$replace_these = array($fbGroupID."\n", $fbGroupName."\n");
	$replace_with = array ("", "");
	$lines = str_replace($replace_these, $replace_with, $lines);
	$fbIDFilePtr = fopen('config/fbid.txt', 'w');
	fprintf($fbIDFilePtr, "%s", $lines);
	fclose($fbIDFilePtr);
	if (!($fbCurFilePtr = fopen('config/fbid_current.txt', 'w'))){
		echo "Cannot write current Group\n";
		return;
	}
	fprintf($fbCurFilePtr, "%s\n", $fbGroupID);
	fprintf($fbCurFilePtr, "%s\n", $fbGroupName);
	fclose($fbCurFilePtr);
}
else {
	if (!($fbCurFilePtr = fopen('config/fbid_current.txt', 'r'))){
		echo "Cannot write current Group\n";
		return;
	}
	fscanf($fbCurFilePtr, "%s\n", $fbGroupID);
	fscanf($fbCurFilePtr, "%s\n", $fbGroupName);
	fclose($fbCurFilePtr);
	
}

echo "I am crawling [ ", $fbGroupID, " ] ", $fbGroupName;
echo " from ", $pages;

if (!($errFilePtr = fopen("config/crash_p1.log", "a"))) {
	return;
}

fprintf($errFilePtr, "I am crawling [ %s, %s ] from page %d\n", $fbGroupID, $fbGroupName, $pages);
fflush($errFilePtr);

if( $user )
{
	$esname = sprintf("outputs/%05d_%s.log", $pages, $user);
	if (!($errFilePtr = fopen($esname, "w"))) {
		return;
	}

	if (!$pages){
		$groups_feed = $facebook->api('/' . $fbGroupID . "/feed");
	}
	else {
		$groups_feed = $facebook->api(substr($pages, 26));
	}
	$next_page = $groups_feed['paging']['next'];
	$count = 0;
	//$group_feed = 0;

	// skipping a few pages ...
	//    for ($i = 0; $i < $pages; $i += 1)
		//      {
	//	$next_page = $groups_feed['paging']['next'];
	//	$groups_feed = $facebook->api(substr($next_page, 26));
	//	fprintf($errFilePtr, "page %d --- \n%s\n", $count, $next_page);
	//	$count += 1;
	//	if (($i % 400) == 0) sleep(2);
	//      }
	flush();
	fflush($errFilePtr);

	$sname = sprintf("config/posts_%s_%s.txt", $user, $fbGroupName);

	if (!($myFilePtr = fopen($sname, "a")))
	{
		echo "I am OUT";
		return;
	}

	while($next_page)
	{
		fprintf($errFilePtr, "BP%d\n%d\n", $count, time());
		foreach($groups_feed['data'] as $curr_feed)
		{
			fprintf($myFilePtr, "%s\n", $curr_feed['created_time']);
			fprintf($myFilePtr, "%s\n", $curr_feed['id']);
			fflush($myFilePtr);
		}
		$count += 1;
		$next_page = $groups_feed['paging']['next'];
		if ($next_page)
		{
			$groups_feed = $facebook->api(substr($next_page, 26));
		}

		//fprintf($errFilePtr, "page %d/%d done\n", $count, $pages);
		if(!($configFilePtr = fopen($cfname, "w")))
		{
			return;
		}
		fprintf($configFilePtr, "%s", $next_page);
		fflush($configFilePtr);
		fclose($configFilePtr);

		fprintf($errFilePtr, "EP%d\n%d\n", $count, time());
		if (($count % 100) == 0) sleep(2);
	}
	fprintf($errFilePtr, "FP%d\n%d\n", $count, time());
	fprintf($errFilePtr, "ALL Posts collected %d\n", $count);
	echo "ALL Posts collected ", $count, "!!!!!";
}
else
{
	try
	{
	 $params = array
	 ('scope' => "email, sms, user_groups, friends_groups, read_stream",
	 		//  'redirect_uri' => "http://apps.facebook.com/spring_demo",
	 );
	 $redirect = $facebook->getLoginUrl($params);
	 ?>
<script>
	      top.location = "<?php echo $redirect; ?>";
	 </script>
<?php
	}
	catch (FacebookApiException $e)
	{
	 print_r($e);
	}
}
?>

