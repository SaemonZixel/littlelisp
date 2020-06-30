<?php

$password = "abc123";

error_reporting(E_ALL|E_STRICT);
ini_set('display_errors', 1);
function exception_handler($exception) { print_r($exception); }
set_exception_handler('exception_handler');

date_default_timezone_set('Europe/Moscow');
if(ini_get('magic_quotes_gpc')) die('magic_quotes_gpc = On!');

if(!empty($_REQUEST["action"]) && $_REQUEST["action"] == "auth") {
	if($_REQUEST["password"] == $password) {
		setcookie("littlelisp-password", $password, 0); // session cookie
		die("ok");
	}
	else
		die("Error! Password invalid!");
}

if(empty($_COOKIE["littlelisp-password"]) and isset($_REQUEST["action"])) {
	die("Error! password required.");
} 

switch(@$_REQUEST["action"]) {
		
	case "list":
		foreach(scandir($_SERVER['DOCUMENT_ROOT']) as $file) {
			if(preg_match("/\.lisp$/" ,$file))
				echo "$file\n";
			elseif(is_dir($_SERVER['DOCUMENT_ROOT']."/$file") and $file[0] != '.') {
				foreach(scandir($_SERVER['DOCUMENT_ROOT']."/$file") as $file2) {
					if(preg_match("/\.lisp$/" ,$file2))
						echo "$file/$file2\n";
				}
			}
		}
		die("");
		break;
		
	case "get":
		if($_REQUEST['file'][0] != '/') $_REQUEST['file'] = '/'.$_REQUEST['file'];
		echo file_get_contents($_SERVER['DOCUMENT_ROOT'].$_REQUEST['file']);
		break;
		
	case "save":
		if($_REQUEST['file'][0] != '/') $_REQUEST['file'] = '/'.$_REQUEST['file'];
		
		// резервная копия
		@copy($_SERVER['DOCUMENT_ROOT'].$_REQUEST['file'], $_SERVER['DOCUMENT_ROOT'].$_REQUEST['file'].'~');
		
		// запишем в файл .lisp
		$fp = fopen($_SERVER['DOCUMENT_ROOT'].$_REQUEST['file'], "w+");
		if(flock($fp, LOCK_EX)) { 
			ftruncate($fp, 0); 
			fwrite($fp, $_REQUEST['lisp']);
			flock($fp, LOCK_UN); 
		} else {
			die("Couldn't get the lock! ({$_REQUEST['file']})");
		}
		fclose($fp);
		
		// запишем в файл .lisp.js
		$fp = fopen($_SERVER['DOCUMENT_ROOT'].$_REQUEST['file'].".js", "w+");
		if(flock($fp, LOCK_EX)) { 
			ftruncate($fp, 0); 
			fwrite($fp, $_REQUEST['lisp_js']);
			flock($fp, LOCK_UN); 
		} else {
			die("Couldn't get the lock! ({$_REQUEST['file']}.js)");
		}
		fclose($fp);
		
		die('ok');
		break;
	default:
		header("Content-Type: text/html;charset:utf-8");
		setcookie("littlelisp-save-url", "/littlelisp.ide.php");
		readfile("littlelisp.ide.html");
}