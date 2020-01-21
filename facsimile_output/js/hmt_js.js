$(document).ready(function () {
	$("#hmt_showAllButton").click(function() {
		
		console.log( $(this).html() );
		
		if ( $(this).html() == "Show Everything"){
			$(this).html("Hide Everything");
			$(".cite_showhide_closed").addClass("cite_showhide_open").removeClass("cite_showhide_closed");
			$(".ohco2_commentBlock_hidden").addClass("ohco2_commentBlock_visible").removeClass("ohco2_commentBlock_hidden");
			$(".cite_showhide_header").next().show();
		} else {
			$(this).html("Show Everything");
			$(".cite_showhide_open").addClass("cite_showhide_closed").removeClass("cite_showhide_open");
			$(".ohco2_commentBlock_visible").addClass("ohco2_commentBlock_hidden").removeClass("ohco2_commentBlock_visible");
			$(".cite_showhide_header").next().hide();
		}
	})

	var imgOffsetLeft = $("span.cite_preview_image > img.citeImage").first().offset().left;
	console.log(imgOffsetLeft);
	var imgOffsetTop = $("span.cite_preview_image > img.citeImage").first().offset().top;
	console.log(imgOffsetTop);
	var imgWidth = $("span.cite_preview_image > img.citeImage").first().width();
	console.log(imgWidth);
	var imgHeight = $("span.cite_preview_image > img.citeImage").first().height();
	console.log(imgHeight);

	function getRoiLeft( imgLeft, imgWidth, roiLeft) {
		var absLeft = imgWidth * roiLeft;
		return absLeft + imgLeft;		
	}
	function getRoiTop( imgTop, imgHeight, roiTop) {
		var absTop = imgHeight * roiTop;
		return imgTop + absTop;		
	}
	function getRoiWidth( imgWidth, roiWidth) {
		var absWidth = imgWidth * roiWidth;
		return absWidth;		
	}
	function getRoiHeight( imgHeight, roiHeight) {
		var absHeight = imgHeight * roiHeight;
		return absHeight;		
	}
	
	

	console.log(imgOverlayROIs);
	var temp = new Object();	

	imgOverlayROIs.forEach(roi => {
		console.log(roi.left);
		var newDiv = $("<div> </div>");
		newDiv.addClass("hmt_imageOverlay_roi");
		newDiv.addClass(roi.class);

		var roi_left = getRoiLeft( imgOffsetLeft, imgWidth, roi.left);
		var roi_top =  getRoiTop( imgOffsetTop, imgHeight,  roi.top);
		var roi_width =  getRoiWidth( imgWidth, roi.width);
		var roi_height =  getRoiHeight( imgHeight, roi.height);

		newDiv.css("width", roi_width );
		newDiv.css("height", roi_height );
		newDiv.css("top", roi_top );
		newDiv.css("left", roi_left );

		$("body").append(newDiv);
	});

	$(".cite_preview_image").mouseenter(function(){
			$(".hmt_imageOverlay_roi").hide();
	});

	$(".cite_preview_image").mouseleave(function(){
			$(".hmt_imageOverlay_roi").show();
	});


})



