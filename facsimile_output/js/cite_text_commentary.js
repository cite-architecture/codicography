
$(document).ready(function () {

			// Pre-hide comments
			$(".ohco2_commentBlock").addClass("ohco2_commentBlock_hidden")
			$(".ohco2_commentBlock").prepend("<span class='ohco2_commentBlock_close close'>×</span>");

      $(".ohco2_commentBlock").click(function() {
      	if ( $(this).hasClass("ohco2_commentBlock_hidden") ) {
		     	$(this).removeClass("ohco2_commentBlock_hidden");
          $(this).addClass("ohco2_commentBlock_visible");
          var thisContainer = $(this).parent();
          $(this).detach().appendTo(thisContainer);
          return false;
      	} 
      });

      $(".ohco2_commentBlock_close").click(function() {
	      	var mom = $(this).parent();
		     	mom.removeClass("ohco2_commentBlock_visible");
          mom.addClass("ohco2_commentBlock_hidden");
          var ancestor = mom.parent(".ohco2_commentedPassage");
          var siblings = mom.siblings(".ohco2_commentBlock");
          mom.detach();
          siblings.detach();
          mom.appendTo(ancestor);
          siblings.appendTo(ancestor);

          return false;
      });
});

