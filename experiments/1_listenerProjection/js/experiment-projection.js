function make_slides(f) {
    var slides = {};

    slides.bot = slide({
        name : "bot",
        start: function() {
            $('.err1').hide();
            $('.err2').hide();
            $('.disq').hide();
            exp.speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
            exp.listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];
            exp.lives = 0;
            var story = exp.speaker + ' says to ' + exp.listener + ': "It\'s a beautiful day, isn\'t it?"'
            var question = 'Who does ' + exp.speaker + ' talk to?';
            document.getElementById("s").innerHTML = story;
            document.getElementById("q").innerHTML = question;
        },

        button : function() {
            // get the response and remove sapces
            exp.text_input = document.getElementById("text_box").value.replace(" ", "")

            // correct response
            if ((exp.lives < 3) && ((exp.text_input.toLowerCase() == exp.listener.toLowerCase()))){
                exp.data_trials.push({
                    "slide_number_in_experiment" : exp.phase,
                    "utterance": "bot_check",
                    "object": exp.listener,
                    "rt" : 0,
                    "response" : exp.text_input
                });
                exp.go();
            } else {
                exp.data_trials.push({
                    "slide_number_in_experiment" : exp.phase,
                    "utterance": "bot_check",
                    "object": exp.listener,
                    "rt" : 0,
                    "response" : exp.text_input
                });
                if (exp.lives == 0){
                    $('.err1').show();
                } else if (exp.lives == 1){
                    $('.err1').hide();
                    $('.err2').show();
                } else if (exp.lives == 2){ // three incorrect responses
                    $('.err2').hide();
                    $('.disq').show();
                    $('.button').hide(); // remove button, so that the participant can't advance
                }
                exp.lives++;
            } 
        }
    });
    
    slides.i0 = slide({
        name : "i0",
        start: function() {
            exp.startT = Date.now();
        }
    });

    slides.instructions = slide({
        name : "instructions",
        button : function() {
            exp.startT = Date.now();
            exp.go(); //use exp.go() if and only if there is no "present" data.
        }
    });
  
    slides.instructions1 = slide({
        name : "instructions1",
        start : function() {
            $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
            var inst1 = "Let's get started! <br><br>Imagine you are at a party. <br><br> You walk into the kitchen and overhear somebody ask something. You'll answer questions about what the people believe and how certain they are.";
            $("#inst1").html(inst1);
        },
        button : function() {
            exp.go(); //use exp.go() if and only if there is no "present" data.
        }
    }); 
     

    slides.block1 = slide({
        name : "block1",
        present : exp.stims_block1,
        start : function() {
            $(".err").hide(); // hide the error message   
        },
        
        present_handle : function(stim) {
            $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	    
            this.stim = stim;
            this.stim.trial_start = Date.now();      
            $(".err").hide();   
            $(".certainty_err").hide() 	
            this.init_sliders();
            $(".continue_button").show(); // show the belief button
            $(".belief_slider_table").show(); // show the belief slider
            $(".question").show();
            exp.belief_sliderPost = null;
            exp.certainty_sliderPost = null;
            $(".certainty_question").hide(); // hide the certainty question in the beginning
            $(".certainty_slider_table").hide(); // hide the certainty slider in the beginning
            $(".next_button").hide(); // hide the next botton

            console.log(this.stim);    

            var utterance = "<strong> Fact (which "+this.stim.name+" knows):</strong> "+this.stim.prior_fact+".<br><br>" + this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\"";
            $(".sentence").html(utterance);

            var leftLabel =  "definitely no";
            $(".leftLabel").html(leftLabel);
            var rightLabel = "definitely yes";
            $(".rightLabel").html(rightLabel);

            // allows the content question to match with the utterance (i.e. not p in question when the embedded  is not p)
            // if (this.stim.trigger.includes("_neg")) {
            //     var question = "Does "+this.stim.name+" believe that "+this.stim.negation+"?";
            // } else {
            //     var question = "Does "+this.stim.name+" believe that "+this.stim.question+"?";
            // }
            var question = "Does "+this.stim.name+" believe that "+this.stim.question+"?";
            
            $(".question").html(question);	  
        },

        belief_button : function() {
            console.log("belief rating: "+exp.belief_sliderPost);
            console.log("default certainty rating: "+exp.certainty_sliderPost);
            
            if (exp.belief_sliderPost != null) {
                    $(".err").hide(); // have a rating, so hide the error message
                    $(".question").hide(); // hide the belief question
                    $(".belief_slider_table").hide(); // hide the belief slider
                    
                    // use this if the content question matches with the utterance (i.e. not p in question when the embedded  is not p)
                    // if ((exp.belief_sliderPost > 0.5 && this.stim.trigger.includes("_neg")) || exp.belief_sliderPost < 0.5 && !this.stim.trigger.includes("_neg")) { // 0: no, 1: yes
                    //     certainty_question = "How certain is " + this.stim.name+" about the fact that "+this.stim.negation+"?";
                    // } else { 
                    //     certainty_question = "How certain is " + this.stim.name+" about the fact that "+this.stim.question+"?";
                    // }
                    if (exp.belief_sliderPost > 0.5) {
                        certainty_question = "How certain is " + this.stim.name+" about the fact that "+this.stim.question+"?";
                    } else { 
                        certainty_question = "How certain is " + this.stim.name+" about the fact that "+this.stim.negation+"?";
                    }

                    $(".certainty_question").show();
                    $(".certainty_question").html(certainty_question);
                    
                    this.init_certainty_slider();
                    exp.certainty_sliderPost = null;
                    var certainty_leftLabel = "very uncertain";
                    $(".certainty_leftLabel").html(certainty_leftLabel);
                    var certainty_rightLabel = "very certain";
                    $(".certainty_rightLabel").html(certainty_rightLabel);
                    
                    $(".continue_button").hide();
                    $(".certainty_slider_table").show();
                    $(".next_button").show()
            } else { //  no belief rating
                $(".err").show();
            }
        },
        
        button : function() {
            console.log("certainty rating: "+exp.certainty_sliderPost);
            console.log("double check belief rating: "+exp.belief_sliderPost);
            if (exp.certainty_sliderPost != null) {
                this.log_responses();
                _stream.apply(this); //use exp.go() if and only if there is no "present" data.
            } else {
                $(".certainty_err").show();
            }
        },

        init_sliders : function() {
            utils.make_slider("#single_slider1", function(event, ui) {
                exp.belief_sliderPost = ui.value;
            });
        },

        init_certainty_slider : function() {
            utils.make_slider("#single_slider2", function(event_1, ui_1) {
                exp.certainty_sliderPost = ui_1.value;
            });
        },

        log_responses : function() {
            exp.data_trials.push({
                "block" : "block1",
                // "question_type" : this.stim.block, // only 1 block
                "slide_number_in_experiment" : exp.phase, // trial number
                "short_trigger": this.stim.short_trigger,
                "trigger": this.stim.trigger,
                "trigger_class": this.stim.trigger_class,
                "content": this.stim.content,
                // "utterance": this.stim.utterance, // record utterance for sanity check?
                "prior" : this.stim.prior,
                "prior_fact" : this.stim.prior_fact,
                // "speakerName": this.stim.name,  // speaker's name doesn't matter	  
                "belief_response" : exp.belief_sliderPost,
                "certainty_respnose" : exp.certainty_sliderPost,
                "rt" : Date.now() - this.stim.trial_start
            });
        }
    }); 
  
 
    slides.questionaire =  slide({
        name : "questionaire",
        submit : function(e){
        //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
        exp.subj_data = {
            language : $("#language").val(),
            enjoyment : $("#enjoyment").val(),
            asses : $('input[name="assess"]:checked').val(),
            age : $("#age").val(),
            gender : $("#gender").val(),
            education : $("#education").val(),
            comments : $("#comments").val(),
        };
        exp.go(); //use exp.go() if and only if there is no "present" data.
        }
    });

    slides.finished = slide({
        name : "finished",
        start : function() {
        exp.data= {
            "trials" : exp.data_trials,
            "catch_trials" : exp.catch_trials,
            "system" : exp.system,
            "condition" : exp.condition,
            "subject_information" : exp.subj_data,
            "time_in_minutes" : (Date.now() - exp.startT)/60000
        };
        setTimeout(function() {turk.submit(exp.data);}, 1000);
        }
    });
    console.log(slides);

    return slides;
}

function init() {

    var names = _.shuffle([
        {
            "name":"Christopher",
            "gender":"M"
        },
        {
            "name":"Daniel",
            "gender":"M"
        },
        {
            "name":"Matthew",
            "gender":"M"
        },
        {
            "name":"Donald",
            "gender":"M"
        },
        {
            "name":"Paul",
            "gender":"M"
        },
        {
            "name":"George",
            "gender":"M"
        },
        {
            "name":"Steven",
            "gender":"M"
        },
        {
            "name":"Kenneth",
            "gender":"M"
        },
        {
            "name":"Edward",
            "gender":"M"
        },
        {
            "name":"Brian",
            "gender":"M"
        },
        {
            "name":"Kevin",
            "gender":"M"
        },
        {
            "name":"Ronald",
            "gender":"M"
        },
        {
            "name":"Timothy",
            "gender":"M"
        },
        {
            "name":"Jason",
            "gender":"M"
        },
        {
            "name":"Jeffrey",
            "gender":"M"
        },
        {
            "name":"Gary",
            "gender":"M"
        },
        {
            "name":"Ryan",
            "gender":"M"
        },
        {
            "name":"Nicholas",
            "gender":"M"
        },
        {
            "name":"Eric",
            "gender":"M"
        },
        {
            "name":"Jacob",
            "gender":"M"
        },
        {
            "name":"Jonathan",
            "gender":"M"
        },
        {
            "name":"Larry",
            "gender":"M"
        },
        {
            "name":"Scott",
            "gender":"M"
        },
        {
            "name":"Justin",
            "gender":"M"
        },
        {
            "name":"Brandon",
            "gender":"M"
        },
        {
            "name":"Raymond",
            "gender":"M"
        },
        {
            "name":"Gregory",
            "gender":"M"
        },
        {
            "name":"Benjamin",
            "gender":"M"
        },
        {
            "name":"Patrick",
            "gender":"M"
        },
        {
            "name":"Dennis",
            "gender":"M"
        },
        {
            "name":"Jerry",
            "gender":"M"
        },
        {
            "name":"Alexander",
            "gender":"M"
        },
        {
            "name":"Tyler",
            "gender":"M"
        },
        {
            "name":"Jennifer",
            "gender":"F"
        },
        {
            "name":"Dorothy",
            "gender":"F"
        },
        {
            "name":"Karen",
            "gender":"F"
        },
        {
            "name":"Nancy",
            "gender":"F"
        },
        {
            "name":"Betty",
            "gender":"F"
        },
        {
            "name":"Lisa",
            "gender":"F"
        },
        {
            "name":"Sandra",
            "gender":"F"
        },
        {
            "name":"Ashley",
            "gender":"F"
        },
        {
            "name":"Donna",
            "gender":"F"
        },
        {
            "name":"Kimberly",
            "gender":"F"
        },
        {
            "name":"Carol",
            "gender":"F"
        },
        {
            "name":"Michelle",
            "gender":"F"
        },
        {
            "name":"Emily",
            "gender":"F"
        },
        {
            "name":"Amanda",
            "gender":"F"
        },
        {
            "name":"Melissa",
            "gender":"F"
        },
        {
            "name":"Deborah",
            "gender":"F"
        },
        {
            "name":"Laura",
            "gender":"F"
        },
        {
            "name":"Stephanie",
            "gender":"F"
        },
        {
            "name":"Rebecca",
            "gender":"F"
        },
        {
            "name":"Sharon",
            "gender":"F"
        },
        {
            "name":"Cynthia",
            "gender":"F"
        },
        {
            "name":"Kathleen",
            "gender":"F"
        },
        {
            "name":"Ruth",
            "gender":"F"
        },
        {
            "name":"Shirley",
            "gender":"F"
        },
        {
            "name":"Amy",
            "gender":"F"
        },
        {
            "name":"Angela",
            "gender":"F"
        },
        {
            "name":"Virginia",
            "gender":"F"
        },
        {
            "name":"Brenda",
            "gender":"F"
        },
        {
            "name":"Nicole",
            "gender":"F"
        },
        {
            "name":"Christina",
            "gender":"F"
        },
        {
            "name":"Carolyn",
            "gender":"F"
        },
        {
            "name":"Rachel",
            "gender":"F"
        },
        {
            "name":"Heather",
            "gender":"F"
        },
        {
            "name":"Diane",
            "gender":"F"
        },
        {
            "name":"Joyce",
            "gender":"F"
        },
        {
            "name":"Julie",
            "gender":"F"
        }
    ]);
        
    var items = _.shuffle([
        {
            "trigger":"know",
            "trigger_class":"C"
        }, 
        {
            "trigger":"know_neg",
            "trigger_class":"C"
        }, 
        {
            "trigger":"say",
            "trigger_class":"C"
        },
        {
            "trigger":"say_neg",
            "trigger_class":"C"
        }, 
        {
            "trigger":"think",
            "trigger_class":"C"
        },
        {
            "trigger":"think_neg",
            "trigger_class":"C"
        },
        {
            "trigger":"confirm",
            "trigger_class":"C"
        },
        {
            "trigger":"confirm_neg",
            "trigger_class":"C"
        },
        {
            "trigger":"inform",
            "trigger_class":"C"
        },
        {
            "trigger":"inform_neg",
            "trigger_class":"C"
        },
        {
            "trigger":"simple_polar1",
            "trigger_class":"Q"
        },
        {
            "trigger":"simple_polar2",
            "trigger_class":"Q"
        }
    ]);

    var contents = {
        "mary": {
            "question":"Mary is pregnant",
            "negation":"Mary isn't pregnant",
            "simple_polar1":"Is Mary pregnant?",
            "simple_polar2":"Is Mary pregnant?",
            "know":"Does Mandy know that Mary is pregnant?",
            "know_neg":"Does Manday know that Mary isn't pregnant?",
            "say":"Did Mandy say that Mary is pregnant?",
            "say_neg":"Did Manday say that Mary isn't pregnant?",
            "think":"Does Mandy think that Mary is pregnant?",
            "think_neg":"Does Mandy think that Mary isn't pregnant?",
            "confirm":"Did Mandy confirm that Mary is pregnant?",
            "confirm_neg":"Did Mandy confirm that Mary isn't pregnant?",
            "inform":"Did Mandy inform Sam that Mary is pregnant?",
            "inform_neg":"Did Mandy inform Sam that Mary isn't pregnant?",
            "low_prior": "Mary is a middle school student",
            "high_prior": "Mary is taking a prenatal yoga class"
        },
        "josie": {
            "question":"Josie went on vacation to France",
            "negation":"Josie didn't go on vacation to France.",
            "simple_polar1":"Did Josie go on vacation to France?",
            "simple_polar2":"Did Josie go on vacation to France?",
            "know":"Does Sarah know that Josie went on vacation to France?",
            "know_neg":"Does Sarah know that Josie didn't go on vacation to France?",
            "say":"Did Sarah say that Josie went on vacation to France?",
            "say_neg":"Did Sarah say that Josie didn't go on vacation to France?",
            "think":"Does Sarah think that Josie went on vacation to France?",
            "think_neg":"Does Sarah think that Josie didn't go on vacation to France?",
            "confirm":"Did Sarah confirm that Josie went on vacation to France?",
            "confirm_neg":"Did Sarah confirm that Josie didn't go on vacation to France?",
            "inform":"Did Sarah inform Sam that Josie went on vacation to France?",
            "inform_neg":"Did Sarah inform Sam that Josie didn't go on vacation to France?",
            "low_prior": "Josie doesn't have a passport",
            "high_prior": "Josie loves France"
        },
        "olivia": {
            "question":"Olivia sleeps until noon",
            "negation":"Olivia doesn't sleep until noon",
            "simple_polar1":"Does Olivia sleep until noon?",
            "simple_polar2":"Does Olivia sleep until noon?",
            "know":"Does Jane know that Olivia sleeps until noon?",
            "know_neg":"Does Jane know that Olivia doesn't sleep until noon?",
            "say":"Did Jane say that Olivia sleeps until noon?",
            "say_neg":"Does Jane say that Olivia doesn't sleep until noon?",
            "think":"Does Jane think that Olivia sleeps until noon?",
            "think_neg":"Does Jane think that Olivia doesn't sleep until noon?",
            "confirm":"Did Jane confirm that Olivia sleeps until noon?",
            "confirm_neg":"Did Jane confirm that Olivia doesn't sleep until noon?",
            "inform":"Did Jane inform Sam that Olivia sleeps until noon?",
            "inform_neg":"Did Jane inform Sam that Olivia doesn't sleep until noon?",
            "low_prior": "Olivia has two small children",
            "high_prior": "Olivia works the third shift"
        },
        "grace": {
            "question":"Grace visited her sister",
            "negation":"Grace didn't visit her sister",
            "simple_polar1":"Did Grace visit her sister?",
            "simple_polar2":"Did Grace visit her sister?",
            "know":"Does Andrew know that Grace visited her sister?",
            "know_neg":"Does Andrew know that Grace didn't visit her sister?",
            "say":"Did Andrew say that Grace visited her sister?",
            "say_neg":"Did Andrew say that Grace didn't visit her sister?",
            "think":"Does Andrew think that Grace visited her sister?",
            "think_neg":"Does Andrew think that Grace didn't visit her sister?",
            "confirm":"Did Andrew confirm that Grace visited her sister?",
            "confirm_neg":"Did Andrew confirm that Grace didn't visit her sister?",
            "inform":"Did Andrew inform Sam that Grace visited her sister?",
            "inform_neg":"Did Andrew inform Sam that Grace didn't visit her sister?",
            "low_prior": "Grace hates her sister",
            "high_prior": "Grace loves her sister"
        },
        "zoe": {
            "question":"Zoe calculated the tip",
            "negation":"Zoe didn't calculate the tip",
            "simple_polar1":"Did Zoe calculate the tip?",
            "simple_polar2":"Did Zoe calculate the tip?",
            "know":"Does Mark know that Zoe calculated the tip?",
            "know_neg":"Does Mark know that Zoe didn't calculate the tip?",
            "say":"Did Mark say that Zoe calculated the tip?",
            "say_neg":"Did Mark say that Zoe didn't calculate the tip?",
            "think":"Does Mark think that Zoe calculated the tip?",
            "think_neg":"Did Mark think that Zoe didn't calculate the tip?",
            "confirm":"Did Mark confirm that Zoe calculated the tip?",
            "confirm_neg":"Did Mark confirm that Zoe didn't calculate the tip?",
            "inform":"Did Mark inform Sam that Zoe calculated the tip?",
            "inform_neg":"Did Mark inform Sam that Zoe didn't calculate the tip?",
            "low_prior": "Zoe is 5 years old",
            "high_prior": "Zoe is a math major"
        },
        "frank": {
            "question":"Frank got a cat",
            "negation":"Frank didn't get a cat",
            "simple_polar1":"Did Frank get a cat?",
            "simple_polar2":"Did Frank get a cat?",
            "know":"Does Walt know that Frank got a cat?",
            "know_neg":"Does Walt know that Frank didn't get a cat?",
            "say":"Did Walt say that Frank got a cat?",
            "say_neg":"Did Walt say that Frank didn't get a cat?",
            "think":"Does Walt think that Frank got a cat?",
            "think_neg":"Does Walt think that Frank didn't get a cat?",
            "confirm":"Did Walt confirm that Frank got a cat?",
            "confirm_neg":"Did Walt confirm that Frank didn't get a cat?",
            "inform":"Did Walt inform Sam that Frank got a cat?",
            "inform_neg":"Did Walt inform Sam that Frank didn't get a cat?",
            "low_prior": "Frank is allergic to cats",
            "high_prior": "Frank has always wanted a pet"
        },
        "jackson": {
            "question":"Jackson ran 10 miles",
            "negation":"Jackson didn't run 10 miles",
            "simple_polar1":"Did Jackson run 10 miles?",
            "simple_polar2":"Did Jackson run 10 miles?",
            "know":"Does Randy know that Jackson ran 10 miles?",
            "know_neg":"Does Randy know that Jackson didn't run 10 miles?",
            "say":"Did Randy say that Jackson ran 10 miles?",
            "say_neg":"Did Randy say that Jackson didn't run 10 miles?",
            "think":"Does Randy think that Jackson ran 10 miles?",
            "think_neg":"Did Randy think that Jackson didn't run 10 miles?",
            "confirm":"Did Randy confirm that Jackson ran 10 miles?",
            "confirm_neg":"Did Randy confirm that Jackson didn't run 10 miles?",
            "inform":"Did Randy inform Sam that Jackson ran 10 miles?",
            "inform_neg":"Did Randy inform Sam that Jackson didn't run 10 miles?",
            "low_prior": "Jackson is obese",
            "high_prior": "Jackson is training for a marathon"
        },
        "jayden": {
            "question":"Jayden rented a car",
            "negation":"Jayden didn't rent a car",
            "simple_polar1":"Did Jayden rent a car?",
            "simple_polar2":"Did Jayden rent a car?",
            "know":"Does Herbert know that Jayden rented a car?",
            "know_neg":"Does Herbert know that Jayden didn't rent a car?",
            "say":"Did Herbert say that Jayden rented a car?",
            "say_neg":"Did Herbert say that Jayden didn't rent a car?",
            "think":"Does Herbert think that Jayden rented a car?",
            "think_neg":"Did Herbert think that Jayden didn't rent a car?",
            "confirm":"Did Herbert confirm that Jayden rented a car?",
            "confirm_neg":"Did Herbert confirm that Jayden didn't rent a car?",
            "inform":"Did Herbert inform Sam that Jayden rented a car?",
            "inform_neg":"Did Herbert inform Sam that Jayden didn't rent a car?",
            "low_prior": "Jayden doesn't have a driver's license",
            "high_prior": "Jayden's car is in the shop"
        },
        "tony": {
            "question":"Tony had a drink last night",
            "negation":"Tony didn't have a drink last night",
            "simple_polar1":"Did Tony have a drink last night?",
            "simple_polar2":"Did Tony have a drink last night?",
            "know":"Does Helen know that Tony had a drink last night?",
            "know_neg":"Does Helen know that Tony didn't have a drink last night?",
            "say":"Did Helen say that Tony had a drink last night?",
            "say_neg":"Did Helen say that Tony didn't have a drink last night?",
            "think":"Does Helen think that Tony had a drink last night?",
            "think_neg":"Does Helen think that Tony didn't have a drink last night?",
            "confirm":"Did Helen confirm that Tony had a drink last night?",
            "confirm_neg":"Does Helen confirm that Tony didn't have a drink last night?",
            "inform":"Did Helen inform Sam that Tony had a drink last night?",
            "inform_neg":"Does Helen inform Sam that Tony didn't have a drink last night?",
            "low_prior": "Tony has been sober for 20 years",
            "high_prior": "Tony really likes to party with his friends"
        },
        "owen": {
            "question":"Owen shoveled snow last winter",
            "negation":"Owen didn't shovel snow last night",
            "simple_polar1":"Did Owen shovel snow last winter?",
            "simple_polar2":"Did Owen shovel snow last winter?",
            "know":"Does Jordan know that Owen shoveled snow last winter?",
            "know_neg":"Does Jordan know that Owen didn't shoveled snow last winter?",
            "say":"Did Jordan say that Owen shoveled snow last winter?",
            "say_neg":"Did Jordan say that Owen didn't shovel snow last winter?",
            "confirm":"Did Jordan confirm that Owen shoveled snow last winter?",
            "confirm_neg":"Did Jordan confirm that Owen didn't shovel snow last winter?",
            "inform":"Did Jordan inform Sam that Owen shoveled snow last winter?",
            "inform_neg":"Did Jordan inform Sam that Owen didn't shovel snow last night?",
            "low_prior": "Owen lives in New Orleans",
            "high_prior": "Owen lives in Chicago"
        },
        "jon": {
            "question":"Jon walks to work",
            "negation":"Jon doesn't walk to work",
            "simple_polar1":"Does Jon walk to work?",
            "simple_polar2":"Does Jon walk to work?",
            "know":"Does Dexter know that Jon walks to work?",
            "know_neg":"Does Dexter know that Jon doesn't walk to work?",
            "say":"Did Dexter say that Jon walks to work?",
            "say_neg":"Did Dexter say that Jon doesn't walk to work?",
            "think":"Does Dexter think that Jon walks to work?",
            "think_neg":"Does Dexter think that Jon doesn't walk to work?",
            "confirm":"Did Dexter confirm that Jon walks to work?",
            "confirm_neg":"Did Dexter confirm that Jon doesn't walk to work?",
            "inform":"Did Dexter inform Sam that Jon walks to work?",
            "inform_neg":"Did Dexter inform Sam that Jon doesn't walk to work?",
            "low_prior": "Jon lives 10 miles away from work",
            "high_prior": "Jon lives 2 blocks away from work"
        },
        "charley": {
            "question":"Charley speaks Spanish",
            "neagtion":"Charley doesn't speak Spanish",
            "simple_polar1":"Does Charley speak Spanish?",
            "simple_polar2":"Does Charley speak Spanish?",
            "know":"Does Anton know that Charley speaks Spanish?",
            "know_neg":"Does Anton know that Charley doesn't speak Spanish?",
            "say":"Did Anton say that Charley speaks Spanish?",
            "say_neg":"Does Anton know that Charley doesn't speak Spanish?",
            "think":"Does Anton think that Charley speaks Spanish?",
            "think_neg":"Does Anton think that Charley doesn't speak Spanish?",
            "confirm":"Did Anton confirm that Charley speaks Spanish?",
            "confirm_neg":"Did Anton confirm that Charley doesn't speak Spanish?",
            "inform":"Did Anton inform Sam that Charley speaks Spanish?",
            "inform_neg":"Did Anton inform Sam that Charley doesn't speak Spanish?",
            "low_prior": "Charley lives in Korea",
            "high_prior": "Charley lives in Mexico"
        }
    }

    var items_content_mapping = {
        "know":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "know_neg":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "say":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "say_neg":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "think":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "think_neg":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "confirm":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "confirm_neg":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "inform":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "inform_neg":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "simple_polar1":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"],
        "simple_polar2":["mary","josie","olivia","grace","zoe","frank","jackson","jayden","tony","owen","jon","charley"]
    };

    // controls
    var mcitemnames = ["muffins","pizza","kids","ballet","garage","hat"];
    var mcitems = {
        "muffins": {
            "question":"these muffins have blueberries in them",
            "negation":"these muffins don't have blueberries in them",
            "MCq":"Do these muffins have blueberries in them?",
            "MCa":"These muffins have blueberries in them.",
            "prior_fact": "Muffins are sold at the bakery"},
        "pizza": {
            "question":"this pizza has mushrooms on it",
            "negation":"this pizza doesn't have mushrooms on it",
            "MCq":"Does this pizza have mushrooms on it?",
            "MCa":"This pizza has mushrooms on it.",
            "prior_fact": "Pizza is sold at the pizzeria"},
        "kids": {
            "question":"Jack was playing outside with the kids",
            "negation":"Jack wasn't playing outside with the kids",
            "MCq":"Was Jack playing outside with the kids?",
            "MCa":"Jack was playing outside with the kids.",
            "prior_fact": "Many children like ice cream"},
        "ballet": {
            "question":"Ann dances ballet",
            "negation":"Ann doesn't dance ballet",
            "MCq":"Does Ann dance ballet?",
            "MCa":"Ann dances ballet.",
            "prior_fact": "Ballet is a type of dance"},
        "garage": {
            "question":"Carl's kids were in the garage",
            "negation":"Carl's kids weren't in the garage",
            "MCq":"Were Carl's kids in the garage?",
            "MCa":"Carl's kids were in the garage.",
            "prior_fact": "Garages are used to store cars and other things"},
        "hat": {
            "question":"Samantha has a new hat",
            "negation":"Samantha doesn't have a new hat",
            "MCq":"Does Samantha have a new hat?",
            "MCa":"Samantha has a new hat.",
            "prior_fact": "Hats are worn on the head"}
    };

    // get trigger contents
    // getContent is called for a particular trigger (e.g., "know")
    // it then gets an array of contents for that trigger and removes the first element and
    // returns that first element (shift)
    // then it loops through all arrays in items_content_mapping, finds the index of the
    // content that the trigger was just paired with and removes it from that array
    // so that no other trigger can be paired with it
    function getContent(trigger) {
        // shuffle the list of contents for the corresponding trigger (predicate)
        items_content_mapping[trigger] = _.shuffle(items_content_mapping[trigger]);
        // get the first content (tag by the PERSON in content/embedded clause)
        // each content includes: 1. the content itself ("question"), 2. the simple polar 
        // 3-23. trigger+content (predicate), 24. low probability fact ("low_prior"),
        // 25. high probability fact ("high_prior")
        var content = items_content_mapping[trigger].shift();  	
        for (var j in items_content_mapping) {
            var index = items_content_mapping[j].indexOf(content);  		
            if (index != -1) {
                // remove the element at [index] 
                items_content_mapping[j].splice(index,1);			
            }		
        }		  	
        // 	return the selected content
        return content;
    }


    var trigger_contents = {
        "know": getContent("know"),
        "know_neg": getContent("know_neg"),  	   	
        "say": getContent("say"),
        "say_neg": getContent("say_neg"),
        "think": getContent("think"),
        "think_neg": getContent("think_neg"),
        "confirm": getContent("confirm"),
        "confirm_neg": getContent("confirm_neg"),
        "inform": getContent("inform"),
        "inform_neg": getContent("inform_neg"),
        "simple_polar1": getContent("simple_polar1"),
        "simple_polar2": getContent("simple_polar2")
    }

    // create the stimulus set
    // makeStim gets a trigger from items, gets a name to create the item
    // then it calls the getContent function for that trigger, which returns a unique content
    // then it gets the utterance and question for that trigger/content combination
    // and returns: name, gender, trigger, content, utterance, question for that trigger
    function makeStim(i) {
        //get item
        var item = items[i];
        //get a speaker
        // var name_data = names[i];
        var name_data = names.pop();
        // console.log(name_data);
        var name = name_data.name;
        var gender = name_data.gender;
        // get content
        var trigger_cont = trigger_contents[item.trigger];
        var trigger = item.trigger;
        var short_trigger = trigger;
        if (trigger.indexOf("MC") != -1) {
            short_trigger = "MC";
        }
        var utterance = contents[trigger_cont][short_trigger];
        if (trigger.includes("_neg")) {}
        var question = contents[trigger_cont].question;
        var negation = contents[trigger_cont].negation;
        return {
            "name": name,
            "gender": gender,	  
            "trigger": item.trigger,
            "short_trigger": short_trigger,	  // short_trigger is the same as trigger?
            "trigger_class": item.trigger_class,
            "content": trigger_cont,
            "utterance": utterance,
            "question": question,
            "negation": negation
        }
    }

    // create the control set
    function makeMCStim(ind,j) {
        // get item
        var item = mcitems[j];
        // get a speaker
        // var name_data = names[ind];
        // console.log("MC names");
        var name_data = names.pop();
        // console.log(name_data);
        var name = name_data.name;
        var gender = name_data.gender;
        // get content
        var trigger_cont = j;
        var trigger = "MC";
        var short_trigger = "MC";

        var utterance = mcitems[j].MCq;
        var question = mcitems[j].question;  
        var negation = mcitems[j].negation;
        var prior_fact = mcitems[j].prior_fact; 
    //    console.log(contents[trigger_cont]); 
        return {
            "name": name,
            "gender": gender,   
            "trigger": trigger,
            "short_trigger": short_trigger,   
            "trigger_class": "MC",
            "content": trigger_cont,
            "utterance": utterance,
            "question": question,
            "negation": negation,
            "prior_fact": prior_fact
        }
    }  
      
    exp.stims_block1 = [];
    
    // loop through the triggers, make each of them a stimuli, and add 
    // them to the stimuli set in block 1 
    for (var i=0; i<items.length; i++) {
        var stim = makeStim(i);
        exp.stims_block1.push(jQuery.extend(true, {}, stim));
    }

	exp.stims_block1 = _.shuffle(exp.stims_block1); 

    // I don't understand why here the block type and the prior type are already fixed	  
    
    // half of the stimuli will be paired with low probability fact
    for (var k=0; k<items.length/2; k++) {
        var content = exp.stims_block1[k].content;
        exp.stims_block1[k].prior = "low_prior";
        if (exp.stims_block1[k].trigger.includes("_neg")) {
            // for negated content, the original high prob fact becomes the low prior fact
            // if we assume that P(not p) = 1 - P(p)
            exp.stims_block1[k].prior_fact = contents[content]["high_prior"]	
        } else {
            exp.stims_block1[k].prior_fact = contents[content]["low_prior"]	
        }
    }  
    
    // the other half will be paired with hight probability fact
    for (var j=items.length/2; j<items.length; j++) {
        var content = exp.stims_block1[j].content;
        exp.stims_block1[j].prior = "high_prior";
        if (exp.stims_block1[k].trigger.includes("_neg")) {
            exp.stims_block1[j].prior_fact = contents[content]["low_prior"]
        } else {
            exp.stims_block1[j].prior_fact = contents[content]["high_prior"]
        }
    }    

    // add the control items
    for (var l=0; l<mcitemnames.length; l++) {
        var stim = makeMCStim(l,mcitemnames[l]);
        exp.stims_block1.push(jQuery.extend(true, {}, stim));
    }  
  
    // original comment: here things are bad already because some stim ai, some projective

    // randomize the items within each block
	exp.stims_block1 = _.shuffle(exp.stims_block1); 
    console.log(exp.stims_block1) 

    // only one block. doesn't matter
    // for (var k=0; k<exp.stims_block1.length; k++) {  	
    //     exp.stims_block1[k].block = "projective";
    // }

    
    /// JT: HERE'S THE PART I DON'T UNDERSTAND
    exp.trials = [];
    exp.catch_trials = [];
    exp.condition = {}; // can randomize between subject conditions here -> not needed?
    exp.system = {
        Browser : BrowserDetect.browser,
        OS : BrowserDetect.OS,
        screenH: screen.height,
        screenUH: exp.height,
        screenW: screen.width,
        screenUW: exp.width
    };
    //blocks of the experiment:
    exp.structure=["bot", "i0", "instructions1", "block1", 'questionaire', 'finished'];
    console.log(exp.structure);

    exp.data_trials = [];
    //make corresponding slides:
    exp.slides = make_slides(exp);

    //   exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                        //relies on structure and slides being defined
                        
    exp.nQs = 3 + 18 + 1; 
    $(".nQs").html(exp.nQs);

    $('.slide').hide(); //hide everything

    // make sure turkers have accepted HIT (or you're not in mturk)
    $("#start_button").click(function() {
        if (turk.previewMode) {
            $("#mustaccept").show();
        } else {
            $("#start_button").click(function() {$("#mustaccept").show();});
            exp.go();
        }
    });

    exp.go(); //show first slide
}
