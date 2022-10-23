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

    // prior intruction+block
    slides.instructions0 = slide({
        name : "instructions0",
        start : function() {
        $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
            var inst0 = "Let's get started!  ";
            inst0 = inst0 + "Tell us how likely the events are, given the facts.";
            $("#inst0").html(inst0);
        },
        button : function() {
            exp.go(); //use exp.go() if and only if there is no "present" data.
        }
    }); 

  
    slides.instructions1 = slide({
        name : "instructions1",
        start : function() {
        $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	
            var inst1 = "Let's get started! ";
    //    	console.log(block_order);
            if (exp.stims_block1[0].block == "prior") {
                inst1 = inst1 + "<br><br>Tell us how likely the events are, given the facts."
            } else {
                inst1 = inst1 + "<br><br>Imagine you are at a party. <br><br> You walk into the kitchen and overhear somebody ask something. You'll answer questions about what the people believe."    		
            }
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

            var utterance = "";
            if (this.stim.block == "prior") {
                utterance = "<strong> Fact:</strong> "+this.stim.prior_fact+".<br>";
            } else {
                utterance = "<strong> Fact (which "+this.stim.name+" knows):</strong> "+this.stim.prior_fact+".<br><br>" + this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""	  	
            }
            $(".sentence").html(utterance);

            var leftLabel = "";
            if (this.stim.block == "prior") {
                leftLabel = "impossible";
            } else {
                leftLabel = "no"
            }
            $(".leftLabel").html(leftLabel);
            var rightLabel = "";
            if (this.stim.block == "prior") {
                rightLabel = "definitely";
            } else {
                rightLabel = "yes"
            }
            $(".rightLabel").html(rightLabel);
            
            var question = "";
            if (this.stim.block == "prior") {
                question = question = "How likely is it that "+this.stim.question+"?";
            } else {
                question = "Does "+this.stim.name+" believe that "+this.stim.question+"?";	  	
            }

            $(".question").html(question);	  
        },

        belief_button : function() {
            console.log("belief rating: "+exp.belief_sliderPost);
            console.log("default certainty rating: "+exp.certainty_sliderPost);
            
            if (this.stim.block == "projective") { // projective block
                if (exp.belief_sliderPost != null) {
                    $(".err").hide(); // have a rating, so hide the error message
                    $(".question").hide(); // hide the belief question
                    $(".belief_slider_table").hide(); // hide the belief slider
                    
                    if (exp.belief_sliderPost > 0.5) { // 0: no, 1: yes
                        certainty_question = "How certain is " + this.stim.name+" about the fact that "+this.stim.question+"?";
                    } else { // FIX THE NEGATION 
                        certainty_question = "How certain is " + this.stim.name+" about the fact that not "+this.stim.question+"?";
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
                } else { // projective block but no belief rating
                    $(".err").show();
                }
            } else { // prior block
                if (exp.belief_sliderPost != null) {
                    this.log_responses();
                    _stream.apply(this);
                } else {
                    $(".err").show();
                }
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
                "question_type" : this.stim.block,     
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

    slides.instructions2 = slide({
        name : "instructions2",
        start : function() {
        $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	
            var inst2 = "That was the first of the two parts! ";
            if (exp.stims_block2[0].block == "prior") {
                inst2 = inst2 + "<br><br>Now tell us how likely the events are, given the facts."
            } else {
                inst2 = inst2 + "<br><br>Now imagine you are at a party. <br><br> You walk into the kitchen and overhear somebody ask something. You'll answer questions about what the people believe."    		
                }
            $("#inst2").html(inst2);
        },
        button : function() {
          exp.go(); //use exp.go() if and only if there is no "present" data.
        }
    });   
      
    slides.block2 = slide({
        name : "block2",
        present : exp.stims_block2,
        start : function() {
        $('.bar').css('width', ( (100*(exp.phase)/exp.nQs) + "%"));    	    	
            $(".err").hide();
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

            var utterance = "";
            if (this.stim.block == "prior") {
                utterance = "<strong> Fact:</strong> "+this.stim.prior_fact+".<br>";
            } else {
                utterance = "<strong> Fact (which "+this.stim.name+" knows):</strong> "+this.stim.prior_fact+".<br><br>" + this.stim.name + " asks: \"<strong><i>"+this.stim.utterance+"</i></strong>\""	  	
            }
            $(".sentence").html(utterance);

            var leftLabel = "";
            if (this.stim.block == "prior") {
                leftLabel = "impossible";
            } else {
                leftLabel = "no"
            }
            $(".leftLabel").html(leftLabel);
            var rightLabel = "";
            if (this.stim.block == "prior") {
                rightLabel = "definitely";
            } else {
                rightLabel = "yes"
            }
            $(".rightLabel").html(rightLabel);
            
            var question = "";
            if (this.stim.block == "prior") {
                question = question = "How likely is it that "+this.stim.question+"?";
            } else {
                question = "Does "+this.stim.name+" believe that "+this.stim.question+"?";	  	
            }

            $(".question").html(question);	  
        },

        belief_button : function() {
            console.log("belief rating: "+exp.belief_sliderPost);
            console.log("default certainty rating: "+exp.certainty_sliderPost);
            
            if (this.stim.block == "projective") { // projective block
                if (exp.belief_sliderPost != null) {
                    $(".err").hide(); // have a rating, so hide the error message
                    $(".question").hide(); // hide the belief question
                    $(".belief_slider_table").hide(); // hide the belief slider
                    
                    if (exp.belief_sliderPost > 0.5) { // 0: no, 1: yes
                        certainty_question = "How certain is " + this.stim.name+" about the fact that "+this.stim.question+"?";
                    } else {
                        certainty_question = "How certain is " + this.stim.name+" about the fact that not "+this.stim.question+"?";
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
                } else { // projective block but no belief rating
                    $(".err").show();
                }
            } else { // prior block
                if (exp.belief_sliderPost != null) {
                    this.log_responses();
                    _stream.apply(this);
                } else {
                    $(".err").show();
                }
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
            utils.make_slider("#single_slider3", function(event, ui) {
                exp.belief_sliderPost = ui.value;
            });
        },

        init_certainty_slider : function() {
            utils.make_slider("#single_slider4", function(event_1, ui_1) {
                exp.certainty_sliderPost = ui_1.value;
            });
        },

        log_responses : function() {
            exp.data_trials.push({
                "block" : "block1",
                "question_type" : this.stim.block,     
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
            "trigger":"be_annoyed",
            "trigger_class":"C"
        }, 
        {
            "trigger":"discover",
            "trigger_class":"C"
        }, 
        {
            "trigger":"know",
            "trigger_class":"C"
        }, 
        {
            "trigger":"reveal",
            "trigger_class":"C"
        },
        {
            "trigger":"see",
            "trigger_class":"C"
        },
        {
            "trigger":"pretend",
            "trigger_class":"C"
        }, 
        {
            "trigger":"suggest",
            "trigger_class":"C"
        }, 
        {
            "trigger":"say",
            "trigger_class":"C"
        }, 
        {
            "trigger":"think",
            "trigger_class":"C"
        },
        {
            "trigger":"be_right",
            "trigger_class":"C"
        },
        {
            "trigger":"demonstrate",
            "trigger_class":"C"
        },
        {
            "trigger":"acknowledge",
            "trigger_class":"C"
        },
        {
            "trigger":"admit",
            "trigger_class":"C"
        },
        {
            "trigger":"announce",
            "trigger_class":"C"
        },
        {
            "trigger":"confess",
            "trigger_class":"C"
        },
        {
            "trigger":"confirm",
            "trigger_class":"C"
        },
        {
            "trigger":"establish",
            "trigger_class":"C"
        },
        {
            "trigger":"hear",
            "trigger_class":"C"
        },
        {
            "trigger":"inform",
            "trigger_class":"C"
        },
        {
            "trigger":"prove",
            "trigger_class":"C"
        },
        {
            "trigger":"simple_polar",
            "trigger_class":"Q"
        }
    ]);

    var contents = {
        "mary": {
            "question":"Mary is pregnant",
            "simple_polar":"Is Mary pregnant?",
            "be_annoyed":"Is Mandy annoyed that Mary is pregnant?",
            "discover":"Did Mandy discover that Mary is pregnant?",
            "know":"Does Mandy know that Mary is pregnant?",
            "reveal":"Did Mandy reveal that Mary is pregnant?",
            "see":"Did Mandy see that Mary is pregnant?",
            "pretend":"Did Mandy pretend that Mary is pregnant?",
            "suggest":"Did Mandy suggest that Mary is pregnant?",
            "say":"Did Mandy say that Mary is pregnant?",
            "think":"Does Mandy think that Mary is pregnant?",
            "be_right":"Is Mandy right that Mary is pregnant?",
            "demonstrate":"Did Mandy demonstrate that Mary is pregnant?",
            "acknowledge":"Did Mandy acknowledge that Mary is pregnant?",
            "admit":"Did Mandy admit that Mary is pregnant?",
            "announce":"Did Mandy announce that Mary is pregnant?",
            "confess":"Did Mandy confess that Mary is pregnant?",
            "confirm":"Did Mandy confirm that Mary is pregnant?",
            "establish":"Did Mandy establish that Mary is pregnant?",
            "hear":"Did Mandy hear that Mary is pregnant?",
            "inform":"Did Mandy inform Sam that Mary is pregnant?",
            "prove":"Did Mandy prove that Mary is pregnant?",
            "low_prior": "Mary is a middle school student",
            "high_prior": "Mary is taking a prenatal yoga class"
        },
        "josie": {
            "question":"Josie went on vacation to France",
            "simple_polar":"Did Josie go on vacation to France?",
            "be_annoyed":"Is Sarah annoyed that Josie went on vacation to France?",
            "discover":"Did Sarah discover that Josie went on vacation to France?",
            "know":"Does Sarah know that Josie went on vacation to France?",
            "reveal":"Did Sarah reveal that Josie went on vacation to France?",
            "see":"Did Sarah see that Josie went on vacation to France?",
            "pretend":"Did Sarah pretend that Josie went on vacation to France?",
            "suggest":"Did Sarah suggest that Josie went on vacation to France?",
            "say":"Did Sarah say that Josie went on vacation to France?",
            "think":"Does Sarah think that Josie went on vacation to France?",
            "be_right":"Is Sarah right that Josie went on vacation to France?",
            "demonstrate":"Did Sarah demonstrate that Josie went on vacation to France?",
            "acknowledge":"Did Sarah acknowledge that Josie went on vacation to France?",
            "admit":"Did Sarah admit that Josie went on vacation to France?",
            "announce":"Did Sarah announce that Josie went on vacation to France?",
            "confess":"Did Sarah confess that Josie went on vacation to France?",
            "confirm":"Did Sarah confirm that Josie went on vacation to France?",
            "establish":"Did Sarah establish that Josie went on vacation to France?",
            "hear":"Did Sarah hear that Josie went on vacation to France?",
            "inform":"Did Sarah inform Sam that Josie went on vacation to France?",
            "prove":"Did Sarah prove that Josie went on vacation to France?",
            "low_prior": "Josie doesn't have a passport",
            "high_prior": "Josie loves France"
        },
        "emma": {
            "question":"Emma studied on Saturday morning",
            "simple_polar":"Did Emma study on Saturday morning?",
            "be_annoyed":"Is Kim annoyed that Emma studied on Saturday morning?",
            "discover":"Did Kim discover that Emma studied on Saturday morning?",
            "know":"Does Kim know that Emma studied on Saturday morning?",
            "reveal":"Did Kim reveal that Emma studied on Saturday morning?",
            "see":"Did Kim see that Emma studied on Saturday morning?",
            "pretend":"Did Kim pretend that Emma studied on Saturday morning?",
            "suggest":"Did Kim suggest that Emma studied on Saturday morning?",
            "say":"Did Kim say that Emma studied on Saturday morning?",
            "think":"Does Kim think that Emma studied on Saturday morning?",
            "be_right":"Is Kim right that Emma studied on Saturday morning?",
            "demonstrate":"Did Kim demonstrate that Emma studied on Saturday morning?",
            "acknowledge":"Did Kim acknowledge that Emma studied on Saturday morning?",
            "admit":"Did Kim admit that Emma studied on Saturday morning?",
            "announce":"Did Kim announce that Emma studied on Saturday morning?",
            "confess":"Did Kim confess that Emma studied on Saturday morning?",
            "confirm":"Did Kim confirm that Emma studied on Saturday morning?",
            "establish":"Did Kim establish that Emma studied on Saturday morning?",
            "hear":"Did Kim hear that Emma studied on Saturday morning?",
            "inform":"Did Kim inform Sam that Emma studied on Saturday morning?",
            "prove":"Did Kim prove that Emma studied on Saturday morning?",
            "low_prior": "Emma is in first grade",
            "high_prior": "Emma is in law school"
        },
        "olivia": {
            "question":"Olivia sleeps until noon",
            "simple_polar":"Does Olivia sleep until noon?",
            "be_annoyed":"Is Jane annoyed that Olivia sleeps until noon?",
            "discover":"Did Jane discover that Olivia sleeps until noon?",
            "know":"Does Jane know that Olivia sleeps until noon?",
            "reveal":"Did Jane reveal that Olivia sleeps until noon?",
            "see":"Did Jane see that Olivia sleeps until noon?",
            "pretend":"Did Jane pretend that Olivia sleeps until noon?",
            "suggest":"Did Jane suggest that Olivia sleeps until noon?",
            "say":"Did Jane say that Olivia sleeps until noon?",
            "think":"Does Jane think that Olivia sleeps until noon?",
            "be_right":"Is Jane right that Olivia sleeps until noon?",
            "demonstrate":"Did Jane demonstrate that Olivia sleeps until noon?",
            "acknowledge":"Did Jane acknowledge that Olivia sleeps until noon?",
            "admit":"Did Jane admit that Olivia sleeps until noon?",
            "announce":"Did Jane announce that Olivia sleeps until noon?",
            "confess":"Did Jane confess that Olivia sleeps until noon?",
            "confirm":"Did Jane confirm that Olivia sleeps until noon?",
            "establish":"Did Jane establish that Olivia sleeps until noon?",
            "hear":"Did Jane hear that Olivia sleeps until noon?",
            "inform":"Did Jane inform Sam that Olivia sleeps until noon?",
            "prove":"Did Jane prove that Olivia sleeps until noon?",
            "low_prior": "Olivia has two small children",
            "high_prior": "Olivia works the third shift"
        },
        "sophia": {
            "question":"Sophia got a tattoo",
            "simple_polar":"Did Sophia get a tattoo?",
            "be_annoyed":"Is Claudia annoyed that Sophia got a tattoo?",
            "discover":"Did Claudia discover that Sophia got a tattoo?",
            "know":"Does Claudia know that Sophia got a tattoo?",
            "reveal":"Did Claudia reveal that Sophia got a tattoo?",
            "see":"Did Claudia see that Sophia got a tattoo?",
            "pretend":"Did Claudia pretend that Sophia got a tattoo?",
            "suggest":"Did Claudia suggest that Sophia got a tattoo?",
            "say":"Did Claudia say that Sophia got a tattoo?",
            "think":"Does Claudia think that Sophia got a tattoo?",
            "be_right":"Is Claudia right that Sophia got a tattoo?",
            "demonstrate":"Did Claudia demonstrate that Sophia got a tattoo?",
            "acknowledge":"Did Claudia acknowledge that Sophia got a tattoo?",
            "admit":"Did Claudia admit that Sophia got a tattoo?",
            "announce":"Did Claudia announce that Sophia got a tattoo?",
            "confess":"Did Claudia confess that Sophia got a tattoo?",
            "confirm":"Did Claudia confirm that Sophia got a tattoo?",
            "establish":"Did Claudia establish that Sophia got a tattoo?",
            "hear":"Did Claudia hear that Sophia got a tattoo?",
            "inform":"Did Claudia inform Sam that Sophia got a tattoo?",
            "prove":"Did Claudia prove that Sophia got a tattoo?",
            "low_prior": "Sophia is a high end fashion model",
            "high_prior": "Sophia is a hipster"
        },
        "mia": {
            "question":"Mia drank 2 cocktails last night",
            "simple_polar":"Did Mia drink 2 cocktails last night?",
            "be_annoyed":"Is Frank annoyed that Mia drank 2 cocktails last night?",
            "discover":"Did Frank discover that Mia drank 2 cocktails last night?",
            "know":"Does Frank know that Mia drank 2 cocktails last night?",
            "reveal":"Did Frank reveal that Mia drank 2 cocktails last night?",
            "see":"Did Frank see that Mia drank 2 cocktails last night?",
            "pretend":"Did Frank pretend that Mia drank 2 cocktails last night?",
            "suggest":"Did Frank suggest that Mia drank 2 cocktails last night?",
            "say":"Did Frank say that Mia drank 2 cocktails last night?",
            "think":"Does Frank think that Mia drank 2 cocktails last night?",
            "be_right":"Is Frank right that Mia drank 2 cocktails last night?",
            "demonstrate":"Did Frank demonstrate that Mia drank 2 cocktails last night?",
            "acknowledge":"Did Frank acknowledge that Mia drank 2 cocktails last night?",
            "admit":"Did Frank admit that Mia drank 2 cocktails last night?",
            "announce":"Did Frank announce that Mia drank 2 cocktails last night?",
            "confess":"Did Frank confess that Mia drank 2 cocktails last night?",
            "confirm":"Did Frank confirm that Mia drank 2 cocktails last night?",
            "establish":"Did Frank establish that Mia drank 2 cocktails last night?",
            "hear":"Did Frank hear that Mia drank 2 cocktails last night?",
            "inform":"Did Frank inform Sam that Mia drank 2 cocktails last night?",
            "prove":"Did Frank prove that Mia drank 2 cocktails last night?",
            "low_prior": "Mia is a nun",
            "high_prior": "Mia is a college student"
        },
        "isabella": {
            "question":"Isabella ate a steak on Sunday",
            "simple_polar":"Did Isabella eat a steak on Sunday?",
            "be_annoyed":"Is Andrea annoyed that Isabella ate a steak on Sunday?",
            "discover":"Did Andrea discover that Isabella ate a steak on Sunday?",
            "know":"Does Andrea know that Isabella ate a steak on Sunday?",
            "reveal":"Did Andrea reveal that Isabella ate a steak on Sunday?",
            "see":"Did Andrea see that Isabella ate a steak on Sunday?",
            "pretend":"Did Andrea pretend that Isabella ate a steak on Sunday?",
            "suggest":"Did Andrea suggest that Isabella ate a steak on Sunday?",
            "say":"Did Andrea say that Isabella ate a steak on Sunday?",
            "think":"Does Andrea think that Isabella ate a steak on Sunday?",
            "be_right":"Is Andrea right that Isabella ate a steak on Sunday?",
            "demonstrate":"Did Andrea demonstrate that Isabella ate a steak on Sunday?",
            "acknowledge":"Did Andrea acknowledge that Isabella ate a steak on Sunday?",
            "admit":"Did Andrea admit that Isabella ate a steak on Sunday?",
            "announce":"Did Andrea announce that Isabella ate a steak on Sunday?",
            "confess":"Did Andrea confess that Isabella ate a steak on Sunday?",
            "confirm":"Did Andrea confirm that Isabella ate a steak on Sunday?",
            "establish":"Did Andrea establish that Isabella ate a steak on Sunday?",
            "hear":"Did Andrea hear that Isabella ate a steak on Sunday?",
            "inform":"Did Andrea inform Sam that Isabella ate a steak on Sunday?",
            "prove":"Did Andrea prove that Isabella ate a steak on Sunday?",
            "low_prior": "Isabella is a vegetarian",
            "high_prior": "Isabella is from Argentina"
        },
        "emily": {
            "question":"Emily bought a car yesterday",
            "simple_polar":"Did Emily buy a car yesterday?",
            "be_annoyed":"Is Chloe annoyed that Emily bought a car yesterday?",
            "discover":"Did Chloe discover that Emily bought a car yesterday?",
            "know":"Does Chloe know that Emily bought a car yesterday?",
            "reveal":"Did Chloe reveal that Emily bought a car yesterday?",
            "see":"Did Chloe see that Emily bought a car yesterday?",
            "pretend":"Did Chloe pretend that Emily bought a car yesterday?",
            "suggest":"Did Chloe suggest that Emily bought a car yesterday?",
            "say":"Did Chloe say that Emily bought a car yesterday?",
            "think":"Does Chloe think that Emily bought a car yesterday?",
            "be_right":"Is Chloe right that Emily bought a car yesterday?",
            "demonstrate":"Did Chloe demonstrate that Emily bought a car yesterday?",
            "acknowledge":"Did Chloe acknowledge that Emily bought a car yesterday?",
            "admit":"Did Chloe admit that Emily bought a car yesterday?",
            "announce":"Did Chloe announce that Emily bought a car yesterday?",
            "confess":"Did Chloe confess that Emily bought a car yesterday?",
            "confirm":"Did Chloe confirm that Emily bought a car yesterday?",
            "establish":"Did Chloe establish that Emily bought a car yesterday?",
            "hear":"Did Chloe hear that Emily bought a car yesterday?",
            "inform":"Did Chloe inform Sam that Emily bought a car yesterday?",
            "prove":"Did Chloe prove that Emily bought a car yesterday?",
            "low_prior": "Emily never has any money",
            "high_prior": "Emily has been saving for a year"
        },
        "grace": {
            "question":"Grace visited her sister",
            "simple_polar":"Did Grace visit her sister?",
            "be_annoyed":"Is Andrew annoyed that Grace visited her sister?",
            "discover":"Did Andrew discover that Grace visited her sister?",
            "know":"Does Andrew know that Grace visited her sister?",
            "reveal":"Did Andrew reveal that Grace visited her sister?",
            "see":"Did Andrew see that Grace visited her sister?",
            "pretend":"Did Andrew pretend that Grace visited her sister?",
            "suggest":"Did Andrew suggest that Grace visited her sister?",
            "say":"Did Andrew say that Grace visited her sister?",
            "think":"Does Andrew think that Grace visited her sister?",
            "be_right":"Is Andrew right that Grace visited her sister?",
            "demonstrate":"Did Andrew demonstrate that Grace visited her sister?",
            "acknowledge":"Did Andrew acknowledge that Grace visited her sister?",
            "admit":"Did Andrew admit that Grace visited her sister?",
            "announce":"Did Andrew announce that Grace visited her sister?",
            "confess":"Did Andrew confess that Grace visited her sister?",
            "confirm":"Did Andrew confirm that Grace visited her sister?",
            "establish":"Did Andrew establish that Grace visited her sister?",
            "hear":"Did Andrew hear that Grace visited her sister?",
            "inform":"Did Andrew inform Sam that Grace visited her sister?",
            "prove":"Did Andrew prove that Grace visited her sister?",
            "low_prior": "Grace hates her sister",
            "high_prior": "Grace loves her sister"
        },
        "zoe": {
            "question":"Zoe calculated the tip",
            "simple_polar":"Did Zoe calculate the tip?",
            "be_annoyed":"Is Mark annoyed that Zoe calculated the tip?",
            "discover":"Did Mark discover that Zoe calculated the tip?",
            "know":"Does Mark know that Zoe calculated the tip?",
            "reveal":"Did Mark reveal that Zoe calculated the tip?",
            "see":"Did Mark see that Zoe calculated the tip?",
            "pretend":"Did Mark pretend that Zoe calculated the tip?",
            "suggest":"Did Mark suggest that Zoe calculated the tip?",
            "say":"Did Mark say that Zoe calculated the tip?",
            "think":"Does Mark think that Zoe calculated the tip?",
            "be_right":"Is Mark right that Zoe calculated the tip?",
            "demonstrate":"Did Mark demonstrate that Zoe calculated the tip?",
            "acknowledge":"Did Mark acknowledge that Zoe calculated the tip?",
            "admit":"Did Mark admit that Zoe calculated the tip?",
            "announce":"Did Mark announce that Zoe calculated the tip?",
            "confess":"Did Mark confess that Zoe calculated the tip?",
            "confirm":"Did Mark confirm that Zoe calculated the tip?",
            "establish":"Did Mark establish that Zoe calculated the tip?",
            "hear":"Did Mark hear that Zoe calculated the tip?",
            "inform":"Did Mark inform Sam that Zoe calculated the tip?",
            "prove":"Did Mark prove that Zoe calculated the tip?",
            "low_prior": "Zoe is 5 years old",
            "high_prior": "Zoe is a math major"
        },
        "danny": {
            "question":"Danny ate the last cupcake",
            "simple_polar":"Did Danny eat the last cupcake?",
            "be_annoyed":"Is Kathryn annoyed that Danny ate the last cupcake?",
            "discover":"Did Kathryn discover that Danny ate the last cupcake?",
            "know":"Does Kathryn know that Danny ate the last cupcake?",
            "reveal":"Did Kathryn reveal that Danny ate the last cupcake?",
            "see":"Did Kathryn see that Danny ate the last cupcake?",
            "pretend":"Did Kathryn pretend that Danny ate the last cupcake?",
            "suggest":"Did Kathryn suggest that Danny ate the last cupcake?",
            "say":"Did Kathryn say that Danny ate the last cupcake?",
            "think":"Does Kathryn think that Danny ate the last cupcake?",
            "be_right":"Is Kathryn right that Danny ate the last cupcake?",
            "demonstrate":"Did Kathryn demonstrate that Danny ate the last cupcake?",
            "acknowledge":"Did Kathryn acknowledge that Danny ate the last cupcake?",
            "admit":"Did Kathryn admit that Danny ate the last cupcake?",
            "announce":"Did Kathryn announce that Danny ate the last cupcake?",
            "confess":"Did Kathryn confess that Danny ate the last cupcake?",
            "confirm":"Did Kathryn confirm that Danny ate the last cupcake?",
            "establish":"Did Kathryn establish that Danny ate the last cupcake?",
            "hear":"Did Kathryn hear that Danny ate the last cupcake?",
            "inform":"Did Kathryn inform Sam that Danny ate the last cupcake?",
            "prove":"Did Kathryn prove that Danny ate the last cupcake?",
            "low_prior": "Danny is a diabetic",
            "high_prior": "Danny loves cake"
        },
        "frank": {
            "question":"Frank got a cat",
            "simple_polar":"Did Frank get a cat?",
            "be_annoyed":"Is Walt annoyed that Frank got a cat?",
            "discover":"Did Walt discover that Frank got a cat?",
            "know":"Does Walt know that Frank got a cat?",
            "reveal":"Did Walt reveal that Frank got a cat?",
            "see":"Did Walt see that Frank got a cat?",
            "pretend":"Did Walt pretend that Frank got a cat?",
            "suggest":"Did Walt suggest that Frank got a cat?",
            "say":"Did Walt say that Frank got a cat?",
            "think":"Does Walt think that Frank got a cat?",
            "be_right":"Is Walt right that Frank got a cat?",
            "demonstrate":"Did Walt demonstrate that Frank got a cat?",
            "acknowledge":"Did Walt acknowledge that Frank got a cat?",
            "admit":"Did Walt admit that Frank got a cat?",
            "announce":"Did Walt announce that Frank got a cat?",
            "confess":"Did Walt confess that Frank got a cat?",
            "confirm":"Did Walt confirm that Frank got a cat?",
            "establish":"Did Walt establish that Frank got a cat?",
            "hear":"Did Walt hear that Frank got a cat?",
            "inform":"Did Walt inform Sam that Frank got a cat?",
            "prove":"Did Walt prove that Frank got a cat?",
            "low_prior": "Frank is allergic to cats",
            "high_prior": "Frank has always wanted a pet"
        },
        "jackson": {
            "question":"Jackson ran 10 miles",
            "simple_polar":"Did Jackson run 10 miles?",
            "be_annoyed":"Is Randy annoyed that Jackson ran 10 miles?",
            "discover":"Did Randy discover that Jackson ran 10 miles?",
            "know":"Does Randy know that Jackson ran 10 miles?",
            "reveal":"Did Randy reveal that Jackson ran 10 miles?",
            "see":"Did Randy see that Jackson ran 10 miles?",
            "pretend":"Did Randy pretend that Jackson ran 10 miles?",
            "suggest":"Did Randy suggest that Jackson ran 10 miles?",
            "say":"Did Randy say that Jackson ran 10 miles?",
            "think":"Does Randy think that Jackson ran 10 miles?",
            "be_right":"Is Randy right that Jackson ran 10 miles?",
            "demonstrate":"Did Randy demonstrate that Jackson ran 10 miles?",
            "acknowledge":"Did Randy acknowledge that Jackson ran 10 miles?",
            "admit":"Did Randy admit that Jackson ran 10 miles?",
            "announce":"Did Randy announce that Jackson ran 10 miles?",
            "confess":"Did Randy confess that Jackson ran 10 miles?",
            "confirm":"Did Randy confirm that Jackson ran 10 miles?",
            "establish":"Did Randy establish that Jackson ran 10 miles?",
            "hear":"Did Randy hear that Jackson ran 10 miles?",
            "inform":"Did Randy inform Sam that Jackson ran 10 miles?",
            "prove":"Did Randy prove that Jackson ran 10 miles?",
            "low_prior": "Jackson is obese",
            "high_prior": "Jackson is training for a marathon"
        },
        "jayden": {
            "question":"Jayden rented a car",
            "simple_polar":"Did Jayden rent a car?",
            "be_annoyed":"Is Herbert annoyed that Jayden rented a car?",
            "discover":"Did Herbert discover that Jayden rented a car?",
            "know":"Does Herbert know that Jayden rented a car?",
            "reveal":"Did Herbert reveal that Jayden rented a car?",
            "see":"Did Herbert see that Jayden rented a car?",
            "pretend":"Did Herbert pretend that Jayden rented a car?",
            "suggest":"Did Herbert suggest that Jayden rented a car?",
            "say":"Did Herbert say that Jayden rented a car?",
            "think":"Does Herbert think that Jayden rented a car?",
            "be_right":"Is Herbert right that Jayden rented a car?",
            "demonstrate":"Did Herbert demonstrate that Jayden rented a car?",
            "acknowledge":"Did Herbert acknowledge that Jayden rented a car?",
            "admit":"Did Herbert admit that Jayden rented a car?",
            "announce":"Did Herbert announce that Jayden rented a car?",
            "confess":"Did Herbert confess that Jayden rented a car?",
            "confirm":"Did Herbert confirm that Jayden rented a car?",
            "establish":"Did Herbert establish that Jayden rented a car?",
            "hear":"Did Herbert hear that Jayden rented a car?",
            "inform":"Did Herbert inform Sam that Jayden rented a car?",
            "prove":"Did Herbert prove that Jayden rented a car?",
            "low_prior": "Jayden doesn't have a driver's license",
            "high_prior": "Jayden's car is in the shop"
        },
        "tony": {
            "question":"Tony had a drink last night",
            "simple_polar":"Did Tony have a drink last night?",
            "be_annoyed":"Is Helen annoyed that Tony had a drink last night?",
            "discover":"Did Helen discover that Tony had a drink last night?",
            "know":"Does Helen know that Tony had a drink last night?",
            "reveal":"Did Helen reveal that Tony had a drink last night?",
            "see":"Did Helen see that Tony had a drink last night?",
            "pretend":"Did Helen pretend that Tony had a drink last night?",
            "suggest":"Did Helen suggest that Tony had a drink last night?",
            "say":"Did Helen say that Tony had a drink last night?",
            "think":"Does Helen think that Tony had a drink last night?",
            "be_right":"Is Helen right that Tony had a drink last night?",
            "demonstrate":"Did Helen demonstrate that Tony had a drink last night?",
            "acknowledge":"Did Helen acknowledge that Tony had a drink last night?",
            "admit":"Did Helen admit that Tony had a drink last night?",
            "announce":"Did Helen announce that Tony had a drink last night?",
            "confess":"Did Helen confess that Tony had a drink last night?",
            "confirm":"Did Helen confirm that Tony had a drink last night?",
            "establish":"Did Helen establish that Tony had a drink last night?",
            "hear":"Did Helen hear that Tony had a drink last night?",
            "inform":"Did Helen inform Sam that Tony had a drink last night?",
            "prove":"Did Helen prove that Tony had a drink last night?",
            "low_prior": "Tony has been sober for 20 years",
            "high_prior": "Tony really likes to party with his friends"
        },
        "josh": {
            "question":"Josh learned to ride a bike yesterday",
            "simple_polar":"Did Josh learn to ride a bike yesterday?",
            "be_annoyed":"Is Brad annoyed that Josh learned to ride a bike yesterday?",
            "discover":"Did Brad discover that Josh learned to ride a bike yesterday?",
            "know":"Does Brad know that Josh learned to ride a bike yesterday?",
            "reveal":"Did Brad reveal that Josh learned to ride a bike yesterday?",
            "see":"Did Brad see that Josh learned to ride a bike yesterday?",
            "pretend":"Did Brad pretend that Josh learned to ride a bike yesterday?",
            "suggest":"Did Brad suggest that Josh learned to ride a bike yesterday?",
            "say":"Did Brad say that Josh learned to ride a bike yesterday?",
            "think":"Does Brad think that Josh learned to ride a bike yesterday?",
            "be_right":"Is Brad right that Josh learned to ride a bike yesterday?",
            "demonstrate":"Did Brad demonstrate that Josh learned to ride a bike yesterday?",
            "acknowledge":"Did Brad acknowledge that Josh learned to ride a bike yesterday?",
            "admit":"Did Brad admit that Josh learned to ride a bike yesterday?",
            "announce":"Did Brad announce that Josh learned to ride a bike yesterday?",
            "confess":"Did Brad confess that Josh learned to ride a bike yesterday?",
            "confirm":"Did Brad confirm that Josh learned to ride a bike yesterday?",
            "establish":"Did Brad establish that Josh learned to ride a bike yesterday?",
            "hear":"Did Brad hear that Josh learned to ride a bike yesterday?",
            "inform":"Did Brad inform Sam that Josh learned to ride a bike yesterday?",
            "prove":"Did Brad prove that Josh learned to ride a bike yesterday?",
            "low_prior": "Josh is a 75-year old man",
            "high_prior": "Josh is a 5-year old boy"
        },
        "owen": {
            "question":"Owen shoveled snow last winter",
            "simple_polar":"Did Owen shovel snow last winter?",
            "be_annoyed":"Is Jordan annoyed that Owen shoveled snow last winter?",
            "discover":"Did Jordan discover that Owen shoveled snow last winter?",
            "know":"Does Jordan know that Owen shoveled snow last winter?",
            "reveal":"Did Jordan reveal that Owen shoveled snow last winter?",
            "see":"Did Jordan see that Owen shoveled snow last winter?",
            "pretend":"Did Jordan pretend that Owen shoveled snow last winter?",
            "suggest":"Did Jordan suggest that Owen shoveled snow last winter?",
            "say":"Did Jordan say that Owen shoveled snow last winter?",
            "think":"Does Jordan think that Owen shoveled snow last winter?",
            "be_right":"Is Jordan right that Owen shoveled snow last winter?",
            "demonstrate":"Did Jordan demonstrate that Owen shoveled snow last winter?",
            "acknowledge":"Did Jordan acknowledge that Owen shoveled snow last winter?",
            "admit":"Did Jordan admit that Owen shoveled snow last winter?",
            "announce":"Did Jordan announce that Owen shoveled snow last winter?",
            "confess":"Did Jordan confess that Owen shoveled snow last winter?",
            "confirm":"Did Jordan confirm that Owen shoveled snow last winter?",
            "establish":"Did Jordan establish that Owen shoveled snow last winter?",
            "hear":"Did Jordan hear that Owen shoveled snow last winter?",
            "inform":"Did Jordan inform Sam that Owen shoveled snow last winter?",
            "prove":"Did Jordan prove that Owen shoveled snow last winter?",
            "low_prior": "Owen lives in New Orleans",
            "high_prior": "Owen lives in Chicago"
        },
        "julian": {
            "question":"Julian dances salsa",
            "simple_polar":"Does Julian dance salsa?",
            "be_annoyed":"Is Cole annoyed that Julian dances salsa?",
            "discover":"Did Cole discover that Julian dances salsa?",
            "know":"Does Cole know that Julian dances salsa?",
            "reveal":"Did Cole reveal that Julian dances salsa?",
            "see":"Did Cole see that Julian dances salsa?",
            "pretend":"Did Cole pretend that Julian dances salsa?",
            "suggest":"Did Cole suggest that Julian dances salsa?",
            "say":"Did Cole say that Julian dances salsa?",
            "think":"Does Cole think that Julian dances salsa?",
            "be_right":"Is Cole right that Julian dances salsa?",
            "demonstrate":"Did Cole demonstrate that Julian dances salsa?",
            "acknowledge":"Did Cole acknowledge that Julian dances salsa?",
            "admit":"Did Cole admit that Julian dances salsa?",
            "announce":"Did Cole announce that Julian dances salsa?",
            "confess":"Did Cole confess that Julian dances salsa?",
            "confirm":"Did Cole confirm that Julian dances salsa?",
            "establish":"Did Cole establish that Julian dances salsa?",
            "hear":"Did Cole hear that Julian dances salsa?",
            "inform":"Did Cole inform Sam that Julian dances salsa?",
            "prove":"Did Cole prove that Julian dances salsa?",
            "low_prior": "Julian is German",
            "high_prior": "Julian is Cuban"
        },
        "jon": {
            "question":"Jon walks to work",
            "simple_polar":"Does Jon walk to work?",
            "be_annoyed":"Is Dexter annoyed that Jon walks to work?",
            "discover":"Did Dexter discover that Jon walks to work?",
            "know":"Does Dexter know that Jon walks to work?",
            "reveal":"Did Dexter reveal that Jon walks to work?",
            "see":"Did Dexter see that Jon walks to work?",
            "pretend":"Did Dexter pretend that Jon walks to work?",
            "suggest":"Did Dexter suggest that Jon walks to work?",
            "say":"Did Dexter say that Jon walks to work?",
            "think":"Does Dexter think that Jon walks to work?",
            "be_right":"Is Dexter right that Jon walks to work?",
            "demonstrate":"Did Dexter demonstrate that Jon walks to work?",
            "acknowledge":"Did Dexter acknowledge that Jon walks to work?",
            "admit":"Did Dexter admit that Jon walks to work?",
            "announce":"Did Dexter announce that Jon walks to work?",
            "confess":"Did Dexter confess that Jon walks to work?",
            "confirm":"Did Dexter confirm that Jon walks to work?",
            "establish":"Did Dexter establish that Jon walks to work?",
            "hear":"Did Dexter hear that Jon walks to work?",
            "inform":"Did Dexter inform Sam that Jon walks to work?",
            "prove":"Did Dexter prove that Jon walks to work?",
            "low_prior": "Jon lives 10 miles away from work",
            "high_prior": "Jon lives 2 blocks away from work"
        },
        "charley": {
            "question":"Charley speaks Spanish",
            "simple_polar":"Does Charley speak Spanish?",
            "be_annoyed":"Is Anton annoyed that Charley speaks Spanish?",
            "discover":"Did Anton discover that Charley speaks Spanish?",
            "know":"Does Anton know that Charley speaks Spanish?",
            "reveal":"Did Anton reveal that Charley speaks Spanish?",
            "see":"Did Anton see that Charley speaks Spanish?",
            "pretend":"Did Anton pretend that Charley speaks Spanish?",
            "suggest":"Did Anton suggest that Charley speaks Spanish?",
            "say":"Did Anton say that Charley speaks Spanish?",
            "think":"Does Anton think that Charley speaks Spanish?",
            "be_right":"Is Anton right that Charley speaks Spanish?",
            "demonstrate":"Did Anton demonstrate that Charley speaks Spanish?",
            "acknowledge":"Did Anton acknowledge that Charley speaks Spanish?",
            "admit":"Did Anton admit that Charley speaks Spanish?",
            "announce":"Did Anton announce that Charley speaks Spanish?",
            "confess":"Did Anton confess that Charley speaks Spanish?",
            "confirm":"Did Anton confirm that Charley speaks Spanish?",
            "establish":"Did Anton establish that Charley speaks Spanish?",
            "hear":"Did Anton hear that Charley speaks Spanish?",
            "inform":"Did Anton inform Sam that Charley speaks Spanish?",
            "prove":"Did Anton prove that Charley speaks Spanish?",
            "low_prior": "Charley lives in Korea",
            "high_prior": "Charley lives in Mexico"
        },
        "liz": {
            "question":"Liz surfs every weekend",
            "simple_polar":"Does Liz surf every weekend?",
            "be_annoyed":"Is Bill annoyed that Liz surfs every weekend?",
            "discover":"Did Bill discover that Liz surfs every weekend?",
            "know":"Does Bill know that Liz surfs every weekend?",
            "reveal":"Did Bill reveal that Liz surfs every weekend?",
            "see":"Did Bill see that Liz surfs every weekend?",
            "pretend":"Did Bill pretend that Liz surfs every weekend?",
            "suggest":"Did Bill suggest that Liz surfs every weekend?",
            "say":"Did Bill say that Liz surfs every weekend?",
            "think":"Does Bill think that Liz surfs every weekend?",
            "be_right":"Is Bill right that Liz surfs every weekend?",
            "demonstrate":"Did Bill demonstrate that Liz surfs every weekend?",
            "acknowledge":"Did Bill acknowledge that Liz surfs every weekend?",
            "admit":"Did Bill admit that Liz surfs every weekend?",
            "announce":"Did Bill announce that Liz surfs every weekend?",
            "confess":"Did Bill confess that Liz surfs every weekend?",
            "confirm":"Did Bill confirm that Liz surfs every weekend?",
            "establish":"Did Bill establish that Liz surfs every weekend?",
            "hear":"Did Bill hear that Liz surfs every weekend?",
            "inform":"Did Bill inform Sam that Liz surfs every weekend?",
            "prove":"Did Bill prove that Liz surfs every weekend?",
            "low_prior": "Liz doesn't know who to swim",
            "high_prior": "Liz lives near the beach"
        },
    };

    var items_content_mapping = {
        "be_annoyed":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "discover":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "know":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "reveal":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "see":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "pretend":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "suggest":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "say":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "think":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "be_right":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "demonstrate":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "acknowledge":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "admit":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "announce":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "confess":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "confirm":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "establish":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "hear":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "inform":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "prove":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"],
        "simple_polar":["mary","josie","emma","olivia","sophia","mia","isabella","emily","grace","zoe","danny","frank","jackson","jayden","tony","josh","owen","julian","jon","charley","liz"]
    };

    // controls
    var mcitemnames = ["muffins","pizza","kids","ballet","garage","hat"];
    var mcitems = {
        "muffins": {
            "question":"these muffins have blueberries in them",
            "MCq":"Do these muffins have blueberries in them?",
            "MCa":"These muffins have blueberries in them.",
            "prior_fact": "Muffins are sold at the bakery"},
        "pizza": {
            "question":"this pizza has mushrooms on it",
            "MCq":"Does this pizza have mushrooms on it?",
            "MCa":"This pizza has mushrooms on it.",
            "prior_fact": "Pizza is sold at the pizzeria"},
        "kids": {
            "question":"Jack was playing outside with the kids",
            "MCq":"Was Jack playing outside with the kids?",
            "MCa":"Jack was playing outside with the kids.",
            "prior_fact": "Many children like ice cream"},
        "ballet": {
            "question":"Ann dances ballet",
            "MCq":"Does Ann dance ballet?",
            "MCa":"Ann dances ballet.",
            "prior_fact": "Ballet is a type of dance"},
        "garage": {
            "question":"Carl's kids were in the garage",
            "MCq":"Were Carl's kids in the garage?",
            "MCa":"Carl's kids were in the garage.",
            "prior_fact": "Garages are used to store cars and other things"},
        "hat": {
            "question":"Samantha has a new hat",
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
        // each content includes: 1. the content itself ("question"), 2. the simple polar ("MC")
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
        "be_annoyed": getContent("be_annoyed"),  	  	
        "discover": getContent("discover"),
        "know": getContent("know"),  	  	
        "reveal": getContent("reveal"),
        "see": getContent("see"),
        "pretend": getContent("pretend"),
        "suggest": getContent("suggest"),  	
        "say": getContent("say"),  	
        "think": getContent("think"),
        "be_right": getContent("be_right"),
        "demonstrate": getContent("demonstrate"),
        "acknowledge": getContent("acknowledge"),
        "admit": getContent("admit"),
        "announce": getContent("announce"),
        "confess": getContent("confess"),
        "confirm": getContent("confirm"),
        "establish": getContent("establish"),
        "hear": getContent("hear"),
        "inform": getContent("inform"),
        "prove": getContent("prove"),
        "simple_polar": getContent("simple_polar")
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
        var question = contents[trigger_cont].question;
        return {
            "name": name,
            "gender": gender,	  
            "trigger": item.trigger,
            "short_trigger": short_trigger,	  // short_trigger is the same as trigger?
            "trigger_class": item.trigger_class,
            "content": trigger_cont,
            "utterance": utterance,
            "question": question
        }
    }

    // create the control set
    function makeMCStim(ind,j) {
        // get item
        var item = mcitems[j];
        // get a speaker
        // var name_data = names[ind];
        console.log("MC names");
        var name_data = names.pop();
        // console.log(name_data);
        var name = name_data.name;
        var gender = name_data.gender;
        // get content
        var trigger_cont = j;
        var trigger = "MC";
        var short_trigger = "MC";
    
    //  console.log("short_trigger: "+short_trigger);
    //  console.log("trigger: "+trigger);
    //    console.log("trigger_cont: "+trigger_cont);
    //    console.log("utterance: "+contents[trigger_cont][short_trigger]);    
    //    console.log(contents[trigger_cont]);    
        var utterance = mcitems[j].MCq;
        var question = mcitems[j].question;  
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
            "prior_fact": prior_fact 
        }
    }  
      
    exp.stims_block1 = [];
    exp.stims_block2 = [];
    
    // loop through the triggers, make each of them a stimuli, and add 
    // them to the stimuli set in block 1 
    for (var i=0; i<items.length; i++) {
        var stim = makeStim(i);
        exp.stims_block1.push(jQuery.extend(true, {}, stim));
    }

	exp.stims_block1 = _.shuffle(exp.stims_block1); 

    // I don't understand why here the block type and the prior type are already fixed	  
    
    // half of the stimuli will be paired with low probability fact
    for (var k=0; k<Math.round(items.length/2); k++) {
        var content = exp.stims_block1[k].content;
        exp.stims_block1[k].prior = "low_prior";
        exp.stims_block1[k].prior_fact = contents[content]["low_prior"]	
    }  
    
    // the other half will be paired with hight probability fact
    for (var j=(Math.round(items.length/2)); j<items.length; j++) {
        var content = exp.stims_block1[j].content;
        exp.stims_block1[j].prior = "high_prior";
        exp.stims_block1[j].prior_fact = contents[content]["high_prior"]		
    }    

    // add the control items
    for (var l=0; l<mcitemnames.length; l++) {
        var stim = makeMCStim(l,mcitemnames[l]);
        exp.stims_block1.push(jQuery.extend(true, {}, stim));
    }  
 
    // why there are three blocks?
    exp.stims_block2 = jQuery.extend(true, [], exp.stims_block1);
  
    // original comment: here things are bad already because some stim ai, some projective

    // randomize the items within each block
	exp.stims_block1 = _.shuffle(exp.stims_block1);  
	exp.stims_block2 = _.shuffle(exp.stims_block2); 
	
	
// decide order of prior and projective blocks
    var block_order = _.shuffle(["prior","projective"]);
    var block1type = block_order[0];
    var block2type = block_order[1];  


    for (var k=0; k<exp.stims_block2.length; k++) {    
        exp.stims_block2[k].block = block2type;//block_order[1];
        // console.log(exp.stims_block2[k].block);   	
        exp.stims_block1[k].block = block1type;//block_order[0];   	
        // console.log(exp.stims_block1[k].block);
    }

    for (var k=0; k<exp.stims_block2.length; k++) {    
        exp.stims_block2[k].block = block2type;//block_order[1];
        // console.log(exp.stims_block2[k].block);   	
        exp.stims_block1[k].block = block1type;//block_order[0];   	
        // console.log(exp.stims_block1[k].block);
    }

    
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
    exp.structure=["bot", "i0", "instructions1", "block1", "instructions2", "block2", 'questionaire', 'finished'];
    console.log(exp.structure);

    exp.data_trials = [];
    //make corresponding slides:
    exp.slides = make_slides(exp);

    //   exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                        //relies on structure and slides being defined
                        
    exp.nQs = 3 + 27 + 1 + 27 + 1; 
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
