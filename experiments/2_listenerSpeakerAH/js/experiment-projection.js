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
                    "text": "bot_check",
                    "object": exp.listener,
                    "rt" : 0,
                    "response" : exp.text_input
                });
                exp.go();
            } else {
                exp.data_trials.push({
                    "slide_number_in_experiment" : exp.phase,
                    "text": "bot_check",
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
            var inst1 = "Let's get started! <br><br>Imagine you are at a party. <br><br> You walk into the kitchen and overhear somebody ask something. You'll answer questions about what people believe.";
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
            $(".second_err").hide() 	
            $(".question").show();
            this.init_sliders();
            $(".first_slider_table").show(); // show the belief slider
            $(".continue_button").show(); // show the belief button
            exp.first_sliderPost = null;
            exp.second_sliderPost = null;
            exp.ah_question = null;
            $(".second_slider_table").hide(); // hide the second slider in the beginning
            $(".next_button").hide(); // hide the next botton

            console.log(this.stim);
            exp.text =  "<strong> Fact (which everyone knows):</strong> "+this.stim.prior_fact+".<br><br>" + 
                    "<font color=\"blue\">" + this.stim.speaker_name + "</font> asks: \"<i>"+this.stim.utterance+"</i>\"";
            $(".sentence").html(exp.text);

            exp.leftLabel =  "definitely no";
            $(".leftLabel").html(exp.leftLabel);
            exp.rightLabel = "definitely yes";
            $(".rightLabel").html(exp.rightLabel);

            // allows the content question to match with the utterance (i.e. not p in question when the embedded is not p)
            if (this.stim.trigger.includes("_neg")) {
                exp.speaker_question = "Does <font color=\"blue\">"+this.stim.speaker_name+"</font> believe that "+this.stim.negation+"?";
                // if there is no attitude holder
                if (this.stim.ah_name == "NA") {
                    exp.ah_question = "NA";
                } else {
                    exp.ah_question = "Does <font color=\"red\">"+this.stim.ah_name+"</font> believe that "+this.stim.negation+"?";
                }
            } else {
                exp.speaker_question = "Does <font color=\"blue\">"+this.stim.speaker_name+"</font> believe that "+this.stim.statement+"?";
                if (this.stim.ah_name == "NA") {
                    exp.ah_question = "NA";
                } else {
                    exp.ah_question = "Does <font color=\"red\">"+this.stim.ah_name+"</font> believe that "+this.stim.statement+"?";
                }
            }

            if (this.stim.question_order == "ah_first") {
                var question_list = [exp.speaker_question, exp.ah_question];
            } else {
                var question_list = [exp.ah_question, exp.speaker_question];
            }
            console.log("question order: "+this.stim.question_order);
            console.log("question list: "+question_list);
            exp.first_question = question_list.pop();
            exp.second_question = question_list.pop();
            if (exp.first_question == "NA") {
                exp.first_question = exp.second_question;
                exp.second_question = "NA";
            }
            $(".question").html(exp.first_question);	  
            console.log("first question: "+exp.first_question); 
        },
        
        // the continue button is pressed to forward to the next question
        first_button : function() {
            console.log("first rating: "+exp.first_sliderPost);
            console.log("default second rating: "+exp.second_sliderPost);

            if (exp.first_sliderPost != null) {
                $(".err").hide(); // have a rating, so hide the error message
                $(".first_slider_table").hide(); // hide the speaker belief slider

                console.log("second question: "+exp.second_question);
                if (exp.second_question != "NA") {
                    $(".question").html(exp.second_question);
                    
                    this.init_second_slider();
                    exp.second_sliderPost = null;
                    $(".leftLabel").html(exp.leftLabel);
                    $(".rightLabel").html(exp.rightLabel);
                    
                    $(".continue_button").hide();
                    $(".second_slider_table").show();
                    $(".next_button").show()
                } else {
                    // exp.second_sliderPost = "NA";
                    this.log_responses();
                    _stream.apply(this); // exp.go()
                }

            } else { //  no speaker belief rating
                $(".err").show();
            }
        },
        
        second_button : function() {
            console.log("second rating: "+exp.second_sliderPost);
            console.log("double check first rating: "+exp.first_sliderPost);
            if (exp.second_sliderPost != null | (exp.second_question == "NA" & exp.second_sliderPost == null)) {
                this.log_responses();
                _stream.apply(this); //use exp.go() if and only if there is no "present" data.
            } else {
                $(".second_err").show();
            }
        },

        init_sliders : function() {
            utils.make_slider("#single_slider1", function(event, ui) {
                exp.first_sliderPost = ui.value;
            });
        },

        init_second_slider : function() {
            utils.make_slider("#single_slider2", function(event_1, ui_1) {
                exp.second_sliderPost = ui_1.value;
            });
        },

        log_responses : function() {
            if (this.stim.trigger == "simple_polar") {
                trigger = "Polar";
                // console.log(predicate)
            } else {
                trigger = this.stim.trigger;
            }

            if (exp.first_question == exp.speaker_question) {
                exp.speaker_sliderPost = exp.first_sliderPost;
                exp.ah_sliderPost = exp.second_sliderPost;
            } else {
                exp.speaker_sliderPost = exp.second_sliderPost;
                exp.ah_sliderPost = exp.first_sliderPost;
            }

            exp.data_trials.push({
                "slide_number_in_experiment" : exp.phase, // trial number
                "question_order" : this.stim.question_order,
                "trigger": trigger,
                "predicate" : this.stim.predicate,
                "trigger_class": this.stim.trigger_class,
                // "content": this.stim.content,
                // "speaker_question": exp.speaker_question, // sanity check
                // "ah_question":exp.ah_question, // sanity check
                "text": exp.text, // record context + utterance for sanity check
                "prior_rating" : this.stim.prior_rating,
                "prior_fact" : this.stim.prior_fact,
                "prior_condition" : this.stim.prior_condition,
                // "speakerName": this.stim.name,  // sanity check  
                "speaker_response" : exp.speaker_sliderPost,
                "ah_response" : exp.ah_sliderPost,
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
            american : $('input[name="ame"]:checked').val(),
            enjoyment : $("#enjoyment").val(),
            asses : $('input[name="assess"]:checked').val(),
            age : $("#age").val(),
            gender : $("#gender").val(),
            education : $("#education").val(),
            fairprice : $("#fairprice").val(),
            problems : $("#problems").val(),
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
            // "condition" : exp.condition,
            "subject_information" : exp.subj_data,
            "time_in_minutes" : (Date.now() - exp.startT)/60000
        };
        // record data using proliferate
        proliferate.submit(exp.data);
        }
    });
    console.log(slides);

    return slides;
}

function init() {

    var question_order = _.sample(["speaker_first", "ah_first"]);
    var speaker_names = _.shuffle(["Christopher","Daniel","Tyler","Paul","George","Steven","Kenneth","Edward","Brian","Kevin","Larry","Scott",
    "Jennifer","Dorothy","Karen","Nancy","Betty","Lisa","Sandra","Ashley","Donna","Kimberly","Cynthia","Kathleen"])
    var ah_names = _.shuffle(["Ronald","Timothy","Jason","Jeffrey","Gary","Ryan","Nicholas","Eric","Jacob","Jonathan",
    "Carol","Michelle","Emily","Amanda","Melissa","Deborah","Laura","Stephanie","Rebecca","Sharon"])

    var triggers = _.shuffle(["know_pos", "know_neg", "say_pos", "say_neg", "think_pos", 
    "think_neg", "inform_pos", "inform_neg", "simple_polar"])

    // a dictionary of contents
    // prior ratings are taken from Degen & Tonhauser (2021)
    // https://github.com/judith-tonhauser/projective-probability/blob/master/results/9-prior-projection/data/prior_means.csv
    var contents = {
        "mary": {
            // "item":"mary",
            "statement":"Mary is pregnant",
            "negation":"Mary isn't pregnant",
            "simple_polar":"Is Mary pregnant?",
            "know_pos":"that Mary is pregnant?",
            "know_neg":"that Mary isn't pregnant?",
            "say_pos":"that Mary is pregnant?",
            "say_neg":"that Mary isn't pregnant?",
            "think_pos":"that Mary is pregnant?",
            "think_neg":"that Mary isn't pregnant?",
            // "confirm_pos":"Did Mandy confirm that Mary is pregnant?",
            // "confirm_neg":"Did Mandy confirm that Mary isn't pregnant?",
            "inform_pos":"Sam that Mary is pregnant?",
            "inform_neg":"Sam that Mary isn't pregnant?",
            "low_prior": "Mary is a middle school student",
            "high_prior": "Mary is taking a prenatal yoga class",
            "high_prior_rating":0.815167785234899,
            "low_prior_rating":0.225401459854015
        },
        "josie": {   
            // "item":"josie",
            "statement":"Josie went on vacation to France",
            "negation":"Josie didn't go on vacation to France",
            "simple_polar":"Did Josie go on vacation to France?",
            "know_pos":"that Josie went on vacation to France?",
            "know_neg":"that Josie didn't go on vacation to France?",
            "say_pos":"that Josie went on vacation to France?",
            "say_neg":"that Josie didn't go on vacation to France?",
            "think_pos":"that Josie went on vacation to France?",
            "think_neg":"that Josie didn't go on vacation to France?",
            // "confirm_pos":"Did Sarah confirm that Josie went on vacation to France?",
            // "confirm_neg":"Did Sarah confirm that Josie didn't go on vacation to France?",
            "inform_pos":"Sam that Josie went on vacation to France?",
            "inform_neg":"Sam that Josie didn't go on vacation to France?",
            "low_prior": "Josie doesn't have a passport",
            "high_prior": "Josie loves France",
            "high_prior_rating":0.73343949044586,
            "low_prior_rating":0.117286821705426
        },
        "emma": {
            // "item":"emma",
            "statement":"Emma studied on Saturday morning",
            "negation":"Emma didn't study on Saturday morning",
            "simple_polar":"Did Emma study on Saturday morning?",
            "know_pos":"that Emma studied on Saturday morning?",
            "know_neg":"that Emma didn't study on Saturday morning?",
            "say_pos":"that Emma studied on Saturday morning?",
            "say_neg":"that Emma didn't study on Saturday morning?",
            "think_pos":"that Emma studied on Saturday morning?",
            "think_neg":"that Emma didn't study on Saturday morning?",
            // "confirm_pos":"Did Kim confirm that Emma studied on Saturday morning?",
            // "confirm_neg":"Did Kim confirm that Emma didn't study on Saturday morning?",
            "inform_pos":"Sam that Emma studied on Saturday morning?",
            "inform_neg":"Sam that Emma didn't study on Saturday morning?",
            "low_prior": "Emma is in first grade",
            "high_prior": "Emma is in law school",
            "high_prior_rating":0.680763888888889,
            "low_prior_rating":0.323098591549296
        },
        "mia":{
            // "item":"mia",
            "statement":"Mia drank 2 cocktails last night",
            "negation":"Mia didn't drink 2 cocktails last night",
            "simple_polar":"Did Mia drink 2 cocktails last night?",
            "know_pos":"that Mia drank 2 cocktails last night?",
            "know_neg":"that Mia didn't drink 2 cocktails last night?",
            "say_pos":"that Mia drank 2 cocktails last night?",
            "say_neg":"that Mia didn't drink 2 cocktails last night?",
            "think_pos":"that Mia drank 2 cocktails last night?",
            "think_neg":"that Mia didn't drink 2 cocktails last night?",
            // "confirm_pos":"Did Frank confirm that Mia drank 2 cocktails last night?",
            // "confirm_neg":"Did Frank confirm that Mia didn't drink 2 cocktails last night?",
            "inform_pos":"Sam that Mia drank 2 cocktails last night?",
            "inform_neg":"Sam that Mia didn't drink 2 cocktails last night?",
            "low_prior": "Mia is a nun",
            "high_prior": "Mia is a college student",
            "high_prior_rating":0.579022556390977,
            "low_prior_rating":0.224901960784314
        },
        "isabella": {
            // "item":"isabella",
            "statement":"Isabella ate a steak on Sunday",
            "negation":"Isabella didn't eat a steak on Sunday",
            "simple_polar":"Did Isabella eat a steak on Sunday?",
            "know_pos":"that Isabella ate a steak on Sunday?",
            "know_neg":"that Isabella didn't eat a steak on Sunday?",
            "say_pos":"that Isabella ate a steak on Sunday?",
            "say_neg":"that Isabella didn't eat a steak on Sunday?",
            "think_pos":"that Isabella ate a steak on Sunday?",
            "think_neg":"that Isabella didn't eat a steak on Sunday?",
            // "confirm_pos":"Did Andrea confirm that Isabella ate a steak on Sunday?",
            // "confirm_neg":"Did Andrea confirm that Isabella didn't eat a steak on Sunday?",
            "inform_pos":"Sam that Isabella ate a steak on Sunday?",
            "inform_neg":"Sam that Isabella didn't eat a steak on Sunday?",
            "low_prior": "Isabella is a vegetarian",
            "high_prior": "Isabella is from Argentina",
            "high_prior_rating":0.517707006369427,
            "low_prior_rating":0.125813953488372
        },
        "emily": {
            // "item":"emily",
            "statement":"Emily bought a car yesterday",
            "negation":"Emily didn't buy a car yesterday",
            "simple_polar":"Did Emily buy a car yesterday?",
            "know_pos":"that Emily bought a car yesterday?",
            "know_neg":"that Emily didn't buy a car yesterday?",
            "say_pos":"that Emily bought a car yesterday?",
            "say_neg":"that Emily didn't buy a car yesterday?",
            "think_pos":"that Emily bought a car yesterday?",
            "think_neg":"that Emily didn't buy a car yesterday?",
            // "confirm_pos":"Did Chloe confirm that Emily bought a car yesterday?",
            // "confirm_neg":"Did Chloe confirm that Emily didn't buy a car yesterday?",
            "inform_pos":"Sam that Emily bought a car yesterday?",
            "inform_neg":"Sam that Emily didn't buy a car yesterday?",
            "low_prior": "Emily never has any money",
            "high_prior": "Emily has been saving for a year",
            "high_prior_rating":0.561555555555556,
            "low_prior_rating":0.151655629139073
        },
        "danny": {
            // "item":"danny",
            "statement":"Danny ate the last cupcake",
            "negation":"Danny didn't eat the last cupcake",
            "simple_polar":"Did Danny eat the last cupcake?",
            "know_pos":"that Danny ate the last cupcake?",
            "know_neg":"that Danny didn't eat the last cupcake?",
            "say_pos":"that Danny ate the last cupcake?",
            "say_neg":"that Danny didn't eat the last cupcake?",
            "think_pos":"that Danny ate the last cupcake?",
            "think_neg":"that Danny didn't eat the last cupcake?",
            // "confirm_pos":"Did Kathryn confirm that Danny ate the last cupcake?",
            // "confirm_neg":"Did Kathryn confirm that Danny didn't eat the last cupcake?",
            "inform_pos":"Sam that Danny ate the last cupcake?",
            "inform_neg":"Sam that Danny didn't eat the last cupcake?",
            "low_prior": "Danny is a diabetic",
            "high_prior": "Danny loves cake",
            "high_prior_rating":0.697062937062937,
            "low_prior_rating":0.27993006993007
        },
        "grace": {
            // "item":"grace",
            "statement":"Grace visited her sister",
            "negation":"Grace didn't visit her sister",
            "simple_polar":"Did Grace visit her sister?",
            "know_pos":"that Grace visited her sister?",
            "know_neg":"that Grace didn't visit her sister?",
            "say_pos":"that Grace visited her sister?",
            "say_neg":"that Grace didn't visit her sister?",
            "think_pos":"that Grace visited her sister?",
            "think_neg":"that Grace didn't visit her sister?",
            // "confirm_pos":"Did Andrew confirm that Grace visited her sister?",
            // "confirm_neg":"Did Andrew confirm that Grace didn't visit her sister?",
            "inform_pos":"Sam that Grace visited her sister?",
            "inform_neg":"Sam that Grace didn't visit her sister?",
            "low_prior": "Grace hates her sister",
            "high_prior": "Grace loves her sister",
            "high_prior_rating":0.790144927536232,
            "low_prior_rating":0.24777027027027
        },
        "zoe": {
            // "item":"zoe",
            "statement":"Zoe calculated the tip",
            "negation":"Zoe didn't calculate the tip",
            "simple_polar":"Did Zoe calculate the tip?",
            "know_pos":"that Zoe calculated the tip?",
            "know_neg":"that Zoe didn't calculate the tip?",
            "say_pos":"that Zoe calculated the tip?",
            "say_neg":"that Zoe didn't calculate the tip?",
            "think_pos":"that Zoe calculated the tip?",
            "think_neg":"that Zoe didn't calculate the tip?",
            // "confirm_pos":"Did Mark confirm that Zoe calculated the tip?",
            // "confirm_neg":"Did Mark confirm that Zoe didn't calculate the tip?",
            "inform_pos":"Sam that Zoe calculated the tip?",
            "inform_neg":"Sam that Zoe didn't calculate the tip?",
            "low_prior": "Zoe is 5 years old",
            "high_prior": "Zoe is a math major",
            "high_prior_rating":0.745971223021583,
            "low_prior_rating":0.192108843537415
        },
        "frank": {
            // "item":"frank",
            "statement":"Frank got a cat",
            "negation":"Frank didn't get a cat",
            "simple_polar":"Did Frank get a cat?",
            "know_pos":"that Frank got a cat?",
            "know_neg":"that Frank didn't get a cat?",
            "say_pos":"that Frank got a cat?",
            "say_neg":"that Frank didn't get a cat?",
            "think_pos":"that Frank got a cat?",
            "think_neg":"that Frank didn't get a cat?",
            // "confirm_pos":"Did Walt confirm that Frank got a cat?",
            // "confirm_neg":"Did Walt confirm that Frank didn't get a cat?",
            "inform":"Sam that Frank got a cat?",
            "inform_neg":"Sam that Frank didn't get a cat?",
            "low_prior": "Frank is allergic to cats",
            "high_prior": "Frank has always wanted a pet",
            "high_prior_rating":0.67972027972028,
            "low_prior_rating":0.16965034965035
        },
        "jackson": {
            // "item":"jackson",
            "statement":"Jackson ran 10 miles",
            "negation":"Jackson didn't run 10 miles",
            "simple_polar":"Did Jackson run 10 miles?",
            "know_pos":"that Jackson ran 10 miles?",
            "know_neg":"that Jackson didn't run 10 miles?",
            "say_pos":"that Jackson ran 10 miles?",
            "say_neg":"that Jackson didn't run 10 miles?",
            "think_pos":"that Jackson ran 10 miles?",
            "think_neg":"that Jackson didn't run 10 miles?",
            // "confirm_pos":"Did Randy confirm that Jackson ran 10 miles?",
            // "confirm_neg":"Did Randy confirm that Jackson didn't run 10 miles?",
            "inform_pos":"Sam that Jackson ran 10 miles?",
            "inform_neg":"Sam that Jackson didn't run 10 miles?",
            "low_prior": "Jackson is obese",
            "high_prior": "Jackson is training for a marathon",
            "high_prior_rating":0.774965034965035,
            "low_prior_rating":0.186013986013986
        },
        "jayden": {
            // "item":"jayden",
            "statement":"Jayden rented a car",
            "negation":"Jayden didn't rent a car",
            "simple_polar":"Did Jayden rent a car?",
            "know_pos":"that Jayden rented a car?",
            "know_neg":"that Jayden didn't rent a car?",
            "say_pos":"that Jayden rented a car?",
            "say_neg":"that Jayden didn't rent a car?",
            "think_pos":"that Jayden rented a car?",
            "think_neg":"that Jayden didn't rent a car?",
            // "confirm_pos":"Did Herbert confirm that Jayden rented a car?",
            // "confirm_neg":"Did Herbert confirm that Jayden didn't rent a car?",
            "inform_pos":"Sam that Jayden rented a car?",
            "inform_neg":"Sam that Jayden didn't rent a car?",
            "low_prior": "Jayden doesn't have a driver's license",
            "high_prior": "Jayden's car is in the shop",
            "high_prior_rating":0.687794117647059,
            "low_prior_rating":0.1756
        },
        "tony": {
            // "item":"tony",
            "statement":"Tony had a drink last night",
            "negation":"Tony didn't have a drink last night",
            "simple_polar":"Did Tony have a drink last night?",
            "know_pos":"that Tony had a drink last night?",
            "know_neg":"that Tony didn't have a drink last night?",
            "say_pos":"that Tony had a drink last night?",
            "say_neg":"that Tony didn't have a drink last night?",
            "think_pos":"that Tony had a drink last night?",
            "think_neg":"that Tony didn't have a drink last night?",
            // "confirm_pos":"Did Helen confirm that Tony had a drink last night?",
            // "confirm_neg":"Does Helen confirm that Tony didn't have a drink last night?",
            "inform_pos":"Sam that Tony had a drink last night?",
            "inform_neg":"Sam that Tony didn't have a drink last night?",
            "low_prior": "Tony has been sober for 20 years",
            "high_prior": "Tony really likes to party with his friends",
            "high_prior_rating":0.747279411764706,
            "low_prior_rating":0.2196
        },
        "josh": {
            // "item":"josh",
            "statement":"Josh learned to ride a bike yesterday",
            "negation":"Josh didn't learn to ride a bike yesterday",
            "simple_polar":"Did Josh learn to ride a bike yesterday?",
            "know_pos":"that Josh learned to ride a bike yesterday?",
            "know_neg":"that Josh didn't learn to ride a bike yesterday?",
            "say_pos":"that Josh learned to ride a bike yesterday?",
            "say_neg":"that Josh didn't learn to ride a bike yesterday?",
            "think_pos":"that Josh learned to ride a bike yesterday?",
            "think_neg":"that Josh didn't learn to ride a bike yesterday?",
            // "confirm_pos":"Did Brad confirm that Josh learned to ride a bike yesterday?",
            // "confirm_neg":"Did Brad confirm that Josh didn't learn to ride a bike yesterday?",
            "inform_pos":"Sam that Josh learned to ride a bike yesterday?",
            "inform_neg":"Sam that Josh didn't learn to ride a bike yesterday?",
            "low_prior": "Josh is a 75-year old man",
            "high_prior": "Josh is a 5-year old boy",
            "high_prior_rating":0.544575163398693,
            "low_prior_rating":0.236917293233083
        },
        "owen": {
            // "item":"owen",
            "statement":"Owen shoveled snow last winter",
            "negation":"Owen didn't shovel snow last winter",
            "simple_polar":"Did Owen shovel snow last winter?",
            "know_pos":"that Owen shoveled snow last winter?",
            "know_neg":"that Owen didn't shovel snow last winter?",
            "say_pos":"that Owen shoveled snow last winter?",
            "say_neg":"that Owen didn't shovel snow last winter?",
            "think_pos":"that Owen shoveled snow last winter?",
            "think_neg":"that Owen didn't shovel snow last winter?",
            // "confirm_pos":"Did Jordan confirm that Owen shoveled snow last winter?",
            // "confirm_neg":"Did Jordan confirm that Owen didn't shovel snow last winter?",
            "inform_pos":"Sam that Owen shoveled snow last winter?",
            "inform_neg":"Sam that Owen didn't shovel snow last winter?",
            "low_prior": "Owen lives in New Orleans",
            "high_prior": "Owen lives in Chicago",
            "high_prior_rating":0.74648275862069,
            "low_prior_rating":0.279503546099291
        },
        "olivia": {
            // "item":"olivia",
            "statement":"Olivia sleeps until noon",
            "negation":"Olivia doesn't sleep until noon",
            "simple_polar":"Does Olivia sleep until noon?",
            "know_pos":"that Olivia sleeps until noon?",
            "know_neg":"that Olivia doesn't sleep until noon?",
            "say_pos":"that Olivia sleeps until noon?",
            "say_neg":"that Olivia doesn't sleep until noon?",
            "think_pos":"that Olivia sleeps until noon?",
            "think_neg":"that Olivia doesn't sleep until noon?",
            // "confirm_pos":"Did Jane confirm that Olivia sleeps until noon?",
            // "confirm_neg":"Did Jane confirm that Olivia doesn't sleep until noon?",
            "inform_pos":"Sam that Olivia sleeps until noon?",
            "inform_neg":"Sam that Olivia doesn't sleep until noon?",
            "low_prior": "Olivia has two small children",
            "high_prior": "Olivia works the third shift",
            "high_prior_rating":0.664776119402985,
            "low_prior_rating":0.210592105263158
        },
        "jon": {
            // "item":"jon",
            "statement":"Jon walks to work",
            "negation":"Jon doesn't walk to work",
            "simple_polar":"Does Jon walk to work?",
            "know_pos":"that Jon walks to work?",
            "know_neg":"that Jon doesn't walk to work?",
            "say_pos":"that Jon walks to work?",
            "say_neg":"that Jon doesn't walk to work?",
            "think_pos":"that Jon walks to work?",
            "think_neg":"that Jon doesn't walk to work?",
            // "confirm_pos":"Did Dexter confirm that Jon walks to work?",
            // "confirm_neg":"Did Dexter confirm that Jon doesn't walk to work?",
            "inform_pos":"Sam that Jon walks to work?",
            "inform_neg":"Sam that Jon doesn't walk to work?",
            "low_prior": "Jon lives 10 miles away from work",
            "high_prior": "Jon lives 2 blocks away from work",
            "high_prior_rating":0.7559375,
            "low_prior_rating":0.235696202531646
        },
        "charley": {
            // "item":"charley",
            "statement":"Charley speaks Spanish",
            "negation":"Charley doesn't speak Spanish",
            "simple_polar":"Does Charley speak Spanish?",
            "know_pos":"that Charley speaks Spanish?",
            "know_neg":"that Charley doesn't speak Spanish?",
            "say_pos":"that Charley speaks Spanish?",
            "say_neg":"that Charley doesn't speak Spanish?",
            "think_pos":"that Charley speaks Spanish?",
            "think_neg":"that Charley doesn't speak Spanish?",
            // "confirm_pos":"Did Anton confirm that Charley speaks Spanish?",
            // "confirm_neg":"Did Anton confirm that Charley doesn't speak Spanish?",
            "inform_pos":"Sam that Charley speaks Spanish?",
            "inform_neg":"Sam that Charley doesn't speak Spanish?",
            "low_prior": "Charley lives in Korea",
            "high_prior": "Charley lives in Mexico",
            "high_prior_rating":0.804632352941177,
            "low_prior_rating":0.277066666666667
        }
    }

    var content_items = _.shuffle(["mary","josie","emma", "mia","isabella","emily","danny","grace","zoe",
    "frank","jackson","jayden","tony","josh","owen","olivia","jon","charley"])

    // split critical contents into two groups, half with high and half with low-prob. prior 
    high_prob_contents = [];
    low_prob_contents = [];

    for (var i=0; i<content_items.length/2; i++){
        var stim = content_items[i];
        high_prob_contents.push(stim)
    }
    console.log(high_prob_contents)

    for (var j=content_items.length/2; j<content_items.length; j++){
        var stim = content_items[j];
        low_prob_contents.push(stim)
    }
    console.log(low_prob_contents)

    function makeStim(i, content_list) {
        // get trigger (predicate_pos/_neg)
        var trigger = triggers[i];
        // get a speaker
        var speaker_name = speaker_names.pop(); // speaker_names is a shuffled list of names
        // console.log(speaker_name);
        // get an attitude holder
        var ah_name = ah_names.pop();
        // get item name of the content
        var item = content_list[i];
        // get content
        var content = contents[item];
        // make the utterance
        // "think" and "know" use present tense; "say" and "inform" use past tense
        if (trigger == "simple_polar") {
            var predicate = "NA";
            var utterance = content[trigger];
            ah_name = "NA"; // no attitude holder if it is simple polar
        } else if (trigger.includes("know") | trigger.includes("think")) {
            var predicate = trigger.split("_")[0]; // only use the predicate, not the entire trigger label with "_pos"
            var utterance = "Does <font color=\"red\">" + ah_name + "</font> " + predicate + " " + content[trigger]
        } else {
            var predicate = trigger.split("_")[0]; // only use the predicate, not the entire trigger label with "_pos"
            var utterance = "Did <font color=\"red\">" + ah_name + "</font> " + predicate + " " + content[trigger]
        }
        var statement = content.statement;
        var negation = content.negation;
        if (content_list == high_prob_contents){
            var prior_fact = content.high_prior;
            if (trigger.includes("_neg")) {
                var prior_rating = 1 - content.high_prior_rating;
            } else {
                var prior_rating = content.high_prior_rating;
            }
            var prior_condition = "high prob of p";
        } else {
            var prior_fact = content.low_prior;
            if (trigger.includes("_neg")) {
                var prior_rating = 1 - content.high_prior_rating;
            } else {
                var prior_rating = content.low_prior_rating;
            }
            var prior_condition = "low prob of p";
        }
        
        return {
            "speaker_name": speaker_name,
            "ah_name": ah_name,	  
            "trigger": trigger,
            "predicate" : predicate,
            "trigger_class": "Critical",
            "content": content,
            "utterance": utterance,
            "statement": statement,
            "negation": negation,
            "prior_rating":prior_rating,
            "prior_fact":prior_fact,
            "prior_condition":prior_condition
        }
    }

    // controls
    var mcitemnames = _.shuffle(["muffins","pizza","kids","ballet","garage","hat"]);
    var mcitems = {
        "muffins": {
            "statement":"these muffins have blueberries in them",
            "negation":"these muffins don't have blueberries in them",
            "MCq":"Do these muffins have blueberries in them?",
            "MCa":"These muffins have blueberries in them.",
            "prior_fact": "Muffins are sold at the bakery"},
        "pizza": {
            "statement":"this pizza has mushrooms on it",
            "negation":"this pizza doesn't have mushrooms on it",
            "MCq":"Does this pizza have mushrooms on it?",
            "MCa":"This pizza has mushrooms on it.",
            "prior_fact": "Pizza is sold at the pizzeria"},
        "kids": {
            "statement":"Jack was playing outside with the kids",
            "negation":"Jack wasn't playing outside with the kids",
            "MCq":"Was Jack playing outside with the kids?",
            "MCa":"Jack was playing outside with the kids.",
            "prior_fact": "Many children like ice cream"},
        "ballet": {
            "statement":"Ann dances ballet",
            "negation":"Ann doesn't dance ballet",
            "MCq":"Does Ann dance ballet?",
            "MCa":"Ann dances ballet.",
            "prior_fact": "Ballet is a type of dance"},
        "garage": {
            "statement":"Carl's kids were in the garage",
            "negation":"Carl's kids weren't in the garage",
            "MCq":"Were Carl's kids in the garage?",
            "MCa":"Carl's kids were in the garage.",
            "prior_fact": "Garages are used to store cars and other things"},
        "hat": {
            "statement":"Samantha has a new hat",
            "negation":"Samantha doesn't have a new hat",
            "MCq":"Does Samantha have a new hat?",
            "MCa":"Samantha has a new hat.",
            "prior_fact": "Hats are worn on the head"}
    };

    function makeMCStim(j, item_list) {
        // get item
        var item = item_list[j];
        // get a speaker
        var speaker_name = speaker_names.pop();
        // get content
        var content = mcitems[item];
        // console.log(content)
        var utterance = content.MCq;
        var statement = content.statement;  
        var negation = content.negation;
        var prior_fact = content.prior_fact; 
    //    console.log(contents[trigger_cont]); 
        return {
            "speaker_name": speaker_name,
            "ah_name":"NA",
            "trigger": "MC", 
            "predicate" : "NA", 
            "trigger_class": "Control",
            "content": content,
            "utterance": utterance,
            "statement": statement,
            "negation": negation,
            "prior_rating":0,
            "prior_fact": prior_fact,
            "prior_condition":"NA"
        }
    }

    exp.stims_block1 = [];
    
    // loop through the two content lists, make each of them a stimuli, and add 
    // them to the stimuli set in the block
    // the two lists should have the same number of contents
    for (var i=0; i<high_prob_contents.length; i++) {
        var high_prob_stim = makeStim(i, high_prob_contents);
        high_prob_stim["question_order"] = question_order;
        exp.stims_block1.push(jQuery.extend(true, {}, high_prob_stim));
        var low_prob_stim = makeStim(i, low_prob_contents);
        low_prob_stim["question_order"] = question_order;
        exp.stims_block1.push(jQuery.extend(true, {}, low_prob_stim));
    }
	exp.stims_block1 = _.shuffle(exp.stims_block1); // randomize the critical items (no need)

    // add the control items
    for (var l=0; l<mcitemnames.length; l++) {
        var stim = makeMCStim(l,mcitemnames);
        stim["question_order"] = question_order;
        exp.stims_block1.push(jQuery.extend(true, {}, stim));
    }  

    // randomize the items within each block
	exp.stims_block1 = _.shuffle(exp.stims_block1); 
    console.log(exp.stims_block1) 

    exp.trials = [];
    exp.catch_trials = [];
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

    exp.data_trials = [];
    //make corresponding slides:
    exp.slides = make_slides(exp);

    //   exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                        //relies on structure and slides being defined
                        
    exp.nQs = 3 + 18 + 6 + 1; 
    $(".nQs").html(exp.nQs);

    $('.slide').hide(); //hide everything

    $("#start_button").click(function() {
        exp.go();
    });

    exp.go(); //show first slide
}
