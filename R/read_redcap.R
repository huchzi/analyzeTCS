read_redcap <- function(filename) {
  
  #Read Data
  data=read.csv(filename)
  #Setting Labels
  
  label(data$record_id)="Study ID"
  label(data$redcap_repeat_instrument)="Repeat Instrument"
  label(data$redcap_repeat_instance)="Repeat Instance"
  label(data$begin_date)="Date: Begin of Exam"
  label(data$end_date)="Date: End of Exam"
  label(data$age_exam)="Age"
  label(data$current_complaints)="Current complaints  "
  label(data$orientation_in_space)="Impaired orientation in space "
  label(data$recognizing_faces)="Difficulty recognizing faces in the street"
  label(data$reading_ability)="Reading ability limited "
  label(data$driving_a_car)="Driving a car "
  label(data$watching_television)="Watching TV possible "
  label(data$eye_surgeries)="History of Eye Surgery"
  label(data$pseudophakia)="Pseudophakia"
  label(data$eye_surgeries_other)="Other Eye Surgeries"
  label(data$medication)="Current Eye Medication"
  label(data$mydriasis)="Mydriasis"
  label(data$comment)="Comment:"
  label(data$scva_od)="Uncorrected Visual Acuity - Right Eye"
  label(data$sphere_od)="Sphere - Right Eye"
  label(data$cylinder_od)="Cylinder - Right Eye"
  label(data$axis_od)="Axis - Right Eye"
  label(data$bcva_od)="Best Corrected Visual Acuity - Right Eye"
  label(data$scva_os)="Uncorrected Visual Acuity - Left Eye"
  label(data$sphere_os)="Sphere - Left Eye"
  label(data$cylinder_os)="Cylinder - Left Eye"
  label(data$axis_os)="Axis - Left Eye"
  label(data$bcva_os)="Best Corrected Visual Acuity - Left Eye"
  label(data$metropsis_visual_acuity_od)="Metropsis Visual Acuity  - Right Eye"
  label(data$metropsis_visual_acuity_os)="Metropsis Visual Acuity - Left Eye"
  label(data$color_vision_tests___0)="Color Vision Tests performed (choice=Cambridge Color Test - Right Eye)"
  label(data$color_vision_tests___1)="Color Vision Tests performed (choice=Cambridge Color Test - Left Eye)"
  label(data$color_vision_tests___2)="Color Vision Tests performed (choice=Anomalscope - Right Eye)"
  label(data$color_vision_tests___3)="Color Vision Tests performed (choice=Anomalscope - Left Eye)"
  label(data$color_vision_tests___4)="Color Vision Tests performed (choice=Panel D15 (saturated) - Right Eye)"
  label(data$color_vision_tests___5)="Color Vision Tests performed (choice=Panel D15 (saturated) - Left Eye)"
  label(data$protan_od)="Protan - Right Eye"
  label(data$protan_os)="Protan - Left Eye"
  label(data$deutan_od)="Deutan - Right Eye"
  label(data$deutan_os)="Deutan - Left Eye"
  label(data$tritan_od)="Tritan - Right Eye"
  label(data$tritan_os)="Tritan - Left Eye"
  label(data$anomaloscope_aq1_od)="Anomaly Quotient 1 - Right Eye"
  label(data$anomaloscope_aq2_od)="Anomaly Quotient 2 - Right Eye"
  label(data$anomaloscope_aq1_os)="Anomaly Quotient 1 - Left Eye"
  label(data$anomaloscope_aq2_os)="Anomaly Quotient 2 - Left Eye"
  label(data$od_1)="Position 1"
  label(data$od_2)="Position 2"
  label(data$od_3)="Position 3"
  label(data$od_4)="Position 4"
  label(data$od_5)="Position 5"
  label(data$od_6)="Position 6"
  label(data$od_7)="Position 7"
  label(data$od_8)="Position 8"
  label(data$od_9)="Position 9"
  label(data$od_10)="Position 10"
  label(data$od_11)="Position 11"
  label(data$od_12)="Position 12"
  label(data$od_13)="Position 13"
  label(data$od_14)="Position 14"
  label(data$od_15)="Position 15"
  label(data$os_1)="Position 1"
  label(data$os_2)="Position 2"
  label(data$os_3)="Position 3"
  label(data$os_4)="Position 4"
  label(data$os_5)="Position 5"
  label(data$os_6)="Position 6"
  label(data$os_7)="Position 7"
  label(data$os_8)="Position 8"
  label(data$os_9)="Position 9"
  label(data$os_10)="Position 10"
  label(data$os_11)="Position 11"
  label(data$os_12)="Position 12"
  label(data$os_13)="Position 13"
  label(data$os_14)="Position 14"
  label(data$os_15)="Position 15"
  label(data$perimetry___0)="Type of Perimetry performed? (choice=Kinetic- Right Eye)"
  label(data$perimetry___1)="Type of Perimetry performed? (choice=Kinetic- Left Eye)"
  label(data$perimetry___2)="Type of Perimetry performed? (choice=Static- Right Eye)"
  label(data$perimetry___3)="Type of Perimetry performed? (choice=Static- Left Eye)"
  label(data$goldmann_od)="Goldmann VF - Right Eye"
  label(data$goldmann_os)="Goldmann VF - Left Eye"
  label(data$octopus_program___0)="Octopus  (choice=G1)"
  label(data$octopus_program___1)="Octopus  (choice=M)"
  label(data$octopus_program___2)="Octopus  (choice=LVP)"
  label(data$octopus_program___3)="Octopus  (choice=other)"
  label(data$static_md_od)="Mean Deviation - Right Eye "
  label(data$static_md_os)="Mean Deviation - Left Eye"
  label(data$comments_perimetry)="Comments Perimetry"
  label(data$fundus_photograph)="Fundus photograph available?"
  label(data$oct_central_od)="Central retinal thickness - Right Eye"
  label(data$oct_central_os)="Central retinal thickness - Left Eye"
  label(data$erg_performed___0)="Were ERG measurements performed? (choice=RetiPort ISCEV ERG)"
  label(data$erg_performed___1)="Were ERG measurements performed? (choice=RETEval ISCEV ERG)"
  label(data$erg_performed___2)="Were ERG measurements performed? (choice=Multifokal ERG)"
  label(data$erg_performed___3)="Were ERG measurements performed? (choice=Experimental Protocol)"
  label(data$rod_b_od)="Scotopic Rod Response - Amplitude b-wave"
  label(data$rod_b_os)="Scotopic Rod Response - Amplitude b-wave"
  label(data$mixed_a_od)="Scotopic Max Response - Amplitude a-wave"
  label(data$mixed_a_os)="Scotopic Max Response - Amplitude a-wave"
  label(data$mixed_b_od)="Scotopic Max Response - Amplitude b-wave"
  label(data$mixed_b_os)="Scotopic Max Response - Amplitude b-wave"
  label(data$cone_a_od)="Photopic Cone Response - Amplitude a-wave"
  label(data$cone_a_os)="Photopic Cone Response - Amplitude a-wave"
  label(data$cone_b_od)="Photopic Cone Response - Amplitude b-wave"
  label(data$cone_b_os)="Photopic Cone Response - Amplitude b-wave"
  label(data$flicker_fft_od)="Photopic Flicker 30 Hz - Amplitude FFT"
  label(data$flicker_fft_os)="Photopic Flicker 30 Hz - Amplitude FFT"
  label(data$basic_examination_complete)="Complete?"
  #Setting Units
  
  
  #Setting Factors(will create new variable for factors)
  data$redcap_repeat_instrument.factor = factor(data$redcap_repeat_instrument,levels=c("basic_examination"))
  data$orientation_in_space.factor = factor(data$orientation_in_space,levels=c("1","0"))
  data$recognizing_faces.factor = factor(data$recognizing_faces,levels=c("1","0"))
  data$reading_ability.factor = factor(data$reading_ability,levels=c("1","0"))
  data$driving_a_car.factor = factor(data$driving_a_car,levels=c("1","0"))
  data$watching_television.factor = factor(data$watching_television,levels=c("1","0"))
  data$eye_surgeries.factor = factor(data$eye_surgeries,levels=c("1","0"))
  data$pseudophakia.factor = factor(data$pseudophakia,levels=c("1","0"))
  data$mydriasis.factor = factor(data$mydriasis,levels=c("1","0"))
  data$sphere_od.factor = factor(data$sphere_od,levels=c("-20","-19.75","-19.5","-19.25","-19","-18.75","-18.5","-18.25","-18","-17.75","-17.5","-17.25","-17","-16.75","-16.5","-16.25","-16","-15.75","-15.5","-15.25","-15","-14.75","-14.5","-14.25","-14","-13.75","-13.5","-13.25","-13","-12.75","-12.5","-12.25","-12","-11.75","-11.5","-11.25","-11","-10.75","-10.5","-10.25","-10","-9.75","-9.5","-9.25","-9","-8.75","-8.5","-8.25","-8","-7.75","-7.5","-7.25","-7","-6.75","-6.5","-6.25","-6","-5.75","-5.5","-5.25","-5","-4.75","-4.5","-4.25","-4","-3.75","-3.5","-3.25","-3","-2.75","-2.5","-2.25","-2","-1.75","-1.5","-1.25","-1","-0.75","-0.5","-0.25","0","0.25","0.5","0.75","1","1.25","1.5","1.75","2","2.25","2.5","2.75","3","3.25","3.5","3.75","4","4.25","4.5","4.75","5","5.25","5.5","5.75","6","6.25","6.5","6.75","7","7.25","7.5","7.75","8","8.25","8.5","8.75","9","9.25","9.5","9.75","10","10.25","10.5","10.75","11","11.25","11.5","11.75","12","12.25","12.5","12.75","13","13.25","13.5","13.75","14","14.25","14.5","14.75","15"))
  data$cylinder_od.factor = factor(data$cylinder_od,levels=c("-6","-5.75","-5.5","-5.25","-5","-4.75","-4.5","-4.25","-4","-3.75","-3.5","-3.25","-3","-2.75","-2.5","-2.25","-2","-1.75","-1.5","-1.25","-1","-0.75","-0.5","-0.25","0"))
  data$sphere_os.factor = factor(data$sphere_os,levels=c("-20","-19.75","-19.5","-19.25","-19","-18.75","-18.5","-18.25","-18","-17.75","-17.5","-17.25","-17","-16.75","-16.5","-16.25","-16","-15.75","-15.5","-15.25","-15","-14.75","-14.5","-14.25","-14","-13.75","-13.5","-13.25","-13","-12.75","-12.5","-12.25","-12","-11.75","-11.5","-11.25","-11","-10.75","-10.5","-10.25","-10","-9.75","-9.5","-9.25","-9","-8.75","-8.5","-8.25","-8","-7.75","-7.5","-7.25","-7","-6.75","-6.5","-6.25","-6","-5.75","-5.5","-5.25","-5","-4.75","-4.5","-4.25","-4","-3.75","-3.5","-3.25","-3","-2.75","-2.5","-2.25","-2","-1.75","-1.5","-1.25","-1","-0.75","-0.5","-0.25","0","0.25","0.5","0.75","1","1.25","1.5","1.75","2","2.25","2.5","2.75","3","3.25","3.5","3.75","4","4.25","4.5","4.75","5","5.25","5.5","5.75","6","6.25","6.5","6.75","7","7.25","7.5","7.75","8","8.25","8.5","8.75","9","9.25","9.5","9.75","10","10.25","10.5","10.75","11","11.25","11.5","11.75","12","12.25","12.5","12.75","13","13.25","13.5","13.75","14","14.25","14.5","14.75","15"))
  data$cylinder_os.factor = factor(data$cylinder_os,levels=c("-6","-5.75","-5.5","-5.25","-5","-4.75","-4.5","-4.25","-4","-3.75","-3.5","-3.25","-3","-2.75","-2.5","-2.25","-2","-1.75","-1.5","-1.25","-1","-0.75","-0.5","-0.25","0"))
  data$color_vision_tests___0.factor = factor(data$color_vision_tests___0,levels=c("0","1"))
  data$color_vision_tests___1.factor = factor(data$color_vision_tests___1,levels=c("0","1"))
  data$color_vision_tests___2.factor = factor(data$color_vision_tests___2,levels=c("0","1"))
  data$color_vision_tests___3.factor = factor(data$color_vision_tests___3,levels=c("0","1"))
  data$color_vision_tests___4.factor = factor(data$color_vision_tests___4,levels=c("0","1"))
  data$color_vision_tests___5.factor = factor(data$color_vision_tests___5,levels=c("0","1"))
  data$od_1.factor = factor(data$od_1,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_2.factor = factor(data$od_2,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_3.factor = factor(data$od_3,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_4.factor = factor(data$od_4,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_5.factor = factor(data$od_5,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_6.factor = factor(data$od_6,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_7.factor = factor(data$od_7,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_8.factor = factor(data$od_8,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_9.factor = factor(data$od_9,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_10.factor = factor(data$od_10,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_11.factor = factor(data$od_11,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_12.factor = factor(data$od_12,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_13.factor = factor(data$od_13,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_14.factor = factor(data$od_14,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$od_15.factor = factor(data$od_15,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_1.factor = factor(data$os_1,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_2.factor = factor(data$os_2,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_3.factor = factor(data$os_3,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_4.factor = factor(data$os_4,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_5.factor = factor(data$os_5,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_6.factor = factor(data$os_6,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_7.factor = factor(data$os_7,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_8.factor = factor(data$os_8,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_9.factor = factor(data$os_9,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_10.factor = factor(data$os_10,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_11.factor = factor(data$os_11,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_12.factor = factor(data$os_12,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_13.factor = factor(data$os_13,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_14.factor = factor(data$os_14,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$os_15.factor = factor(data$os_15,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15"))
  data$perimetry___0.factor = factor(data$perimetry___0,levels=c("0","1"))
  data$perimetry___1.factor = factor(data$perimetry___1,levels=c("0","1"))
  data$perimetry___2.factor = factor(data$perimetry___2,levels=c("0","1"))
  data$perimetry___3.factor = factor(data$perimetry___3,levels=c("0","1"))
  data$octopus_program___0.factor = factor(data$octopus_program___0,levels=c("0","1"))
  data$octopus_program___1.factor = factor(data$octopus_program___1,levels=c("0","1"))
  data$octopus_program___2.factor = factor(data$octopus_program___2,levels=c("0","1"))
  data$octopus_program___3.factor = factor(data$octopus_program___3,levels=c("0","1"))
  data$fundus_photograph.factor = factor(data$fundus_photograph,levels=c("1","0"))
  data$erg_performed___0.factor = factor(data$erg_performed___0,levels=c("0","1"))
  data$erg_performed___1.factor = factor(data$erg_performed___1,levels=c("0","1"))
  data$erg_performed___2.factor = factor(data$erg_performed___2,levels=c("0","1"))
  data$erg_performed___3.factor = factor(data$erg_performed___3,levels=c("0","1"))
  data$basic_examination_complete.factor = factor(data$basic_examination_complete,levels=c("0","1","2"))
  
  levels(data$redcap_repeat_instrument.factor)=c("Basic Examination")
  levels(data$orientation_in_space.factor)=c("Yes","No")
  levels(data$recognizing_faces.factor)=c("Yes","No")
  levels(data$reading_ability.factor)=c("Yes","No")
  levels(data$driving_a_car.factor)=c("Yes","No")
  levels(data$watching_television.factor)=c("Yes","No")
  levels(data$eye_surgeries.factor)=c("Yes","No")
  levels(data$pseudophakia.factor)=c("Yes","No")
  levels(data$mydriasis.factor)=c("Yes","No")
  levels(data$sphere_od.factor)=c("-20","-19.75","-19.5","-19.25","-19","-18.75","-18.5","-18.25","-18","-17.75","-17.5","-17.25","-17","-16.75","-16.5","-16.25","-16","-15.75","-15.5","-15.25","-15","-14.75","-14.5","-14.25","-14","-13.75","-13.5","-13.25","-13","-12.75","-12.5","-12.25","-12","-11.75","-11.5","-11.25","-11","-10.75","-10.5","-10.25","-10","-9.75","-9.5","-9.25","-9","-8.75","-8.5","-8.25","-8","-7.75","-7.5","-7.25","-7","-6.75","-6.5","-6.25","-6","-5.75","-5.5","-5.25","-5","-4.75","-4.5","-4.25","-4","-3.75","-3.5","-3.25","-3","-2.75","-2.5","-2.25","-2","-1.75","-1.5","-1.25","-1","-0.75","-0.5","-0.25","0","0.25","0.5","0.75","1","1.25","1.5","1.75","2","2.25","2.5","2.75","3","3.25","3.5","3.75","4","4.25","4.5","4.75","5","5.25","5.5","5.75","6","6.25","6.5","6.75","7","7.25","7.5","7.75","8","8.25","8.5","8.75","9","9.25","9.5","9.75","10","10.25","10.5","10.75","11","11.25","11.5","11.75","12","12.25","12.5","12.75","13","13.25","13.5","13.75","14","14.25","14.5","14.75","15")
  levels(data$cylinder_od.factor)=c("-6","-5.75","-5.5","-5.25","-5","-4.75","-4.5","-4.25","-4","-3.75","-3.5","-3.25","-3","-2.75","-2.5","-2.25","-2","-1.75","-1.5","-1.25","-1","-0.75","-0.5","-0.25","0")
  levels(data$sphere_os.factor)=c("-20","-19.75","-19.5","-19.25","-19","-18.75","-18.5","-18.25","-18","-17.75","-17.5","-17.25","-17","-16.75","-16.5","-16.25","-16","-15.75","-15.5","-15.25","-15","-14.75","-14.5","-14.25","-14","-13.75","-13.5","-13.25","-13","-12.75","-12.5","-12.25","-12","-11.75","-11.5","-11.25","-11","-10.75","-10.5","-10.25","-10","-9.75","-9.5","-9.25","-9","-8.75","-8.5","-8.25","-8","-7.75","-7.5","-7.25","-7","-6.75","-6.5","-6.25","-6","-5.75","-5.5","-5.25","-5","-4.75","-4.5","-4.25","-4","-3.75","-3.5","-3.25","-3","-2.75","-2.5","-2.25","-2","-1.75","-1.5","-1.25","-1","-0.75","-0.5","-0.25","0","0.25","0.5","0.75","1","1.25","1.5","1.75","2","2.25","2.5","2.75","3","3.25","3.5","3.75","4","4.25","4.5","4.75","5","5.25","5.5","5.75","6","6.25","6.5","6.75","7","7.25","7.5","7.75","8","8.25","8.5","8.75","9","9.25","9.5","9.75","10","10.25","10.5","10.75","11","11.25","11.5","11.75","12","12.25","12.5","12.75","13","13.25","13.5","13.75","14","14.25","14.5","14.75","15")
  levels(data$cylinder_os.factor)=c("-6","-5.75","-5.5","-5.25","-5","-4.75","-4.5","-4.25","-4","-3.75","-3.5","-3.25","-3","-2.75","-2.5","-2.25","-2","-1.75","-1.5","-1.25","-1","-0.75","-0.5","-0.25","0")
  levels(data$color_vision_tests___0.factor)=c("Unchecked","Checked")
  levels(data$color_vision_tests___1.factor)=c("Unchecked","Checked")
  levels(data$color_vision_tests___2.factor)=c("Unchecked","Checked")
  levels(data$color_vision_tests___3.factor)=c("Unchecked","Checked")
  levels(data$color_vision_tests___4.factor)=c("Unchecked","Checked")
  levels(data$color_vision_tests___5.factor)=c("Unchecked","Checked")
  levels(data$od_1.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_2.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_3.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_4.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_5.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_6.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_7.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_8.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_9.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_10.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_11.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_12.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_13.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_14.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$od_15.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_1.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_2.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_3.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_4.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_5.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_6.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_7.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_8.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_9.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_10.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_11.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_12.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_13.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_14.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$os_15.factor)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")
  levels(data$perimetry___0.factor)=c("Unchecked","Checked")
  levels(data$perimetry___1.factor)=c("Unchecked","Checked")
  levels(data$perimetry___2.factor)=c("Unchecked","Checked")
  levels(data$perimetry___3.factor)=c("Unchecked","Checked")
  levels(data$octopus_program___0.factor)=c("Unchecked","Checked")
  levels(data$octopus_program___1.factor)=c("Unchecked","Checked")
  levels(data$octopus_program___2.factor)=c("Unchecked","Checked")
  levels(data$octopus_program___3.factor)=c("Unchecked","Checked")
  levels(data$fundus_photograph.factor)=c("Yes","No")
  levels(data$erg_performed___0.factor)=c("Unchecked","Checked")
  levels(data$erg_performed___1.factor)=c("Unchecked","Checked")
  levels(data$erg_performed___2.factor)=c("Unchecked","Checked")
  levels(data$erg_performed___3.factor)=c("Unchecked","Checked")
  levels(data$basic_examination_complete.factor)=c("Incomplete","Unverified","Complete")

  data.table(data)  
}
