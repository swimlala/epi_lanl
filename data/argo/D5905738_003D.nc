CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  e   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:27Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        (  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  G   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  I�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  T�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     (  W�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  b�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  p�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  |   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ~�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     (  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ր   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ֈ   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ֐   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �  ֘   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                     �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                     �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ɸ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ո      �  ոArgo profile    3.1 1.2 19500101000000  20180724220227  20210722160147  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�c��>�@�c��>�11  @�c�O�@@�c�O�@@6�5?|�@6�5?|��c��6���c��6��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @   @Fff@�33@�  @�33@�  A   A33A&ffAC33Ac33A���A�  A���A���A�  Aљ�A���A�  B   B  BffBffB ��B(  B0  B8  B@  BHffBP  BXffB`ffBhffBo��Bw��B�  B�33B�ffB�33B�  B���B���B�33B�33B���B�  B�ffB�33B�ffB�33B���B�33B�ffB�  B�33B�ffB�ffB�  B�33B�  B�33B�33B���B�  B�33B�33B�ffC 33CL�C33C  C�C
  C�fC�fC  C�C  C  C�fC�C33C�C �C"�C$  C&  C'�fC)�fC,  C-�fC0  C2  C3�fC5�fC7�fC:  C<  C>�C@33CB�CD�CE�fCH�CJ  CL  CN�CP�CR33CT  CV  CX�CZ  C\  C^  C_�fCb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx33Cz�C|�C~33C��C�  C��C��C�  C��3C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C��C��C��C��3C�  C��C��3C�  C�  C��C��C�  C��C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C��C��C��C��C��C��C�  C�  C��C�  C��C��C�  C��3C��3C��3C�  C�  C��C�  C�  C��C��C�  C��C��C�  C��3C�  C��3C�  C��C��C��3C�  C�  C��3C��3C��C��C��C��C�  C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�  C�  C��3C�  C��C�  C��3C�  C��3C��3C��D ��D�fD  DY�D� DٚD�fD
  D3D&fD9�DY�Dy�D�fD��D�3D�DFfDl�D� D� DٚD��D33D@ D ` D!s3D"� D#��D%��D&��D'�fD(ٚD)�3D+3D,,�D.y�D/�3D0� D1ٚD2�3D4�D5&fD633D8@ D933D:  D;�fD<�fD>9�D>� D@FfDA�fDC  DCٚDE�fDF�fDH��DI� DJ��DK��DM� DN` DOL�DQ  DQ��DSy�DT&fDUs3DV�fDX�DYl�DZ��D[l�D\�fD^ffD_� D`�3Db3Dc� Dd` De�3Dgy�Dh9�Di��Dj��Dl  Dm�fDn` DoٚDq@ Dr��Ds,�Dty�Du��Dw&fDx�Dy� Dz��>���>���>���>���>���>���>���>���>���?   ?   ?   ?   >���?   ?   ?��?333?333?fff?�  ?���?���?�33?���?ٙ�?�33@��@   @&ff@@  @L��@Y��@l��@�33@���@�ff@�  @���@�ff@�33@�  @ٙ�@�ff@�33A   A  AffAffA��A!��A+33A0  A8  A@  AFffAL��AT��A\��Aa��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444441444414114111411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           ?fff?�  @   @fff@�33@�  @�33@�  A  A33A.ffAK33Ak33A���A�  A���A���A�  Aՙ�A���A�  B  B
  BffBffB"��B*  B2  B:  BB  BJffBR  BZffBbffBjffBq��By��B�  B�33B�ffB�33B�  B���B���B�33B�33B���B�  B�ffB�33B�ffB�33B���B�33B�ffB�  B�33B�ffB�ffB�  B�33B�  B�33B�33B���B�  B�33B�33B�ffC �3C��C�3C� C��C
� CffCffC� C��C� C� CffC��C�3C��C ��C"��C$� C&� C(ffC*ffC,� C.ffC0� C2� C4ffC6ffC8ffC:� C<� C>��C@�3CB��CD��CFffCH��CJ� CL� CN��CP��CR�3CT� CV� CX��CZ� C\� C^� C`ffCb� CdffCf� Ch� Cj� Cl� Cn� Cp� Cr� Ct� Cv��Cx�3Cz��C|��C~�3C�L�C�@ C�L�C�Y�C�@ C�33C�@ C�@ C�33C�@ C�@ C�@ C�33C�@ C�@ C�@ C�L�C�L�C�Y�C�33C�@ C�L�C�33C�@ C�@ C�L�C�L�C�@ C�L�C�@ C�@ C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�L�C�@ C�@ C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�L�C�@ C�L�C�L�C�@ C�33C�33C�33C�@ C�@ C�L�C�@ C�@ C�Y�C�L�C�@ C�L�C�L�C�@ C�33C�@ C�33C�@ C�Y�C�L�C�33C�@ C�@ C�33C�33C�L�C�Y�C�Y�C�L�C�@ C�Y�C�L�C�L�C�@ C�L�C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�Y�C�L�C�@ C�@ C�33C�@ C�L�C�@ C�33C�@ C�33C��3C�L�D ��DfD@ Dy�D� D��D	fD
  D33DFfDY�Dy�D��D�fD��D3D9�DffD��D� D� D��D�DS3D` D � D!�3D"� D#��D%��D&ٚD'�fD(��D*3D+33D,L�D.��D/�3D0� D1��D33D4,�D5FfD6S3D8` D9S3D:@ D<fD<�fD>Y�D?  D@ffDA�fDC@ DC��DE�fDF�fDH��DI� DJ��DK��DM� DN� DOl�DQ  DQ��DS��DTFfDU�3DV�fDX,�DY��DZ��D[��D]fD^�fD`  D`�3Db33Dc� Dd� Df3Dg��DhY�Di��Dj��Dl@ Dm�fDn� Do��Dq` Dr��DsL�Dt��Du��DwFfDx9�Dy� Dz��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�G�O�?fffG�O�?�  ?���G�O�?���?�33?�  G�O�?ٙ�?�33@ff@��@��@,��@@  @Fff@`  @l��@y��@�ff@�33@���@�ff@�  @���@�ff@�33@�  @陚@�ffA��A  A  AffAffA$��A)��A333A8  A@  AH  ANffAT��A\��Ad��Ai��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444444441444414114111411111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           @ �@ �@ �@ *@ �@ ""@ )�@ /�@ 6�@ >�@ F�@ SI@ `�@ n�@ z�@ �7@ ��@ ��@ ��@ ��@ ��@ �t@ �@ �q@@o@�@,`@:@G�@V@b�@qS@~�@��@��@��@��@�>@є@ލ@�@�~@%@*@"�@/@=q@Lu@Yn@g�@t�@�@�@�a@��@�@�W@��@�H@�@��@
�@�@$�@33@A�@O0@]�@k.@y�@�|@��@�@�@�@ȴ@�
@�`@�Y@  @�@�@*S@7L@D�@R�@_�@m:@z3@��@�0@�(@�~@�&@�@��@�m@��@j@�@ @-@:�@F�@V@b�@p�@~�@��@��@�A@��@�>@�7@��@�@�~@�@�@""@/�@=q@K@X�@ff@t@��@�@�a@�Y@�@�W@�O@�H@�@��@
=@6@%�@33@@,@N�@\)@i�@v�@�@��@�m@��@�k@��@�[@�@�@�Q@�@O@)�@7L@DD@R�@_�@m:@{�@�7@�0@��@�~@�&@��@�t@�@��@	j@	�@	�@	,`@	:�@	H]@	V@	c�@	qS@	~�@	��@	�H@	�A@	��@	�>@	�7@	ލ@	�4@	�,@
%@
�@
!s@
/�@
=q@
K�@
X�@
ff@
uk@
�d@
�\@
��@
�Y@
�R@
�J@
Ӡ@
��@
��@
��@
�@6@%�@33@@,@M�@\�@k.@x�@��@��@��@��@�k@�c@׹@�`@�@^@@�@*S@7L@D�@R�@`B@m�@{�@�7@��@��@�-@��@�*@�#@�@��@�@@g@,`@9X@G�@T�@^�@�H@��@�#@��@
@=q@}�@�H@�R@խ@�@b@/@M�@��@��@�@�m@�@&�@G�@ff@�p@�(@��@��@O@8�@UU@r@�@ȴ@�`@�@ �@?}@]�@�U@�^@�t@�~@�@4�@R�@oF@�A@�2@�t@
�@g@Ji@\)@�d@��@�7@�@*@-@dZ@~K@��@�F@�@j@�@K@`�@��@�@Ĝ@��@�@1'@V�@g�@�@�@�H@�e@[@G�@\)@�+@��@�J@�L@�@/�@Yn@m:@��@��@��@�L@�@2�@\�@v�@�m@��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ �G�O�G�O�G�O�G�O�@ �G�O�@ j@ G�O�@ �@ %@ �G�O�@ 1@ 	�@ 
�@ �@ �@ @ @ �@ {@ �@ 6@ B@ �@ 
@  @ ""@ $.@ &�@ )�@ ,`@ .l@ 1'@ 3�@ 6�@ :@ <�@ @,@ B�@ D�@ I@ K@ N�@ Q�@ T�@ Wb@ Z�@ ^5@ `BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AżjA���A���A�33A�A�A�A�A�A�A�?}A�=qA�;dA�5?A�"�A���A�&�A�
=A���A��A��/A�Aħ�Aě�A�l�A���AÓuA�~�A�5?A�A�+A�A���A��A�ĜA��7A��;A��A�A�n�A�
=A��HA�5?A�x�A��A�C�A�ZA��HA�v�A�VA���A�VA�+A�$�A���A���A��A�r�A�ffA�A�A��A��/A�r�A�/A��A��`A��A���A�$�A��
A�/A�$�A�{A�G�A�l�A��A���A�`BA��A��hA��A�~�A��HA��\A�XA��^A�VA��A�33A�|�A�z�A�VA�%A�  A��A��A��`A�I�A�  A��\A���A�bNA��A�ȴA���A��DA���A��A���A�M�A�t�A��mA�bNA��
A�l�A�ZA�;dA�hsA�|�A��A��DA��HA��yA�I�A�VA~�yA{�Aw�At^5Ar��Ap��An��Ak/Af�!Ad �Ac�Ab��A`�/A^�RA]�A\��A[��AZ�HAY�FAX=qAWhsAV��AV�DAU�7AT{ASC�ARE�AQp�AO�-AN5?AM�^ALVAKVAJVAJ �AG33AEK�AA�^A@{A>�A<�`A:�/A:{A8��A7�A6�A5O�A4�uA49XA3p�A0��A/O�A.��A-��A,�A,JA+33A*ĜA*�A*v�A)�A)|�A(��A(A�A';dA%��A$A�A#hsA!��A�A+A�+A�AoAn�A�-Ar�A�A�wA�!A5?A{A��A/A&�A�^A~�A�FA�TAbNA��A�\AJAx�A
=A�hAQ�A1A\)A
�jA
ffA	O�A�A�TA�-A�9A�DA�-A~�A�;A��A�PAt�AXA v�@�%@���@��@���@���@�X@�l�@���@�@��@�%@�9X@�z�@�@��@�@���@�b@��;@�~�@�%@܃@ى7@�%@�ƨ@ҸR@�X@�^5@ȴ9@�33@�33@�v�@���@�/@��u@�bN@��;@��y@�`B@�j@���@���@��@�33@��+@�~�@�V@��P@��@��R@��@�n�@��@�O�@���@��;@���@��@��@�O�@�`B@���@��9@�9X@���@��@���@��m@��R@�dZ@�Ĝ@�z�@�1@�b@��@�+@���@���@���@��h@�X@��@��m@��w@�|�@�K�@��y@��H@�ȴ@�ȴ@�V@�-@�@�I�@��@�+@���@�E�@�hs@��D@��m@���@��P@�K�@�
=@�^5@�-@��@��@���@�bN@�l�@���@��!@�n�@�X@���@�9X@��@��AŴ9AŲ-Aź^AŴ9AŸRA���AŸRAżjA���A���A���A���A���AžwA���A���A���A���A���A�A�A�ĜA���A��`A��A���A���A�A�oA�$�A�5?A�5?A�?}A�?}A�A�A�A�A�?}A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�=qA�=qA�;dA�9XA�7LA�7LA�5?A�33A�/A�&�A��A�oA�A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           AżjA���A���A�33A�A�A�A�A�A�A�?}A�=qA�;dA�5?A�"�A���A�&�A�
=A���A��A��/A�Aħ�Aě�A�l�A���AÓuA�~�A�5?A�A�+A�A���A��A�ĜA��7A��;A��A�A�n�A�
=A��HA�5?A�x�A��A�C�A�ZA��HA�v�A�VA���A�VA�+A�$�A���A���A��A�r�A�ffA�A�A��A��/A�r�A�/A��A��`A��A���A�$�A��
A�/A�$�A�{A�G�A�l�A��A���A�`BA��A��hA��A�~�A��HA��\A�XA��^A�VA��A�33A�|�A�z�A�VA�%A�  A��A��A��`A�I�A�  A��\A���A�bNA��A�ȴA���A��DA���A��A���A�M�A�t�A��mA�bNA��
A�l�A�ZA�;dA�hsA�|�A��A��DA��HA��yA�I�A�VA~�yA{�Aw�At^5Ar��Ap��An��Ak/Af�!Ad �Ac�Ab��A`�/A^�RA]�A\��A[��AZ�HAY�FAX=qAWhsAV��AV�DAU�7AT{ASC�ARE�AQp�AO�-AN5?AM�^ALVAKVAJVAJ �AG33AEK�AA�^A@{A>�A<�`A:�/A:{A8��A7�A6�A5O�A4�uA49XA3p�A0��A/O�A.��A-��A,�A,JA+33A*ĜA*�A*v�A)�A)|�A(��A(A�A';dA%��A$A�A#hsA!��A�A+A�+A�AoAn�A�-Ar�A�A�wA�!A5?A{A��A/A&�A�^A~�A�FA�TAbNA��A�\AJAx�A
=A�hAQ�A1A\)A
�jA
ffA	O�A�A�TA�-A�9A�DA�-A~�A�;A��A�PAt�AXA v�@�%@���@��@���@���@�X@�l�@���@�@��@�%@�9X@�z�@�@��@�@���@�b@��;@�~�@�%@܃@ى7@�%@�ƨ@ҸR@�X@�^5@ȴ9@�33@�33@�v�@���@�/@��u@�bN@��;@��y@�`B@�j@���@���@��@�33@��+@�~�@�V@��P@��@��R@��@�n�@��@�O�@���@��;@���@��@��@�O�@�`B@���@��9@�9X@���@��@���@��m@��R@�dZ@�Ĝ@�z�@�1@�b@��@�+@���@���@���@��h@�X@��@��m@��w@�|�@�K�@��y@��H@�ȴ@�ȴ@�V@�-@�@�I�@��@�+@���@�E�@�hs@��D@��m@���@��P@�K�@�
=@�^5@�-@��@��@���@�bN@�l�@���@��!@�n�@�X@���@�9X@��@��AŴ9AŲ-Aź^AŴ9AŸRA���AŸRAżjA���A���A���A���A���AžwA���A���A���A���A���A�A�A�ĜA���A��`A��A���A���A�A�oA�$�A�5?A�5?A�?}A�?}A�A�A�A�A�?}A�A�A�A�A�A�A�A�A�A�A�?}A�?}A�?}A�=qA�=qA�;dA�9XA�7LA�7LA�5?A�33A�/A�&�A��A�oA�A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
$�B
VB
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�!B
�-B
�3B
�3B
�?B
�LB
�dB
��B
��B%B
=B#�BVBy�B�%B�%B�+B�VB��B�B�#B�B��BVB&�B#�B$�B%B�BN�BbNBiyB�B� B�1Bz�BXBQ�BhsB�DB�VB�VB�bB�PB~�Bu�Bl�Bp�Bk�B]/BE�B,B)�B�B�B5?B~�B�\B�7B�DB�B`BB.B{B��B��B+B�`BǮB��B�B�BB��B��B��B�JBL�B�BPB
��B
�`B
��B1B{BP�B��B�{Bu�B��B�FB��B�uB�%Bv�BW
B:^B)�BPB
��B
ÖB
��B
��B
�hB
�1B
r�B
W
B
J�B
E�B
:^B
%�B
%B	�B	�ZB	��B	ȴB	�-B	�VB	�B	~�B	{�B	jB	_;B	]/B	T�B	N�B	J�B	L�B	R�B	Q�B	R�B	YB	YB	YB	S�B	N�B	I�B	>wB	7LB	5?B	.B	&�B	#�B	!�B	hB		7B��B�B�B�5B�
B��B��BǮBB�dB�LB�?B�-B��B��B��B��B��B��B�{B�oB�{B�{B�oB�hB�VB�VB�JB�7B�1B�+B�1B�B�B�B|�Bx�Bu�Bv�Bq�Bo�BgmBcTBaHB`BB`BB^5BZBT�BS�BT�BcTBm�Bs�Bw�Bt�Bt�Bs�Bn�Bl�Bq�Bs�Bs�Bs�Bp�Bt�Br�Bq�BjBbNBm�BjBhsBgmBffBe`Be`BbNB[#BZBXBVBYBZBT�BQ�BR�BT�BVBYB]/BffBhsBgmBe`BdZBe`BaHB=qB7LB:^B;dB=qBB�BJ�BL�BP�BP�BW
BW
BZB_;BbNBaHBbNBcTBgmBhsBp�Bq�B�B�DB��B�{B��B��B��B��B�FBǮB��B��B�/B�/B�/B�NB�ZB��B	%B	%B	%B	%B	DB	!�B	(�B	(�B	+B	6FB	G�B	J�B	M�B	\)B	n�B	u�B	|�B	~�B	�B	�1B	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�9B	�RB	�}B	B	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�5B	�BB	�BB	�ZB	�`B	�mB	�sB	�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
%�B
$�B
!�B
�B
�B
�B
9XB
6FB
C�B
G�B
R�B
bNB
m�B
�bB
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           B
�B
#�B
T�B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�'B
�-B
�-B
�9B
�FB
�^B
��B
�BB	7B"�BT�Bx�B�B�B�%B�PB��B�B�B�B��BPB%�B"�B#�BB�BM�BaHBhsB�B~�B�+By�BW
BP�BgmB�=B�PB�PB�\B�JB}�Bt�Bk�Bo�BjB\)BD�B+B(�B�B�B49B}�B�VB�1B�=B�B_;B-BuB��B��B%B�ZBƨB��B�
B�;B��B��B��B�DBK�B�BJB
��B
�ZB
��B+BuBO�B��B�uBt�B��B�?B��B�oB�Bu�BVB9XB(�BJB
�B
B
��B
��B
�bB
�+B
q�B
VB
I�B
D�B
9XB
$�B
B	�B	�TB	��B	ǮB	�'B	�PB	� B	}�B	z�B	iyB	^5B	\)B	S�B	M�B	I�B	K�B	Q�B	P�B	Q�B	XB	XB	XB	R�B	M�B	H�B	=qB	6FB	49B	-B	%�B	"�B	 �B	bB	1B��B�B�yB�/B�B��B��BƨB��B�^B�FB�9B�'B��B��B��B��B��B��B�uB�hB�uB�uB�hB�bB�PB�PB�DB�1B�+B�%B�+B�B�B�B{�Bw�Bt�Bu�Bp�Bn�BffBbNB`BB_;B_;B]/BYBS�BR�BS�BbNBl�Br�Bv�Bs�Bs�Br�Bm�Bk�Bp�Br�Br�Br�Bo�Bs�Bq�Bp�BiyBaHBl�BiyBgmBffBe`BdZBdZBaHBZBYBW
BT�BXBYBS�BP�BQ�BS�BT�BXB\)Be`BgmBffBdZBcTBdZB`BB<jB6FB9XB:^B<jBA�BI�BK�BO�BO�BVBVBYB^5BaHB`BBaHBbNBffBgmBp�Bq�B�B�DB��B�{B��B��B��B��B�FBǮB��B��B�/B�/B�/B�NB�ZB��B	%B	%B	%B	%B	DB	!�B	(�B	(�B	+B	6FB	G�B	J�B	M�B	\)B	n�B	u�B	|�B	~�B	�B	�1B	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�-B	�-B	�9B	�RB	�}B	B	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	�B	�B	�B	�B	�/B	�5B	�BB	�BB	�ZB	�`B	�mB	�sB	�yB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
$�B
#�B
 �B
�B
�B
�B
8RB
5?B
B�B
F�B
Q�B
aHB
l�B
�\B
�{B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                           <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202272021061413515620210614135156202106141746112021061417461120210614174611201807242202272021061413515620210614135156202106141746112021061417461120210614174611PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422022720180724220227  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022720180724220227QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422022720180724220227QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014720210722160147IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                