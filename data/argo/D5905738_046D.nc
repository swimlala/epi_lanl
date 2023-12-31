CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-12-02T01:00:45Z creation      
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
resolution        =���   axis      Z        `  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  PX   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  u0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �p   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   @   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   H   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � X   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar           HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        $   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    4   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � x      � xArgo profile    3.1 1.2 19500101000000  20181202010045  20210722160155  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               .   .DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ؒ��d�@ؒ��d�11  @ؒ���M�@ؒ���M�@5����@5�����c��4C�k�c��4C�k11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?333@   @@  @y��@�  @�33@�33A��AffA$��AA��Aa��A�  A�  A�  A�33A�  A�33A�33A�  B   B  B  BffB!33B)33B0  B8  B@  BH  BQ33BXffB`ffBg��Bp  BxffB���B�ffB�  B�  B�  B�  B�33B�33B�ffB�  B�  B���B�33B�ffB�ffB�ffB�33B�33B�  B�33B���B�  B���B�ffB�  B�  B�ffB�33B���B�33B�  B�ffB���C�fC  C�CL�C
�C��C�fC�C33C�C�fC  C�fC��C�C �C!��C$33C&�C(�C*  C+�fC.�C0  C1�fC433C6  C8  C:33C<33C>�C@  CA��CD�CE�fCG��CJ�CK�fCM�fCP  CRL�CT�CV  CXL�CZ33C\  C^33C`�Ca�fCd�CfL�Ch�Ci�fCl  Cn33Co�fCq��Cs�fCv�Cx33Cz�C{��C}��C�fC�  C��C�  C��fC��3C��C��C�  C��fC�  C��C�  C��fC��fC�  C��C��C�&fC��C��3C�  C��C��C�  C�ٚC��fC��3C��3C��C��C��C��C��3C��C��C�&fC��C��fC��3C�  C��C�&fC�&fC��C��3C��3C��C��C�  C��fC�  C��C��C�  C��fC��3C�  C��C��C��C�  C�  C�&fC��C��3C��C�&fC��C��fC�  C��C�  C��fC��C��3C�ٚC�  C��C��C��C��3C��C�  C��fC�  C��C��C��3C�  C��C�  C��3C�  C��C��C�&fC��C�  C��C�&fC�&fC��C��fC�  C��C��C��C�&fC�33C��C��3C�  C��C��C�&fC�  C��fC��3C��C��C�  C��fC��fC�  C�  C�  C��D 3D ��D ��DL�DL�D� D,�D�3D��D` D�fD  D�fDٚD9�D � D"��D%3D'` D)�fD,L�D.�3D1S3D3�fD6�fD9fD;y�D>�D@� DC,�DE� DHffDJ�fDMY�DO�3DRY�DT�3DWL�DY� D\l�D^�fDal�Dc��Df` DhٚDkL�Dm�3Dp&fDr��Dt��DwL�Dy� D{�fD~3D�C3D�s3D���D�ٚD� D�@ D�p D�� D���D�fD�I�D�� D���D��3D��D�` D�� D�ٚD��D�C3D�� D���D��fD�0 D�\�D��3D�� D���D��D�P D�s3D���D�� D���D�3D�9�D�` D��fD��fD�� D���D��D�33D�L�D�c3D�� D���D�� D���D��fD��3D�ٚD��fD�� D���D�3D�	�D��D�,�D�33D�@ D�L�D�Y�D�c3D�p D�y�Dˉ�D̖fDͣ3D�� D���D���D� D�  D�@ D�ffD֌�D׳3D��3D�  D�&fD�P D�i�Dމ�DߦfD��fD��fD�3D��D�0 D�6fD�FfD�S3D�ffD�vfD�|�D��D홚D� D署D�D� D� D�3D���D��3D��3D��3D���D�� D�� D���D���D�� D�� D���E K3E � EK3E� EK3E�3E�fEQ�E[3E��E� E
S3EP E� E!�E El�E� EfEQ�E��EɚE EC3E��E�3E��E8 E{3E � E" E#T�E$��E%� E&�E(^fE)P E*� E,�E-p E.[3E/��E13E2\�E3� E4� E5� E7  E8h E9� E:�fE<<�E?x EB|�EE� EH� EK� EN�fERfEU@ EXd�E[S3E^��Ea��Ed��Eh  Ek)�En�Eq,�Etl�Ew[3Ez��E}�3E�`�E��3E���E��fE�%�E�y�E��fE��E�^fE���E�fE�T E���E� E�NfE���E�� E�L�E���E�њ>���?   ?   ?��?   ?   ?   ?��?   ?333?   ?   ?��?��?��?��?��?L��?fff?���?���?�ff?�33?���?�ff@ff@��@,��@@  @L��@fff@y��@�ff@�33@���@�ff@�  @���@�33@���@ٙ�@�33@�  @���A��A	��AffA��A33A!��A(  A,��A333A;33AA��AH  AL��AT��A[33Ac33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141444141441444411141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?�  ?���@   @`  @���@�  @�33@�33A	��AffA,��AI��Ai��A�  A�  A�  A�33A�  A�33A�33A�  B  B
  B  BffB#33B+33B2  B:  BB  BJ  BS33BZffBbffBi��Br  BzffB���B�ffB�  B�  B�  B�  B�33B�33B�ffB�  B�  B���B�33B�ffB�ffB�ffB�33B�33B�  B�33B���B�  B���B�ffB�  B�  B�ffB�33B���B�33B�  B�ffC ffCffC� C��C��C
��CL�CffC��C�3C��CffC� CffCL�C��C ��C"L�C$�3C&��C(��C*� C,ffC.��C0� C2ffC4�3C6� C8� C:�3C<�3C>��C@� CBL�CD��CFffCHL�CJ��CLffCNffCP� CR��CT��CV� CX��CZ�3C\� C^�3C`��CbffCd��Cf��Ch��CjffCl� Cn�3CpffCrL�CtffCv��Cx�3Cz��C|L�C~L�C�33C�@ C�Y�C�@ C�&fC�33C�L�C�Y�C�@ C�&fC�@ C�Y�C�@ C�&fC�&fC�@ C�L�C�Y�C�ffC�L�C�33C�@ C�Y�C�Y�C�@ C��C�&fC�33C�33C�L�C�L�C�Y�C�L�C�33C�L�C�Y�C�ffC�L�C�&fC�33C�@ C�L�C�ffC�ffC�L�C�33C�33C�L�C�Y�C�@ C�&fC�@ C�Y�C�Y�C�@ C�&fC�33C�@ C�L�C�Y�C�L�C�@ C�@ C�ffC�L�C�33C�L�C�ffC�L�C�&fC�@ C�L�C�@ C�&fC�L�C�33C��C�@ C�L�C�Y�C�L�C�33C�L�C�@ C�&fC�@ C�L�C�L�C�33C�@ C�Y�C�@ C�33C�@ C�L�C�Y�C�ffC�Y�C�@ C�Y�C�ffC�ffC�L�C�&fC�@ C�L�C�Y�C�Y�C�ffC�s3C�Y�C�33C�@ C�L�C�Y�C�ffC�@ C�&fC�33C�L�C�Y�C�@ C�&fC�&fC�@ C�@ C�@ C�L�D 33D ��D�Dl�Dl�D� DL�D�3D�D� D�fD@ D�fD��DY�D � D"��D%33D'� D)�fD,l�D.�3D1s3D4fD6�fD9&fD;��D>,�D@� DCL�DE� DH�fDKfDMy�DO�3DRy�DT�3DWl�DZ  D\��D_fDa��Dd�Df� Dh��Dkl�Dm�3DpFfDr��Du�Dwl�Dy� D{�fD~33D�S3D��3D���D��D�  D�P D�� D�� D���D�&fD�Y�D�� D���D��3D�,�D�p D�� D��D��D�S3D�� D���D�fD�@ D�l�D��3D�� D���D�,�D�` D��3D���D�� D���D�#3D�I�D�p D��fD��fD�� D�	�D�)�D�C3D�\�D�s3D�� D���D�� D���D��fD��3D��D��fD�  D�	�D�3D��D�)�D�<�D�C3D�P D�\�D�i�D�s3Dɀ Dʉ�D˙�D̦fDͳ3D�� D���D�	�D�  D�0 D�P D�vfD֜�D��3D��3D� D�6fD�` D�y�Dޙ�D߶fD��fD��fD�3D�,�D�@ D�FfD�VfD�c3D�vfD�fD��D��D���D� D﹚D�D�� D�� D��3D���D��3D��3D��3D���D�� D�� D���D���D�� D�� D���E S3E � ES3E� ES3E�3E�fEY�Ec3E��E� E
[3EX E� E)�E Et�E� EfEY�E��EњE EK3E��E�3E�E@ E�3E � E" E#\�E$��E%� E&��E(ffE)X E*� E,�E-x E.c3E/��E13E2d�E3� E5  E5� E7( E8p E9� E;fE<D�E?� EB��EE� EH� EK� EN�fERfEUH EXl�E[[3E^��Ea��Ed��Eh Ek1�En!�Eq4�Ett�Ewc3Ez��E}�3E�d�E��3E���E��fE�)�E�}�E��fE��E�bfE���E�fE�X E���E� E�RfE���E�� E�P�E���E�՚?fffG�O�?�  G�O�G�O�G�O�?�  G�O�?�  G�O�G�O�?�  G�O�G�O�G�O�G�O�?���?�ff?�33G�O�?���?�ff?�33@ff@33@&ff@9��@L��@`  @l��@�33@���@�ff@�33@���@�ff@�  @ə�@�33@���@陚@�33A   AffA��A��AffA��A#33A)��A0  A4��A;33AC33AI��AP  AT��A\��Ac33Ak33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141444141441444411141111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ j@ �@ �@ {@ �@ ""@ )�@ 0x@ 7L@ <�@ FQ@ R�@ `B@ m:@ z�@ ��@ ��@ ��@ ��@ �w@ ��@ �t@ �@ ��@@@ �@,`@:@G�@UU@e	@qS@~�@�D@��@��@��@��@�7@��@�@�,@�@*@#�@/�@=q@Ji@Yn@g�@uk@�@�@��@��@�@�J@Ӡ@��@�L@��@
=@B@&;@2�@A�@N�@Z@i!@v�@�@�u@�z@��@�^@ȴ@׹@�@�@�Q@�@�@'�@7L@D�@P�@`�@m�@{�@��@��@��@�~@�w@�*@�t@�@� @�@�@�@+@:�@F�@S�@c�@o�@}�@��@��@��@��@Ĝ@є@��@��@��@%@*@$.@0x@<�@K@Z@e�@r�@�@�@�a@�Y@��@Ĝ@��@�H@�L@��@�@6@&;@4�@@�@M$@\)@k.@ww@��@�h@�m@��@�@�o@׹@�@�Y@^@@O@&�@5?@C�@Q=@`B@m�@|?@�7@��@��@��@�2@�|@�@�m@��@	@	@	 �@	-@	9X@	F�@	V@	dZ@	p�@	|�@	��@	��@	��@	��@	�2@	ψ@	��@	�4@	��@
�@
{@
""@
1�@
>@
Ji@
Yn@
hs@
t�@
�W@
�\@
��@
��@
��@
ƨ@
��@
�;@
��@
�E@�@�@$�@3�@@�@M$@\)@j@x&@�p@��@��@�@�@�c@׹@�@�e@^@�@�@+@8�@D�@P�@_�@m�@|?@��@�<@��@��@�w@��@�#@�y@��@j@�@
@-@;d@G�@S�@a�@p�@~K@��@�H@�M@��@��@��@SI@�0@�h@�@[z@�@ލ@�@`A@��@��@�@]�@��@�t@�@`�@��@�(@0x@x&@�j@�Q@E�@��@є@�@`A@��@�m@+@o�@��@� @=q@�@ƨ@�@O�@��@�\@B@Z�@��@�;@ @`�@�m@׹@�@\�@��@��@ �@b�@��@�@%�@i!@�@�@/�@oF@�-@�@:@~K@�2@ �@ D�@ ��@ �@!@!Q�@!�@!�O@"{@"T�@"��@"�
@#*@#S�@#��@#��@$�@$P�@$�\@$�*@%�@%K@%��@%�@&@&@,@&{�@&�R@&�e@'/�@'i!@'��@'�#@(@(Lu@(�@(��@(�q@).l@)hs@)�(@)�#@*{@*M�@*�+@*��@*�,@+1�@+k�@+�4@+ލ@,O@,X@,��@,�7@-
=@-G�@-�|@-�J@.@.A�@.��@.��@/  @/<@/y�@/�F@/�@01'@0m�@0��@0�@1�@1V�@1�@1��@2�@2<�@2v�@2�!@2�@3 �@3Wb@3�\@3��@3�E@41�@4g@4��@4�O@5J@5DD@5ww@5��@5�@6O@6Q�@6��@6�@6�Y@7*S@7_�@7��@7�*@8<�@8��@9SI@9��@:b�@;�@;p�@<�@<��@=
=@=�@>/�@>�@?H]@?�C@@V@@��@AdZ@A�@Buk@B�9@C��@D�@D�0@E""@E�!@FB�@F��@G:�@G�[@H=q@HӠ@Ihs@J  @JdZ@J�q@K��@L�@L�A@M6�@M�0@N""@N�@O:@Oȴ@PP�@Q�~@R�9@TDD@U��@V��@X<�@Y�T@Z��@\S�@]�#@_�@`I�@a�z@b��@dV@e��@f�@hI�@i��@j��@l7�@m��@n��@pI@py�@p��@q�@qS�@q�u@q��@r�@re	@r�m@r��@s6�@sv@s��@s�Y@tO1@t�7@t��@ �G�O�@ jG�O�G�O�G�O�@ jG�O�@ jG�O�G�O�@ jG�O�G�O�G�O�G�O�@ @ v@ %G�O�@ �@ �@ 	�@ 
�@ J@ V@ b@ o@ {@ �@ �@ �@ �@ g@  �@ #�@ %�@ '�@ )�@ +�@ .l@ 0x@ 33@ 5�@ 8�@ :�@ <�@ ?}@ B8@ D�@ G�@ I�@ Lu@ O�@ R�@ UU@ Wb@ Z�@ ]�@ `�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��mA��mA���A�A�A�  A�  A�  A�  A�A�%A�  A�JA�JA�VA�bA�oA�oA�bA�bA�
=A�
=A�
=A�JA�VA�JA�bA�oA�oA�{A��A��A��A� �A�%A���A�bNA�r�A���A��A���A��HA�bNA��A�l�A�K�A��A���A�p�A��wA��!A�C�A��HA���A�9XA���A�;dA��;A��jA�^5A��wA���A��/A�  A�$�A���A���A�S�A�1A�ȴA��A��jA�1'A�K�A�G�A��A��PA�$�A��-A��A�A���A��HA�l�A��9A��A�;dA���A��A���A�|�A��`A�E�A���A�~�A��A��A�A�+A��A�Q�A��TA�M�A�M�A�=qA�v�A�hsA��+A}K�A{��A{/A{?}Az�`Aw�FAu��ArbNAn��AmK�Ai��AeG�Ab�+A_��A^(�A]\)A]/A]A\��AZ�AY�AY�hAX��AXM�AV�jASoAP��AN��ALVAK��AK7LAJffAIK�AG��AF5?AC�AC`BAB�ABJA@�A@A>��A=VA:�A8��A7&�A5�#A5G�A4��A4bA3��A2��A1�A1C�A0�A0VA/�A/dZA.�jA-��A,��A,r�A+�^A*�\A)G�A(�yA(�A'�A&=qA$��A#�;A#hsA"�`A"M�A ��A;dAƨA%A�;A�A��A��A9XAl�A�7AVA�AJA+A�7AM�A�;AAO�A�jAM�A�hA�DA��A
�\A�A9XA�FAoA�RAJA"�A�!AVA-A��AƨA�AI�A�TAx�A+A �+@��y@���@��
@�~�@�1@��@���@�-@�
=@�
=@�D@��#@�@�K�@�\@���@�x�@�O�@�?}@�&�@���@�9@��;@��`@��
@�|�@�"�@�ȴ@��T@���@��m@ۮ@��@٩�@���@� �@��@���@ʟ�@ź^@Ý�@�`B@�7L@��\@��;@��@���@�1@���@�-@�z�@�A�@�I�@��F@��R@�b@�|�@��D@�v�@�ff@�j@��w@�\)@��@��`@�t�@�ff@��@���@���@�M�@�9X@��@�ff@��@���@�@��@�/@�r�@��@��+@��@�Q�@��m@�ƨ@�@�?}@��@~$�@}?}@{��@yx�@w�@wl�@v��@t�j@q�7@p��@n��@n$�@j�H@j^5@h�9@f5?@ep�@cƨ@bn�@`��@^E�@]/@[�
@Z�!@Y&�@W�w@U@U/@SC�@Q�7@O�@M�T@LI�@K��@J�!@H��@HA�@G��@F�@Dj@C�F@B�!@B�!@Ax�@@�u@@bN@>$�@<(�@9�#@7��@7l�@5�h@3S�@1��@1�#@1G�@0Q�@/�@.ȴ@.v�@,j@*��@(��@'�@%@%O�@$��@"�H@"�\@"�@"-@"�@!hs@ �`@�@E�@�-@�D@�m@�!@��@��@X@+@�+@��@�@Z@��@��@&�@�@r�@bN@  @�w@��@�y@E�@�-@V@j@(�@ƨ@��@dZ@S�@	��@	X@	7L@�u@�@1'@��@\)@;d@�R@@�@�@(�@��@ �?�|�?��?���?���?�1'?�ff?��/?�&�?��;?�?�~�?�X?�j?�o?�Ĝ?���?���?��m?�?�~�?���?׍P?֧�?�$�?ա�?ԛ�?���?ѩ�?У�?�A�?�\)?�;d?̋D?�C�?�~�?���?�1'?Ǯ?ǍP?�+?Ƈ+?��T?��?�9X?���?�A�?���?���?��?�(�?�I�?�j?��D?��D?���?�dZ?��H?��H?�~�?���?�X?��^?���?��?���?�dZ?�(�?�I�?�j?��D?��?��?���?���?��?�V?�O�?�p�?�p�?��-?���?��?�5??�{A��#A���A���A���A���A��#A��A��A���A���A���A��HA��TA��mA��`A��A��A��A��A��A��A��A��yA��yA��A��`A��TA��/A��`A���A���A���A���A�  A�A�A�A�A�A�A�A�  A�  A�  A���A�  A�  A���A�  A�  A�  A���A�A�  A�A�A�A�1A�1A�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A���A��A��mA��mA���A�A�A�  A�  A�  A�  A�A�%A�  A�JA�JA�VA�bA�oA�oA�bA�bA�
=A�
=A�
=A�JA�VA�JA�bA�oA�oA�{A��A��A��A� �A�%A���A�bNA�r�A���A��A���A��HA�bNA��A�l�A�K�A��A���A�p�A��wA��!A�C�A��HA���A�9XA���A�;dA��;A��jA�^5A��wA���A��/A�  A�$�A���A���A�S�A�1A�ȴA��A��jA�1'A�K�A�G�A��A��PA�$�A��-A��A�A���A��HA�l�A��9A��A�;dA���A��A���A�|�A��`A�E�A���A�~�A��A��A�A�+A��A�Q�A��TA�M�A�M�A�=qA�v�A�hsA��+A}K�A{��A{/A{?}Az�`Aw�FAu��ArbNAn��AmK�Ai��AeG�Ab�+A_��A^(�A]\)A]/A]A\��AZ�AY�AY�hAX��AXM�AV�jASoAP��AN��ALVAK��AK7LAJffAIK�AG��AF5?AC�AC`BAB�ABJA@�A@A>��A=VA:�A8��A7&�A5�#A5G�A4��A4bA3��A2��A1�A1C�A0�A0VA/�A/dZA.�jA-��A,��A,r�A+�^A*�\A)G�A(�yA(�A'�A&=qA$��A#�;A#hsA"�`A"M�A ��A;dAƨA%A�;A�A��A��A9XAl�A�7AVA�AJA+A�7AM�A�;AAO�A�jAM�A�hA�DA��A
�\A�A9XA�FAoA�RAJA"�A�!AVA-A��AƨA�AI�A�TAx�A+A �+@��y@���@��
@�~�@�1@��@���@�-@�
=@�
=@�D@��#@�@�K�@�\@���@�x�@�O�@�?}@�&�@���@�9@��;@��`@��
@�|�@�"�@�ȴ@��T@���@��m@ۮ@��@٩�@���@� �@��@���@ʟ�@ź^@Ý�@�`B@�7L@��\@��;@��@���@�1@���@�-@�z�@�A�@�I�@��F@��R@�b@�|�@��D@�v�@�ff@�j@��w@�\)@��@��`@�t�@�ff@��@���@���@�M�@�9X@��@�ff@��@���@�@��@�/@�r�@��@��+@��@�Q�@��m@�ƨ@�@�?}@��@~$�@}?}@{��@yx�@w�@wl�@v��@t�j@q�7@p��@n��@n$�@j�H@j^5@h�9@f5?@ep�@cƨ@bn�@`��@^E�@]/@[�
@Z�!@Y&�@W�w@U@U/@SC�@Q�7@O�@M�T@LI�@K��@J�!@H��@HA�@G��@F�@Dj@C�F@B�!@B�!@Ax�@@�u@@bN@>$�@<(�@9�#@7��@7l�@5�h@3S�@1��@1�#@1G�@0Q�@/�@.ȴ@.v�@,j@*��@(��@'�@%@%O�@$��@"�H@"�\@"�@"-@"�@!hs@ �`@�@E�@�-@�D@�m@�!@��@��@X@+@�+@��@�@Z@��@��@&�@�@r�@bN@  @�w@��@�y@E�@�-@V@j@(�@ƨ@��@dZ@S�@	��@	X@	7L@�u@�@1'@��@\)@;d@�R@@�@�@(�@��@ �?�|�?��?���?���?�1'?�ff?��/?�&�?��;?�?�~�?�X?�j?�o?�Ĝ?���?���?��m?�?�~�?���?׍P?֧�?�$�?ա�?ԛ�?���?ѩ�?У�?�A�?�\)?�;d?̋D?�C�?�~�?���?�1'?Ǯ?ǍP?�+?Ƈ+?��T?��?�9X?���?�A�?���?���?��?�(�?�I�?�j?��D?��D?���?�dZ?��H?��H?�~�?���?�X?��^?���?��?���?�dZ?�(�?�I�?�j?��D?��?��?���?���?��?�V?�O�?�p�?�p�?��-?���?��?�5??�{A��#A���A���A���A���A��#A��A��A���A���A���A��HA��TA��mA��`A��A��A��A��A��A��A��A��yA��yA��A��`A��TA��/A��`A���A���A���A���A�  A�A�A�A�A�A�A�A�  A�  A�  A���A�  A�  A���A�  A�  A�  A���A�A�  A�A�A�A�1A�1A�%G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�7B�=B�=B�=B�7B�7B�7B�7B�7B�1B�7B�7B�7B�7B�=B�=B�=B�=B�=B�DB�=B�7B�7B�7B�7B�1B�7B�7B�7B�7B�1B�JB�DB�oB�#BB�B_;BdZBiyB]/BdZB}�B~�B�=B�+B�B�7B�1B�%B�JB�{B��B��B�bB�JB�\B�oB�hB��B��B�{B�oB�JB�DB�=B�1B�1B�B|�Bq�Be`BZBO�BH�BA�B0!B�B	7B�B�B�`B�
B��BÖB�?B�B��B��B�VB�By�BbNBR�BJ�B@�B;dB1'B)�B{B	7B
��B
�B
�B
��B
�7B
x�B
l�B
^5B
T�B
D�B
.B
%�B
&�B
,B
(�B
\B	��B	�BB	ĜB	��B	��B	�B	aHB	N�B	N�B	VB	W
B	VB	T�B	N�B	L�B	F�B	B�B	A�B	6FB	#�B	�B	JB��B��B�B�B�sB�fB�/B�;B�5B�#B�B��B��B��BÖB�dB�LB��B��B��B��B��B��B�bB�VB�DB�JB�=B�=B�7B�1B�B� B|�Bw�Bq�Bn�BjBhsBcTBbNB`BB`BB^5B]/BZBT�BQ�BL�BJ�BF�BG�BF�BE�BC�BA�B<jB>wB<jB<jB9XB49B5?B2-B-B.B-B+B+B)�B&�B$�B)�B'�B%�B&�B%�B%�B&�B&�B%�B$�B%�B$�B#�B&�B%�B%�B%�B$�B&�B'�B'�B%�B'�B)�B(�B$�B&�B+B)�B-B+B,B,B-B-B-B,B,B,B-B,B1'B33B33B49B49B6FB7LB9XB8RB8RB;dB;dB=qB<jBA�BG�BP�BVBYBk�Br�B~�B�bB�B�-BƨB�B�B	B	bB	$�B	@�B	F�B	^5B	r�B	x�B	�B	�=B	�\B	�uB	��B	��B	�B	�9B	�dB	�jB	�jB	ÖB	��B	�
B	�B	�)B	�/B	�NB	�HB	�NB	�sB	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
1B
+B
1B
DB
bB
bB
oB
uB
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
&�B
'�B
(�B
+B
-B
-B
/B
0!B
2-B
49B
5?B
6FB
7LB
8RB
9XB
:^B
:^B
=qB
=qB
?}B
=qB
@�B
A�B
A�B
C�B
E�B
H�B
I�B
H�B
K�B
L�B
M�B
M�B
M�B
N�B
O�B
P�B
P�B
R�B
T�B
VB
W
B
XB
XB
ZB
[#B
ZB
\)B
\)B
[#B
\)B
]/B
^5B
aHB
aHB
bNB
bNB
cTB
e`B
e`B
e`B
hsB
hsB
iyB
iyB
k�B
l�B
m�B
n�B
m�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
r�B
q�B
r�B
r�B
r�B
s�B
s�B
r�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
x�B
w�B
x�B
x�B
y�B
z�B
z�B
}�B
� B
� B
� B
�B
�B
�B
�%B
�+B
�=B
�=B
�JB
�VB
�\B
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�!B
�'B
�3B
�3B
�9B
�9B
�?B
�?B
�FB
�LB
�LB
�RB
�RB
�XB
�^B
�^B
�dB
�dB
�dB
�jB
�dB
�dB
�jB
�jB
�dB
�jB
�jB
�dB
�dB
�dB
�jB
�dB
�jB
�jB
�dB
�dB
�jB
�dB
�jB�+B�=B�=B�1B�uB�DB�JB�+B�7B�%B�1B�1B�=B�7B�DB�DB�7B�7B�7B�7B�7B�7B�=B�=B�=B�7B�7B�=B�JB�7B�=B�7B�7B�=B�7B�7B�1B�7B�7B�7B�1B�7B�7B�7B�7B�1B�1B�7B�7B�1B�1B�7B�7B�7B�7B�7B�7B�7B�7B�=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B�%B�+B�+B�+B�%B�%B�%B�%B�%B�B�%B�%B�%B�%B�+B�+B�+B�+B�+B�1B�+B�%B�%B�%B�%B�B�%B�%B�%B�%B�B�7B�1B�\B�B?}B\)BaHBffBZBaHBz�B{�B�+B�B�B�%B�B�B�7B�hB�oB�oB�PB�7B�JB�\B�VB�uB�{B�hB�\B�7B�1B�+B�B�B�By�Bn�BbNBW
BL�BE�B>wB-B�B%B�B�B�NB��BȴB��B�-B��B��B�{B�DB�Bv�B_;BO�BG�B=qB8RB.B&�BhB%B
�B
��B
��B
�oB
�%B
u�B
iyB
[#B
Q�B
A�B
,B
#�B
$�B
)�B
&�B
PB	��B	�5B	B	�}B	��B	� B	_;B	L�B	L�B	S�B	T�B	S�B	R�B	L�B	J�B	D�B	@�B	?}B	49B	!�B	uB	
=B��B��B�B�B�fB�ZB�#B�/B�)B�B�
B��B��B��B��B�XB�?B��B��B��B��B��B�uB�VB�JB�7B�=B�1B�1B�+B�%B� B}�Bz�Bu�Bo�Bl�BhsBffBaHB`BB^5B^5B\)B[#BXBR�BO�BJ�BH�BD�BE�BD�BC�BA�B?}B:^B<jB:^B:^B7LB2-B33B0!B+B,B+B(�B(�B'�B$�B"�B'�B%�B#�B$�B#�B#�B$�B$�B#�B"�B#�B"�B!�B$�B#�B#�B#�B"�B$�B%�B%�B#�B%�B'�B&�B"�B$�B(�B'�B+B(�B)�B)�B+B+B+B)�B)�B)�B+B)�B/B1'B1'B2-B2-B49B5?B7LB6FB6FB9XB9XB;dB:^B?}BE�BN�BS�BW
BiyBp�B|�B�VB�B�!BĜB�
B�B��B	VB	"�B	>wB	D�B	\)B	p�B	v�B	~�B	�1B	�PB	�hB	��B	��B	�B	�-B	�XB	�^B	�^B	��B	��B	��B	�
B	�B	�#B	�BB	�;B	�BB	�fB	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B
  B
B
%B
B
%B
	7B
\B
\B
hB
oB
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
%�B
&�B
'�B
)�B
,B
,B
.B
/B
1'B
33B
49B
5?B
6FB
7LB
8RB
9XB
9XB
<jB
<jB
>wB
<jB
?}B
@�B
@�B
B�B
D�B
G�B
H�B
G�B
J�B
K�B
L�B
L�B
L�B
M�B
N�B
O�B
O�B
Q�B
S�B
T�B
VB
W
B
W
B
YB
ZB
YB
[#B
[#B
ZB
[#B
\)B
]/B
`BB
`BB
aHB
aHB
bNB
dZB
dZB
dZB
gmB
gmB
hsB
hsB
jB
k�B
l�B
m�B
l�B
m�B
m�B
n�B
o�B
o�B
p�B
p�B
q�B
p�B
q�B
q�B
q�B
r�B
r�B
q�B
t�B
u�B
u�B
u�B
u�B
v�B
v�B
x�B
w�B
x�B
x�B
y�B
z�B
z�B
}�B
� B
� B
� B
�B
�B
�B
�%B
�+B
�=B
�=B
�JB
�VB
�\B
�uB
�uB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�'B
�-B
�9B
�9B
�?B
�?B
�FB
�FB
�LB
�RB
�RB
�XB
�^B
�dB
�jB
�jB
�qB
�qB
�qB
�wB
�qB
�qB
�wB
�wB
�qB
�wB
�wB
�qB
�qB
�qB
�wB
�qB
�wB
�wB
�qB
�qB
�wB
�qB
�wB�B�+B�+B�B�bB�1B�7B�B�%B�B�B�B�+B�%B�1B�1B�%B�%B�%B�%B�%B�%B�+B�+B�+B�%B�%B�+B�7B�%B�+B�%B�%B�+B�%B�%B�B�%B�%B�%B�B�%B�%B�%B�%B�B�B�%B�%B�B�B�%B�%B�%B�%B�%B�%B�%B�%B�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201812020100452021061413525920210614135259202106141746572021061417465720210614174657201812020100452021061413525920210614135259202106141746572021061417465720210614174657PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018120201004520181202010045  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018120201004520181202010045QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018120201004520181202010045QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015520210722160155IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                