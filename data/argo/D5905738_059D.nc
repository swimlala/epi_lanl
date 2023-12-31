CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2019-03-13T07:00:35Z creation      
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
resolution        =���   axis      Z        X  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   L8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  PP   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   `�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     X  d�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  u   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     X  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                       HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                      HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  �    HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � 0      � 0Argo profile    3.1 1.2 19500101000000  20190313070035  20210722160158  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               ;   ;DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @ةM(d&4@ةM(d&411  @ةM'҃�@ةM'҃�@5��/�V�@5��/�V��c��"���c��"��11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AC  AA  AA  >���?�  @ff@L��@�  @�  @���@�33A   A��A$��AA��Ac33A�  A�  A���A���A�  A���A�  A�  B ffBffB��BffB   B(  B0ffB8ffB@��BH  BP  BX  B`  Bh  BpffBxffB�ffB�ffB�  B�  B�33B�33B�33B�  B�33B�  B���B���B���B�33B�33B�33B�  B�33B�  B�33B�33B�33B�33B�33B�ffB䙚B�33B�33B�ffB�B���B���C   C�C��C�fC�C	�fC�fC�C�C�C�C33C33CffC  C�C �C!�fC#�fC&  C(  C*  C,  C.  C0  C2�C4  C6  C7�fC:33C<�C>  C@�CB  CC��CF  CH33CJ�CK�fCN�CP33CR  CS��CU�fCX  CZ�C\L�C^�C_�fCb�Cd33Cf�Ch  Cj33Cl  Cm�fCpL�Cr33Ct�Cv  Cx�Cz  C|  C~L�C��C��C��C��C�  C�  C�  C�  C�  C��3C�  C��3C��3C��3C��C��C��C��C��C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�&fC�&fC�  C��fC��C�  C�  C��C��C��3C��C��C��3C�  C��C�  C��fC�  C��C��C�  C�&fC��C��C�&fC��C�  C��C��C�  C��fC��3C��C��C�  C��fC�  C��C�&fC��C��fC�  C��C��C��3C��C��C��C��3C�  C��C��C�&fC��C��3C�  C��C��C��C��3C�  C�&fC�&fC��C�  C��C�  C��fC�  C��C�&fC��C�  C��C��C�  C��C�&fC��C��C�&fC��C��C��C��C��3C��3C��C��C��C�  C��3C��C�ٚC�  C�33C��3C��3D y�D�3D�fD�fDY�D  D�3D�3Dl�D9�D�D!�fD$��D'�fD*ffD-@ D03D2� D5�3D8y�D;FfD>�D@�3DC��DFS3DI3DK��DN�fDQ@ DSٚDV��DY9�D[�3D^y�Da�Dc�3DfS3Dh��Dk��DnL�Dq  Ds�3Dvl�DyfD{9�D}�fD�FfD��fD��3D�6fD��fD�ٚD�9�D��fD��fD�VfD���D�3D�|�D��fD�6fD��3D�� D�I�D��fD�  D�c3D���D���D�C3D�� D�ٚD�&fD�s3D���D��D�Y�D��fD��fD�<�D��fD��3D�#3D�s3D�� D�	�D�VfD��3D��fD�FfD�� D�� D�9�D�� D�� D�&fD�s3D��3D��D�S3DǖfD��fD�	�D�<�D�s3D͠ D��3D�  D�)�D�\�DӐ D��3D��D�fD�P D�|�Dک�D��3D���D�#3D�L�D�y�D�fD��3D�3D�0 D�i�D� D��3D�	�D�<�D�p D���D���D�  D�Y�D��D��D���D�&fD�c3D��3D��fD�	�D�FfD�y�D��3D���E �fE+3EɚEd�E  E��E8 EњEp EfE��E<�E��Ep E	3E	�3E
0 E
�3ES3E�fEx E3E� E)�E�E�E#3EFfE��E�E,�E@ E�fE��E�E1�E�3E fE!+3E"P E#|�E$� E&i�E'��E(�3E)��E*��E,��E-њE.�3E0�E1( E2ɚE3� E4��E6vfE7��E9�E:fE;#3E<� E?��EB� EE�3EI	�EL�EO.fERH EUd�EX� E[�fE_  Ea�3Ee;3EhnfEkC3En{3Eq�fEt��Ex3E{3E~3E���E�K3E���E�d�E��fE�nfE��fE�� E�2fE�� E�T�E�� E�H�E��fE���E��E��3E� E�� E�	�E�NfE���E���E�?3E���E�� E�8�E�~fE���E�,�E�q�>���>���>���>���?   >���>���>���>���>���>���>���>���>���>���>���>���>���?   ?   ?333?��?L��?fff?�  ?���?�ff?�  ?ٙ�?�33@   @33@   @333@@  @S33@`  @l��@�33@���@�ff@���@���@�33@���@ə�@�ff@�  @�  @���A33A	��A��A  A��A$��A+33A4��A;33AC33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144414414444441414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?fff?�  @&ff@l��@�  @�  @���@�33A  A��A,��AI��Ak33A�  A�  A���A���A�  A���A�  A�  BffB
ffB��BffB"  B*  B2ffB:ffBB��BJ  BR  BZ  Bb  Bj  BrffBzffB�ffB�ffB�  B�  B�33B�33B�33B�  B�33B�  B���B���B���B�33B�33B�33B�  B�33B�  B�33B�33B�33B�33B�33B�ffB噚B�33B�33B�ffB���B���B���C � C��CL�CffC��C
ffCffC��C��C��C��C�3C�3C�fC� C��C ��C"ffC$ffC&� C(� C*� C,� C.� C0� C2��C4� C6� C8ffC:�3C<��C>� C@��CB� CDL�CF� CH�3CJ��CLffCN��CP�3CR� CTL�CVffCX� CZ��C\��C^��C`ffCb��Cd�3Cf��Ch� Cj�3Cl� CnffCp��Cr�3Ct��Cv� Cx��Cz� C|� C~��C�Y�C�Y�C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�33C�@ C�33C�33C�33C�Y�C�Y�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�L�C�@ C�@ C�@ C�@ C�@ C�@ C�@ C�ffC�ffC�@ C�&fC�L�C�@ C�@ C�Y�C�L�C�33C�Y�C�L�C�33C�@ C�Y�C�@ C�&fC�@ C�Y�C�Y�C�@ C�ffC�Y�C�L�C�ffC�Y�C�@ C�L�C�Y�C�@ C�&fC�33C�L�C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�&fC�@ C�Y�C�L�C�33C�L�C�Y�C�L�C�33C�@ C�Y�C�Y�C�ffC�L�C�33C�@ C�L�C�Y�C�L�C�33C�@ C�ffC�ffC�Y�C�@ C�L�C�@ C�&fC�@ C�Y�C�ffC�Y�C�@ C�Y�C�Y�C�@ C�L�C�ffC�Y�C�L�C�ffC�Y�C�L�C�L�C�L�C�33C�33C�Y�C�Y�C�L�C�@ C�33C�Y�C��C�@ C�s3C�33C��3D ��D3D�fD�fDy�D@ D�3D�3D��DY�D9�D"fD$ٚD'�fD*�fD-` D033D3  D5�3D8��D;ffD>9�D@�3DC��DFs3DI33DKٚDN�fDQ` DS��DV��DYY�D[�3D^��Da9�Dc�3Dfs3Di�Dk��Dnl�Dq  Ds�3Dv��Dy&fD{Y�D~fD�VfD��fD��3D�FfD��fD��D�I�D��fD�fD�ffD�ɚD�#3D���D��fD�FfD��3D�  D�Y�D��fD� D�s3D���D�	�D�S3D�� D��D�6fD��3D���D��D�i�D��fD�fD�L�D��fD��3D�33D��3D�� D��D�ffD��3D�fD�VfD�� D�  D�I�D�� D�� D�6fD3D��3D��D�c3DǦfD��fD��D�L�D̃3DͰ D��3D� D�9�D�l�DӠ D��3D���D�&fD�` Dٌ�Dڹ�D��3D��D�33D�\�D���D�fD��3D�3D�@ D�y�D� D��3D��D�L�D� D���D���D�0 D�i�D��D���D���D�6fD�s3D��3D��fD��D�VfD���D��3D���E �fE33EњEl�E E��E@ EٚEx EfE��ED�E��Ex E	3E	�3E
8 E
�3E[3E�fE� E3E� E1�E�E	�E+3ENfE��E�E4�EH E�fE�E�E9�E�3E fE!33E"X E#��E$� E&q�E'��E(�3E)��E+�E,��E-ٚE.�3E0�E10 E2њE3� E4��E6~fE7��E9�E:&fE;+3E<� E?��EB� EE�3EI�EL�EO6fERP EUl�EX� E[�fE_ Ea�3EeC3EhvfEkK3En�3Eq�fEt��Ex3E{3E~3E���E�O3E���E�h�E��fE�rfE��fE�� E�6fE�� E�X�E�� E�L�E��fE���E��E��3E� E�� E��E�RfE���E���E�C3E���E�� E�<�E��fE���E�0�E�u�G�O�G�O�G�O�?L��G�O�G�O�G�O�?L��G�O�G�O�?L��G�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�?�  G�O�?���?�ff?�33?�  ?���?�ff@   @��@��@   @333@@  @S33@`  @s33@�  @�ff@�33@���@�ff@���@���@�33@���@ٙ�@�ff@�  A   A��A33A��A��A   A$��A,��A333A<��AC33AK33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144414414444441414111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ �@ �@ V@ �@ O@ ""@ (G@ 0x@ 6�@ >@ FQ@ R�@ `�@ m:@ z�@ �7@ ��@ ��@ �-@ �&@ ��@ �#@ ��@ � @@@�@-@:�@I@UU@b�@p�@~K@��@�H@��@�F@��@�7@��@�4@��@�@{@"�@/�@<�@Ji@X@g@t�@�d@�\@��@��@�@ƨ@�O@��@�@��@J@�@&;@4�@?}@M�@[z@i�@x&@��@�@�@�f@�@�@׹@�`@�@^@@
@(�@7L@D�@Q=@^�@m:@z�@��@�0@��@�~@��@��@�t@�m@� @@@g@,`@8�@G�@V�@c�@o�@~�@�P@��@��@�9@@��@��@�4@�~@�@�@"�@/�@>�@K@X@hs@uk@�d@�\@��@��@�R@�@��@�@�@�E@
=@�@%�@33@@�@M�@\)@i!@v�@�p@�$@��@��@�k@�@׹@�`@�@ �@V@O@(�@6�@DD@Q�@_�@m:@|�@��@�0@�y@�-@�&@��@��@��@�@	�@	�@	
@	,`@	;d@	G�@	S�@	b�@	r@	�@	��@	��@	��@	��@	Ĝ@	є@	��@	�4@	��@
�@
@
!s@
0x@
>�@
K@
Wb@
ff@
t�@
��@
�@
��@
��@
��@
ƨ@
��@
��@
�L@
�E@	�@�@&�@4�@B�@O0@[z@i�@x&@�|@�u@��@�@��@�o@�h@�@�@  @J@O@*S@8�@E�@Q�@`�@n�@z�@�7@�<@�5@�-@�2@�*@�#@��@�q@�@b@ @-�@:�@G�@T�@dZ@n�@~K@��@��@�(@�9@I�@�u@��@/@z�@Ĝ@�@]�@��@�~@D�@�@ލ@-@z�@�@{@a�@�f@��@F�@�h@��@'�@r�@�@�@Q�@��@�@+�@r�@�@�@I�@�h@�h@ @k.@��@��@I@�@�@*@]�@�4@�4@4�@|?@Ĝ@�@Z@�4@�L@<@��@��@�@g�@�-@��@ FQ@ ��@ �t@!&;@!l�@!��@!��@"@�@"�+@"�*@#*@#[z@#�(@#�(@$1'@$x�@$�w@%�@%K�@%�u@%�#@&""@&hs@&�r@&�q@'>�@'�|@'�7@(�@(^5@(�A@(��@)4�@){�@)�>@*	�@*O0@*�#@*�h@+�@+[z@+��@+��@,g@,_�@,�@,��@-""@-c�@-�z@-�@.%�@.e�@.��@.�`@/$�@/c�@/�(@/�T@0#�@0c�@0��@0�@1'�@1i�@1�Y@1�@2/@2p�@2�9@2�~@3:@3|�@3�w@3�Q@4@,@4�@4ƨ@5
�@5O�@5��@5є@6@6V@6��@6�/@7�@7`B@7�z@7�@8&;@8i�@8�Z@8��@9/@9qS@9��@9�e@:6�@:x�@:�F@:��@;4�@;r@;��@;��@<-�@<i�@<��@=c�@=ލ@>Z@>�[@?��@@�@@�W@@��@A��@Bg@B�<@C�@C��@DF�@D��@E@�@E�2@F@�@G �@G~�@G��@Hv@H�e@I��@J)�@J�4@K[@K�0@LH]@L��@M.l@M��@NM$@N��@Oi!@O�h@Pz�@Q�^@S�@Tuk@Uƨ@W@Xe�@Y�R@[J@\_�@]��@_%�@`g�@a�*@c+�@d`�@e��@g$�@hhs@i�h@k�@lg�@m�-@o/@p�d@q�O@s$.@tk�@u� @wO@x|�@y��@{)�@|ff@}��@@�7L@�܀@��@�,`@���@� �@�	@�I�@�g@���@��z@��7@��F@��@�+@�Wb@�t�G�O�G�O�G�O�@ G�O�G�O�G�O�@ G�O�G�O�@ G�O�G�O�G�O�G�O�G�O�G�O�@ �G�O�@ jG�O�@ @ v@ %@ �@ �@ �@ 
=@ �@ �@ �@ �@ @ @ {@ �@ �@ B@ �@ [@  @ !s@ $.@ &;@ (G@ +@ -�@ /�@ 33@ 5?@ 7�@ :�@ >@ @�@ B�@ FQ@ I@ M$@ O�@ SIG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bA�bA���A�5?A�jA�S�A�9XA��A�A��A��TA���A��^A��9A��A���A���A���A���A���A���A���A���A��A��A��A��A��A���A���A���A���A��uA��hA��\A��\A��DA�jA�JA�ƨA��A��A���A���A�(�A��+A��A���A��-A���A�E�A��A�33A�A�S�A�A�{A�A��A�  A���A��PA��A�l�A�;dA�bNA��jA�7LA���A�A��A�{A�?}A��A�33A���A��9A��!A��DA��A���A�XA��A��A�I�A�1A��;A��7A�1'A���A�A��;A�-A�ĜA��A��
A�VA� �A�+A��A�;A}��A|�A|  Az��Ay�#AyVAwC�Arn�Ap�Ap$�Am�hAj�Ai|�Ah�+Ag��Ae��Ad �Aa7LA_;dA]"�A[�wAZ�9AZA�AXQ�AV�yATn�AS&�ARffAP��AO�PAN5?AI��AGS�AE��AD�+AC�AC�wAC�PACG�AA��A@A�A>(�A<M�A:A7dZA5oA2 �A1XA0�DA/+A.-A-A,bNA)�FA)�wA)�mA(ZA&�`A%/A$A#dZA"��A"5?A!%A ��A ^5A�A�hAS�AA�AĜA�+A9XA��A/A%A��AjA�A�Ar�A(�AƨA�jA-A�#A��A�A��A�PAA
v�A
1'A	��A	;dA�A"�A�AbNA�A\)A7LA%A&�A33Ax�AA ~�A 5?@��H@�$�@�?}@���@�X@�v�@�+@�9@���@�S�@��#@��@��y@�J@�@�u@���@��@�p�@߾w@�~�@�hs@�v�@�j@��;@��@�E�@��@�p�@�Q�@�|�@�o@��y@Η�@��@� �@ˍP@���@��@��/@�  @�33@�J@ř�@ũ�@��T@��@��@ư!@ǅ@���@��T@�Ĝ@��@�z�@��y@��@���@�v�@�I�@���@��
@�~�@��`@���@��j@��h@��
@��@�hs@��@�=q@��@�&�@�^5@��/@�X@���@�K�@�dZ@�M�@��9@��
@���@�j@��;@�t�@��\@��@���@��@�{@��j@��@��-@���@�bN@���@��T@�z�@�Q�@~�y@~v�@}��@}�@|�@z~�@y&�@w�@tI�@rn�@q��@pr�@ol�@n�R@m�@k�@i��@g\)@eO�@c33@`bN@^E�@\�@\1@[��@[S�@Y�7@X�@Wl�@T�@So@R�@O�@M@L�@K�@I��@H1'@F�@D9X@A��@@A�@>$�@<�@:�\@97L@81'@7�@6V@4��@3�F@2n�@0��@0b@/K�@.V@,�@+��@*�!@)�@(�9@'�@';d@&{@$�/@$9X@"��@!��@ Ĝ@�@�+@�@��@��@�@1@�F@C�@M�@��@��@Ĝ@1'@+@5?@?}@V@(�@�H@J@�`@ �@+@V@$�@O�@�D@�F@��@S�@
�@	�7@��@bN@�w@�@E�@�@O�@�@�@1@ƨ@"�@�\@�#@�^@G�@ A�?�|�?���?�/?�ƨ?�X?��P?��?�J?�A�?��?�1?ꟾ?�Q�?�$�?��/?���?��`?�  ?�5??��?ݑh?��?�=q?�7L?�1'?�l�?�l�?��?ԛ�?��?�o?�-?ѩ�?�%?Ѓ?�;d?Η�?͑h?�O�?��?�(�?���?�$�?ļj?���?���?��`?�  ?�\)?���?�v�?�O�?���?�1?�dZ?�"�?���?�^5?�^5?��?���?��?�=q?�^5?�~�?�~�?��H?���?�"�?��?�dZ?��m?��D?�/?��h?�5??��R?��?�;d?�;d?�5??���?��-?�p�?�p�?��h?��h?��h?��h?�p�?��h?��-?���A�%A�1A�A�A�A�VA��A�
=A�%A�VA�bA�{A��A��A��A��A��A��A��A��A��A��A��A�oA�JA�1A�JA�
=A�1A�JA�A�  A��
A�O�A�Q�A���A��PA�p�A�hsA�dZA�bNA�M�A�I�A�C�A�=qA�(�A�"�A��A�bA�1A���A��A��A��A��A��TA��#A��A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�bA�bA���A�5?A�jA�S�A�9XA��A�A��A��TA���A��^A��9A��A���A���A���A���A���A���A���A���A��A��A��A��A��A���A���A���A���A��uA��hA��\A��\A��DA�jA�JA�ƨA��A��A���A���A�(�A��+A��A���A��-A���A�E�A��A�33A�A�S�A�A�{A�A��A�  A���A��PA��A�l�A�;dA�bNA��jA�7LA���A�A��A�{A�?}A��A�33A���A��9A��!A��DA��A���A�XA��A��A�I�A�1A��;A��7A�1'A���A�A��;A�-A�ĜA��A��
A�VA� �A�+A��A�;A}��A|�A|  Az��Ay�#AyVAwC�Arn�Ap�Ap$�Am�hAj�Ai|�Ah�+Ag��Ae��Ad �Aa7LA_;dA]"�A[�wAZ�9AZA�AXQ�AV�yATn�AS&�ARffAP��AO�PAN5?AI��AGS�AE��AD�+AC�AC�wAC�PACG�AA��A@A�A>(�A<M�A:A7dZA5oA2 �A1XA0�DA/+A.-A-A,bNA)�FA)�wA)�mA(ZA&�`A%/A$A#dZA"��A"5?A!%A ��A ^5A�A�hAS�AA�AĜA�+A9XA��A/A%A��AjA�A�Ar�A(�AƨA�jA-A�#A��A�A��A�PAA
v�A
1'A	��A	;dA�A"�A�AbNA�A\)A7LA%A&�A33Ax�AA ~�A 5?@��H@�$�@�?}@���@�X@�v�@�+@�9@���@�S�@��#@��@��y@�J@�@�u@���@��@�p�@߾w@�~�@�hs@�v�@�j@��;@��@�E�@��@�p�@�Q�@�|�@�o@��y@Η�@��@� �@ˍP@���@��@��/@�  @�33@�J@ř�@ũ�@��T@��@��@ư!@ǅ@���@��T@�Ĝ@��@�z�@��y@��@���@�v�@�I�@���@��
@�~�@��`@���@��j@��h@��
@��@�hs@��@�=q@��@�&�@�^5@��/@�X@���@�K�@�dZ@�M�@��9@��
@���@�j@��;@�t�@��\@��@���@��@�{@��j@��@��-@���@�bN@���@��T@�z�@�Q�@~�y@~v�@}��@}�@|�@z~�@y&�@w�@tI�@rn�@q��@pr�@ol�@n�R@m�@k�@i��@g\)@eO�@c33@`bN@^E�@\�@\1@[��@[S�@Y�7@X�@Wl�@T�@So@R�@O�@M@L�@K�@I��@H1'@F�@D9X@A��@@A�@>$�@<�@:�\@97L@81'@7�@6V@4��@3�F@2n�@0��@0b@/K�@.V@,�@+��@*�!@)�@(�9@'�@';d@&{@$�/@$9X@"��@!��@ Ĝ@�@�+@�@��@��@�@1@�F@C�@M�@��@��@Ĝ@1'@+@5?@?}@V@(�@�H@J@�`@ �@+@V@$�@O�@�D@�F@��@S�@
�@	�7@��@bN@�w@�@E�@�@O�@�@�@1@ƨ@"�@�\@�#@�^@G�@ A�?�|�?���?�/?�ƨ?�X?��P?��?�J?�A�?��?�1?ꟾ?�Q�?�$�?��/?���?��`?�  ?�5??��?ݑh?��?�=q?�7L?�1'?�l�?�l�?��?ԛ�?��?�o?�-?ѩ�?�%?Ѓ?�;d?Η�?͑h?�O�?��?�(�?���?�$�?ļj?���?���?��`?�  ?�\)?���?�v�?�O�?���?�1?�dZ?�"�?���?�^5?�^5?��?���?��?�=q?�^5?�~�?�~�?��H?���?�"�?��?�dZ?��m?��D?�/?��h?�5??��R?��?�;d?�;d?�5??���?��-?�p�?�p�?��h?��h?��h?��h?�p�?��h?��-?���A�%A�1A�A�A�A�VA��A�
=A�%A�VA�bA�{A��A��A��A��A��A��A��A��A��A��A��A�oA�JA�1A�JA�
=A�1A�JA�A�  A��
A�O�A�Q�A���A��PA�p�A�hsA�dZA�bNA�M�A�I�A�C�A�=qA�(�A�"�A��A�bA�1A���A��A��A��A��A��TA��#A��A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BVBT�BS�BI�B_;BdZBk�Bs�Bx�B|�B~�B�B�VB�oB��B��B�9B�RB�jB��BȴB��B�/B�ZB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��BB	7B�BB�BE�BC�BR�B\)B`BB[#BXBN�BN�BJ�BB�B;dB5?B$�B�BbBJB
=B+B+BB��B�B�B�TB�5B�
B��BĜB�dB�9B��B�hB{�B`BBP�BH�B?}B33B-B%�B
��B
�wB
�
B
�B
��B
�dB
�B
��B
��B
�PB
�B
u�B
bNB
XB
C�B
1'B
!�B
�B
{B

=B
%B
  B	�B	�)B	�B	��B	�'B	�B	��B	��B	��B	�1B	�B	o�B	^5B	VB	I�B	D�B	?}B	1'B	+B	�B	�B	\B	B��B�B�B��BȴB��B�wB�jB�^B�LB�B��B��B�JBs�BhsBVBR�BN�BK�BL�BL�BK�BK�BO�BbNBiyBe`BffBdZBdZBbNBaHB]/B\)B[#BZBXBW
BVBA�BN�BL�BI�BH�BH�BF�BC�BB�B>wB>wB;dB;dB7LB7LB7LB5?B33B6FB5?B6FB7LB9XB:^B9XB9XB:^BA�BF�BE�BF�BH�BH�BK�BT�B]/B_;B`BB`BBe`BbNB`BB^5BVBM�BA�B;dB5?B2-B6FB;dB;dBB�BE�BC�BF�BG�BH�BG�BF�BG�BF�B@�B;dB<jB;dB;dB:^B:^B<jB>wB>wB?}B?}BB�BG�BF�BG�BF�BH�BI�BJ�BN�BW
BXB\)B`BBffBr�B�B�7B�PB�bB�LB�-B��B�B	oB	�B	'�B	6FB	B�B	Q�B	z�B	�1B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	ÖB	��B	�BB	�fB	�NB	�sB	�fB	�B	�B	�B	��B	��B	��B	��B
B
B
  B	��B
B
B
B
B
B
%B
%B
VB
bB
{B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
"�B
$�B
%�B
%�B
#�B
$�B
%�B
'�B
)�B
/B
0!B
1'B
33B
49B
5?B
8RB
:^B
;dB
<jB
>wB
@�B
@�B
B�B
C�B
D�B
H�B
J�B
K�B
L�B
O�B
P�B
Q�B
R�B
R�B
S�B
VB
W
B
VB
XB
XB
YB
YB
\)B
[#B
]/B
]/B
^5B
`BB
`BB
`BB
bNB
cTB
dZB
dZB
e`B
hsB
gmB
iyB
hsB
iyB
k�B
jB
jB
l�B
l�B
m�B
m�B
n�B
o�B
q�B
q�B
q�B
r�B
r�B
t�B
t�B
u�B
v�B
w�B
x�B
x�B
y�B
{�B
{�B
{�B
|�B
}�B
}�B
}�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�+B
�+B
�1B
�7B
�=B
�DB
�DB
�JB
�VB
�PB
�bB
�hB
�oB
�{B
�{B
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
�B
�B
�B
�'B
�'B
�3B
�9B
�?B
�FB
�LB
�LB
�RB
�XB
�^B
�^B
�dB
�dB
�jB
�jB
�jB
�qB
�qB
�wB
�wB
�wB
�}B
�}B
��B
��B
�}B
�}B
�}B
�}B
��B
��B
��B
��B
ÖB
ÖB
B
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
B
ÖB
ÖBVBT�BT�BVBXBW
BT�BVBVBVBT�BVBT�BW
BT�BVBT�BVBT�BVBVBVBT�BT�BS�BVBT�BVBT�BT�BS�BT�BR�B7LB?}BS�B]/B^5B`BB_;B_;Be`BgmBhsBiyBo�Bq�Br�Bu�Bw�Bx�B{�B|�B|�B|�B~�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 BR�BQ�BP�BF�B\)BaHBhsBp�Bu�By�B{�B�B�DB�\B��B��B�'B�?B�XB�wBŢB��B�B�HB�sB�B�B�B�B��B�B�B�B�B�B��B��B��B��B��B��B%B�B?}BB�B@�BO�BYB]/BXBT�BK�BK�BG�B?}B8RB2-B!�B�BPB	7B+BBB��B��B�B�mB�BB�#B��B��B��B�RB�'B��B�VBx�B]/BM�BE�B<jB0!B)�B"�B
�B
�dB
��B
��B
ǮB
�RB
��B
��B
��B
�=B
}�B
r�B
_;B
T�B
@�B
.B
�B
�B
hB
+B
B	��B	�B	�B	��B	��B	�B	�B	��B	��B	��B	�B	}�B	l�B	[#B	R�B	F�B	A�B	<jB	.B	'�B	uB	oB	JB	B��B�B�
B��BŢB�wB�dB�XB�LB�9B��B��B�uB�7Bp�Be`BR�BO�BK�BH�BI�BI�BH�BH�BL�B_;BffBbNBcTBaHBaHB_;B^5BZBYBXBW
BT�BS�BR�B>wBK�BI�BF�BE�BE�BC�B@�B?}B;dB;dB8RB8RB49B49B49B2-B0!B33B2-B33B49B6FB7LB6FB6FB7LB>wBC�BB�BC�BE�BE�BH�BQ�BZB\)B]/B]/BbNB_;B]/B[#BR�BJ�B>wB8RB2-B/B33B8RB8RB?}BB�B@�BC�BD�BE�BD�BC�BD�BC�B=qB8RB9XB8RB8RB7LB7LB9XB;dB;dB<jB<jB?}BD�BC�BD�BC�BE�BF�BG�BK�BS�BT�BYB]/BcTBo�B� B�%B�=B�PB�9B�B��B�B	\B	�B	$�B	33B	?}B	N�B	w�B	�%B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�5B	�ZB	�BB	�fB	�ZB	�B	�B	�B	��B	��B	��B	��B	��B
  B	��B	��B
B
B
  B
  B
B
B
B
JB
VB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
!�B
"�B
#�B
%�B
'�B
-B
.B
/B
1'B
2-B
33B
6FB
8RB
9XB
:^B
<jB
>wB
>wB
@�B
A�B
B�B
F�B
H�B
I�B
J�B
M�B
N�B
O�B
Q�B
Q�B
R�B
T�B
VB
T�B
W
B
W
B
XB
XB
[#B
ZB
\)B
\)B
]/B
_;B
_;B
_;B
aHB
bNB
cTB
cTB
dZB
gmB
ffB
hsB
gmB
hsB
jB
iyB
iyB
k�B
k�B
l�B
l�B
m�B
n�B
p�B
p�B
p�B
q�B
q�B
s�B
s�B
t�B
u�B
v�B
w�B
w�B
x�B
z�B
z�B
z�B
{�B
|�B
|�B
|�B
~�B
~�B
~�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�+B
�1B
�7B
�=B
�=B
�DB
�PB
�JB
�\B
�hB
�oB
�{B
�{B
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
�B
�B
�B
�-B
�-B
�9B
�?B
�FB
�LB
�RB
�RB
�XB
�^B
�dB
�dB
�jB
�jB
�qB
�qB
�qB
�}B
�}B
��B
��B
��B
��B
��B
B
B
��B
��B
��B
��B
B
B
B
ÖB
ƨB
ƨB
ŢB
ŢB
ƨB
ŢB
ƨB
ƨB
ƨB
ƨB
ŢB
ƨB
ƨBR�BQ�BQ�BR�BT�BS�BQ�BR�BR�BR�BQ�BR�BQ�BS�BQ�BR�BQ�BR�BQ�BR�BR�BR�BQ�BQ�BP�BR�BQ�BR�BQ�BQ�BP�BQ�BO�B49B<jBP�BZB[#B]/B\)B\)BbNBdZBe`BffBl�Bn�Bo�Br�Bt�Bu�Bx�By�By�By�B{�B}�B}�B� B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201903130700352021061413531920210614135319202106141747102021061417471020210614174710201903130700352021061413531920210614135319202106141747102021061417471020210614174710PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.003 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2019031307003520190313070035  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019031307003520190313070035QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019031307003520190313070035QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015820210722160158IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                