CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  $   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-21T07:01:04Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  M    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        QH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  bh   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        f�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        w�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �8   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        �(   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  �H   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ސ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                      HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                       HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   (   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   0   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar           HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�          HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                       SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �X   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �X   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � X      � XArgo profile    3.1 1.2 19500101000000  20180921070104  20210722160153  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؀�n���@؀�n���11  @؀�ff{`@؀�ff{`@6!�P�@6!�P��c�N;�5��c�N;�5�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff?�33@@  @�  @�  @�33@�33A   AffA$��AC33A`  A���A���A���A�  A�  A���AᙚA���B   BffBffB��B ffB(  B0  B8ffB@��BHffBO��BW��B`��BhffBpffBxffB�33B�  B�  B�ffB�ffB�33B�  B���B�  B�33B�33B�ffB�33B�33B�33B�  B�33B�ffB�33B���B�33Bә�B���B�  B�33B�ffB�  B�33B�B���B�  B���B���C  C  C�fC�fC
L�C33C33C33C�C�C�fC33C�C  C�C 33C"�C#��C%�fC(  C*33C,33C.�C0�C2  C4  C6  C7�fC9�fC<�C>ffC@L�CB33CD33CF33CH�CJ33CLL�CN  CO��CQ��CS�fCV�CXL�CZ33C\  C^�C`  Cb  Cd  Ce�fCh33Cj33Cl�Cn33Cp�Cr33Ct33Cv33Cx  Cz�C|33C~  C��C�&fC�  C��C��C��3C�  C�  C��C��C��C�&fC��3C��3C�  C�  C��C�  C��C��C��C��C�  C�  C��C��3C��3C��fC��C��3C��3C��C��C��C��3C��C��C�33C��C�  C��C��C�&fC��C�  C��C�  C��3C��C�  C��fC�  C�&fC��C��3C�  C��C�&fC��C��fC�  C��C�&fC��C�  C��C�  C��fC�  C��C��C��C��fC��3C��C�&fC��C��3C�  C��C�&fC��C�  C��C��C��3C��C�&fC�  C��fC��3C��3C�  C��C��C��fC�  C��C��C��C�&fC�33C��C��3C��3C��3C�  C�  C��C��C��C�&fC�  C��fC�ٚC��fC��fC��3C��3C�  C�  C�  C��fC��C��C��C�&fC�ٚC��3D ��D �fDs3D�3DFfDffD
  D�fD�fD` D��D�3D9�D�3D,�D!�fD$,�D&� D)�D+� D-��D0&fD2��D5�D7�fD:�D<�fD>�3DA` DC�3DF@ DH� DK@ DM��DPS3DR�fDUffDW��DZs3D\�3D_s3Da��Dds3Df��Dil�Dk� DnL�Dp�3Ds  Du�3Dw��DzffD|s3D~��D��3D���D�)�D�c3D�� D���D��D�\�D��3D��3D�,�D�` D���D���D��D�FfD�y�D���D�ٚD��D�VfD���D���D�	�D�C3D�vfD��fD���D�  D�\�D�� D��fD��fD�)�D�\�D���D��3D���D���D��D�C3D�ffD���D���D���D�� D��D���D�fD�fD�  D�)�D�33D�33D�9�D�<�D�I�D�S3D�` D�i�D�i�D�p DȀ DɌ�Dʓ3Dˠ D̶fD�� D��fD��fD��3D��D�#3D�9�D�P D�i�D׆fDؙ�D٦fDڼ�D��fD���D�  D��D�6fD�VfD�vfD�fD�fD�ٚD��fD�3D�0 D�I�D�ffD�|�D� D��D��fD�� D�  D�fD�0 D�C3D�\�D�p D��fD���D���D��3D���D�� D���D�ٚE q�E ��E��E E��E�E��E1�EI�ES3Ec3E��E	��E
�fE��E�fE� Ei�E` E�3E�3E4�E�3E��E��EI�E<�E�3E�Ea�E L�E!��E#fE$fE%�3E&��E( E) E*� E+��E-3E.fE/�fE0��E2�E3d�E4T�E5��E7�E8S3E9�fE:� E;�fE?fEB>fEEfEH[3EK��EN� EQ�fET� EW�fE[;3E^C3Ea{3Ed�fEg� Ej��Em�3Ep��Et0 Ew6fEzT�E}�fE�X�E�ŚE�i�E���E���E��E���E��fE�>fE��3E�ٚE�;3E�{3E�� E�3E�t�E��3E��E�q�E�� E��E�M�E���E��E�@ E���E�� E�5�E�s3E���E�'3E�c3E��3E�3E�k3E���E��3E�P E���E���E�2fE��fE��fE�, >���>���?   >���>���>���>���>���>���?   ?   ?��?��?L��?fff?�  ?�ff?�ff?���?ٙ�?�33@ff@33@&ff@9��@Fff@Y��@fff@�  @���@�33@�  @���@���@�33@���@���@陚@���A��A33A��A��A   A(  A0  A8  A>ffAFffANffAT��A\��Ac33Ai��Aq��A{33A���A�  A�33A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444441414111141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ?fff?�33@��@`  @�  @�  @�33@�33A  AffA,��AK33Ah  A���A���A���A�  A�  A���A噚A���B  B
ffBffB��B"ffB*  B2  B:ffBB��BJffBQ��BY��Bb��BjffBrffBzffB�33B�  B�  B�ffB�ffB�33B�  B���B�  B�33B�33B�ffB�33B�33B�33B�  B�33B�ffB�33B���B�33Bԙ�B���B�  B�33B�ffB�  B�33B�B���B�  B���C ffC� C� CffCffC
��C�3C�3C�3C��C��CffC�3C��C� C��C �3C"��C$L�C&ffC(� C*�3C,�3C.��C0��C2� C4� C6� C8ffC:ffC<��C>�fC@��CB�3CD�3CF�3CH��CJ�3CL��CN� CPL�CRL�CTffCV��CX��CZ�3C\� C^��C`� Cb� Cd� CfffCh�3Cj�3Cl��Cn�3Cp��Cr�3Ct�3Cv�3Cx� Cz��C|�3C~� C�L�C�ffC�@ C�L�C�Y�C�33C�@ C�@ C�L�C�Y�C�Y�C�ffC�33C�33C�@ C�@ C�L�C�@ C�L�C�L�C�L�C�L�C�@ C�@ C�L�C�33C�33C�&fC�L�C�33C�33C�L�C�Y�C�L�C�33C�L�C�Y�C�s3C�Y�C�@ C�L�C�Y�C�ffC�Y�C�@ C�Y�C�@ C�33C�L�C�@ C�&fC�@ C�ffC�L�C�33C�@ C�Y�C�ffC�L�C�&fC�@ C�L�C�ffC�Y�C�@ C�Y�C�@ C�&fC�@ C�L�C�Y�C�L�C�&fC�33C�Y�C�ffC�L�C�33C�@ C�L�C�ffC�L�C�@ C�Y�C�L�C�33C�L�C�ffC�@ C�&fC�33C�33C�@ C�Y�C�L�C�&fC�@ C�L�C�L�C�Y�C�ffC�s3C�L�C�33C�33C�33C�@ C�@ C�L�C�Y�C�Y�C�ffC�@ C�&fC��C�&fC�&fC�33C�33C�@ C�@ C�@ C�&fC�L�C�L�C�Y�C�ffC��D �D ��DfD�3D3DffD�fD
@ DfD�fD� D�D�3DY�D�3DL�D!�fD$L�D&� D)9�D+� D-��D0FfD2��D5,�D7�fD:,�D<�fD?3DA� DC�3DF` DH� DK` DM��DPs3DSfDU�fDX�DZ�3D]3D_�3Db�Dd�3Dg�Di��Dl  Dnl�Dp�3Ds@ Du�3Dx�Dz�fD|�3D�D��3D���D�9�D�s3D�� D���D�)�D�l�D��3D��3D�<�D�p D���D���D��D�VfD���D���D��D�)�D�ffD���D���D��D�S3D��fD��fD���D�0 D�l�D�� D��fD�fD�9�D�l�D���D��3D���D��D�)�D�S3D�vfD���D���D�ɚD�� D���D�	�D�fD�&fD�0 D�9�D�C3D�C3D�I�D�L�D�Y�D�c3D�p D�y�D�y�Dǀ DȐ Dɜ�Dʣ3D˰ D��fD�� D��fD��fD�3D��D�33D�I�D�` D�y�DזfDة�DٶfD���D��fD���D� D�)�D�FfD�ffD�fD�fD��fD��D�fD�#3D�@ D�Y�D�vfD��D�� D��D��fD�� D� D�&fD�@ D�S3D�l�D�� D��fD���D���D��3D���D�� D���D��E y�E�E��E E��E$�E��E9�EQ�E[3Ek3E��E
�EfE��E�fE  Eq�Eh E�3E�3E<�E�3E��E��EQ�ED�E�3E	�Ei�E T�E!��E#&fE$fE%�3E&��E( E) E*� E+��E-3E.fE/�fE0��E2	�E3l�E4\�E5��E7�E8[3E9�fE:� E;�fE?fEBFfEEfEHc3EK��EN� EQ�fEU  EW�fE[C3E^K3Ea�3Ed�fEg� Ej��Em�3Ep��Et8 Ew>fEz\�E}�fE�\�E�ɚE�m�E��E���E��E���E�fE�BfE��3E�ݚE�?3E�3E�� E�3E�x�E��3E��E�u�E�� E��E�Q�E���E��E�D E���E�� E�9�E�w3E���E�+3E�g3E��3E�3E�o3E���E��3E�T E���E���E�6fE��fE��fE�0 G�O�?fffG�O�G�O�G�O�G�O�G�O�G�O�?fffG�O�?�  G�O�?���?�ff?�33?�  G�O�?�ff@ff@��@��@&ff@333@Fff@Y��@fff@y��@�33@�  @���@�33@�  @���@ə�@�33@���@���@���A��A	��A33A��A!��A(  A0  A8  A@  AFffANffAVffA\��Ad��Ak33Aq��Ay��A���A���A�  A�33A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444441414111141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        @ �@ %@ �@ {@ O@ ""@ )�@ 0x@ 6�@ <�@ FQ@ SI@ _�@ m�@ {�@ �7@ �0@ ��@ �-@ ��@ �|@ �t@ ��@ �q@�@�@�@,`@:�@I@V@bN@o�@�@��@�H@��@��@@�7@�;@��@��@�@�@""@0x@>@Lu@Yn@g@t�@��@�@�a@�Y@�@ƨ@�C@��@��@�E@�@�@&;@5?@@,@N�@[z@i!@ww@�@�@��@�!@�@��@�h@�`@�@�Q@@�@(�@7L@E�@R�@^5@l�@z�@��@��@��@�-@�&@��@�t@�m@�@@�@ �@-�@;d@I@V@dZ@r�@~K@��@�<@��@��@Ĝ@є@��@�4@�,@�@{@!s@1'@>�@K�@Z@g@uk@�@��@�@�Y@��@��@�O@�T@��@�E@�@6@%�@33@A�@O�@]�@k�@v�@�p@��@�m@��@��@�@׹@�`@�@  @�@�@(G@5�@B�@R�@^�@l�@{�@��@��@�(@�-@��@ψ@��@�@�q@	�@	@	 @	,`@	;d@	G�@	T�@	c�@	p�@	|�@	��@	��@	��@	�9@	@	є@	��@	�4@	��@
�@
*@
$.@
1'@
=q@
Lu@
X�@
e	@
t@
�d@
��@
��@
�M@
��@
�W@
խ@
��@
�@@
��@
�@�@&;@33@B8@O0@[z@j@y�@�@�h@��@�f@��@��@׹@�T@�Y@ �@V@�@+@9X@D�@Q=@^�@l�@z�@��@��@�5@��@�2@��@�@�@�e@@b@
@,`@:@G�@S�@c�@qS@�@��@��@��@�F@��@��@܀@�`@qS@��@�@R�@�@�@+@s_@��@��@>@�@��@	�@K@��@�@�@O�@�u@�h@�@^5@�m@�T@%�@i�@�@�@8�@~�@�>@1@M$@�h@խ@�@^5@��@�@(�@k.@��@��@1�@s_@��@�@1'@t�@��@�9@>@��@�J@�@M�@�u@׹@
@_�@�z@�@(G@k.@��@�@@ .l@ r�@ �F@ �~@!<�@!�W@!�>@"�@"I@"�D@"��@#b@#Q�@#�#@#��@$�@$X@$�<@$׹@%6@%T�@%�i@%��@&@&M$@&��@&Ĝ@'  @'<@'v@'�r@'�y@(""@(Z�@(�u@(�@)@)9X@)r�@)�Y@)�@*[@*S�@*��@*��@*�Q@+7L@+p�@+�@+�@, @,Z@,�u@,ψ@-
�@-FQ@-��@-��@-��@.5@@.n�@.��@.�@/!s@/\)@/�<@/��@0o@0O�@0�P@0��@1�@1E�@1�d@1�&@1�9@27�@2s_@2�@2��@3&�@3b�@3�m@3��@4�@4R�@4��@4�c@5�@5@,@5v@5��@5�m@6""@6[z@6��@6�|@7�@7A�@7z�@7��@7��@8-@8g@8ލ@9O�@9��@:k�@:ލ@;M�@;�@<bN@=  @=��@>�@>��@?@?�M@@B8@@��@A<@A�O@B<@B��@Cj@D �@De	@D�E@E��@F�@F��@GV@G�~@H�@H��@I5@@I�h@JF�@J��@KS�@K�@L��@L��@M��@N�@N�4@O/@O��@P2�@Q�@R��@T6@U|?@V�@X6�@Yo�@Z��@\	@]��@^��@`4�@a��@b��@d �@euk@fĜ@h/�@iz2@j��@l+�@m��@n�@p#�@q|?@rє@t"�@uqS@u�1@u��@vF�@v|?@vψ@w%@wUU@w�C@w�#@xb@x`�@x��@x�@y7�@yn�@y�k@y�M@z=q@z��@z�&@{@{C�@{�@{�/@|b@|[z@|��@|�@}#�@}l�@}��@}�E@~D�@~v@~��@v@KG�O�@ �G�O�G�O�G�O�G�O�G�O�G�O�@ �G�O�@ jG�O�@ @ v@ %@ �G�O�@ �@ 
�@ �@ �@ V@ �@ �@ �@ *@ 6@ �@ O@ [@ g@ ""@ $�@ '�@ )�@ +�@ /@ 1�@ 5?@ 7L@ ;d@ >@ A�@ DD@ G�@ K@ N�@ Q=@ T�@ X@ Z�@ ^5@ `�@ c�@ g@ k.@ m�@ p�@ s_@ wwG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�bNA�jA�|�A�~�A�~�A�|�A�x�A�z�A�z�A�x�A�z�A�|�AօA։7AփA�x�A�ZA���Aԗ�A���A�x�A��A��A�A�C�A��mA�ȴAэPA�(�A���A�ZA���A�oA�XA�x�A��
A�G�Aǉ7Aơ�A���A�oA�%A�A�5?A���A� �A�$�A���A��A��A��FA��A�ƨA��HA��uA�33A�t�A��A���A��A���A��DA�JA��HA���A���A�
=A�n�A���A��A�$�A�jA�M�A���A���A�~�A�  A���A���A���A�ZA�/A���A��A���A��PA�n�A�E�A�+A��A�{A�Q�A��yA�z�A��uA��A���A�{A�%A���A��`A��A�A��uA� �A��wA�7LA�A�v�A��-A�O�A���A��A�`BA�~�A�
=A��wA�A��jA�/A~�9Az��Aw��Av�!AuG�Asl�Aq�AoS�AmoAj�Ag�PAf1Ae��Ad��AdA�Ac�Ab�A`�!A]��A\bNA[VAY�^AXbNAT��AS��AS��AR�`ARA�AQ�AP��APJAN�uAM&�AL��AL^5AK��AJ�\AIl�AH�jAG�AGoAD(�ACl�AAA@A�A>��A=
=A:ȴA8�!A8A7�A6�/A5A2~�A0ffA/&�A.bNA-�FA,�A,E�A+�A*ZA)7LA(�jA'l�A$��A"��A!AjA?}A"�AoA�`Az�A�AbNA�mAbNA&�AVA �AK�AffA��A+AƨA�uA�wA&�A��A�jA~�Ap�A�+AQ�A9XA�AƨA�7A
��A	O�A��A�;A�uA�wA�AĜAz�AA ff@�p�@��D@�G�@���@�33@��+@��u@�"�@��@��@��@�@���@���@��@�-@�G�@�~�@�|�@�~�@��@���@� �@�`B@؛�@�\)@�{@�7L@���@���@�t�@ҟ�@��T@�O�@��@д9@�r�@�7L@�C�@Ɵ�@��@�@��y@�{@�j@�=q@�\)@���@�t�@���@��@�
=@��D@�{@� �@�~�@���@�b@�`B@�1'@�9X@���@�@�z�@��!@��@�@�/@�(�@�J@��@�V@�@�@��u@�x�@�A�@�b@�\)@�V@��^@�bN@�S�@��h@���@�z�@~�R@y��@v��@t�D@s��@sS�@q�7@p�9@n�@n@k33@i�7@h��@hA�@f�R@e�T@b��@ax�@_+@]V@\1@[�m@[C�@ZJ@V��@UO�@S��@SC�@R��@Qx�@O�;@O��@O�@M@M/@Kt�@K"�@I�@HĜ@F��@E�T@D�j@C�m@C"�@BJ@A7L@?�;@=��@<�@;��@;t�@:�@9�@8 �@7�w@6ff@5`B@5�@4��@2��@1�7@0��@/��@/l�@-�h@,1@+ƨ@+33@)�7@(bN@'�@&�+@%`B@$j@#��@!�@!%@ Ĝ@�@��@V@��@�@o@��@�@�9@�;@��@E�@`B@�
@t�@��@~�@��@1'@�P@�@��@j@�@
��@
n�@	��@r�@��@��@{@p�@O�@Z@�
@33@�H@��@M�@��@%@ Ĝ@  �?���?��?��?�C�?��u?�E�?�F?�M�?�hs?�  ?�p�?��?�ƨ?�^?�+?�E�?��T?�Z?�o?���?�A�?�/?�?��#?�b?֧�?�`B?Լj?��/?ӕ�?��?�J?�&�?�Ĝ?��;?�\)?���?�{?Ͳ-?�/?�I�?˥�?��H?��#?��?�r�?Ƨ�?ě�?\?���?���?�5??��?��m?��?��H?��#?���?��#?��#?��?�^5?��?���?�=q?�~�?���?�?�C�?��?��m?�I�?��?�p�?��-?��-?���?��?�{?�5??�V?�V?���?���?��?���?���?��?�;d?�\)?�|�?���?���?��w?��;?�  ?�  ?� �?�bN?��?��?�Ĝ?��`?��`?�&�?�&�?�hs?��7?���?���?��A�hsA�ffA�ffA�dZA�bNA�`BA�`BA�`BA�dZA�`BA�`BA�\)A�^5A�`BA�hsA�t�A�z�A�|�A�~�A�~�A�|�A�|�A�|�A�~�A�~�A�~�AցA�~�A�~�A�|�A�~�A�|�A�x�A�x�A�x�A�v�A�x�A�z�A�z�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�z�A�x�A�|�A�~�A�~�AցAօA֍PA։7A։7AօA֋DA֏\A֏\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        A�bNA�jA�|�A�~�A�~�A�|�A�x�A�z�A�z�A�x�A�z�A�|�AօA։7AփA�x�A�ZA���Aԗ�A���A�x�A��A��A�A�C�A��mA�ȴAэPA�(�A���A�ZA���A�oA�XA�x�A��
A�G�Aǉ7Aơ�A���A�oA�%A�A�5?A���A� �A�$�A���A��A��A��FA��A�ƨA��HA��uA�33A�t�A��A���A��A���A��DA�JA��HA���A���A�
=A�n�A���A��A�$�A�jA�M�A���A���A�~�A�  A���A���A���A�ZA�/A���A��A���A��PA�n�A�E�A�+A��A�{A�Q�A��yA�z�A��uA��A���A�{A�%A���A��`A��A�A��uA� �A��wA�7LA�A�v�A��-A�O�A���A��A�`BA�~�A�
=A��wA�A��jA�/A~�9Az��Aw��Av�!AuG�Asl�Aq�AoS�AmoAj�Ag�PAf1Ae��Ad��AdA�Ac�Ab�A`�!A]��A\bNA[VAY�^AXbNAT��AS��AS��AR�`ARA�AQ�AP��APJAN�uAM&�AL��AL^5AK��AJ�\AIl�AH�jAG�AGoAD(�ACl�AAA@A�A>��A=
=A:ȴA8�!A8A7�A6�/A5A2~�A0ffA/&�A.bNA-�FA,�A,E�A+�A*ZA)7LA(�jA'l�A$��A"��A!AjA?}A"�AoA�`Az�A�AbNA�mAbNA&�AVA �AK�AffA��A+AƨA�uA�wA&�A��A�jA~�Ap�A�+AQ�A9XA�AƨA�7A
��A	O�A��A�;A�uA�wA�AĜAz�AA ff@�p�@��D@�G�@���@�33@��+@��u@�"�@��@��@��@�@���@���@��@�-@�G�@�~�@�|�@�~�@��@���@� �@�`B@؛�@�\)@�{@�7L@���@���@�t�@ҟ�@��T@�O�@��@д9@�r�@�7L@�C�@Ɵ�@��@�@��y@�{@�j@�=q@�\)@���@�t�@���@��@�
=@��D@�{@� �@�~�@���@�b@�`B@�1'@�9X@���@�@�z�@��!@��@�@�/@�(�@�J@��@�V@�@�@��u@�x�@�A�@�b@�\)@�V@��^@�bN@�S�@��h@���@�z�@~�R@y��@v��@t�D@s��@sS�@q�7@p�9@n�@n@k33@i�7@h��@hA�@f�R@e�T@b��@ax�@_+@]V@\1@[�m@[C�@ZJ@V��@UO�@S��@SC�@R��@Qx�@O�;@O��@O�@M@M/@Kt�@K"�@I�@HĜ@F��@E�T@D�j@C�m@C"�@BJ@A7L@?�;@=��@<�@;��@;t�@:�@9�@8 �@7�w@6ff@5`B@5�@4��@2��@1�7@0��@/��@/l�@-�h@,1@+ƨ@+33@)�7@(bN@'�@&�+@%`B@$j@#��@!�@!%@ Ĝ@�@��@V@��@�@o@��@�@�9@�;@��@E�@`B@�
@t�@��@~�@��@1'@�P@�@��@j@�@
��@
n�@	��@r�@��@��@{@p�@O�@Z@�
@33@�H@��@M�@��@%@ Ĝ@  �?���?��?��?�C�?��u?�E�?�F?�M�?�hs?�  ?�p�?��?�ƨ?�^?�+?�E�?��T?�Z?�o?���?�A�?�/?�?��#?�b?֧�?�`B?Լj?��/?ӕ�?��?�J?�&�?�Ĝ?��;?�\)?���?�{?Ͳ-?�/?�I�?˥�?��H?��#?��?�r�?Ƨ�?ě�?\?���?���?�5??��?��m?��?��H?��#?���?��#?��#?��?�^5?��?���?�=q?�~�?���?�?�C�?��?��m?�I�?��?�p�?��-?��-?���?��?�{?�5??�V?�V?���?���?��?���?���?��?�;d?�\)?�|�?���?���?��w?��;?�  ?�  ?� �?�bN?��?��?�Ĝ?��`?��`?�&�?�&�?�hs?��7?���?���?��A�hsA�ffA�ffA�dZA�bNA�`BA�`BA�`BA�dZA�`BA�`BA�\)A�^5A�`BA�hsA�t�A�z�A�|�A�~�A�~�A�|�A�|�A�|�A�~�A�~�A�~�AցA�~�A�~�A�|�A�~�A�|�A�x�A�x�A�x�A�v�A�x�A�z�A�z�A�x�A�x�A�x�A�x�A�x�A�z�A�z�A�z�A�x�A�|�A�~�A�~�AցAօA֍PA։7A։7AօA֋DA֏\A֏\G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BVBVBPBPBPBPBPBVBPBVBVBPBPBJBJBDB
=B%B�B!�B%�B%�B)�B/B2-B2-B5?B7LB:^B=qBG�BcTB�B�{B��B��B��B�3B�wBŢB�B�5B�B��B��B��B��B%B\BhBbB{B#�B)�B+B,B:^BC�BE�BG�BJ�BM�B\)B\)B]/B[#B^5Be`BhsBl�Bl�BjBp�Bt�Bu�Bw�Bs�Bs�Bq�Br�Bn�BgmB^5BbNBP�BB�B=qB:^B7LB+BPB%B��B��B�sB��B�3B��B�{B�Bp�BQ�BH�BA�B:^B49B,B&�B�BB
�B
�HB
�
B
�qB
�?B
�hB
}�B
]/B
P�B
B�B
7LB
\B
B	��B	�B	�/B	��B	�jB	��B	��B	�%B	z�B	u�B	n�B	hsB	bNB	\)B	I�B	D�B	:^B	0!B	'�B	�B	DB	+B	  B��B��B�B�B�B�HB�/B�)B�B��B��B��B�B�B��B��BȴB��B�wB�FB�-B��B��B��B��B�{B�bB�+B�B~�B|�Bz�Bx�Bv�Br�Bo�Bm�Bk�BgmB\)B`BB\)B^5B_;B\)B[#BZBYBT�BT�BQ�BO�BN�BN�BL�BI�BG�BG�BC�BB�BB�BE�BH�BJ�BI�BH�BG�BE�BF�BE�BE�BF�BD�B@�BA�B@�B>wB?}B?}B?}B>wB=qB:^B;dB;dB8RB5?B33B2-B1'B0!B1'B2-B2-B49BC�B`BBjBl�Bm�Bo�Bk�Bm�Bo�Bn�Bm�Bl�Bp�Br�Br�By�Bx�Bx�By�By�B{�B{�Bz�B{�B{�B{�B��B��B�3BƨB��B��B��BȴB�dB�TB�B��B	bB	oB	%�B	)�B	/B	6FB	9XB	;dB	C�B	F�B	S�B	XB	_;B	hsB	u�B	}�B	�PB	�bB	�uB	��B	�'B	ĜB	ƨB	ȴB	ɺB	��B	�B	�BB	�HB	�fB	�B	�B	�B	��B	��B	��B	��B	��B
B
1B
	7B

=B
DB
\B
bB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
"�B
#�B
%�B
$�B
%�B
&�B
,B
,B
,B
-B
/B
/B
2-B
2-B
2-B
49B
49B
6FB
6FB
8RB
9XB
;dB
=qB
>wB
>wB
>wB
@�B
@�B
C�B
D�B
E�B
E�B
E�B
G�B
I�B
H�B
I�B
K�B
K�B
K�B
K�B
M�B
P�B
O�B
P�B
Q�B
S�B
T�B
T�B
VB
XB
YB
ZB
ZB
\)B
]/B
]/B
`BB
`BB
_;B
_;B
aHB
`BB
`BB
cTB
dZB
dZB
ffB
ffB
gmB
hsB
hsB
gmB
iyB
jB
jB
k�B
l�B
n�B
n�B
p�B
r�B
r�B
s�B
t�B
t�B
u�B
w�B
x�B
x�B
y�B
z�B
z�B
{�B
|�B
}�B
}�B
|�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�%B
�+B
�7B
�DB
�=B
�JB
�PB
�PB
�VB
�\B
�oB
�oB
�oB
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
��B
�B
�B
�B
�B
�'B
�-B
�9B
�9B
�9B
�FB
�LB
�LB
�LB
�RB
�RB
�XB
�XB
�^B
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�jB
�jB
�jB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�jB
�jB
�jB
�jB
�jBVBPBVBVBVBVBVBVBVBPBPBVBVBVBVBPBPBPBPBPBJBJBPBPBPBVBJBPBPBPBPBPBPBPBPBVBVBPBPBPBPBVBVBVBPBVBPBPBVBPBPBPBVBJBDBDBJBPBPBJG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        BJBJBDBDBDBDBDBJBDBJBJBDBDB
=B
=B	7B1BB�B�B#�B#�B'�B-B0!B0!B33B5?B8RB;dBE�BaHB~�B�oB��B��B��B�'B�jBÖB��B�)B�B��B��B��B��BBPB\BVBoB!�B'�B(�B)�B8RBA�BC�BE�BH�BK�BZBZB[#BYB\)BcTBffBjBjBhsBn�Br�Bs�Bu�Bq�Bq�Bo�Bp�Bl�Be`B\)B`BBN�B@�B;dB8RB5?B(�BDBB��B�B�fB��B�'B��B�oB�Bn�BO�BF�B?}B8RB2-B)�B$�B�BB
�sB
�;B
��B
�dB
�3B
�\B
{�B
[#B
N�B
@�B
5?B
PB
  B	��B	�sB	�#B	��B	�^B	��B	�uB	�B	x�B	s�B	l�B	ffB	`BB	ZB	G�B	B�B	8RB	.B	%�B	�B		7B	B��B��B��B�B�B�sB�;B�#B�B�B��B��B��B��B�B��BɺBƨB�}B�jB�9B�!B��B��B��B��B�oB�VB�B�B|�Bz�Bx�Bv�Bt�Bp�Bm�Bk�BiyBe`BZB^5BZB\)B]/BZBYBXBW
BR�BR�BO�BM�BL�BL�BJ�BG�BE�BE�BA�B@�B@�BC�BF�BH�BG�BF�BE�BC�BD�BC�BC�BD�BB�B>wB?}B>wB<jB=qB=qB=qB<jB;dB8RB9XB9XB6FB33B1'B0!B/B.B/B0!B0!B2-BA�B^5BhsBjBk�Bm�BiyBk�Bm�Bl�Bk�BjBn�Bp�Bp�Bw�Bv�Bv�Bw�Bw�By�By�Bx�By�By�By�B��B��B�'BĜB��B��BȴBƨB�^B�NB�B��B	\B	hB	$�B	(�B	.B	5?B	8RB	:^B	B�B	E�B	R�B	W
B	^5B	gmB	t�B	|�B	�JB	�\B	�oB	��B	�!B	ÖB	ŢB	ǮB	ȴB	��B	�B	�;B	�BB	�`B	�B	�B	�B	�B	��B	��B	��B	��B
B
+B
1B
	7B

=B
VB
\B
bB
hB
�B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
$�B
#�B
$�B
%�B
+B
+B
+B
,B
.B
.B
1'B
1'B
1'B
33B
33B
5?B
5?B
7LB
8RB
:^B
<jB
=qB
=qB
=qB
?}B
?}B
B�B
C�B
D�B
D�B
D�B
F�B
I�B
H�B
I�B
K�B
K�B
K�B
K�B
M�B
P�B
O�B
P�B
Q�B
S�B
T�B
T�B
VB
XB
YB
ZB
ZB
\)B
]/B
]/B
`BB
`BB
_;B
_;B
aHB
`BB
`BB
cTB
dZB
dZB
ffB
ffB
gmB
hsB
hsB
gmB
iyB
jB
jB
k�B
l�B
n�B
n�B
p�B
r�B
r�B
s�B
t�B
t�B
u�B
w�B
x�B
x�B
y�B
z�B
z�B
{�B
|�B
}�B
}�B
|�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�%B
�+B
�7B
�DB
�=B
�JB
�PB
�PB
�VB
�\B
�oB
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
��B
�B
�B
�B
�B
�!B
�3B
�9B
�FB
�FB
�FB
�RB
�XB
�XB
�XB
�^B
�^B
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�wB
�wB
�wB
�}B
�}B
�}B
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
��B
�}B
�}B
�}B
�}B
�}BJBDBJBJBJBJBJBJBJBDBDBJBJBJBJBDBDBDBDBDB
=B
=BDBDBDBJB
=BDBDBDBDBDBDBDBDBJBJBDBDBDBDBJBJBJBDBJBDBDBJBDBDBDBJB
=B	7B	7B
=BDBDB
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809210701042021061413523720210614135237202106141746422021061417464220210614174642201809210701042021061413523720210614135237202106141746422021061417464220210614174642PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018092107010420180921070104  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018092107010420180921070104QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018092107010420180921070104QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015320210722160153IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                