CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-23T07:00:49Z creation      
references        (http://www.argodatamgt.org/Documentation   comment           user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7d   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8D   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9    WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9`   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~       axis      T           9h   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9x   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�E�vQ�   
_FillValue        A.�~            9|   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Q   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  b   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  fD   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  w<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �4   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �l   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  Ü   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   |   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    8   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        X   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        `   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    p   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20181023070049  20210617131506  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               )   )DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @؈�ԓ4s@؈�ԓ4s11  @؈����0@؈����0@6��3rT@6��3rT�c�{��0�c�{��011  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  @ff@@  @�33@�ff@�  @�  A��AffA$��AC33Aa��A�  A���A���A�  A�33A���A�ffA�B ffBffB��B  B��B(  B0ffB8ffB@��BH  BP��BX��B`  Bh  Bp  Bw��B�33B�ffB�33B�33B�33B�ffB�33B�  B�  B�33B�33B�  B���B�  B�  B�  B�ffB�ffB�  B�  B���B�33Bؙ�B�ffB���B�33B�  B뙚B�  B�ffB�33B���C   C33C33C�C�fC
  C�C�C��C  C  C��C  C33C�C�fC 33C"�C#�fC&33C(�C)��C,  C.L�C033C2  C433C6  C7��C:  C<L�C>�C?�fCB�CD33CF�CG�fCJ  CL33CN  CO��CR�CTL�CV33CW�fCZ�C\L�C^33C_�fCb�Cd  Ce��Ch  Cj33Cl  Cm��Cp  Cr33Ct�Cu�fCx  Cz33C|  C}��C�fC��C��C��3C��fC��fC��3C�  C��C��C�  C�ٚC��C��C��3C�  C��C�33C��C��3C��C��C��C�&fC�&fC��C��fC�ٚC��fC��3C�  C�  C��C�&fC��C��fC��3C��C��C�&fC��C��3C�  C��C�&fC�  C��fC��3C�  C��C��C�  C��fC��3C��C�&fC��C��3C��C��C��C��3C��C�&fC�  C��fC��3C��C��C��C��fC��C�&fC��C��3C��C�&fC��C�  C�  C��C��C��3C��3C��C��C�  C��3C�  C��C��C��3C��3C��C��C�  C��fC�  C��3C�ٚC��3C�  C��C�&fC��C�  C��C�  C��fC��3C�  C��C�&fC�  C��fC��fC��3C�  C��C�&fC��C��3C��fC��C�33C��3C�  C��C���D s3D  DS3D��D	33D�3D� DY�D3D�3D` DfD��D!FfD#�fD&��D)  D+��D.Y�D0�3D3�fD6�D8� D;S3D=� D@ffDB�3DE��DH�DJ�fDML�DOٚDRY�DT� DWffDY��D\l�D_fDay�Dc��Dfs3Dh� DkS3Dm� Dp33Dr��Du,�Dw�3DzfD|  D~s3D�p D���D���D�3D�FfD�vfD��fD��fD�	�D�<�D�p D�� D�� D���D�)�D�Y�D��3D���D��fD�  D�)�D�\�D��fD��3D��3D� D�<�D�c3D���D���D��3D��D�9�D�i�D��3D��3D��D�3D�9�D�c3D��3D�� D��3D�  D�S3D�� D��fD��fD��D�C3D�vfD�� D��D�  D�\�D�� D���D� D�I�Dƀ DǶfD���D�&fD�c3D̖fD�ɚD��3D�fD�C3D�s3DӠ D�� D���D�&fD�C3D�i�Dڌ�DۦfD�� D���D��3D�fD�#3D�<�D�S3D�i�D� D� D��D� D�� D��3D��fD���D�	�D�3D��D�  D�#3D�,�D�,�D�9�D�@ D�C3D�L�D�S3D�\�D�L�D�Y�D�c3D�p D�� E C3E � EP E� Ed�E�fEt�E  E��EfE�3E��EњE��E
�3E� E�fE.fE1�E��E��EFfE@ E�fE��EfE E��E�3EɚE�EP E ��E"&fE#VfE$��E%��E&� E($�E)c3E*�fE+��E-4�E.t�E/�fE0�fE2A�E3� E4�3E6( E7fE8X E9��E;�E<K3E?Y�EBS3EE� EH�fEK�3EO3ER3EUH EXffE[~fE^��Ea�fEd��Eh�Ej��En�Eq\�EtffEw� Ez��E}�fE�p E��E���E��3E�9�E�|�E���E�#3E�e�E�� E�	�E�l E���E��3E�NfE��3E��E�<�E��fE��fE�, E�� E��3E�3E�m�E���E��fE�M�E���E���E�G3E���E�� E�=�E���E��3E�0 E�|�E���E�fE�d E���>���>���>���>���>���>���>���>���>���>���>���>���>���>���?   ?��?��?333?L��?fff?�  ?���?�33?�  ?�ff?�33@��@��@&ff@333@@  @Y��@l��@y��@���@�  @���@�ff@�  @���@�33@�33@�  @�  @���A��A33A��A��A   A(  A0  A6ffA@  AFffAL��AT��A^ffAd��Al��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444444411411111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ?fff?�  @&ff@`  @�33@�ff@�  @�  A	��AffA,��AK33Ai��A�  A���A���A�  A�33A���A�ffA���BffB
ffB��B  B!��B*  B2ffB:ffBB��BJ  BR��BZ��Bb  Bj  Br  By��B�33B�ffB�33B�33B�33B�ffB�33B�  B�  B�33B�33B�  B���B�  B�  B�  B�ffB�ffB�  B�  B���B�33Bٙ�B�ffB���B�33B�  B왚B�  B�ffB�33B���C � C�3C�3C��CffC
� C��C��CL�C� C� CL�C� C�3C��CffC �3C"��C$ffC&�3C(��C*L�C,� C.��C0�3C2� C4�3C6� C8L�C:� C<��C>��C@ffCB��CD�3CF��CHffCJ� CL�3CN� CPL�CR��CT��CV�3CXffCZ��C\��C^�3C`ffCb��Cd� CfL�Ch� Cj�3Cl� CnL�Cp� Cr�3Ct��CvffCx� Cz�3C|� C~L�C�33C�L�C�Y�C�33C�&fC�&fC�33C�@ C�L�C�Y�C�@ C��C�L�C�L�C�33C�@ C�L�C�s3C�L�C�33C�L�C�Y�C�Y�C�ffC�ffC�L�C�&fC��C�&fC�33C�@ C�@ C�Y�C�ffC�L�C�&fC�33C�L�C�Y�C�ffC�L�C�33C�@ C�L�C�ffC�@ C�&fC�33C�@ C�L�C�Y�C�@ C�&fC�33C�L�C�ffC�L�C�33C�L�C�Y�C�L�C�33C�L�C�ffC�@ C�&fC�33C�L�C�Y�C�L�C�&fC�L�C�ffC�L�C�33C�L�C�ffC�L�C�@ C�@ C�Y�C�L�C�33C�33C�L�C�Y�C�@ C�33C�@ C�Y�C�L�C�33C�33C�L�C�Y�C�@ C�&fC�@ C�33C��C�33C�@ C�Y�C�ffC�Y�C�@ C�Y�C�@ C�&fC�33C�@ C�L�C�ffC�@ C�&fC�&fC�33C�@ C�L�C�ffC�L�C�33C�&fC�Y�C�s3C�33C�@ C�L�D fD �3D  Ds3D��D	S3D3D� Dy�D33D�3D� D&fD��D!ffD$fD&��D)@ D+��D.y�D13D3�fD69�D8� D;s3D>  D@�fDC3DE��DH9�DJ�fDMl�DO��DRy�DU  DW�fDZ�D\��D_&fDa��Dd�Df�3Di  Dks3Dm� DpS3DrٚDuL�Dw�3Dz&fD|  D~�3D�� D���D���D�#3D�VfD��fD��fD��fD��D�L�D�� D�� D�� D��D�9�D�i�D��3D���D��fD� D�9�D�l�D��fD��3D��3D�  D�L�D�s3D���D���D��3D��D�I�D�y�D��3D��3D���D�#3D�I�D�s3D��3D�� D�3D�0 D�c3D�� D��fD��fD�,�D�S3D��fD�� D���D�0 D�l�D�� D���D�  D�Y�DƐ D��fD���D�6fD�s3D̦fD�ٚD�3D�&fD�S3D҃3DӰ D�� D��D�6fD�S3D�y�Dڜ�D۶fD�� D���D�3D�fD�33D�L�D�c3D�y�D� D� D��D�� D�� D��3D��fD�	�D��D�#3D�,�D�0 D�33D�<�D�<�D�I�D�P D�S3D�\�D�c3D�l�D�\�D�i�D�s3D�� D�� E K3E � EX E� El�E�fE|�E E��EfE�3E��EٚE��E
�3E� E�fE6fE9�EɚE��ENfEH E�fE��E&fE  E��E�3EњE�EX E ��E".fE#^fE$��E%��E&� E(,�E)k3E*�fE+��E-<�E.|�E/�fE1fE2I�E3� E4�3E60 E7fE8` E9��E;	�E<S3E?a�EB[3EE� EH�fEK�3EO#3ER3EUP EXnfE[�fE^��Ea�fEd��Eh�Ej��En	�Eqd�EtnfEw� Ez��E}�fE�t E��E���E��3E�=�E���E���E�'3E�i�E�� E��E�p E���E��3E�RfE��3E��E�@�E��fE��fE�0 E�� E��3E�3E�q�E�ŚE��fE�Q�E���E���E�K3E���E�� E�A�E���E��3E�4 E���E���E�fE�h E���?L��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?fff?�  G�O�?���?���?�ff?�33?�  ?ٙ�?�33@   @33@��@,��@9��@Fff@S33@`  @y��@�ff@���@���@�  @���@�ff@�  @ə�@�33@�33@�  A   A��A��A33A��A!��A(  A0  A8  A>ffAH  ANffAT��A\��AfffAl��At��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444444444411411111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     @ �@ �@ V@ {@ �@ #�@ (�@ /�@ 7L@ <�@ FQ@ SI@ `B@ m:@ {�@ ��@ �0@ �(@ �-@ �2@ �*@ �#@ ��@ � @j@b@�@-@:�@I@UU@dZ@r@~K@��@��@��@��@��@��@ލ@�4@��@�@{@""@0x@>@K@X@ff@t@��@��@�a@��@�R@�J@�O@�T@�L@��@
�@�@$.@33@B8@O0@[z@i�@x�@�|@�u@��@�@�k@�@խ@�@�Y@��@�@�@)�@5�@E�@R�@^�@n�@{�@�+@�0@��@��@�&@�*@�t@�@��@v@�@
@-@;d@H]@T�@b�@r@~K@��@�H@�M@�F@��@��@��@��@�~@�@{@ �@/�@>�@K@Wb@ff@uk@�d@��@�@�@�R@Ĝ@��@��@�L@��@�@�@$�@33@A�@O�@\)@g�@x&@��@�@�m@��@�w@�@�[@�`@�@^@�@[@)�@5?@B8@P�@^�@m:@z�@��@�<@��@�!@�w@�|@��@�(@�q@	�@	@	g@	.l@	:@	FQ@	T�@	b�@	qS@	�@	��@	�<@	��@	��@	Ĝ@	��@	�/@	�4@	��@
�@
�@
"�@
1�@
=q@
I�@
X@
g@
uk@
�d@
��@
��@
��@
�@
�J@
�O@
�T@
�@
��@
=@B@&;@2�@@,@O0@]�@i�@v�@�@�$@�@�f@�@�@�h@�@��@  @�@B@(G@6�@E�@S�@`�@m:@|?@��@��@�(@�~@��@��@�t@�@�e@�@@g@.l@:�@F�@S�@dZ@s_@}�@��@�H@��@��@@�o@Z@�y@�@6�@�@�o@@\)@��@��@3�@{�@��@
=@SI@��@��@&;@l�@��@�9@@�@��@�o@o@X@��@�@+�@o�@��@��@>�@�@�@�@O�@��@�
@�@\)@�@�@&�@hs@�Y@�H@$.@ff@��@��@-@n�@�r@�L@1'@r�@�9@��@6�@ww@��@��@8�@x&@��@� @ 6�@ v@ ��@ � @!7L@!x&@!�R@!�~@"7L@"v�@"��@"�q@#5�@#v@#��@#�q@$7L@$v@$��@$�e@%3�@%t�@%��@%�q@&6�@&x&@&�R@&��@';d@'}�@'�j@'��@(@�@(��@(��@)	�@)N�@)�@)Ӡ@*�@*X�@*��@*�/@+ @+c�@+�4@+�@,&;@,dZ@,��@,�`@-%�@-ff@-��@-�@."�@.a�@.��@.��@/�@/T�@/�@/��@0�@0C�@0~�@0�^@0��@1/�@1i!@1��@1��@2�@2SI@2��@2�@3 �@39X@3p�@3��@3��@46@4P�@4��@4��@4�~@50x@5i!@5�U@5խ@6V@6G�@6��@6��@6�Y@7,`@7ff@7�z@7�/@8�@8Q�@8��@8ȴ@9�@9|�@9�@:hs@;B@;��@;�Q@<��@=�@=�>@>1�@>�[@?@�@?��@@I�@@�l@AQ�@A�4@B�@B�`@Cr�@C�,@D~�@E/@E��@F1�@F��@G:@G��@HE�@Hψ@IZ�@I�@JoF@J�}@K�p@LV@L��@M&�@M�R@N6@N�A@O:�@O�@PV�@Q��@R�y@TX�@U��@V�@X]�@Y��@[  @\T�@]��@^�q@`C�@a�i@c@d<@e��@f�9@hF�@i��@j��@l5�@m��@n�M@pB8@p}�@p��@qV@qH]@q�T@q��@r(�@r`�@r��@r��@s$�@sv@s��@s� @tA�@t��@t�1@u�@uX�@u�C@u�[@v @vg�@v�<@v�<@w&�@wn�@w�9@w��@x@�@x�}@x�|@yb@yUU@y��@y׹@z�@z\)@z�a@ G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�@ �@ jG�O�@ @ �@ v@ %@ �@ 1@ 	�@ 
=@ J@ �@ @ b@ �@ @ {@ 6@ B@ �@ [@ �@  �@ #�@ %�@ '�@ )�@ -@ /�@ 33@ 5?@ 8�@ ;d@ >@ A�@ DD@ G�@ K@ M�@ Q�@ T�@ Wb@ Z�@ ^�@ a�@ e	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AּjAּjA��A��;A��;A��;A��/A��;A��/A��/A��/A��HA��HA��;A��;A��HA��TA��TA��TA��A֬A�  Aգ�A�l�A�A��A�AԴ9Aԡ�Aԛ�A�  A�^5Aˉ7Aǟ�Aŏ\A��/A��A���A�ffA���A���A���A�r�A�VA�JA�O�A��
A�\)A��A���A�7LA�;dA�|�A�33A���A��9A�VA��A�K�A���A��yA��\A�A��A��A�-A�n�A��A�ZA��wA���A�n�A�hsA�{A�ffA���A�ȴA�XA���A��A�oA��A�A�x�A�A�ffA���A�"�A�jA�-A��
A�{A��PA�I�A��hA��`A�bNA�bNA��A��;A�33A�C�A���A�XA�ƨA��;A���A� �A��A���A�K�A��9A���A��PA���A�S�A�I�A��A~�`A~Q�A|��Az�!Ay&�Axr�Aw��AvI�Au�Au�At1'As�At1As�#As��AqAm��AlI�AkO�AjVAhZAeG�Ad�Aa;dA^��A\�AZ�9AY%AVȴAUp�AS�AR�AQ\)APbNANĜAN$�AL�/AKx�AI�AHz�AG�-AFffAE�AE33AD1AB�A@��A?p�A=�mA<�A:��A8=qA6�A5��A5�A4�A4A3
=A1��A1S�A0�HA/�FA.bNA-hsA+��A+`BA+G�A+G�A+?}A)�hA'�#A%/A#�wA#&�A#A"��A"�HA"I�A ��A JA��A?}AJAdZA��A�PA��A~�A�PAĜAK�A��A$�A33A�TA7LA�RA(�A�TA�-A
bA	��A	"�An�A�A��A1A~�A��A�AK�A�AA��A�DA��A z�A =q@���@��j@��m@��!@��@��u@�o@�hs@�A�@�@���@��@�  @���@�9@��m@ꗍ@��@�
=@��/@�@��@���@��@���@�ƨ@�5?@��@�(�@��@֟�@�&�@�;d@�1'@ŉ7@�l�@���@��7@���@���@���@���@�p�@��\@��@��;@�-@�%@�dZ@�V@��@�K�@��h@�1'@�v�@���@�z�@�S�@���@�{@��T@�`B@�hs@�&�@���@�  @��@��@���@��h@�/@�  @���@�$�@�O�@�Q�@~ȴ@{��@z��@x�`@w\)@vE�@s�m@qhs@o|�@o\)@lj@l1@j^5@hQ�@e/@b^5@_��@^ff@\�@\1@[S�@Y��@X�9@X�9@W
=@V@T(�@Q�@O;d@M`B@K��@Kt�@J~�@H��@H  @F��@E��@D1@C@B=q@A�7@?�P@>�R@=@<�j@:~�@:=q@9��@9�@7;d@7+@6�@5�h@4�@3"�@2M�@2J@1&�@0 �@.��@-�@,�D@+33@*n�@)�@'��@&ff@%�-@$�@$�@#t�@"�H@"M�@"-@!�@��@�R@�@�@j@��@dZ@�!@�@�`@  @
=@�+@�T@��@(�@t�@@=q@��@G�@Q�@�P@��@5?@�-@��@�@j@�@
��@
~�@	��@	��@	7L@��@bN@�@+@��@ȴ@V@$�@��@�@z�@�@�!@~�@ A�?�V?��?���?�Q�?�l�?�9X?�!?�;d?��?�I�?�"�?���?�+?�j?�F?��?�A�?�\)?�{?�1?���?׮?�E�?�?}?�9X?�t�?���?�A�?Ͼw?͑h?�p�?�/?̬?��m?�dZ?��#?��?ȓu?�r�?�b?ǍP?�+?�+?�E�?��?���?�&�?�Ĝ?�  ?�|�?�5??�p�?��?��m?���?�C�?�"�?���?��?��?��#?���?��?�~�?���?�"�?���?�ƨ?��m?��m?�1?�1?�(�?�I�?�j?�j?��D?��D?���?���?�/?�O�?�p�?�p�?��h?���?��?��?�5??�V?�v�?���?��R?���?���?��?�;d?�\)?�|�?���?��w?��w?�  ?��;?��;?� �A���A���A�AּjA���A�AָRAֶFAֶFAִ9Aֺ^Aִ9Aִ9AָRAּjAֶFAֺ^A־wA�ƨA���A־wAָRAָRAּjA���A��#A��;A��;A��;A��;A��;A��;A��;A��/A��;A��/A��;A��;A��;A��/A��/A��;A��/A��;A��/A��/A��/A��/A��#A��/A��;A��;A��;A��HA��HA��TA��HA��HA��HA��;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     AּjAּjA��A��;A��;A��;A��/A��;A��/A��/A��/A��HA��HA��;A��;A��HA��TA��TA��TA��A֬A�  Aգ�A�l�A�A��A�AԴ9Aԡ�Aԛ�A�  A�^5Aˉ7Aǟ�Aŏ\A��/A��A���A�ffA���A���A���A�r�A�VA�JA�O�A��
A�\)A��A���A�7LA�;dA�|�A�33A���A��9A�VA��A�K�A���A��yA��\A�A��A��A�-A�n�A��A�ZA��wA���A�n�A�hsA�{A�ffA���A�ȴA�XA���A��A�oA��A�A�x�A�A�ffA���A�"�A�jA�-A��
A�{A��PA�I�A��hA��`A�bNA�bNA��A��;A�33A�C�A���A�XA�ƨA��;A���A� �A��A���A�K�A��9A���A��PA���A�S�A�I�A��A~�`A~Q�A|��Az�!Ay&�Axr�Aw��AvI�Au�Au�At1'As�At1As�#As��AqAm��AlI�AkO�AjVAhZAeG�Ad�Aa;dA^��A\�AZ�9AY%AVȴAUp�AS�AR�AQ\)APbNANĜAN$�AL�/AKx�AI�AHz�AG�-AFffAE�AE33AD1AB�A@��A?p�A=�mA<�A:��A8=qA6�A5��A5�A4�A4A3
=A1��A1S�A0�HA/�FA.bNA-hsA+��A+`BA+G�A+G�A+?}A)�hA'�#A%/A#�wA#&�A#A"��A"�HA"I�A ��A JA��A?}AJAdZA��A�PA��A~�A�PAĜAK�A��A$�A33A�TA7LA�RA(�A�TA�-A
bA	��A	"�An�A�A��A1A~�A��A�AK�A�AA��A�DA��A z�A =q@���@��j@��m@��!@��@��u@�o@�hs@�A�@�@���@��@�  @���@�9@��m@ꗍ@��@�
=@��/@�@��@���@��@���@�ƨ@�5?@��@�(�@��@֟�@�&�@�;d@�1'@ŉ7@�l�@���@��7@���@���@���@���@�p�@��\@��@��;@�-@�%@�dZ@�V@��@�K�@��h@�1'@�v�@���@�z�@�S�@���@�{@��T@�`B@�hs@�&�@���@�  @��@��@���@��h@�/@�  @���@�$�@�O�@�Q�@~ȴ@{��@z��@x�`@w\)@vE�@s�m@qhs@o|�@o\)@lj@l1@j^5@hQ�@e/@b^5@_��@^ff@\�@\1@[S�@Y��@X�9@X�9@W
=@V@T(�@Q�@O;d@M`B@K��@Kt�@J~�@H��@H  @F��@E��@D1@C@B=q@A�7@?�P@>�R@=@<�j@:~�@:=q@9��@9�@7;d@7+@6�@5�h@4�@3"�@2M�@2J@1&�@0 �@.��@-�@,�D@+33@*n�@)�@'��@&ff@%�-@$�@$�@#t�@"�H@"M�@"-@!�@��@�R@�@�@j@��@dZ@�!@�@�`@  @
=@�+@�T@��@(�@t�@@=q@��@G�@Q�@�P@��@5?@�-@��@�@j@�@
��@
~�@	��@	��@	7L@��@bN@�@+@��@ȴ@V@$�@��@�@z�@�@�!@~�@ A�?�V?��?���?�Q�?�l�?�9X?�!?�;d?��?�I�?�"�?���?�+?�j?�F?��?�A�?�\)?�{?�1?���?׮?�E�?�?}?�9X?�t�?���?�A�?Ͼw?͑h?�p�?�/?̬?��m?�dZ?��#?��?ȓu?�r�?�b?ǍP?�+?�+?�E�?��?���?�&�?�Ĝ?�  ?�|�?�5??�p�?��?��m?���?�C�?�"�?���?��?��?��#?���?��?�~�?���?�"�?���?�ƨ?��m?��m?�1?�1?�(�?�I�?�j?�j?��D?��D?���?���?�/?�O�?�p�?�p�?��h?���?��?��?�5??�V?�v�?���?��R?���?���?��?�;d?�\)?�|�?���?��w?��w?�  ?��;?��;?� �A���A���A�AּjA���A�AָRAֶFAֶFAִ9Aֺ^Aִ9Aִ9AָRAּjAֶFAֺ^A־wA�ƨA���A־wAָRAָRAּjA���A��#A��;A��;A��;A��;A��;A��;A��;A��/A��;A��/A��;A��;A��;A��/A��/A��;A��/A��;A��/A��/A��/A��/A��#A��/A��;A��;A��;A��HA��HA��TA��HA��HA��HA��;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B �B!�B �B�B �B�B �B�B�B�B�B�B�B �B �B�B�B �B�B-B��B�B9XBI�BM�BM�BO�BP�BO�BP�BI�B8RBVB�B7LB"�B�B{BVBuB�B+B0!B/B:^BE�BF�BH�BG�BJ�BO�B]/B^5B^5B^5BaHBffBe`BjBm�Bm�Bn�Bu�Bu�B{�B� B~�B}�Bz�Bv�Bs�BhsB]/BN�BT�BN�B;dB;dB49B33B.B'�B&�B&�B$�B"�B#�B �B�B%B��B�B�TB�;B��B|�BdZBO�B9XB,B!�BuB1BBB
��B
�NB
�5B
�B
�B
�wB
��B
��B
��B
�B
n�B
]/B
C�B
B�B
E�B
7LB
'�B
�B
�B
{B
  B	�B	�B	�sB	�sB	�B	��B	��B	�B	�)B	��B	��B	ǮB	�3B	��B	��B	�B	t�B	cTB	YB	P�B	C�B	33B	%�B	�B	�B	JB	B	B��B�B�mB�TB�/B�B��B��B��BB�jB�FB�B��B��B��B�hB�DB�1B�B�B}�B|�B|�By�Bv�Bs�Bq�Bq�Bs�Bs�Bs�Bs�Bo�BiyBdZBcTBaHB`BB_;B^5B]/B[#BYBXBR�BVBT�BS�BP�BP�BL�BL�BK�BJ�BH�BL�BI�BI�BH�BF�BE�BD�BA�B?}B>wB=qB<jB:^B:^B9XB8RB9XB7LB7LB8RB9XB:^B;dB8RB:^B8RB7LB8RB7LB6FB8RB7LB7LB7LB8RB7LB8RB7LB7LB8RB9XB8RB8RB9XB8RB=qB=qB=qB>wBB�BB�BF�BC�BE�BH�Bp�B�B�bB�uB��B��B��B�BBÖB��BȴB�BB�;B�B�ZB�B��B	  B	DB	hB	�B	&�B	7LB	=qB	L�B	W
B	_;B	iyB	m�B	v�B	� B	�DB	�oB	��B	��B	�B	�B	�-B	�qB	ÖB	ƨB	��B	��B	�B	�/B	�ZB	�yB	�B	�B	�B	��B	��B	��B	��B
  B
B
B
1B
	7B
DB
DB
JB
VB
uB
�B
�B
�B
�B
�B
$�B
&�B
(�B
)�B
/B
/B
2-B
33B
2-B
49B
5?B
6FB
8RB
8RB
:^B
<jB
=qB
<jB
=qB
=qB
>wB
?}B
B�B
B�B
B�B
B�B
E�B
D�B
E�B
G�B
G�B
H�B
I�B
I�B
J�B
M�B
M�B
N�B
O�B
P�B
Q�B
T�B
T�B
W
B
XB
XB
ZB
[#B
[#B
\)B
\)B
]/B
_;B
_;B
aHB
aHB
cTB
bNB
cTB
dZB
dZB
e`B
ffB
gmB
hsB
hsB
jB
jB
k�B
l�B
m�B
m�B
l�B
n�B
n�B
o�B
p�B
p�B
p�B
r�B
q�B
r�B
s�B
t�B
t�B
t�B
u�B
v�B
v�B
w�B
y�B
x�B
x�B
x�B
x�B
z�B
{�B
{�B
|�B
}�B
}�B
�B
�B
�B
�B
�%B
�%B
�1B
�7B
�DB
�JB
�JB
�PB
�\B
�bB
�hB
�oB
�uB
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
�B
�B
�B
�!B
�-B
�-B
�3B
�9B
�3B
�?B
�FB
�FB
�LB
�RB
�RB
�XB
�XB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�XB
�^B
�^B
�^B
�XB
�XB
�^B
�^B!�B!�B�B"�B!�B�B �B �B�B"�B�B#�B �B �B �B!�B"�B �B �B �B!�B �B�B#�B#�B �B�B�B�B�B�B�B�B�B �B �B�B�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     B �B!�B �B�B �B�B �B�B�B�B�B�B�B �B �B�B�B �B�B,�B��BcB9;BI�BM�BM�BO�BP�BO�BP�BI�B8:B>B�B74B"�B�BeB@B_B~B*�B0B/B:KBE�BF�BH�BG�BJ�BO�B] B^&B^'B^'Ba;BfYBeTBjtBm�Bm�Bn�Bu�Bu�B{�B�B~�B}�Bz�Bv�Bs�BhnB]+BN�BT�BN�B;bB;bB48B32B.B'�B&�B&�B$�B"�B#�B �B�B)B��B�B�ZB�AB��B|�BdaBO�B9`B,B!�B~B;B#BB
��B
�ZB
�AB
�*B
�*B
��B
��B
��B
��B
�B
n�B
]?B
C�B
B�B
E�B
7^B
(B
�B
�B
�B
 B	��B	�B	�B	�B	�B	�B	�B	�B	�BB	�B	��B	��B	�NB	�B	��B	�5B	t�B	cqB	Y4B	QB	C�B	3RB	&B	�B	�B	jB	@B	'B��B��B�B�wB�SB�(B�B�B��BµB��B�mB�<B�B��B��B��B�nB�[B�DB�7B~ B}B}BzBv�Bs�Bq�Bq�Bs�Bs�Bs�Bs�Bo�Bi�Bd�Bc�Ba|B`vB_pB^jB]eB[YBYNBXGBS*BV<BU7BT1BQBQBMBMBLBJ�BH�BM
BI�BI�BH�BF�BE�BD�BA�B?�B>�B=�B<�B:�B:�B9�B8�B9�B7�B7�B8�B9�B:�B;�B8�B:�B8�B7�B8�B7�B6�B8�B7�B7�B7�B8�B7�B8�B7�B7�B8�B9�B8�B8�B9�B8�B=�B=�B=�B>�BB�BB�BF�BC�BE�BIBqB�{B��B��B��B�^B�ZB�|B�B�B�{B�3B��B��BڥB��B�B�fB	 �B	�B	B	UB	'�B	7�B	>B	MvB	W�B	_�B	j+B	nFB	w�B	��B	�B	�1B	�wB	��B	��B	��B	��B	�DB	�lB	ǁB	̣B	��B	��B	�B	�AB	�cB	�B	��B	�B	��B	��B	��B	��B
B
B
&B
	;B

DB
SB
VB
_B
nB
�B
�B
�B
�B
�B
 �B
&B
(B
*'B
+0B
0QB
0TB
3iB
4rB
3nB
5}B
6�B
7�B
9�B
9�B
;�B
=�B
>�B
=�B
>�B
>�B
?�B
@�B
C�B
C�B
C�B
D B
GB
FB
GB
I+B
I.B
J7B
K@B
KCB
LMB
ObB
OdB
PmB
QvB
RB
S�B
V�B
V�B
X�B
Y�B
Y�B
[�B
\�B
\�B
]�B
]�B
^�B
`�B
`�B
cB
cB
eB
dB
e#B
f,B
f.B
g7B
h@B
iIB
jRB
jTB
lcB
lfB
mnB
nwB
oB
o�B
n~B
p�B
p�B
q�B
r�B
r�B
r�B
t�B
s�B
t�B
u�B
v�B
v�B
v�B
w�B
x�B
x�B
y�B
{�B
z�B
z�B
z�B
z�B
}B
~B
~B
#B
�+B
�.B
�EB
�KB
�VB
�kB
�|B
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
�B
�B
�!B
�+B
�2B
�DB
�PB
�^B
�jB
�vB
�|B
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
�B
�B
� B
�'B
�-B
�4B
�IB
�cB
��B
��B
��B
��B
��B
��B
��B
�B
�$B
�-B
�HB
�`B
�nB
��B
��B
��B
��B
��B
��B
��B
��B
�
B
�B
�B
�B
�B
�B
�B
� B
�"B
�,B
�.B
�1B
�/B
�7B
�4B
�8B
�;B
�=B
�AB
�DB
�GB
�PB
�MB
�PB
�SB
�VB
�YB
�\B
�eB
�cB
�fB
�iB
�lB
�uB
�xB
�{B
�xB
�{B
��B
��B!�B!�B�B"�B!�B�B �B �B�B"�B�B#�B �B �B �B!�B"�B �B �B �B!�B �B�B#�B#�B �B�B�B�B�B�B�B�B�B �B �B�B�B �B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201810230700492021061413554720210614135547202106171313162021061713131620210617131316201810230700492021061413554720210614135547202106171313162021061713131620210617131316PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018102307004920181023070049  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102307004920181023070049QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018102307004920181023070049QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713150620210617131506IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                