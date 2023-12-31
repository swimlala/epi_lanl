CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-09-17T23:13:45Z creation      
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
resolution        =���   axis      Z          ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       t    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   	�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   	�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   	�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   	�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 	�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   
H   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   
d   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    
l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        
�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        
�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       
�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    
�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180917231345  20210722160152  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�}�e��@�}�e��11  @�}�O�p@�}�O�p@6wF�{@6wF�{�c�g��*�c�g��*11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              
   
AB  AA  AA  >���?L��@   @@  @�33@�33@�  @�  A��A  A!��A@  Ad��A���A���A���A���A���A���A�  A�  B ffBffB��B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBp  BxffB�33B�33B���B�  B�33B�ffB�33B���B�33B�33B���B�  B�ffB�33B�ffB�ffB���B�ffB�33B̙�B�ffB�ffB�ffB�33B�33B�  B�  B�  B���B�ffB�ffB�33C �C  CL�CL�C�C
�C�C  C�fC33C33C�CL�C33C  CL�C L�C"33C$�C&  C(  C)�fC,  C.�C0  C2  C3�fC5�fC7�fC9��C<�C>33C@�CB  CC�fCFL�CH33CJ�CL  CM�fCP33CR�CS�fCVL�CX33CZ  C\  C]�fC`33Cb33Cd  CfL�Ch�Cj  Cl�Cn33Cp�Cq�fCt  Cv33CxL�Cz33C|  C~L�C��C�  C��C��C�  C��C��C��3C��C�&fC��C��3C�  C��C��C�&fC��C��fC�  C��C�  C��fC�  C��C��C��3C��C��C��fC��C�  C��fC�  C��3C��fC��3C�  C��C��C��3C��3C��C��C�  C�ٚC�  C��C�&fC��C��fC��3C��3C�  C��C�&fC�&fC�&fC�  C��fC��3C��C��C��C��fC�  C��C�&fC�  C��fC�  C��C�&fC��C��fC�  C��C��3C��fC��3C�  C��C�&fC�  C��fC��3C��C��C��C�  C��C��C��3C��3C��fC��C�  C��3C��C�  C��3C��C�  C��fC��C��3C��fC��3C��C��C��3C�  C�&fC��C��3C�  C��C��C��fC��3C�  C��C��C��fC��3C��3C��fC�  C�  D   D �fD�D�3D��DL�D��D	FfD��D&fD��D,�D� D@ DٚDy�D &fD"��D%�3D(l�D+33D-�fD0�fD333D5ٚD8s3D;fD=�fD@�DB��DEfDG� DI�3DL� DO3DQ�fDS��DVy�DX��D[s3D^�D`��Dc,�De�fDh` Dj��Dm�fDp3Dr�3Du�Dw�fDz,�D|@ D~�fD�� D���D�fD�I�D�vfD�� D�� D�fD�C3D�p D�� D��fD��3D�&fD�VfD��fD���D��D��D�P D��fD��3D��3D�fD�FfD�|�D��3D���D�)�D�ffD��3D�� D�  D�\�D���D�ɚD�fD�C3D��3D��3D�3D�FfD�|�D��fD��D�  D�S3D���D��3D�� D�  D�I�D�p D��fD��3D���D��fD��D�#3D�9�D�Y�D�vfDʌ�Dˣ3D̼�D�ٚD���D�	�D�  D�33D�C3D�S3D�ffD�|�DזfDج�D��3D���D��fD�3D�&fD�<�D�S3D�s3D�fD㹚D��3D��D�)�D�L�D�vfD� D���D���D�33D�\�D���D��3D��D�  D�L�D�y�D��fD���D�� D�fD�33D�` D��3D��fE ^fE �EnfE��E� E	�E�fE�E�3E�E��E3E��E�fE	fE
fE�fE�fE�3Ex Et�E�E�fE^fEd�E�E�fE` Ea�E� E��Eh EffE ��E!� E#A�E$�3E%��E'#3E(3E)vfE*� E,)�E-fE.nfE/�3E0ɚE2+3E3�fE53E5�3E7x E8y�E9�fE;�E<��E?��EB� EE�3EH��EKњEO�ERfEUVfEXx E[� E^�fEa�3Ed��Eg�fEj�fEn33Eqt�EtP Ew��Ez�3E}�fE�q�E� E��3E��E���E�73E��fE�W3E���E�p E��f?��?   ?��?   >���>���>���?��?   ?   ?   ?��?333?333?L��?fff?�  ?���?���?�  ?���@   @ff@��@333@@  @S33@s33@�33@���@�ff@�33@���@�ff@�ff@�33@�33@���@���AffA33A33A33A!��A)��A1��A9��A@  AH  AP  AX  A`  Ah  Ap  Ax  A�  A�33A�  A�  A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444144411411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ?fff?�ff@   @`  @�33@�33@�  @�  A	��A  A)��AH  Al��A���A���A���A���A���A���A�  A�  BffB
ffB��B  B"  B*  B2  B:  BB  BJ  BR  BZ  BbffBjffBr  BzffB�33B�33B���B�  B�33B�ffB�33B���B�33B�33B���B�  B�ffB�33B�ffB�ffB���B�ffB�33B͙�B�ffB�ffB�ffB�33B�33B�  B�  B�  B���B�ffB�ffB�33C ��C� C��C��C��C
��C��C� CffC�3C�3C��C��C�3C� C��C ��C"�3C$��C&� C(� C*ffC,� C.��C0� C2� C4ffC6ffC8ffC:L�C<��C>�3C@��CB� CDffCF��CH�3CJ��CL� CNffCP�3CR��CTffCV��CX�3CZ� C\� C^ffC`�3Cb�3Cd� Cf��Ch��Cj� Cl��Cn�3Cp��CrffCt� Cv�3Cx��Cz�3C|� C~��C�L�C�@ C�Y�C�L�C�@ C�Y�C�L�C�33C�L�C�ffC�L�C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�@ C�L�C�@ C�&fC�@ C�Y�C�L�C�33C�L�C�L�C�&fC�L�C�@ C�&fC�@ C�33C�&fC�33C�@ C�Y�C�L�C�33C�33C�L�C�Y�C�@ C��C�@ C�L�C�ffC�L�C�&fC�33C�33C�@ C�Y�C�ffC�ffC�ffC�@ C�&fC�33C�L�C�Y�C�L�C�&fC�@ C�Y�C�ffC�@ C�&fC�@ C�L�C�ffC�L�C�&fC�@ C�Y�C�33C�&fC�33C�@ C�Y�C�ffC�@ C�&fC�33C�L�C�Y�C�L�C�@ C�Y�C�L�C�33C�33C�&fC�L�C�@ C�33C�L�C�@ C�33C�L�C�@ C�&fC�L�C�33C�&fC�33C�Y�C�L�C�33C�@ C�ffC�L�C�33C�@ C�Y�C�L�C�&fC�33C�@ C�Y�C�L�C�&fC�33C�33C�&fC�@ C�@ D   D �fD,�D�3D�Dl�D�D	ffDٚDFfD��DL�D� D` D��D��D FfD#�D%�3D(��D+S3D.fD0�fD3S3D5��D8�3D;&fD=�fD@,�DB��DE&fDG� DJ3DL� DO33DQ�fDT�DV��DY�D[�3D^,�D`��DcL�De�fDh� Dk�Dm�fDp33Dr�3Du9�Dw�fDzL�D|` D~�fD�� D���D�&fD�Y�D��fD�� D�� D�&fD�S3D�� D�� D��fD�3D�6fD�ffD��fD���D���D�)�D�` D��fD��3D��3D�&fD�VfD���D��3D���D�9�D�vfD��3D�� D�0 D�l�D���D�ٚD�fD�S3D��3D��3D�3D�VfD���D��fD���D�0 D�c3D���D��3D�  D�0 D�Y�D�� D��fD��3D���D�fD��D�33D�I�D�i�DɆfDʜ�D˳3D���D��D���D��D�0 D�C3D�S3D�c3D�vfD֌�DצfDؼ�D��3D���D�fD�#3D�6fD�L�D�c3D�3D�fD�ɚD��3D��D�9�D�\�D�fD� D���D��D�C3D�l�D��D��3D���D�0 D�\�D���D��fD���D�  D�fD�C3D�p D��3D��fE ffE �EvfE��E� E�E�fE�E�3E!�E��E#3E��E�fE	fE
fE�fEfE3E� E|�E�E�fEffEl�E�E�fEh Ei�E� E��Ep EnfE ��E!� E#I�E$�3E%��E'+3E(3E)~fE*� E,1�E-fE.vfE/�3E0њE233E3�fE53E63E7� E8��E:fE;	�E<��E?��EB� EE�3EH��EKٚEO�ERfEU^fEX� E[� E^�fEa�3Ed��Eg�fEj�fEn;3Eq|�EtX Ew��Ez�3E}�fE�u�E� E��3E��E���E�;3E��fE�[3E���E�t E�fG�O�?�  G�O�G�O�G�O�G�O�?fffG�O�G�O�G�O�?�  ?���G�O�?���?�ff?�33?�  ?���?ٙ�@   @ff@   @&ff@9��@S33@`  @s33@���@�33@���@�ff@�33@���@�ff@�ff@�33@�33@���A��AffA33A33A#33A)��A1��A9��AA��AH  AP  AX  A`  Ah  Ap  Ax  A�  A�  A�33A�  A�  A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111414444144411411111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       @ �@ v@ �@ {@ �@ "�@ (�@ /�@ 7L@ =q@ D�@ Q�@ a�@ n�@ |?@ �7@ ��@ ��@ �-@ �&@ ��@ �#@ ��@ �@j@@�@,`@:@G�@UU@b�@p�@~�@��@��@��@��@�>@ψ@��@�4@��@�@�@"�@0x@<�@K@Z@g@uk@�@�h@�a@�Y@�^@�W@��@�@�@�E@
=@�@%�@2�@B8@O�@\�@j@ww@�+@��@�@��@�k@�c@�[@�@�@ �@�@�@(�@8�@FQ@SI@`B@m:@z�@��@�0@��@�~@�&@�@��@�m@�e@@o@g@,`@9X@I�@V�@c�@p�@}�@�P@�H@��@��@��@�7@��@��@��@1@{@$.@0x@=q@K�@Z@g@s_@��@��@�@�@�R@�@�O@�H@�L@�E@
=@B@&;@2�@A�@P�@\�@i!@ww@��@�$@�y@��@�^@�c@׹@�@��@  @@�@(G@7L@D�@P�@`B@m:@y�@��@��@�y@��@�&@�*@�#@�m@�@	@	o@	�@	*S@	:@	H]@	Wb@	c�@	oF@	}�@	�D@	��@	��@	��@	Ĝ@	�C@	��@	�(@	�~@
�@
�@
"�@
.l@
=q@
Lu@
Z�@
ff@
r�@
��@
�@
�@
�Y@
��@
��@
��@
��@
�@
��@
=@B@'�@33@?}@M�@\�@k.@x&@�@�$@�@�f@�@�@׹@�@�@ �@�@�@)�@6�@B�@R�@^�@k�@z3@��@��@�(@�~@�2@�|@��@�@� @@�@
@,`@;d@H]@S�@bN@o�@|�@��@��@�A@��@��@�C@��@�@dZ@��@�m@)�@l�@��@�,@=q@�p@�@*@`�@��@� @B�@��@�O@[@e�@��@�@7L@|?@��@@G�@��@�7@�@Yn@�U@��@#�@hs@�r@�@;d@�d@�c@b@V@��@��@$�@j@�r@�@-@p�@�9@� @8�@x�@��@��@>�@~�@�&@  @>�@~�@��@]@B8@�p@Ĝ@ v@ G�@ ��@ �@!
�@!Lu@!�P@!ψ@"�@"T�@"�<@"��@#g@#b�@#�A@#��@$+�@$oF@$��@$�q@%:�@%~�@%�>@&1@&Ji@&�P@&��@'@'R�@'��@'׹@(�@(X�@(�<@(�
@)�@)V@)�@)�*@*	�@*D�@*�W@*��@*��@+5�@+qS@+�f@+�(@,$�@,a�@,�@,׹@-�@-K�@-�|@-��@-��@.9X@.t�@.��@.��@/)�@/dZ@/��@/�#@0�@0V�@0��@0�O@1�@1P�@1��@1�*@2�@2M�@2��@2��@3b@3Q=@3�u@3�C@4{@4T�@4��@4��@5�@5Q�@5�P@5�|@6�@6K�@6��@6�J@7 �@79X@7r�@7�@7��@8!s@8Z�@8�#@8�@9 �@97�@9m�@9�/@:}�@:�l@;�C@<(G@<�#@=6�@=�z@>A�@>�f@?M�@?��@@`B@@�@Ap�@Aލ@B��@B��@C�0@D�@D�z@E
=@E��@FE�@F��@GO�@G�F@HM�@H�H@It�@I�[@Jl�@K1@Km�@L�@L��@M;d@M�4@NG�@N��@O[z@O�@PqS@Q�@S�@Tn�@U��@V�q@XYn@Y�T@[%@\\)@]�@^�@`;d@a�@b�@d<�@e��@gv@h=q@i�z@j�@lT�@m��@o  @pF�@q��@r�y@t<�@u��@v�l@xLv@y��@z��G�O�@ jG�O�G�O�G�O�G�O�@ �G�O�G�O�G�O�@ j@ G�O�@ �@ v@ %@ �@ �@ 1@ 
=@ 
�@ �@ V@ b@ @ {@ �@ �@ �@ 
@  @ "�@ $�@ &�@ *S@ -@ 0x@ 2�@ 5?@ 9X@ ;d@ >�@ B8@ D�@ H]@ K�@ O0@ Q�@ UU@ X�@ \)@ _�@ b�@ ff@ i�@ m:@ o�@ t@ ww@ z�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�n�A�p�A�p�A�r�A�x�A�z�A�|�A�z�A�~�A�|�A�~�AفAكAمAكA�z�A�|�A�v�A�z�A�x�A�v�A�^5A�VA�33A�K�A�G�A���Aә�A��AҺ^AҰ!Aҕ�A�=qA�  Aϩ�A�jA�1A˙�A�x�A��yA��A��AľwA�(�A��A��7A�9XA��
A�1'A��A���A��A�I�A��^A��A�x�A���A���A�XA�$�A�I�A�A���A�M�A� �A��A�K�A��A��FA��7A�9XA���A�;dA��A���A�$�A��-A���A��A�/A�`BA��-A��FA�C�A�r�A���A��A�1A�C�A�-A��A��A��wA�E�A��RA�VA��yA��A�ffA�C�A�v�A�A��\A�x�A�%A���A�t�A���A�ƨA���A��wA�-A��-A�Q�A���A�^5A��/A�n�A�1A|~�Ay\)Aw�Au�As/Ap��Ao�wAo
=Al�AfM�Ad��Ad  Ab�yAb�Abz�Abn�Ab^5Ab  A`��A_�A]�A[��AZ�AX��AT�\AS`BAR=qAQXAPĜAPjAO�TAOC�AN�RAJ�AI�wAIoAF�DADz�AC�mAChsABz�AA��A@bNA?|�A?�A>��A;x�A8��A7��A5�A4v�A45?A3�^A1�-A0�yA0�`A0E�A-��A-VA,r�A+XA*1A'G�A&�+A&M�A%��A%�7A%�A%�A%S�A$�/A$ffA#��A!�A��A��AffA�
A�yA��A��A��AA�AA{A�/A��A��A�!Ar�AA
=AA�AAE�AS�A
ZA��AXA��A(�A�hA�RA-A�A�A(�A?}A Q�@�"�@�ȴ@���@��+@�{@��9@�Q�@���@�1@�  @���@�F@�@��@�C�@��#@�7L@�V@��/@��D@�9X@���@�!@��^@�@�@��/@柾@�z�@�\@�9X@�;d@�@�z�@�ƨ@���@�Z@�+@�
=@ȼj@�bN@��`@��h@�  @�t�@���@�I�@��@�~�@�Ĝ@�1@��@�5?@���@��T@�Z@��@�33@��y@��`@�S�@���@�9X@���@�`B@��@���@��
@�"�@���@�o@��@�bN@��@���@���@���@���@���@��@�+@��+@�hs@�G�@�/@��`@�@{"�@zn�@y�@xbN@u�T@sC�@o;d@m�@k��@j�!@h�`@g�@g|�@e/@c33@bn�@`bN@^��@\��@\1@Y��@Y%@Xb@V�y@U@Tj@Qx�@Nȴ@N$�@M�h@K�F@J�!@J^5@I�7@I%@HbN@G��@Fv�@E�@E�-@E`B@D�D@B�!@@��@?|�@?
=@=�@=�h@<��@<1@9��@7��@5�h@5p�@5?}@3�F@3"�@1x�@0bN@/��@.�@-�@,�@+ƨ@+o@*��@)��@(  @';d@&��@$�@#��@!G�@ Ĝ@ �9@;d@�T@?}@�
@�H@M�@�7@%@1'@�@�P@l�@@p�@1@��@�\@�@��@�`@�u@�w@+@�@�T@`B@��@��@
M�@	x�@b@��@E�@5?@��@�@/@�j@�F@33@�!@J@�#@��@�@ bN@ 1'?���?���?���?���?��?��;?�O�?�?��?�
=?�9X?�o?�7?�%?�{?��?���?�=q?ٺ^?ش9?�l�?��T?��T?���?��
?ӕ�?��?�G�?�A�?ϝ�?���?�5??�O�?�O�?�dZ?��?��#?ȓu?Ǯ?�+?�ȴ?�ff?�$�?�9X?�S�?�-?��;?�A�?���?�\)?�p�?��m?���?�?���?��#?��?��^?��^?���?���?��?���?�~�?���?�?���?�(�?��D?�V?���?�v�?��?��;?��A�x�A�t�A�p�A�n�A�n�A�p�A�jA�jA�jA�l�A�r�A�p�A�p�A�p�A�r�A�p�A�p�A�p�A�r�A�p�A�p�A�p�A�n�A�n�A�p�A�r�A�r�A�v�A�z�A�z�A�z�A�z�A�z�A�z�A�|�A�z�A�z�A�|�A�~�A�|�A�|�A�|�A�|�A�~�AفA�~�A�~�AفAكAكAفAكAمAمAمAمAمAكAفAكG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       A�n�A�p�A�p�A�r�A�x�A�z�A�|�A�z�A�~�A�|�A�~�AفAكAمAكA�z�A�|�A�v�A�z�A�x�A�v�A�^5A�VA�33A�K�A�G�A���Aә�A��AҺ^AҰ!Aҕ�A�=qA�  Aϩ�A�jA�1A˙�A�x�A��yA��A��AľwA�(�A��A��7A�9XA��
A�1'A��A���A��A�I�A��^A��A�x�A���A���A�XA�$�A�I�A�A���A�M�A� �A��A�K�A��A��FA��7A�9XA���A�;dA��A���A�$�A��-A���A��A�/A�`BA��-A��FA�C�A�r�A���A��A�1A�C�A�-A��A��A��wA�E�A��RA�VA��yA��A�ffA�C�A�v�A�A��\A�x�A�%A���A�t�A���A�ƨA���A��wA�-A��-A�Q�A���A�^5A��/A�n�A�1A|~�Ay\)Aw�Au�As/Ap��Ao�wAo
=Al�AfM�Ad��Ad  Ab�yAb�Abz�Abn�Ab^5Ab  A`��A_�A]�A[��AZ�AX��AT�\AS`BAR=qAQXAPĜAPjAO�TAOC�AN�RAJ�AI�wAIoAF�DADz�AC�mAChsABz�AA��A@bNA?|�A?�A>��A;x�A8��A7��A5�A4v�A45?A3�^A1�-A0�yA0�`A0E�A-��A-VA,r�A+XA*1A'G�A&�+A&M�A%��A%�7A%�A%�A%S�A$�/A$ffA#��A!�A��A��AffA�
A�yA��A��A��AA�AA{A�/A��A��A�!Ar�AA
=AA�AAE�AS�A
ZA��AXA��A(�A�hA�RA-A�A�A(�A?}A Q�@�"�@�ȴ@���@��+@�{@��9@�Q�@���@�1@�  @���@�F@�@��@�C�@��#@�7L@�V@��/@��D@�9X@���@�!@��^@�@�@��/@柾@�z�@�\@�9X@�;d@�@�z�@�ƨ@���@�Z@�+@�
=@ȼj@�bN@��`@��h@�  @�t�@���@�I�@��@�~�@�Ĝ@�1@��@�5?@���@��T@�Z@��@�33@��y@��`@�S�@���@�9X@���@�`B@��@���@��
@�"�@���@�o@��@�bN@��@���@���@���@���@���@��@�+@��+@�hs@�G�@�/@��`@�@{"�@zn�@y�@xbN@u�T@sC�@o;d@m�@k��@j�!@h�`@g�@g|�@e/@c33@bn�@`bN@^��@\��@\1@Y��@Y%@Xb@V�y@U@Tj@Qx�@Nȴ@N$�@M�h@K�F@J�!@J^5@I�7@I%@HbN@G��@Fv�@E�@E�-@E`B@D�D@B�!@@��@?|�@?
=@=�@=�h@<��@<1@9��@7��@5�h@5p�@5?}@3�F@3"�@1x�@0bN@/��@.�@-�@,�@+ƨ@+o@*��@)��@(  @';d@&��@$�@#��@!G�@ Ĝ@ �9@;d@�T@?}@�
@�H@M�@�7@%@1'@�@�P@l�@@p�@1@��@�\@�@��@�`@�u@�w@+@�@�T@`B@��@��@
M�@	x�@b@��@E�@5?@��@�@/@�j@�F@33@�!@J@�#@��@�@ bN@ 1'?���?���?���?���?��?��;?�O�?�?��?�
=?�9X?�o?�7?�%?�{?��?���?�=q?ٺ^?ش9?�l�?��T?��T?���?��
?ӕ�?��?�G�?�A�?ϝ�?���?�5??�O�?�O�?�dZ?��?��#?ȓu?Ǯ?�+?�ȴ?�ff?�$�?�9X?�S�?�-?��;?�A�?���?�\)?�p�?��m?���?�?���?��#?��?��^?��^?���?���?��?���?�~�?���?�?���?�(�?��D?�V?���?�v�?��?��;?��A�x�A�t�A�p�A�n�A�n�A�p�A�jA�jA�jA�l�A�r�A�p�A�p�A�p�A�r�A�p�A�p�A�p�A�r�A�p�A�p�A�p�A�n�A�n�A�p�A�r�A�r�A�v�A�z�A�z�A�z�A�z�A�z�A�z�A�|�A�z�A�z�A�|�A�~�A�|�A�|�A�|�A�|�A�~�AفA�~�A�~�AفAكAكAفAكAمAمAمAمAمAكAفAكG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
gmB
gmB
hsB
k�B
k�B
q�B
��B
�/B�B�-B�HB�`B�mB�B��B,BT�BbNBffBffBe`BZB\)B]/BYBS�BT�BT�BbNB_;B_;B^5BjBq�Bu�Bx�Bz�B�=B�JB��B��B�uB�oB��B��B��B�7B�{B�hB�1B�7B�B�B~�B{�Bu�Bu�Bm�B[#BT�B:^B �B�B��B�BɺB�qB�B�VB�B�1B�1B�1B�1B�%B�Bz�Bv�Bp�BbNB]/BK�BD�B8RB&�B�B�BJB
��B
�B
�BB
B
�?B
�B
��B
��B
��B
�7B
�%B
|�B
x�B
Q�B
:^B
,B
�B
�B
	7B
B	��B	�#B	�!B	��B	��B	��B	�oB	�hB	�bB	�\B	�7B	�B	x�B	n�B	gmB	aHB	J�B	A�B	:^B	49B	0!B	/B	-B	+B	(�B	$�B	�B	�B	VB	  B��B��B��B�B�B�yB�fB�`B�5B��BŢB�^B�B�B��B��B��B��B��B��B��B��B��B�{B�JB�B{�Bw�Bs�Bs�Bt�Bw�Bv�Bu�Bp�BjBaHBaHBaHBcTBhsBaHBXB[#B[#B[#B[#B[#B_;B^5B^5B\)BZBYBVBVBQ�BF�BE�BD�B?}B;dB9XB8RB5?B49B49B.B.B/B.B.B-B/B.B,B,B)�B+B,B2-B5?B8RB8RB7LB7LB9XB9XBB�BH�BH�BM�B[#BgmBk�Br�B{�B� B�B� B�B~�B}�B~�B~�B|�B{�B�Bv�Bv�Bq�B�+B�oB��B�}B�}B�qB��B��B�B�
B�#B�B�B	B	oB	+B	9XB	F�B	K�B	T�B	S�B	bNB	�%B	�PB	�VB	��B	�\B	�B	��B	�!B	�9B	�RB	��B	ǮB	��B	��B	�/B	�;B	�;B	�yB	�B	�B	��B	��B	��B	��B	��B	��B
  B
%B
B
B
+B
DB
\B
oB
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
$�B
%�B
&�B
)�B
)�B
+B
,B
-B
/B
2-B
49B
49B
5?B
7LB
8RB
7LB
8RB
9XB
:^B
;dB
<jB
<jB
<jB
<jB
=qB
@�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
H�B
J�B
L�B
L�B
L�B
M�B
M�B
N�B
P�B
O�B
Q�B
R�B
S�B
S�B
T�B
VB
W
B
YB
YB
YB
\)B
]/B
`BB
_;B
`BB
aHB
cTB
cTB
e`B
e`B
ffB
e`B
ffB
gmB
gmB
hsB
hsB
jB
jB
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
u�B
w�B
w�B
w�B
x�B
x�B
z�B
y�B
z�B
|�B
}�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�%B
�1B
�1B
�7B
�=B
�PB
�VB
�hB
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
��B
��B
��B
��B
�B
�B
�B
�!B
�!B
�'B
�-B
�3B
�9B
�?B
�FB
�FB
�LB
�LB
�XB
�XB
�XB
�XB
�XB
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�jB
�jB
�dB
ffB
ffB
gmB
hsB
ffB
gmB
gmB
gmB
gmB
hsB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
ffB
gmB
gmB
gmB
ffB
gmB
gmB
gmB
ffB
ffB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
e`B
e`B
ffB
iyB
iyB
o�B
��B
�#B�B�!B�;B�TB�`B�yB��B)�BR�B`BBdZBdZBcTBXBZB[#BW
BQ�BR�BR�B`BB]/B]/B\)BhsBo�Bs�Bv�Bx�B�1B�=B�uB�uB�hB�bB�{B�uB�{B�+B�oB�\B�%B�+B�B� B|�By�Bs�Bs�Bk�BYBR�B8RB�B�B��B�BǮB�dB�B�JB�B�%B�%B�%B�%B�B~�Bx�Bt�Bn�B`BB[#BI�BB�B6FB$�B�BuB
=B
��B
�B
�5B
��B
�3B
��B
��B
��B
�uB
�+B
�B
z�B
v�B
O�B
8RB
)�B
�B
uB
+B	��B	��B	�B	�B	��B	��B	��B	�bB	�\B	�VB	�PB	�+B	�B	v�B	l�B	e`B	_;B	H�B	?}B	8RB	2-B	.B	-B	+B	(�B	&�B	"�B	�B	{B	JB��B��B��B�B�B�B�mB�ZB�TB�)B��BÖB�RB�B��B��B��B��B��B��B��B��B��B��B�oB�=B~�By�Bu�Bq�Bq�Br�Bu�Bt�Bs�Bn�BhsB_;B_;B_;BaHBffB_;BVBYBYBYBYBYB]/B\)B\)BZBXBW
BS�BS�BO�BD�BC�BB�B=qB9XB7LB6FB33B2-B2-B,B,B-B,B,B+B-B,B)�B)�B'�B(�B)�B0!B33B6FB6FB5?B5?B7LB7LB@�BF�BF�BK�BYBe`BiyBp�By�B}�B� B}�B~�B|�B{�B|�B|�B{�By�B~�Bu�Bu�Bp�B�%B�hB��B�wB�wB�jB��B��B��B�B�B�B�B	B	hB	)�B	8RB	E�B	J�B	S�B	R�B	aHB	�B	�JB	�PB	�{B	�VB	�B	��B	�B	�3B	�LB	��B	ƨB	��B	��B	�)B	�5B	�5B	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B
B
B
B
%B

=B
VB
hB
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
%�B
(�B
(�B
)�B
+B
,B
.B
1'B
33B
33B
49B
6FB
7LB
6FB
7LB
8RB
9XB
:^B
;dB
;dB
;dB
;dB
=qB
@�B
B�B
C�B
C�B
D�B
D�B
E�B
E�B
H�B
J�B
L�B
L�B
L�B
M�B
M�B
N�B
P�B
O�B
Q�B
R�B
S�B
S�B
T�B
VB
W
B
YB
YB
YB
\)B
]/B
`BB
_;B
`BB
aHB
cTB
cTB
e`B
e`B
ffB
e`B
ffB
gmB
gmB
hsB
hsB
jB
jB
l�B
l�B
m�B
m�B
n�B
n�B
n�B
o�B
o�B
o�B
p�B
q�B
q�B
r�B
r�B
s�B
u�B
w�B
w�B
w�B
x�B
x�B
z�B
y�B
z�B
|�B
}�B
~�B
~�B
~�B
� B
�B
�B
�B
�B
�%B
�1B
�1B
�7B
�DB
�VB
�\B
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
��B
��B
��B
��B
�B
�B
�!B
�-B
�-B
�3B
�9B
�?B
�FB
�LB
�RB
�RB
�XB
�XB
�dB
�dB
�dB
�dB
�dB
�qB
�qB
�wB
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�}B
�}B
�wB
dZB
dZB
e`B
ffB
dZB
e`B
e`B
e`B
e`B
ffB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
e`B
dZB
e`B
dZB
e`B
e`B
e`B
dZB
e`B
e`B
e`B
dZB
dZB
dZB
dZB
dZB
e`B
dZB
dZB
dZB
dZB
dZB
dZB
e`B
dZB
dZB
dZB
dZB
dZB
dZB
dZB
e`B
dZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201809172313452021061413523320210614135233202106141746382021061417463820210614174638201809172313452021061413523320210614135233202106141746382021061417463820210614174638PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018091723134520180917231345  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134520180917231345QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018091723134520180917231345QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015220210722160152IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                