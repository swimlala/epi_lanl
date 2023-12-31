CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:48Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �L   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �4   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   L   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   \   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � l   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   	   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    	   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        	0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        	8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       	@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    	H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220248  20210617131455  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�g7�;��@�g7�;��11  @�g7���`@�g7���`@6�պ�@6�պ��c�u��!��c�u��!�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@ff@Fff@�  @�  @�  @�33A33A33A$��AA��Ac33A���A���A�  A�33A�  A���AᙚA�ffB ffB  BffB��B   B(  B0  B7��B@ffBH  BO��BXffB`��BhffBo��BxffB���B�33B�  B�33B�  B�33B�  B���B�  B���B�  B�  B���B�ffB�ffB�ffB�ffB�33B���B�33B�  B�33B�33B���B���B�33B�ffB�33B���B�ffB�33B�33C   C��C  C33C�C	�fC�C  C�fC�CL�C33C  C�C  C��C   C"33C$33C&  C(  C*  C,33C.L�C0�C2  C4  C6  C7�fC9�fC;�fC=�fC?��CA��CC�fCFL�CHL�CI�fCK�fCN  CO�fCR  CS�fCV33CX�CZ  C\33C^33C`  Cb33Cd�Ce�fCh33Cj�Ck�fCn33Cp�Cq�fCt�Cv33CxL�Cz33C|  C~�C��C��3C��C�  C��3C��C��3C��fC�  C��C�  C��3C�  C�&fC��C�  C��C��C��3C��C�&fC��C�  C��C��C�  C��C��C��3C��C��C��3C��C�&fC��C�  C�&fC��C��3C�  C��C��C��3C��C�&fC��C��fC�  C��C�&fC��C��3C�  C��C��C�&fC��C��fC��3C��C��C�&fC��C��3C��C��C�&fC��C�  C��C�&fC��C��3C��C��C��C��3C��C�  C�ٚC��3C��C�  C��fC�  C��C�&fC��C��C��C��C��3C��C�&fC��C��3C��C�&fC��C��3C�  C�&fC��C��fC�  C��C�  C��fC��3C�  C��C�&fC��C��fC�  C��C�&fC��C��3C�  C��C�  C�ٚC��fC�  C�  C��C�&fC���D s3D �3D� DfD` Dy�D
3D�fDy�DL�D,�D�D� D�fD �3D#Y�D&  D(��D+� D.Y�D1fD3�fD6FfD8��D;��D>3D@�3DC�DE�fDHFfDJ�fDMy�DP  DR�3DU�fDXL�D[  D]��D`� Dcy�DfL�Di�Dk�fDny�Dq9�Ds� Dv��Dy33D{` D~fD�L�D���D��fD�,�D�p D���D��3D�,�D�` D��3D�ɚD��fD�,�D�ffD���D�ɚD�  D�@ D�|�D���D���D�6fD�vfD���D�3D�FfD���D�� D��D�\�D���D�� D�)�D�ffD���D��fD�#3D�c3D�� D�ٚD� D�I�D��3D���D��D�3D�C3D�ffD��3D���D�� D��D�33D�` DfDð D�ٚD�  D�)�D�S3D�|�Dʣ3D���D���D��D�C3D�s3Dќ�D��fD���D��D�C3D�y�Dج�D�� D�	�D�6fD�ffDޖfD�ɚD��3D�)�D�P D�|�D�3D���D���D�,�D�L�D�vfD��D�ɚD��fD��D�I�D�|�D�D���D�  D�&fD�S3D��3D���D�ɚD���D�  D�FfE 6fE ��ET�E�EnfE��E��E3E�3E�E�fE!�E��E( E��E)�E��E
1�E<�ED�E�fE��EffEt�EfEfE( EA�E��E E33EVfEfE#3EC3E h E" E##3E$9�E%T�E&� E(3E)�E*�fE+� E,� E.I�E/NfE0��E1�3E3)�E4�3E5��E6ɚE8fE9K3E:��E;��E>�fEB�EEY�EHT�EKY�EN�3EQ� ET�3EX$�E[	�E^K3Ea�fEd` Eg��Ej� Em� Eq&fEt)�Ew  Ezi�E}�fE�P E�њE�jfE��fE�� E��E���E�6fE���E�L E���>���>���>���?   >���>���>L��>���>L��>L��>���>���>L��>���>���?   >���>���>���?   >���?��?��?333?333?fff?���?���?�  ?���?�33@   @33@   @333@Fff@Y��@s33@�  @���@�ff@�  @���@�33@�  @���@�ff@�ff@�  A��A��AffAffA��A#33A,��A333A9��AA��AH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141414144144141444141414114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ?L��?�33@&ff@fff@�  @�  @�  @�33A33A33A,��AI��Ak33A���A���A�  A�33A�  A���A噚A�ffBffB
  BffB��B"  B*  B2  B9��BBffBJ  BQ��BZffBb��BjffBq��BzffB���B�33B�  B�33B�  B�33B�  B���B�  B���B�  B�  B���B�ffB�ffB�ffB�ffB�33B���B�33B�  B�33B�33B���BᙚB�33B�ffB�33B���B�ffB�33B�33C � CL�C� C�3C��C
ffC��C� CffC��C��C�3C� C��C� CL�C � C"�3C$�3C&� C(� C*� C,�3C.��C0��C2� C4� C6� C8ffC:ffC<ffC>ffC@L�CBL�CDffCF��CH��CJffCLffCN� CPffCR� CTffCV�3CX��CZ� C\�3C^�3C`� Cb�3Cd��CfffCh�3Cj��ClffCn�3Cp��CrffCt��Cv�3Cx��Cz�3C|� C~��C�L�C�33C�L�C�@ C�33C�L�C�33C�&fC�@ C�L�C�@ C�33C�@ C�ffC�L�C�@ C�Y�C�L�C�33C�Y�C�ffC�Y�C�@ C�Y�C�L�C�@ C�Y�C�L�C�33C�Y�C�L�C�33C�L�C�ffC�L�C�@ C�ffC�L�C�33C�@ C�Y�C�L�C�33C�L�C�ffC�L�C�&fC�@ C�Y�C�ffC�Y�C�33C�@ C�L�C�Y�C�ffC�L�C�&fC�33C�L�C�Y�C�ffC�L�C�33C�L�C�Y�C�ffC�Y�C�@ C�L�C�ffC�L�C�33C�L�C�Y�C�L�C�33C�L�C�@ C��C�33C�L�C�@ C�&fC�@ C�Y�C�ffC�Y�C�L�C�Y�C�L�C�33C�L�C�ffC�L�C�33C�L�C�ffC�L�C�33C�@ C�ffC�L�C�&fC�@ C�Y�C�@ C�&fC�33C�@ C�Y�C�ffC�L�C�&fC�@ C�Y�C�ffC�L�C�33C�@ C�Y�C�@ C��C�&fC�@ C�@ C�Y�C�ffD fD �3D3D� D&fD� D��D
33D�fD��Dl�DL�D,�D  D�fD �3D#y�D&@ D)�D+� D.y�D1&fD3�fD6ffD9�D;��D>33D@�3DC9�DE�fDHffDKfDM��DP@ DR�3DU�fDXl�D[@ D^�D`� Dc��Dfl�Di,�Dk�fDn��DqY�Dt  Dv��DyS3D{� D~&fD�\�D���D��fD�<�D�� D�ɚD�3D�<�D�p D��3D�ٚD�fD�<�D�vfD���D�ٚD� D�P D���D�ɚD�	�D�FfD��fD���D�3D�VfD���D�� D�)�D�l�D���D�  D�9�D�vfD���D��fD�33D�s3D�� D��D�  D�Y�D��3D�ɚD���D�#3D�S3D�vfD��3D�ɚD�� D��D�C3D�p DfD�� D��D� D�9�D�c3DɌ�Dʳ3D���D�	�D�,�D�S3DЃ3DѬ�D��fD���D�)�D�S3D׉�Dؼ�D�� D��D�FfD�vfDަfD�ٚD�3D�9�D�` D��D�3D���D��D�<�D�\�D�fD��D�ٚD�fD�,�D�Y�D��D�D���D� D�6fD�c3D��3D���D�ٚD�	�D�0 D�VfE >fE ��E\�E�EvfE�E��E3E�3E!�E�fE)�E��E0 E��E1�E��E
9�ED�EL�E�fE��EnfE|�EfEfE0 EI�E��E E;3E^fEfE+3EK3E p E" E#+3E$A�E%\�E'  E(3E)!�E*�fE+� E,� E.Q�E/VfE0��E1�3E31�E4�3E5��E6њE8fE9S3E:��E;��E>�fEB�EEa�EH\�EKa�EN�3EQ� ET�3EX,�E[�E^S3Ea�fEdh Eg��Ej� Em� Eq.fEt1�Ew( Ezq�E}�fE�T E�՚E�nfE��fE�� E��E���E�:fE�ŚE�P E���?L��G�O�?L��G�O�?L��G�O�?333G�O�G�O�?333G�O�G�O�?333G�O�?L��G�O�G�O�G�O�?fffG�O�?fffG�O�?���G�O�?���?�33G�O�?ٙ�@   @ff@��@   @333@@  @S33@fff@y��@���@�  @���@�ff@�  @���@�33@�  @���@�ff@�ffA   A	��A��AffAffA$��A+33A4��A;33AA��AI��AP  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141414144144141444141414114111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   @ @ %@ V@ *@ O@ ""@ (�@ 0x@ 7�@ >�@ FQ@ R�@ `�@ m�@ {�@ ��@ ��@ ��@ �-@ ��@ ��@ �#@ �@ �q@�@@�@,`@9X@H]@UU@bN@qS@�@��@��@��@��@�>@�7@ލ@�@��@�@�@""@/@=q@K@Z�@g�@uk@�@��@��@��@�@��@�O@��@�@@��@
�@B@&;@2�@B8@O0@\�@i�@v@�@�#@�@�f@�k@�c@�[@�`@�e@^@�@�@(�@5?@DD@SI@`�@m:@z�@��@��@��@�-@�&@��@�t@�m@�@�@b@[@+@9X@I�@Wb@bN@o�@~K@�D@��@��@�F@�>@�7@�;@��@�,@1@*@!s@1'@>@Ji@Z@g@s_@�d@��@�@�@�R@ƨ@�O@��@�@��@	�@�@$�@1�@@�@O0@\)@i!@ww@�+@�u@�m@�r@�k@ȴ@�h@�@�@  @@�@(�@7�@D�@Q=@`�@m�@z3@�7@�<@��@�~@�2@�|@��@�@� @	@	b@	g@	.l@	:�@	FQ@	UU@	dZ@	r�@	�@	�D@	��@	��@	�F@	Ĝ@	��@	܀@	��@	��@
1@
�@
"�@
/@
>@
Lu@
Z�@
g�@
t@
�d@
�h@
��@
��@
�@
�W@
�O@
��@
�@
��@1@6@&;@33@?}@N�@]�@k�@x�@��@�$@�@�f@�k@�o@׹@�@�@@V@�@(�@8�@D�@P�@_�@n�@z�@�+@��@��@��@�2@�|@�@�@� @v@�@
@,`@;d@G�@SI@a�@p�@~K@�P@��@��@��@�2@�7@ލ@�@s_@�^@@M�@��@�y@7�@�@�O@ �@l�@�R@�@N�@��@��@)�@qS@��@^@FQ@��@ψ@*@\�@��@��@33@|�@ƨ@o@_�@�@�,@C�@��@��@&;@o�@�@j@Lu@��@�7@�@^�@��@��@2�@ww@��@ �@C�@�@ƨ@�@I@�D@�*@�@P�@��@�
@ �@ ^5@ �z@ �@!*S@!o�@!��@!��@"?}@"�@"�o@#b@#Wb@#�@#��@$#�@$hs@$�@$�@%3�@%ww@%�^@%��@&?}@&�d@&Ĝ@'v@'D�@'��@'��@(@(B�@(��@(��@) �@)@�@)�@)�&@)��@*=q@*|�@*�j@*��@+:�@+z3@+�^@+�~@,7L@,x&@,��@,� @-5�@-v@-��@-��@.9X@.z�@.�^@.��@/;d@/|?@/��@/�E@0?}@0~K@0�w@0�E@1<�@1}�@1�w@1��@2;d@2z3@2�^@2��@39X@3y�@3�@3�9@4<�@4z�@4��@4��@5:�@5v�@5��@5��@66�@6uk@6�9@6��@7.l@7j@7��@7��@8[@8V�@8��@8�@9�@9:�@9r�@9��@9��@:�@:O1@:�q@;hs@;�@<�@<��@=�T@>�@>�@?/@?��@@�@@�C@AN�@A��@BF�@B�Q@Cx�@C�@Dp�@E%�@E��@F�@F��@G=q@G��@H&;@H��@IDD@I��@J\�@J�@KoF@K�h@LqS@M1@M��@M�E@N��@O@O��@P�@Qo�@R��@T3�@Uy�@V�>@X7�@Y��@Z�W@\8�@]t�@^�h@`<�@ap�@bƨ@dO@ei�@f�@h-@ip�@j׹@l/@m~K@n�W@p$.@qr�@rӠ@t @uul@v�o@x�@ym:@zƨ@ G�O�@ G�O�@ G�O�@ ^G�O�G�O�@ ^G�O�G�O�@ ^G�O�@ G�O�G�O�G�O�@ �G�O�@ �G�O�@ G�O�@ �@ %G�O�@ 1@ 
=@ 
�@ �@ �@ �@ @ @ *@ 6@ �@ O@ [@  @ ""@ $.@ &;@ (�@ +�@ -�@ 1'@ 33@ 7L@ 8�@ <�@ @,@ B�@ E�@ I�@ Lu@ O0@ R�@ UUG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aɇ+AɍPAɋDAɉ7AɃA�~�A�`BA�t�A�`BA�I�A�;dA�=qA�9XA�9XA�/A� �A�
=A��A���Aț�A�|�A�Q�A�7LA�ĜA�+A��yA�;dA�ĜA���A��`A���A���A�ƨA�A���A�n�A���A���A�A��A�r�A��RA�O�A�A���A�VA��A��
A��9A�?}A��TA���A�;dA��wA��DA�-A���A���A�ƨA��A�dZA�%A��A�?}A��!A�/A��/A�ZA��A�{A�^5A�bNA���A���A���A��hA�?}A��HA��#A�A�  A���A���A��`A��TA�x�A���A�XA��\A�l�A�/A��A�~�A�&�A���A��7A��/A�G�A��7A�hsA���A���A��^A�~�A�-A��A�jA�-A���A��-A�A���A��A�-A|�A~ĜA~1'Az��Ax�yAx��Av�jAt$�Aq7LAlE�Ai�mAfA�Ab��A`z�A\~�AZȴAZ�uAZ=qAV��AU�-AUx�AU33ATz�AR��AO��AM�#AM/ALZAJ�/AJffAI��AHbAGoAFȴAE"�AC��AC\)AB�/AA�mAA�AB��AB�AB�+AA�mAA�A>��A:��A9��A9&�A8�/A8�jA8jA7�wA7O�A6JA4$�A2$�A1��A1x�A0~�A/�#A/��A/;dA.��A-�A-�A,I�A+;dA*n�A*  A(��A'�A&n�A%O�A#K�A"E�A!�A!`BA �DA JA A��A��A�A�/A�A�FA�A�A?}A�AM�A`BA7LA��A^5A\)A�A33A5?A�^A33AA�A
�A	�;A	dZA�9A�A��Av�A  A��A��AK�A�A �+A �DA ^5@�V@���@��@���@�~�@��7@�z�@�@�R@�7@�"�@��@�b@���@�E�@�?}@�1'@���@�{@�X@���@��;@⟾@�@� �@���@�X@�bN@ۥ�@��@�@�x�@�1@���@�p�@��@ȓu@�%@���@�@��@�Ĝ@�J@���@��P@�$�@�Z@�/@�M�@�Q�@��^@��@�5?@�7L@���@�"�@���@��@�V@� �@�O�@�{@�I�@�S�@�-@��^@��@��@�9X@�|�@�-@�O�@�b@���@��R@���@���@��
@�"�@�@���@� �@~�@|z�@yx�@w�@vV@t�@s@q&�@o�P@l��@k��@iG�@hA�@f��@e�@d1@`��@_
=@\��@[�@Z�@Y&�@W�w@Vff@T�/@Rn�@QX@P1'@O�@M�h@K�
@J-@I7L@HA�@G\)@Ep�@D(�@B��@AG�@@  @>�R@<�@:J@8�9@7��@6ȴ@5O�@3ƨ@2�H@2�@1hs@0 �@.�@-�-@,�j@,Z@+o@*�@)��@)%@(  @'\)@&ff@%V@$9X@#33@!��@ �`@ A�@�P@�@�D@�F@��@^5@�^@��@b@\)@�y@5?@V@�@I�@��@33@~�@G�@Ĝ@ �@��@$�@`B@��@I�@1@��@"�@
�\@	�^@�`@bN@  @|�@\)@�y@�y@��@$�@?}@j@�@��@n�@��@��@G�@ 1'?���?�5??�(�?��H?���?��?�?�7?�  ?�5??�I�?���?�Q�?�ff?���?��?���?�7?�A�?��?�p�?�ƨ?�C�?�X?�r�?���?��y?�E�?ա�?Ձ?Լj?�Z?�33?ѩ�?�bN?��?Η�?�{?�O�?�j?˅?�"�?��?�7L?�Q�?�ff?Ł?��
?�M�?�&�?��;?���?�v�?�V?��-?�V?���?�1?�C�?���?�^5?���?��?���?��?�=q?���?�?�C�?���?�I�?���?�/?���?�V?���?�|�AɁAɁAɅAɅAɃAɅAɇ+AɃAɅAɅAɇ+AɍPAɇ+Aɉ7Aɇ+AɍPAɋDAɋDAɉ7AɋDAɏ\AɍPAɏ\Aɏ\AɋDAɉ7AɋDAɏ\AɍPAɍPAɋDAɍPAɉ7AɋDAɋDAɉ7Aɇ+AɅAɁAɁA�|�A�~�A�~�A�bNA�^5A�`BA�t�A�v�A�t�A�hsA�VA�M�A�C�A�;dA�9XA�;dA�;dA�=qA�=qA�=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Aɇ+AɍPAɋDAɉ7AɃA�~�A�`BA�t�A�`BA�I�A�;dA�=qA�9XA�9XA�/A� �A�
=A��A���Aț�A�|�A�Q�A�7LA�ĜA�+A��yA�;dA�ĜA���A��`A���A���A�ƨA�A���A�n�A���A���A�A��A�r�A��RA�O�A�A���A�VA��A��
A��9A�?}A��TA���A�;dA��wA��DA�-A���A���A�ƨA��A�dZA�%A��A�?}A��!A�/A��/A�ZA��A�{A�^5A�bNA���A���A���A��hA�?}A��HA��#A�A�  A���A���A��`A��TA�x�A���A�XA��\A�l�A�/A��A�~�A�&�A���A��7A��/A�G�A��7A�hsA���A���A��^A�~�A�-A��A�jA�-A���A��-A�A���A��A�-A|�A~ĜA~1'Az��Ax�yAx��Av�jAt$�Aq7LAlE�Ai�mAfA�Ab��A`z�A\~�AZȴAZ�uAZ=qAV��AU�-AUx�AU33ATz�AR��AO��AM�#AM/ALZAJ�/AJffAI��AHbAGoAFȴAE"�AC��AC\)AB�/AA�mAA�AB��AB�AB�+AA�mAA�A>��A:��A9��A9&�A8�/A8�jA8jA7�wA7O�A6JA4$�A2$�A1��A1x�A0~�A/�#A/��A/;dA.��A-�A-�A,I�A+;dA*n�A*  A(��A'�A&n�A%O�A#K�A"E�A!�A!`BA �DA JA A��A��A�A�/A�A�FA�A�A?}A�AM�A`BA7LA��A^5A\)A�A33A5?A�^A33AA�A
�A	�;A	dZA�9A�A��Av�A  A��A��AK�A�A �+A �DA ^5@�V@���@��@���@�~�@��7@�z�@�@�R@�7@�"�@��@�b@���@�E�@�?}@�1'@���@�{@�X@���@��;@⟾@�@� �@���@�X@�bN@ۥ�@��@�@�x�@�1@���@�p�@��@ȓu@�%@���@�@��@�Ĝ@�J@���@��P@�$�@�Z@�/@�M�@�Q�@��^@��@�5?@�7L@���@�"�@���@��@�V@� �@�O�@�{@�I�@�S�@�-@��^@��@��@�9X@�|�@�-@�O�@�b@���@��R@���@���@��
@�"�@�@���@� �@~�@|z�@yx�@w�@vV@t�@s@q&�@o�P@l��@k��@iG�@hA�@f��@e�@d1@`��@_
=@\��@[�@Z�@Y&�@W�w@Vff@T�/@Rn�@QX@P1'@O�@M�h@K�
@J-@I7L@HA�@G\)@Ep�@D(�@B��@AG�@@  @>�R@<�@:J@8�9@7��@6ȴ@5O�@3ƨ@2�H@2�@1hs@0 �@.�@-�-@,�j@,Z@+o@*�@)��@)%@(  @'\)@&ff@%V@$9X@#33@!��@ �`@ A�@�P@�@�D@�F@��@^5@�^@��@b@\)@�y@5?@V@�@I�@��@33@~�@G�@Ĝ@ �@��@$�@`B@��@I�@1@��@"�@
�\@	�^@�`@bN@  @|�@\)@�y@�y@��@$�@?}@j@�@��@n�@��@��@G�@ 1'?���?�5??�(�?��H?���?��?�?�7?�  ?�5??�I�?���?�Q�?�ff?���?��?���?�7?�A�?��?�p�?�ƨ?�C�?�X?�r�?���?��y?�E�?ա�?Ձ?Լj?�Z?�33?ѩ�?�bN?��?Η�?�{?�O�?�j?˅?�"�?��?�7L?�Q�?�ff?Ł?��
?�M�?�&�?��;?���?�v�?�V?��-?�V?���?�1?�C�?���?�^5?���?��?���?��?�=q?���?�?�C�?���?�I�?���?�/?���?�V?���?�|�AɁAɁAɅAɅAɃAɅAɇ+AɃAɅAɅAɇ+AɍPAɇ+Aɉ7Aɇ+AɍPAɋDAɋDAɉ7AɋDAɏ\AɍPAɏ\Aɏ\AɋDAɉ7AɋDAɏ\AɍPAɍPAɋDAɍPAɉ7AɋDAɋDAɉ7Aɇ+AɅAɁAɁA�|�A�~�A�~�A�bNA�^5A�`BA�t�A�v�A�t�A�hsA�VA�M�A�C�A�;dA�9XA�;dA�;dA�=qA�=qA�=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�ZB
�ZB
�ZB
�TB
�ZB
�ZB
�ZB
�TB
�TB
�NB
�TB
�TB
�TB
�NB
�TB
�`B
�yB
�B
��BB+B\BoB �B7LB?}B~�B�B?}B?}B?}B?}B?}B@�BE�BB�B?}B>wB?}B?}BG�BS�B\)B]/B^5B_;B^5B]/B\)B\)B]/B\)BYBZB[#BcTBffBhsBhsBjBm�Bs�Bv�Bv�B~�B�B�%B�bB��B��B��B��B��B��B�oB�VB�7B�Bx�B]/B@�B$�B\BB�B�BB�#BĜB�XB�B��B��B�PB�1B�Bz�BiyBM�B<jB-B#�BuB1BB
��B
��B
�HB
�B
�jB
��B
y�B
l�B
R�B
@�B
6FB
/B
&�B
%B
B	��B	�B	�)B	�FB	��B	��B	y�B	e`B	N�B	7LB	1'B	.B	+B	oB	bB	PB	
=B	B��B�fB�)B�#B�B�#B�B�B�
B��B��B��B��B�B��B��B�B	+B	1B	1B	B	  B�B�TB�/B�B�B�B��B��B�B��B��B�wB��B��B��B��B��BɺBƨBB��B�jB�LB�?B�9B�B��B��B��B��B�{B�oB�\B�PB�PB�JB�JB�+B�B�B~�B}�B{�Bx�Br�Bl�BhsBe`BdZBaHB`BBZBW
BW
BVBVBZB\)BYBYBVBO�BQ�BVBXBT�BYBT�BK�BB�B>wBG�BG�B8RB1'B-B+B)�B)�B(�B(�B'�B(�B&�B)�B(�B(�B'�B'�B(�B)�B,B-B,B-B/B.B0!B0!B1'B2-B33B33B5?B49B7LB:^B:^BC�BJ�BK�BT�B^5BgmBq�B{�B�DB��B��B�B�}BɺB�
B�NB��B	\B	-B	<jB	R�B	_;B	dZB	n�B	s�B	�B	�{B	��B	��B	�B	�FB	�}B	B	ÖB	ɺB	��B	��B	�B	�#B	�5B	�TB	�`B	�sB	�B	�B	�B	��B	��B	��B
B
B
B
%B
	7B

=B
PB
bB
bB
uB
uB
�B
�B
�B
�B
�B
 �B
!�B
"�B
%�B
%�B
'�B
)�B
,B
-B
/B
/B
1'B
33B
5?B
5?B
6FB
7LB
9XB
;dB
<jB
<jB
?}B
?}B
A�B
C�B
E�B
F�B
F�B
H�B
I�B
J�B
J�B
K�B
L�B
N�B
O�B
P�B
P�B
Q�B
Q�B
R�B
T�B
VB
VB
W
B
XB
XB
ZB
\)B
\)B
]/B
]/B
^5B
`BB
aHB
bNB
bNB
bNB
cTB
dZB
e`B
ffB
ffB
hsB
hsB
hsB
iyB
iyB
jB
jB
l�B
l�B
m�B
n�B
n�B
o�B
p�B
p�B
q�B
q�B
r�B
r�B
s�B
t�B
u�B
u�B
t�B
u�B
v�B
u�B
w�B
w�B
y�B
z�B
{�B
z�B
{�B
{�B
|�B
}�B
}�B
~�B
� B
�B
�B
�B
�%B
�+B
�1B
�1B
�=B
�JB
�PB
�VB
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
�B
�B
�B
�B
�'B
�'B
�'B
�-B
�-B
�3B
�9B
�FB
�FB
�FB
�LB
�LB
�LB
�RB
�LB
�RB
�XB
�XB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�TB
�ZB
�TB
�`B
�ZB
�NB
�ZB
�NB
�TB
�`B
�TB
�TB
�ZB
�ZB
�TB
�TB
�TB
�ZB
�TB
�ZB
�ZB
�ZB
�ZB
�ZB
�ZB
�TB
�`B
�ZB
�TB
�TB
�ZB
�ZB
�TB
�ZB
�ZB
�ZB
�TB
�TB
�TB
�fB
�ZB
�NB
�NB
�ZB
�NB
�NB
�NB
�TB
�TB
�ZB
�TB
�NB
�TB
�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   B
�1B
�2B
�2B
�,B
�3B
�3B
�3B
�.B
�.B
�(B
�.B
�/B
�0B
�*B
�1B
�=B
�WB
��B
��B �BB=BQB �B7/B?`B~�BkB?aB?bB?bB?cB?dB@jBE�BBwB?fB>`B?gB?gBG�BS�B\B]B^"B_(B^#B]B\B\B]B\BYBZB[BcGBfZBhgBhhBjtBm�Bs�Bv�Bv�B~�B�B�B�\B��B��B��B��B��B��B�lB�TB�6B�Bx�B]/B@�B$�B]BB�B�DB�&BğB�[B�B��B��B�UB�7B�%Bz�Bi�BM�B<rB-B#�B~B;B#B
��B
��B
�TB
�B
�vB
��B
y�B
l�B
S B
@�B
6UB
/*B
&�B
5B
B	�B	�B	�;B	�XB	��B	��B	y�B	esB	N�B	7`B	1;B	.)B	+B	�B	xB	gB	
TB	1B��B�B�BB�<B�*B�>B�8B�,B�&B�B�B��B�B�/B�B��B�B	LB	SB	SB	5B	 #B��B�xB�SB�<B�5B�*B�B��B�,B�B��B��B�B�B�B��B��B��B��B¼B��B��B�{B�nB�iB�2B�&B�B��B��B��B��B��B��B��B�B��B�aB�VB�IB2B~,B| ByBr�Bl�Bh�Be�Bd�Ba�B`BZZBWGBWHBVBBVCBZ\B\iBYWBYXBVEBP!BR.BVGBXSBUBBY[BUCBLBB�B>�BG�BG�B8�B1oB-WB+KB*FB*FB)AB)AB(<B)BB'6B*IB)DB)DB(?B(?B)FB*MB,YB-`B,ZB-aB/nB.hB0uB0vB1|B2�B3�B3�B5�B4�B7�B:�B:�BC�BK#BL,BUfB^�Bg�BrB|[B��B�B�6B��B�B�ABהB��B�lB	�B	-�B	=B	S�B	_�B	d�B	o?B	t`B	��B	�+B	�qB	��B	��B	�B	�>B	�SB	�]B	ʄB	ѳB	��B	��B	��B	�B	�2B	�AB	�WB	�fB	�B	�B	��B	��B	��B
�B
B
B
'B

<B
EB
[B
oB
rB
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
#�B
'B
'B
)'B
+6B
-EB
.NB
0^B
0aB
2pB
4B
6�B
6�B
7�B
8�B
:�B
<�B
=�B
=�B
@�B
@�B
B�B
EB
GB
HB
HB
J.B
K7B
LAB
LCB
MLB
NUB
PdB
QmB
RuB
RxB
S�B
S�B
T�B
V�B
W�B
W�B
X�B
Y�B
Y�B
[�B
]�B
]�B
^�B
^�B
_�B
bB
cB
dB
dB
dB
e"B
f+B
g4B
h=B
h@B
jOB
jRB
jUB
k^B
kaB
liB
llB
n{B
n~B
o�B
p�B
p�B
q�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
v�B
w�B
w�B
v�B
w�B
x�B
w�B
y�B
y�B
|B
}B
~B
}B
~B
~!B
*B
�3B
�5B
�>B
�FB
�OB
�VB
�nB
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
� B
�B
�B
�&B
�8B
�CB
�CB
�QB
�\B
�hB
�tB
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
��B
� B
�B
�B
�B
�*B
�@B
�VB
�rB
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�;B
�PB
�lB
�{B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�'B
�6B
�FB
�UB
�dB
�yB
��B
�+B
�2B
�1B
�2B
�1B
�2B
�+B
�1B
�+B
�7B
�1B
�%B
�1B
�%B
�+B
�8B
�,B
�,B
�2B
�2B
�,B
�,B
�,B
�2B
�,B
�2B
�2B
�2B
�2B
�2B
�2B
�,B
�8B
�2B
�,B
�,B
�2B
�3B
�-B
�3B
�3B
�3B
�-B
�-B
�-B
�?B
�3B
�(B
�(B
�4B
�(B
�(B
�(B
�.B
�.B
�5B
�/B
�)B
�/B
�/G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202482021061413552020210614135520202106171311592021061713115920210617131159201807242202482021061413552020210614135520202106171311592021061713115920210617131159PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024820180724220248  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024820180724220248QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024820180724220248QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145520210617131455IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                