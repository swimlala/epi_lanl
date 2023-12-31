CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:49Z creation      
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
_FillValue                 8  L�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  P�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  a�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  f   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �$   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   l   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   |   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   (   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        X   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       `   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    h   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �Argo profile    3.1 1.2 19500101000000  20180724220249  20210617131456  5905739 5905739 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7219                            7219                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12002                           12002                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�h��;/@�h��;/11  @�h홙�`@�h홙�`@6�{��0@6�{��0�c���v��c���v�11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?fff@ff@Fff@�33@�ff@�ff@�  A��A33A$��AA��Ac33A���A�  A���A���A���Aљ�AᙚA���B   B  BffBffB ffB(  B0ffB8  B?��BG��BO33BW��B`ffBi33Bq33Bx��B�33B�  B�33B�33B���B�  B�  B�  B�  B�  B�ffB�ffB�  B�33B�ffB�  B���B���B�ffB���B�  B�33B�33B�33B�33B�  B虚B왚B�ffB�33B�  B�ffC �C�fC33C�C�fC
33CL�C�C�fC�CL�C�C  C33C�C��C �C"  C#�fC&�C(  C)�fC,33C.�C0  C1�fC3��C6�C8L�C:33C<�C>  C?�fCB33CD�CF�CH  CI�fCK�fCM��CP�CQ�fCS�fCV�CX�CZ  C\  C]��C`  Cb  Cc��Cf�ChL�Cj�Cl  CnL�Cp33Cr  Ct33Cv�Cw��Cy�fC|�C~L�C�&fC�33C��C��3C��3C�  C��C��C��C�&fC��C��fC��3C��C��C��C��3C�  C��C�&fC��C�  C��C��C��3C��C�&fC��C�  C��C�&fC��C��3C��C��C�  C��fC�  C�  C��C�&fC��C��3C��C��C��C�&fC�&fC��C�ٚC��fC��3C�  C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC�&fC�&fC�  C�  C�&fC��C��fC��3C��C��C��C��C��C�&fC�&fC��C��fC��fC��3C��3C��C��C��C��C��3C�ٚC�ٚC��fC��3C��3C�  C��C��C�  C��fC��fC��3C��C�&fC�33C��C��3C��C�&fC��C��3C��C�  C��fC��C��C�  C��fC��C��C�&fC�ٚC�  D fD ��D3Dl�D�3D� D� D
�D� DY�D�3D��DL�D�3D��D33D!��D$l�D'�D)�fD,� D/&fD1ٚD4� D7&fD9��D<` D?fDA�fDDL�DF��DI��DL33DNٚDQy�DT  DV��DYS3D[�3D^S3D`ٚDcL�De��Dh3Dj�3Dl��DoffDq�3DtS3Dv� Dy&fD{,�D}�3D�  D�c3D���D���D�33D�� D�ɚD�3D�VfD���D�ٚD��D�S3D���D��3D��fD�&fD�P D�s3D���D�� D�ٚD�3D�#3D�@ D�\�D�y�D��3D���D��3D���D��3D�fD�  D�0 D�@ D�S3D�i�D��fD���D��fD��3D��D�3D��D�<�D�\�D��fD��fD���D��3D�  D�C3D�s3D��3D�� D���D��D�S3DĆfD�� D���D�33D�c3Dʠ D�� D�3D�,�D�i�DМ�D��fD�fD�33D�` D֐ D׹�D���D��D�<�D�` D݀ DަfD��fD��fD�fD�#3D�@ D�VfD�l�D� D�fD驚D�fD��D���D�� D���D���D� D�  D�)�D�0 D�9�D�C3D�I�D�P D�S3D�VfD�<�D�9�D�9�D�<�D�<�E �E ��E�E��E�E��E�E�fEfE�fE��E��E( E	)�E
� E�3EK3ET�Ea�E�E��E�fE��E�3EH E[3EnfE3E3E�fE�fE��E!6fE"6fE#� E$�3E&fE'� E(s3E)�fE+#3E,x E-[3E.� E/�3E1;3E2~fE3��E5�E6K3E7��E8њE:�E;P E<�3E?�3EBњEE�3EH�fEL$�EOq�ER^fEU��EX� E[� E_  Eb  Ee<�EhY�EkY�En��Eq��Et��Ew��Ez�fE}� E��fE�>fE��3E�BfE��fE�m�E�� E�}�E�fE�\�E���E��E�C3E��fE��3E�K3E���E���E�( E��fE�ŚE�#3E�bfE��3E��E�ZfE���E��fE�G3E���E���E�< E��fE��3E�0 E��fE���>���>���>���>���>���>���?   >���?   >���>���>���>���>���?   >���?   ?   ?��?��?333?L��?�  ?�  ?���?�33?���?�33@   @33@   @333@L��@Y��@y��@�33@���@���@�ff@�  @�  @ə�@�33@�33@陚@�ffA��A  A  A��A��A#33A,��A333A8  AA��AH  AP  AX  A^ffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444411414414414141411141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?fff?�33@&ff@fff@�33@�ff@�ff@�  A	��A33A,��AI��Ak33A���A�  A���A���A���Aՙ�A噚A���B  B
  BffBffB"ffB*  B2ffB:  BA��BI��BQ33BY��BbffBk33Bs33Bz��B�33B�  B�33B�33B���B�  B�  B�  B�  B�  B�ffB�ffB�  B�33B�ffB�  B���B���B�ffB���B�  B�33B�33B�33B�33B�  B陚B홚B�ffB�33B�  B�ffC ��CffC�3C��CffC
�3C��C��CffC��C��C��C� C�3C��CL�C ��C"� C$ffC&��C(� C*ffC,�3C.��C0� C2ffC4L�C6��C8��C:�3C<��C>� C@ffCB�3CD��CF��CH� CJffCLffCNL�CP��CRffCTffCV��CX��CZ� C\� C^L�C`� Cb� CdL�Cf��Ch��Cj��Cl� Cn��Cp�3Cr� Ct�3Cv��CxL�CzffC|��C~��C�ffC�s3C�L�C�33C�33C�@ C�L�C�Y�C�Y�C�ffC�L�C�&fC�33C�L�C�Y�C�L�C�33C�@ C�Y�C�ffC�L�C�@ C�Y�C�L�C�33C�L�C�ffC�Y�C�@ C�Y�C�ffC�L�C�33C�L�C�Y�C�@ C�&fC�@ C�@ C�L�C�ffC�L�C�33C�L�C�L�C�Y�C�ffC�ffC�L�C��C�&fC�33C�@ C�L�C�L�C�L�C�L�C�L�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�Y�C�L�C�ffC�ffC�ffC�@ C�@ C�ffC�L�C�&fC�33C�L�C�L�C�Y�C�Y�C�Y�C�ffC�ffC�L�C�&fC�&fC�33C�33C�L�C�L�C�Y�C�Y�C�33C��C��C�&fC�33C�33C�@ C�L�C�Y�C�@ C�&fC�&fC�33C�L�C�ffC�s3C�Y�C�33C�L�C�ffC�L�C�33C�L�C�@ C�&fC�L�C�Y�C�@ C�&fC�L�C�Y�C�ffC��C�@ D &fD ��D33D��D3D� D� D
9�D� Dy�D3D��Dl�D3D��DS3D!��D$��D',�D)�fD,� D/FfD1��D4� D7FfD9��D<� D?&fDA�fDDl�DG�DI��DLS3DN��DQ��DT@ DVٚDYs3D[�3D^s3D`��Dcl�DeٚDh33Dj�3Dm�Do�fDq�3Dts3Dv� DyFfD{L�D}�3D�0 D�s3D���D���D�C3D�� D�ٚD�#3D�ffD���D��D�)�D�c3D���D��3D�fD�6fD�` D��3D���D�� D��D�3D�33D�P D�l�D���D��3D���D��3D���D�3D�fD�0 D�@ D�P D�c3D�y�D��fD���D��fD��3D���D�3D�)�D�L�D�l�D��fD��fD���D�3D�0 D�S3D��3D��3D�� D���D�,�D�c3DĖfD�� D��D�C3D�s3Dʰ D�� D�3D�<�D�y�DЬ�D��fD�fD�C3D�p D֠ D�ɚD���D�)�D�L�D�p Dݐ D޶fD��fD��fD�fD�33D�P D�ffD�|�D� D�fD鹚D��fD���D���D�� D���D��D�  D�0 D�9�D�@ D�I�D�S3D�Y�D�` D�c3D�ffD�L�D�I�D�I�D�L�D�L�E $�E ��E$�E��E$�E��E$�E�fE&fE�fE��E��E0 E	1�E
� E�3ES3E\�Ei�E��E�E�fE��E�3EP Ec3EvfE3E3E�fE�fE��E!>fE">fE#� E$�3E&&fE'� E({3E)�fE++3E,� E-c3E.� E/�3E1C3E2�fE3��E5�E6S3E7��E8ٚE:�E;X E<�3E?�3EBٚEE�3EH�fEL,�EOy�ERffEU��EX� E[� E_ Eb( EeD�Eha�Eka�En��Eq��Et��Ew��E{fE}� E��fE�BfE��3E�FfE��fE�q�E�� E���E�fE�`�E���E��E�G3E��fE��3E�O3E���E���E�, E��fE�ɚE�'3E�ffE��3E�!�E�^fE���E��fE�K3E���E���E�@ E��fE��3E�4 E��fE���G�O�G�O�G�O�G�O�?L��?fffG�O�?L��G�O�G�O�?L��G�O�G�O�?fffG�O�?fffG�O�?�  G�O�?���?���?�ffG�O�?�  ?ٙ�?�33@ff@��@   @333@@  @S33@l��@y��@���@�33@���@���@�ff@�  @�  @ٙ�@�33@�33@���A33A	��A  A  A��A$��A+33A4��A;33A@  AI��AP  AX  A`  AfffG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444411414414414141411141111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ �@ %@ V@ *@ �@ #�@ *S@ /�@ 7L@ >�@ FQ@ R�@ `�@ m�@ z�@ �7@ ��@ ��@ ��@ ��@ �|@ �t@ �@ �q@@�@�@-@:@F�@T�@a�@o�@~�@��@��@��@��@@��@ލ@��@�,@�@{@""@/�@>�@Lu@X�@g@uk@��@��@�U@��@��@��@�O@��@�@�E@
=@�@'�@4�@A�@N�@]�@j@v�@�|@�u@��@�r@��@�@�[@�`@�e@ �@�@�@)�@5?@D�@Q�@^�@m�@z�@��@��@��@�~@�w@�o@�#@�(@� @@@
@-�@:�@H]@UU@bN@o�@|�@��@��@��@��@�>@�7@��@�(@�,@�@@"�@1�@>@K@Z�@g�@t@�@�@��@��@�@�@խ@�@�@��@	�@�@&;@4�@B8@P�@\�@hs@v�@��@�$@�@�f@��@��@�@�`@�Y@^@V@�@)�@8�@E�@Q�@`�@oF@{�@��@��@�5@�~@��@��@�t@��@��@	@	b@	g@	-@	;d@	I�@	Wb@	c�@	n�@	|�@	�D@	��@	��@	��@	�>@	��@	ލ@	��@	��@
1@
�@
#�@
1'@
>�@
Lu@
Z@
g�@
t�@
��@
�h@
�@
��@
�R@
�@
�O@
��@
�@@
�E@
�@B@&�@4�@B�@P�@\�@hs@v@�p@�@�@��@�@��@�[@�@�L@��@�@�@(�@7L@E�@Q�@^5@k�@z3@�7@�<@��@��@�w@�|@܀@��@�@@@[@-@;d@G�@S�@c�@r@�W@��@��@��@�F@Ĝ@�*@܀@�@t@�@j@Ji@�h@��@#�@k�@�9@�9@B8@��@є@�@ff@��@�~@@�@�7@є@�@`A@��@�L@7�@�@�@b@X@�m@�m@.l@r�@��@��@>�@�@�2@v@F�@�7@�o@�@Q�@�u@��@�@UU@�H@��@$�@j@�~@��@>@�@ȴ@J@P�@�u@�\@�@Z@��@�t@ �@ Wb@ �0@ �C@!�@!O0@!��@!ȴ@"v@"A�@"}�@"�@"�@#0x@#k.@#�A@#�H@$O@$V@$�i@$�*@%	�@%E�@%�d@%��@%��@&5@@&s_@&��@&�L@'-�@'l�@'�Y@'�@()�@(j@(��@(�@)(G@)i!@)�Y@)��@*/�@*s_@*��@*�q@+:@+z�@+�j@+��@,?}@,�@,��@-�@-D�@-�@-��@.v@.F�@.�+@.�J@/j@/@�@/�@/�@/��@07�@0t�@0�~@0��@1(G@1b�@1�a@1�@2o@2Ji@2�p@2�&@2�~@32�@3m:@3�A@3��@4�@4P�@4�7@4�2@4�,@50x@5g�@5��@5��@6v@6<�@6s_@6�M@6��@7�@7M$@7��@7�@7��@8(G@8^�@8��@9@9qS@:�@:��@;,`@;�a@<I@<�^@=-@=׹@>I�@>�@?g@?��@@��@A]@Av�@B#�@B�#@C@,@C��@D �@Dȴ@E5�@E�t@FB8@F��@Gz�@G�;@Hs_@I�@I�0@I� @J�@Ko@K�a@L(G@L��@M?}@M�W@NR�@N�#@Oc�@O�@Pr@Q�F@Sg@TZ@U�@W�@X�d@Y��@[-@\ff@]�c@_%�@`z�@a��@c"�@dj@e��@g'�@hZ�@i��@k6@lX�@m��@o$.@p^�@q��@sv@tk.@u�~@w1@xhr@x�@x�q@y-�@ye�@y�^@y�@zF�@z|?@z�|@{j@{S�@{��@{��@|�@|^�@|�r@|�T@}3�@}e	@}�f@}�@~:�@~~K@~ě@	�@N�@��@�#G�O�G�O�G�O�G�O�@ @ �G�O�@ G�O�G�O�@ G�O�G�O�@ �G�O�@ �G�O�@ jG�O�@ @ �@ vG�O�@ �@ 1@ 	�@ 
�@ �@ �@ �@ @ @ �@ 6@ �@ �@ 
@  �@ #�@ %�@ (�@ +@ -@ 0x@ 1�@ 4�@ 7L@ :@ =q@ ?}@ B�@ E�@ I�@ Lu@ N�@ R�@ UU@ X�@ \)@ ^�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A˟�A˥�A˧�A˧�AˬA˩�AˬA˩�AˬA˰!A˰!A˰!AˮA˰!AˮAˮAˬA˰!AˮA˥�AˁA�&�A�p�A�S�A� �A�v�A��mA�r�A�oA���A���A�G�A��PA�bA��A�A�A�p�A�ffA�JA�;dA� �A��9A��A��A��hA�JA��DA���A�G�A���A�^5A��A�9XA���A�hsA�^5A�\)A�\)A�E�A�bA�A�;dA��A��RA���A�/A��A�M�A�VA�O�A�=qA�9XA���A�z�A�oA�I�A��7A�
=A���A���A�VA��A�ZA�/A��A�XA��A�l�A��jA��wA���A�%A���A�A�1'A���A�ƨA�G�A���A�VA�9XA��TA�`BA��A��FA��A�ȴA���A��!A�r�A��`A�5?A��`A�~�A�E�A��A��A�O�A�
=A���A��A~�9A~I�A}�A}p�A{33Ax�jAw��AuAt(�Aq�mAoS�Ak��AiK�Agl�Ae�PAd  AbA`{A^��A\E�AZ�\AXr�AVr�AT{ARbNAQ�APQ�AN�AMO�AL�!AK�AKAJE�AIVAH�AFffAE��AE�AGVAGdZAFn�AD�DAB$�AA�AA/A?G�A<��A;�A;G�A:$�A7�A7hsA7�A6�/A6��A5�A4��A2��A21A1�FA1K�A0�A0��A/"�A.M�A-A+�-A*�`A+A*�jA)��A(5?A&�A$=qA#��A!�A �9AE�A/A�!A�A�A`BA��A��AAĜA-A�wA?}AE�A��AbAx�A��A(�A33A�`AA�A�#AdZA�RAQ�A�;A?}A
ĜA
A�A��Al�A��A$�A��A�A9XA �A  A�AA�A�A�wAC�@��@�{@��@���@�^5@��-@���@���@�bN@�$�@���@���@�K�@�@���@��@���@��@���@��;@�
=@��@��y@��@�F@أ�@�Z@ř�@�`B@��@�l�@��#@�(�@�/@�{@�C�@�j@�bN@�v�@�{@���@�~�@���@���@���@�`B@�\)@�V@�O�@�E�@�r�@���@�"�@���@��@��@�@�?}@�`B@�|�@�ȴ@���@��w@�33@�V@�hs@��9@�  @�dZ@���@��@���@�I�@~�@{�@z�@y�7@w\)@uV@r�@pbN@o;d@n{@mV@k@j=q@iG�@g�@e�-@dj@b�!@aG�@_��@^�+@]�@[��@Z�@Y�^@Y�@VV@U�-@S��@Q�7@O�@Nȴ@M�-@MV@K"�@JM�@H�u@Fv�@E�@Ct�@B~�@A�@?�@>��@>@=/@<(�@;�@9�#@9G�@8bN@7�P@6��@5�-@4�j@3"�@2�!@1�#@1��@1�@0A�@.�y@.ȴ@-�-@-�@,z�@*��@)��@(��@&�y@%�@"�@"-@ ��@  �@ȴ@�@�h@��@1@�@�\@-@��@A�@l�@�@ȴ@5?@/@�j@S�@�\@�@G�@Ĝ@A�@\)@��@@V@�@ƨ@
�!@	��@	��@	%@�u@�w@
=@$�@@/@9X@��@@-@x�@ ��@ �`@ r�@   ?��-?�ƨ?�1'?�ff?�n�?� �?�5??���?�dZ?�X?��?�E�?䛦?��
?���?�7?��`?�;d?�V?��?�1?�~�?���?���?��?�1'?�?�z�?�t�?�-?��`?� �?�|�?���?�5??�O�?�I�?�ƨ?��H?��?��?�b?ǍP?�+?��y?�`B?�t�?�M�?���?���?���?��?���?�1?��?�C�?�dZ?�dZ?�dZ?�"�?��H?���?�^5?���?���?�?�"�?�dZ?�ƨ?�(�?��D?�V?�p�?��?���?���?��?���?���?��?�;d?�\)?�\)?���?���?���?��w?��;?��;?��;?��;?�  ?� �?�A�?�bN?�bN?��?���?�Ĝ?�%?�%?�&�?�G�Aˣ�Aˡ�A˧�Aˡ�Aˡ�Aˡ�A˛�A˛�A˛�A˙�A˛�A˝�A˟�A˝�Aˡ�Aˡ�Aˣ�Aˣ�Aˣ�A˥�Aˣ�Aˣ�A˥�A˧�A˧�A˧�A˧�A˧�A˩�A˧�A˧�A˥�A˧�AˬAˬAˬA˩�A˩�A˩�A˧�A˩�AˬAˬA˩�A˩�AˬAˬAˮA˰!A˰!A˰!A˰!A˰!A˲-A˰!A˰!A˰!A˰!AˮAˮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                A˟�A˥�A˧�A˧�AˬA˩�AˬA˩�AˬA˰!A˰!A˰!AˮA˰!AˮAˮAˬA˰!AˮA˥�AˁA�&�A�p�A�S�A� �A�v�A��mA�r�A�oA���A���A�G�A��PA�bA��A�A�A�p�A�ffA�JA�;dA� �A��9A��A��A��hA�JA��DA���A�G�A���A�^5A��A�9XA���A�hsA�^5A�\)A�\)A�E�A�bA�A�;dA��A��RA���A�/A��A�M�A�VA�O�A�=qA�9XA���A�z�A�oA�I�A��7A�
=A���A���A�VA��A�ZA�/A��A�XA��A�l�A��jA��wA���A�%A���A�A�1'A���A�ƨA�G�A���A�VA�9XA��TA�`BA��A��FA��A�ȴA���A��!A�r�A��`A�5?A��`A�~�A�E�A��A��A�O�A�
=A���A��A~�9A~I�A}�A}p�A{33Ax�jAw��AuAt(�Aq�mAoS�Ak��AiK�Agl�Ae�PAd  AbA`{A^��A\E�AZ�\AXr�AVr�AT{ARbNAQ�APQ�AN�AMO�AL�!AK�AKAJE�AIVAH�AFffAE��AE�AGVAGdZAFn�AD�DAB$�AA�AA/A?G�A<��A;�A;G�A:$�A7�A7hsA7�A6�/A6��A5�A4��A2��A21A1�FA1K�A0�A0��A/"�A.M�A-A+�-A*�`A+A*�jA)��A(5?A&�A$=qA#��A!�A �9AE�A/A�!A�A�A`BA��A��AAĜA-A�wA?}AE�A��AbAx�A��A(�A33A�`AA�A�#AdZA�RAQ�A�;A?}A
ĜA
A�A��Al�A��A$�A��A�A9XA �A  A�AA�A�A�wAC�@��@�{@��@���@�^5@��-@���@���@�bN@�$�@���@���@�K�@�@���@��@���@��@���@��;@�
=@��@��y@��@�F@أ�@�Z@ř�@�`B@��@�l�@��#@�(�@�/@�{@�C�@�j@�bN@�v�@�{@���@�~�@���@���@���@�`B@�\)@�V@�O�@�E�@�r�@���@�"�@���@��@��@�@�?}@�`B@�|�@�ȴ@���@��w@�33@�V@�hs@��9@�  @�dZ@���@��@���@�I�@~�@{�@z�@y�7@w\)@uV@r�@pbN@o;d@n{@mV@k@j=q@iG�@g�@e�-@dj@b�!@aG�@_��@^�+@]�@[��@Z�@Y�^@Y�@VV@U�-@S��@Q�7@O�@Nȴ@M�-@MV@K"�@JM�@H�u@Fv�@E�@Ct�@B~�@A�@?�@>��@>@=/@<(�@;�@9�#@9G�@8bN@7�P@6��@5�-@4�j@3"�@2�!@1�#@1��@1�@0A�@.�y@.ȴ@-�-@-�@,z�@*��@)��@(��@&�y@%�@"�@"-@ ��@  �@ȴ@�@�h@��@1@�@�\@-@��@A�@l�@�@ȴ@5?@/@�j@S�@�\@�@G�@Ĝ@A�@\)@��@@V@�@ƨ@
�!@	��@	��@	%@�u@�w@
=@$�@@/@9X@��@@-@x�@ ��@ �`@ r�@   ?��-?�ƨ?�1'?�ff?�n�?� �?�5??���?�dZ?�X?��?�E�?䛦?��
?���?�7?��`?�;d?�V?��?�1?�~�?���?���?��?�1'?�?�z�?�t�?�-?��`?� �?�|�?���?�5??�O�?�I�?�ƨ?��H?��?��?�b?ǍP?�+?��y?�`B?�t�?�M�?���?���?���?��?���?�1?��?�C�?�dZ?�dZ?�dZ?�"�?��H?���?�^5?���?���?�?�"�?�dZ?�ƨ?�(�?��D?�V?�p�?��?���?���?��?���?���?��?�;d?�\)?�\)?���?���?���?��w?��;?��;?��;?��;?�  ?� �?�A�?�bN?�bN?��?���?�Ĝ?�%?�%?�&�?�G�Aˣ�Aˡ�A˧�Aˡ�Aˡ�Aˡ�A˛�A˛�A˛�A˙�A˛�A˝�A˟�A˝�Aˡ�Aˡ�Aˣ�Aˣ�Aˣ�A˥�Aˣ�Aˣ�A˥�A˧�A˧�A˧�A˧�A˧�A˩�A˧�A˧�A˥�A˧�AˬAˬAˬA˩�A˩�A˩�A˧�A˩�AˬAˬA˩�A˩�AˬAˬAˮA˰!A˰!A˰!A˰!A˰!A˲-A˰!A˰!A˰!A˰!AˮAˮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
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
�B
�3B
�wB
��B:^B��B�B/B5?B9XB;dB;dB>wB8RBD�BF�BJ�BJ�BW
BW
BS�BW
B\)B[#BaHBdZBl�Bq�Bq�Bx�B}�B� B�B�B�=B�PB�VB�hB�{B��B��B��B��B��B��B��B��B��B�\B�=B�7B�%B�B{�Bq�Bm�BjB\)BD�BB�B@�B(�B"�B{B1BB��B��B�B�NB�BȴB��B�^B�B��B��B��B�PB�+B�B�Bz�BffBW
BF�B8RB-B�BPB  B
��B
�5B
��B
ƨB
�B
�uB
s�B
hsB
aHB
M�B
G�B
>wB
<jB
9XB
49B
�B
oB
B	��B	�B	�/B	B	��B	�oB	�B	t�B	jB	\)B	O�B	K�B	>wB	6FB	(�B	�B	
=B��B��B��B�B�yB�mB�`B�TB�;B�/B�
B�
B��B�mB��B	B	B��B�B�B�B�HB�/B�NB�TB�5B�B�B�B��B�B��B��B��BǮBƨBÖBÖBÖB�^B�!B��B�oB�hB�uB�VB�VB�B� B|�Bx�Bu�Bt�Bm�Bl�BiyBiyBiyBgmBgmBaHBbNB\)BW
BS�BO�BL�BK�BL�BQ�BP�BN�BM�BI�BF�BF�BC�BB�B@�B>wB=qB;dB:^B9XB7LB6FB33B33B1'B1'B1'B0!B-B0!B0!B/B.B+B.B-B.B.B-B.B)�B(�B'�B(�B(�B(�B+B+B.B1'B0!B1'B1'B33B2-B2-B33B5?B=qBL�B`BB�DBm�B�B��B��B�B�jB�XB�BB��B�B	PB	bB	�B	 �B	 �B	6FB	N�B	VB	R�B	dZB	gmB	}�B	�B	�PB	�PB	��B	��B	�B	�B	��B	ǮB	ȴB	ȴB	��B	�B	�#B	�5B	�TB	�fB	�mB	�B	�B	�B	��B	��B	��B	��B
B
B
+B
+B
DB
JB
VB
\B
hB
hB
oB
{B
�B
�B
�B
�B
�B
�B
 �B
"�B
#�B
#�B
$�B
(�B
'�B
+B
-B
-B
/B
0!B
1'B
2-B
33B
33B
33B
33B
6FB
6FB
8RB
8RB
:^B
:^B
<jB
>wB
>wB
@�B
A�B
A�B
B�B
B�B
C�B
D�B
F�B
F�B
G�B
J�B
L�B
L�B
L�B
N�B
P�B
P�B
Q�B
R�B
S�B
T�B
W
B
XB
ZB
[#B
\)B
\)B
^5B
_;B
_;B
`BB
aHB
bNB
cTB
cTB
dZB
e`B
ffB
ffB
ffB
gmB
hsB
iyB
jB
k�B
k�B
l�B
l�B
l�B
n�B
n�B
o�B
p�B
p�B
p�B
r�B
r�B
r�B
s�B
t�B
v�B
v�B
x�B
w�B
x�B
y�B
y�B
y�B
z�B
{�B
|�B
|�B
}�B
}�B
~�B
� B
�B
�B
�%B
�1B
�7B
�=B
�DB
�JB
�PB
�PB
�\B
�\B
�hB
�hB
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
�B
�B
�B
�!B
�!B
�-B
�-B
�3B
�9B
�9B
�?B
�9B
�?B
�?B
�FB
�FB
�LB
�LB
�LB
�RB
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
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�^B
�^B
�^B
�XB
�XB
�XB
�^B
�^B
�XB
�^B
�^B
�^B
�^B
�^B
�XB
�^B
�^B
�^B
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
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                B
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
�B
�XB
��B:@B��B�B.�B5#B9<B;IB;IB>]B88BD�BF�BJ�BJ�BV�BV�BS�BV�B\B[Ba4BdFBlxBq�Bq�Bx�B}�B�B�B�
B�.B�BB�HB�[B�oB��B��B��B��B��B��B��B��B��B�UB�7B�1B� B�B{�Bq�Bm�Bj|B\'BD�BB�B@�B(�B"�B|B2BB��B��B�B�RB�BȹB��B�dB�B��B��B��B�XB�4B�B�Bz�BfqBWBF�B8^B-B�B]B B
��B
�DB
��B
ƸB
�B
��B
s�B
h�B
aZB
M�B
G�B
>�B
<}B
9lB
4NB
�B
�B
(B	��B	�B	�FB	¦B	��B	��B	�+B	t�B	j�B	\CB	O�B	K�B	>�B	6bB	)B	�B	
ZB�B�B��B��B�B�B�B�uB�\B�PB�,B�,B��B�B��B	DB	7B�B��B��B��B�pB�WB�wB�}B�_B�:B�:B�/B�*B�0B�%B�B��B��B��B��B��B��B��B�RB�"B��B��B��B��B��B�AB�5B}$ByBu�Bt�Bm�Bl�Bi�Bi�Bi�Bg�Bg�Ba�Bb�B\eBWFBT5BPBMBLBMBR+BQ%BOBNBI�BF�BF�BC�BB�B@�B>�B=�B;�B:�B9�B7�B6�B3{B3{B1pB1qB1qB0lB-YB0mB0mB/hB.aB+PB.bB-]B.cB.dB-^B.eB*MB)HB(BB)IB)IB)JB+VB+WB.jB1}B0xB1~B1B3�B2�B2�B3�B5�B=�BM/B`�B��Bm�B�sB��B�WB��B��B��B��B�zB�B	�B	�B	5B	!WB	!ZB	6�B	OuB	V�B	S�B	d�B	hB	~�B	��B	�B	�B	�EB	��B	��B	��B	�MB	�uB	�~B	ɁB	��B	��B	��B	�B	�/B	�DB	�NB	�iB	�B	�B	��B	��B	��B	��B
B
B
*B
-B
IB
RB
aB
jB
yB
|B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
$ B
%	B
%B
&B
*0B
),B
,AB
.PB
.RB
0bB
1jB
2sB
3{B
4�B
4�B
4�B
4�B
7�B
7�B
9�B
9�B
;�B
;�B
=�B
?�B
?�B
A�B
B�B
CB
DB
DB
EB
F B
H/B
H2B
I;B
LQB
N`B
NcB
NeB
PtB
R�B
R�B
S�B
T�B
U�B
V�B
X�B
Y�B
[�B
\�B
]�B
]�B
_�B
`�B
aB
bB
cB
dB
e%B
e(B
f0B
g9B
hBB
hDB
hGB
iQB
jYB
kbB
ljB
msB
muB
n~B
n�B
n�B
p�B
p�B
q�B
r�B
r�B
r�B
t�B
t�B
t�B
u�B
v�B
x�B
x�B
z�B
y�B
z�B
{�B
{�B
|B
}
B
~B
B
B
�'B
�)B
�4B
�?B
�XB
�dB
�wB
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
�B
�"B
�/B
�4B
�EB
�SB
�RB
�_B
�kB
�~B
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
�B
�B
�B
�"B
�/B
�.B
�IB
�`B
�zB
��B
��B
��B
��B
��B
��B
�B
�&B
�6B
�KB
�TB
�iB
�yB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�*B
�:B
�HB
�XB
�gB
�pB
�nB
�pB
�sB
�wB
�yB
�}B
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
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202492021061413552220210614135522202106171312052021061713120520210617131205201807242202492021061413552220210614135522202106171312052021061713120520210617131205PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            No significant salinity drift detected.                                                                                                                                                                                                                         PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422024920180724220249  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024920180724220249QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422024920180724220249QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021061713145620210617131456IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                