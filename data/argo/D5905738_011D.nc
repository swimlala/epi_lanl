CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-07-24T22:02:33Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        O�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    _�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        c�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        s�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   	H   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   	P   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   	X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   	`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � 	h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   	�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   
   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    
   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        
,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        
4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       
<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    
D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180724220233  20210722160149  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�h��3w@�h��3w11  @�h��L�@�h��L�@6���vݭ@6���vݭ�c֏>�(�c֏>�(11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?   ?fff@   @@  @�33@�  @���@���A��A  A#33AA��Ac33A���A�  A���A���A���A�  A�  A�33B   BffBffB  B ffB(��B0  B8  B@ffBH  BP��BXffB`ffBg��Bo��Bx  B��B���B�  B�33B�ffB�ffB�ffB�33B�33B�  B�  B�33B�  B�33B�ffB�33B�33Bę�B�33B�33B�  Bԙ�B�ffB�  B�  B���B�33B�  B�  B���B�ffB�  C L�C33C  C�fC�fC
�C�C  CL�C�C�C  C  C�fC��C�C   C!�fC$L�C&33C(  C*  C,  C.L�C0L�C233C433C6�C8�C:33C<�C>�C@�CB�CD�CF�CH�CJ�CL  CM�fCP33CR33CT  CV�CX�CZ  C[��C^�C`  Ca�fCd33Cf�Ch�Cj�Ck�fCn�Cp�Cr  Ct33Cv33Cx�Cz  C|  C~L�C��C��C��C��C�  C��C�&fC��C��3C��C�  C��3C��C�  C��fC��C�&fC��C��C�  C��3C��C��C��C��C��3C��C��C�  C�&fC��C�  C��C��C��3C��C�  C��3C��C�&fC�&fC��C��C��C��3C��C�  C��3C��C�  C��3C��C��C��fC�  C�&fC��C��C�&fC��C�  C��3C��3C��C�&fC��C�  C��C��C��3C��C��C�  C��C��C��3C��C�  C��fC��C�  C��3C��C��C��fC��C�  C��3C��C��C�  C�&fC�&fC��C��C��3C��C��C��3C��C��C��3C��C�&fC��C�  C��C��C��fC��C�&fC��C�  C�&fC��C�  C�&fC��C��C�  C��fC��C��D��D,�D�fD
�3D9�D��Dy�D  D� DfD�fDL�D"  D$� D'��D*s3D-L�D0�D2� D5��D8y�D;L�D=�3D@� DCFfDE�3DHl�DJ�3DMY�DOٚDRs3DU  DW�fDZ  D\� D_  Dal�Dc�3Df�fDi�Dk�3Dn�Dp�3DsL�Du��Dx�fD{33D}l�D��D�ffD��fD� D�c3D�� D���D�FfD��fD�ɚD� D�I�D���D��3D��3D�)�D�` D�� D��3D��3D�,�D�\�D���D�� D��3D��D�VfD���D���D�� D�#3D�` D�� D��3D�#3D�ffD���D���D�0 D�s3D��3D��3D�0 D�l�D��fD��fD�#3D�` D��3D�ٚD��D�Y�D��3D���D��fD�&fD�VfDƀ DǦfD�ɚD�� D� D�0 D�S3D�y�DϜ�Dг3D���D��D���D�3D�0 D�FfD�c3D�p DچfD۠ DܶfD���D���D�� D��D�)�D�C3D�P D�ffD�|�D��D�fD�fD��fD�� D��D�,�D�P D�vfD�D��D��3D��D�6fD�Y�D���D��fD�ɚD���D�)�D�VfD�� E Y�E �E� E!�E�3ET�E�fE��E  E��ES3E�3E�3E3E��E	I�E
~fE33ES3EnfE��E!�E1�E9�E�3E�3E3EvfEa�E��E�E\�E�fE�fE � E".fE#� E$�3E&�E'VfE(� E)�E+A�E,!�E-t�E.� E0fE1h E2��E3� E5I�E6L�E7�3E8� E:4�E;#3E<� E?� EB��EF!�EIA�ELP EOX ERVfEU�3EX�fE[�3E^�fEbfEeFfEhffEki�En` Eq��Et� Ew��E{ E~;3E�� E��E���E�:fE�� E�k3E� E�� E�fE��3E�<�E��3E�U�E��fE���E�>fE���E���E�'3E���E��f?   ?   ?   >���?   ?   ?   ?   ?��?   ?333?333?��?L��?fff?fff?���?���?�ff?�  ?���?�33@   @33@&ff@,��@Fff@S33@`  @l��@�33@���@�33@�  @���@�33@���@�ff@�33@���@陚@�ffA   A  A��A33A��A   A(  A+33A1��A8  A>ffAD��AK33AP  AVffA\��Ac33Ah  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144414144114111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ?�  ?�33@   @`  @�33@�  @���@���A	��A  A+33AI��Ak33A���A�  A���A���A���A�  A�  A�33B  B
ffBffB  B"ffB*��B2  B:  BBffBJ  BR��BZffBbffBi��Bq��Bz  B���B���B�  B�33B�ffB�ffB�ffB�33B�33B�  B�  B�33B�  B�33B�ffB�33B�33Bř�B�33B�33B�  Bՙ�B�ffB�  B�  B���B�33B�  B�  B���B�ffB�  C ��C�3C� CffCffC
��C��C� C��C��C��C� C� CffCL�C��C � C"ffC$��C&�3C(� C*� C,� C.��C0��C2�3C4�3C6��C8��C:�3C<��C>��C@��CB��CD��CF��CH��CJ��CL� CNffCP�3CR�3CT� CV��CX��CZ� C\L�C^��C`� CbffCd�3Cf��Ch��Cj��ClffCn��Cp��Cr� Ct�3Cv�3Cx��Cz� C|� C~��C�Y�C�L�C�Y�C�L�C�@ C�L�C�ffC�L�C�33C�Y�C�@ C�33C�L�C�@ C�&fC�L�C�ffC�Y�C�L�C�@ C�33C�Y�C�Y�C�L�C�L�C�33C�L�C�L�C�@ C�ffC�Y�C�@ C�Y�C�Y�C�33C�L�C�@ C�33C�L�C�ffC�ffC�L�C�Y�C�L�C�33C�L�C�@ C�33C�Y�C�@ C�33C�L�C�L�C�&fC�@ C�ffC�Y�C�L�C�ffC�Y�C�@ C�33C�33C�L�C�ffC�L�C�@ C�Y�C�L�C�33C�L�C�L�C�@ C�Y�C�L�C�33C�L�C�@ C�&fC�L�C�@ C�33C�Y�C�L�C�&fC�L�C�@ C�33C�L�C�L�C�@ C�ffC�ffC�Y�C�L�C�33C�L�C�L�C�33C�Y�C�L�C�33C�L�C�ffC�Y�C�@ C�Y�C�L�C�&fC�L�C�ffC�Y�C�@ C�ffC�Y�C�@ C�ffC�L�C�L�C�@ C�&fC�L�C�L�D��DL�DfD
�3DY�D��D��D  D� D&fD�fDl�D"  D$� D'��D*�3D-l�D09�D3  D5��D8��D;l�D>3D@� DCffDE�3DH��DK3DMy�DO��DR�3DU  DW�fDZ  D\� D_  Da��Dd3Df�fDi,�Dk�3Dn9�Dp�3Dsl�Dv�Dx�fD{S3D}��D��D�vfD��fD�  D�s3D�� D��D�VfD��fD�ٚD�  D�Y�D���D��3D�3D�9�D�p D�� D��3D�3D�<�D�l�D���D�� D�3D�,�D�ffD���D�ɚD�  D�33D�p D�� D��3D�33D�vfD���D���D�@ D��3D��3D�3D�@ D�|�D��fD��fD�33D�p D��3D��D�)�D�i�D��3D���D�fD�6fD�ffDƐ DǶfD�ٚD�  D�  D�@ D�c3DΉ�DϬ�D��3D���D���D��D�#3D�@ D�VfD�s3Dـ DږfD۰ D��fD���D���D�  D��D�9�D�S3D�` D�vfD��D��D�fD��fD��fD�  D��D�<�D�` D��fD�D���D��3D��D�FfD�i�D���D��fD�ٚD�	�D�9�D�ffD�� E a�E ��E� E)�E�3E\�E�fE��E( E��E[3E�3E�3E#3E��E	Q�E
�fE;3E[3EvfE��E)�E9�EA�E�3E�3E3E~fEi�E��E�Ed�E�fE fE � E"6fE#� E$�3E&�E'^fE(� E)��E+I�E,)�E-|�E.� E0fE1p E2��E3� E5Q�E6T�E7�3E8� E:<�E;+3E<� E?� EC�EF)�EII�ELX EO` ER^fEU�3EX�fE[�3E^�fEbfEeNfEhnfEkq�Enh Eq��Et� Ew��E{  E~C3E�� E��E���E�>fE�� E�o3E� E�� E�fE��3E�@�E��3E�Y�E��fE� �E�BfE���E���E�+3E���E��fG�O�G�O�G�O�?fffG�O�G�O�G�O�?�  G�O�?�  G�O�G�O�?���?�ffG�O�?�33?���?ٙ�?�ff@   @ff@��@   @333@Fff@L��@fff@s33@�  @�ff@�33@���@�33@�  @���@�33@���@�ff@�33@���@���A33A  A  A��A33A!��A(  A0  A333A9��A@  AFffAL��AS33AX  A^ffAd��Ak33Ap  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444144414144114111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    @ j@ %@ �@ {@ �@ ""@ (G@ /@ 7L@ =q@ E�@ R�@ `�@ m�@ z�@ �7@ ��@ ��@ �~@ �&@ �@ �t@ ��@ �q@j@�@ @,`@:@H]@UU@dZ@qS@~�@�D@��@�A@�9@��@�7@ލ@��@��@1@*@"�@/�@=q@K�@X�@g@uk@�d@�@�@�Y@�@��@խ@�@��@��@	�@�@%�@33@B�@O�@\)@k�@x�@�@�@��@��@�k@�c@�@�`@�@  @�@�@'�@7L@DD@Q=@a�@n�@z�@��@�0@��@��@��@�*@�#@��@� @@�@g@-@:�@H]@V@c�@p�@}�@�P@��@�A@��@�>@�7@܀@�4@�,@%@�@"�@0x@>@Ji@Yn@g@t@�@��@��@��@�R@�@��@��@�L@�E@
=@�@'�@3�@@,@O�@\)@i!@x&@�@�h@�@�!@�@�@�
@�@�@^@V@�@(G@7L@D�@Q�@a�@n�@z�@��@��@�(@�-@�&@�@�#@�(@��@	@	o@	g@	+�@	:�@	G�@	T�@	dZ@	p�@	}�@	��@	�H@	��@	��@	Ĝ@	є@	ލ@	�@	��@
�@
�@
!s@
0x@
?}@
K�@
X�@
g�@
t�@
�@
�@
��@
��@
��@
ƨ@
��@
��@
��@
�9@
�@�@$�@4�@A�@M$@\�@i�@v�@��@�u@�m@�!@��@��@׹@�@�@ �@�@�@)�@5�@D�@S�@`�@m:@|?@�7@��@��@��@��@��@܀@�y@��@v@�@g@,`@8�@H]@V@�@@4�@~�@�@b@V�@��@�@(�@m�@��@��@G�@��@�;@.l@|?@ȴ@{@`�@�f@��@B�@��@�O@�@`�@��@�m@+�@r�@�R@�E@@�@�@�c@�@P�@��@��@ �@e�@��@�@;d@�d@�o@1@Q=@��@�@,`@t�@��@�@I@�P@�C@�@Z�@�@��@"�@e	@�A@�@ )�@ j@ �f@ �@@!.l@!p�@!�-@!�@"4�@"v�@"��@"�,@#:�@#~K@#@$�@$K�@$��@$�\@%�@%_�@%��@%��@&-@&p�@&�9@&� @';d@'~�@'@(�@(I�@(��@(�C@)*@)X@)��@)�h@*B@*X�@*��@*խ@+{@+Q�@+�\@+�|@,J@,Ji@,��@,��@,��@-9X@-t�@-�~@-��@.)�@.b�@.�a@.�t@/�@/Q=@/�D@/��@0�@0?}@0{�@0��@0�L@1+�@1e�@1��@1��@2B@2UU@2�@2ψ@3�@3Lu@3��@3ȴ@4�@4F�@4�|@4Ĝ@5v@5E�@5�W@5�2@6@6B8@6��@6�>@7@7DD@7��@7�W@8�@8Ji@8��@8��@9V@9O�@9��@9є@:o@:S�@:�u@;6@;є@<Lu@<�J@=> @=�4@>`B@>��@?r@?��@@uk@A�@AqS@Bv@B��@C$/@C�9@DC�@D��@E2�@E@FO�@F�#@Ge�@G�Y@H�d@I�@IqS@J@J�\@K	@K�~@LM$@L��@MYn@M�@Nk.@N�
@Or�@O�h@Pp�@Q�7@S0x@T�7@Uލ@W,`@Xww@Y�w@[-�@\l�@]��@_�@`s_@a��@c(G@dqS@e��@gg@h\)@i��@k""@lx�@m@o�@p\�@q� @s""@ti!@u�c@w
=@x^5@y�w@{*@|bN@}��@}�e@~H]@~�W@~��@V@F�@�I@�G�O�G�O�G�O�@ �G�O�G�O�G�O�@ jG�O�@ jG�O�G�O�@ @ vG�O�@ %@ �@ 1@ �@ 
=@ 
�@ �@ �@ �@ �@ o@ *@ �@ �@ B@ �@ 
@ g@ ""@ $�@ &;@ (G@ *S@ -@ /@ 1�@ 4�@ 6�@ :@ <@ >�@ A�@ DD@ G�@ I@ K�@ N�@ Q=@ S�@ V�@ X�@ [z@ ^5@ `�@ b�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA��TA��/A��#A��#A��#A��#A��A��#A��A��#A��A���A���A���A���AʾwA�|�AɋDA���AƁA�ĜA��TA�bNA�K�A�1A��`A�ĜA�~�A�XA�I�A�=qA�A���A��A��9A���A�r�A�7LA���A��A��A�1A�n�A��A�r�A�E�A���A�7LA���A�-A���A���A���A�bA���A�VA��A��A�S�A���A�I�A��wA��wA�`BA��+A��A��A�I�A���A�ffA�A�A�A��A�C�A�ZA�VA��RA���A�JA��;A��hA�hsA�Q�A�A��TA���A���A��A�oA�hsA�\)A���A���A���A�oA��`A�5?A�;dA�A�l�A�1A��A��\A��A�bA�"�A��A�ƨA��DA���A��A�  A�C�A��;A���A��!A�;dA�VA�S�A��DA}
=Aw�-Av{Ar��Ar$�Aq��Ao��AnȴAlbNAj��Aj(�Aix�Ah��Agt�Ae�Ac�A_�PA\  AZ��AZ-AYhsAWx�AT��ARZAQG�AQoAP��AP-AOC�AM�hAKl�AJ�AI��AH9XADM�AA�FA@�A?%A>�DA>r�A>Q�A>5?A>5?A=�mA;
=A:$�A:JA9�#A8�/A6�\A4�9A2��A0��A/�A-��A,��A,~�A,VA,=qA, �A+�mA+��A+&�A)��A)K�A(5?A'��A'�wA'\)A&�HA&^5A%�A$JA"��A ȴA��A�jAƨAr�AXA��A�#A�hA|�Al�AK�A"�A�FA��AVA��A�A`BA?}A��A�;A~�A{A��A?}AffAffA
�jA	7LA5?A��AC�A�A�7AK�A��A�A��A��A��A��A �A|�A =q@���@�Ĝ@���@��w@� �@�1@�Z@�&�@��@�+@�p�@�  @�o@�~�@�X@���@�@�bN@���@�M�@���@�ȴ@�|�@�&�@��@�C�@�Z@�V@��H@�v�@���@��@�ƨ@��`@��@�+@��T@�A�@�\)@��T@���@��9@�;d@�{@��@�hs@�r�@�S�@�M�@�G�@�z�@�  @�K�@�5?@�1'@�;d@��#@�p�@� �@�t�@���@���@���@��@~E�@|Z@|(�@z��@yx�@x  @v��@uV@r�@q��@p  @o�@m�T@kƨ@i��@i&�@g��@f�+@f$�@d9X@b�@`�9@_+@]@\1@Z=q@Xr�@V�@Tj@S"�@P��@O�@M�-@L(�@J�H@I7L@H�u@G;d@E/@D�@C��@A�#@@bN@?�@>��@=�@;��@;��@9��@9&�@8r�@7�P@6ȴ@5�@4��@4��@3�@2�!@0�`@/�@/
=@-`B@,�/@+@)hs@(�9@'�w@&��@%��@$��@#dZ@"�@!�7@!hs@!�@ r�@ Q�@��@�h@�@�m@S�@^5@�@bN@K�@v�@��@�@��@"�@J@�`@bN@bN@�@�@V@p�@�/@�F@
��@
=q@	%@1'@�P@+@v�@5?@�T@O�@�/@�@��@��@~�@J@�@��@%@ 1'?�|�?�V?��?��?�~�?�r�?���?��?��?�%?�bN?��;?�ƨ?��H?�1'?�?��?�\?��`?�V?ۥ�?��?�X?���?׍P?��y?�$�?�?}?�z�?�S�?���?�G�?� �?��?͑h?���?�"�?�~�?���?ə�?��?��?�l�?�
=?��y?�?��?Õ�?���?��?�A�?��?��-?�p�?���?��m?�C�?���?�~�?��?��?��?�~�?�=q?�~�?���?�"�?�dZ?�1?�I�?��?�O�?��?�V?��R?���?�  ?��?��`?��`?��`?�%?�G�?�hs?�G�?�hs?�hsA��HA��TA��HA��TA��TA��TA��mA��TA��`A��TA��TA��`A��`A��`A��`A��`A��TA��`A��HA��#A��;A��#A��#A��#A��A��/A��#A��#A��#A��#A��#A��#A��#A��A��#A��#A��#A��#A��A��A��#A��/A��A��A��A��A��#A��A��#A��A��#A��A��A��A��#A��A��A��
A��
A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    A��TA��TA��/A��#A��#A��#A��#A��A��#A��A��#A��A���A���A���A���AʾwA�|�AɋDA���AƁA�ĜA��TA�bNA�K�A�1A��`A�ĜA�~�A�XA�I�A�=qA�A���A��A��9A���A�r�A�7LA���A��A��A�1A�n�A��A�r�A�E�A���A�7LA���A�-A���A���A���A�bA���A�VA��A��A�S�A���A�I�A��wA��wA�`BA��+A��A��A�I�A���A�ffA�A�A�A��A�C�A�ZA�VA��RA���A�JA��;A��hA�hsA�Q�A�A��TA���A���A��A�oA�hsA�\)A���A���A���A�oA��`A�5?A�;dA�A�l�A�1A��A��\A��A�bA�"�A��A�ƨA��DA���A��A�  A�C�A��;A���A��!A�;dA�VA�S�A��DA}
=Aw�-Av{Ar��Ar$�Aq��Ao��AnȴAlbNAj��Aj(�Aix�Ah��Agt�Ae�Ac�A_�PA\  AZ��AZ-AYhsAWx�AT��ARZAQG�AQoAP��AP-AOC�AM�hAKl�AJ�AI��AH9XADM�AA�FA@�A?%A>�DA>r�A>Q�A>5?A>5?A=�mA;
=A:$�A:JA9�#A8�/A6�\A4�9A2��A0��A/�A-��A,��A,~�A,VA,=qA, �A+�mA+��A+&�A)��A)K�A(5?A'��A'�wA'\)A&�HA&^5A%�A$JA"��A ȴA��A�jAƨAr�AXA��A�#A�hA|�Al�AK�A"�A�FA��AVA��A�A`BA?}A��A�;A~�A{A��A?}AffAffA
�jA	7LA5?A��AC�A�A�7AK�A��A�A��A��A��A��A �A|�A =q@���@�Ĝ@���@��w@� �@�1@�Z@�&�@��@�+@�p�@�  @�o@�~�@�X@���@�@�bN@���@�M�@���@�ȴ@�|�@�&�@��@�C�@�Z@�V@��H@�v�@���@��@�ƨ@��`@��@�+@��T@�A�@�\)@��T@���@��9@�;d@�{@��@�hs@�r�@�S�@�M�@�G�@�z�@�  @�K�@�5?@�1'@�;d@��#@�p�@� �@�t�@���@���@���@��@~E�@|Z@|(�@z��@yx�@x  @v��@uV@r�@q��@p  @o�@m�T@kƨ@i��@i&�@g��@f�+@f$�@d9X@b�@`�9@_+@]@\1@Z=q@Xr�@V�@Tj@S"�@P��@O�@M�-@L(�@J�H@I7L@H�u@G;d@E/@D�@C��@A�#@@bN@?�@>��@=�@;��@;��@9��@9&�@8r�@7�P@6ȴ@5�@4��@4��@3�@2�!@0�`@/�@/
=@-`B@,�/@+@)hs@(�9@'�w@&��@%��@$��@#dZ@"�@!�7@!hs@!�@ r�@ Q�@��@�h@�@�m@S�@^5@�@bN@K�@v�@��@�@��@"�@J@�`@bN@bN@�@�@V@p�@�/@�F@
��@
=q@	%@1'@�P@+@v�@5?@�T@O�@�/@�@��@��@~�@J@�@��@%@ 1'?�|�?�V?��?��?�~�?�r�?���?��?��?�%?�bN?��;?�ƨ?��H?�1'?�?��?�\?��`?�V?ۥ�?��?�X?���?׍P?��y?�$�?�?}?�z�?�S�?���?�G�?� �?��?͑h?���?�"�?�~�?���?ə�?��?��?�l�?�
=?��y?�?��?Õ�?���?��?�A�?��?��-?�p�?���?��m?�C�?���?�~�?��?��?��?�~�?�=q?�~�?���?�"�?�dZ?�1?�I�?��?�O�?��?�V?��R?���?�  ?��?��`?��`?��`?�%?�G�?�hs?�G�?�hs?�hsA��HA��TA��HA��TA��TA��TA��mA��TA��`A��TA��TA��`A��`A��`A��`A��`A��TA��`A��HA��#A��;A��#A��#A��#A��A��/A��#A��#A��#A��#A��#A��#A��#A��A��#A��#A��#A��#A��A��A��#A��/A��A��A��A��A��#A��A��#A��A��#A��A��A��A��#A��A��A��
A��
A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
��B
�}B
��B
��B
��B
�}B
��B
�}B
�}B
��B
��B
��B
��B
B
��B
�BF�B�%B�BBC�BN�BM�BK�BK�BG�BB�B?}B<jB;dB6FBB�BM�BP�BQ�BP�BP�BL�BL�BH�BA�BK�BK�BP�BVBZBXB]/Be`Bl�Bo�B�B�DB�1B�1B�hB�{B��B��B��B�\B�oB�VB�1B�+B�B�B{�Bw�Bl�Be`B_;BS�BN�BO�BYBR�BH�B;dB49B1'B/B$�BhBoB\B%B��B�B�B�PB~�Bn�B[#BJ�B>wB-B)�B"�B�B�B�BoB
��B
�B
ƨB
ŢB
��B
�XB
�-B
��B
��B
��B
��B
�VB
�1B
t�B
]/B
P�B
-B
+B
B	�B	�mB	�TB	�
B	��B	�jB	�-B	�B	��B	��B	��B	�7B	t�B	]/B	N�B	D�B	?}B	7LB	$�B	�B	
=B	%B	B��B��B�B�B�;B�)B�
BɺB�dB�LB�XB�RB�LB�FB�?B�9B�?B�'B��B�B��B��B��B��B��B��B��B��B�{B�{B�uB�oB�hB�bB�\B�bB�\B�hB�\B�oB��B��B��B��B��B�hB�\B�VB�DB�VB�=B�B�B� B{�By�Bx�Bw�Bv�Bt�Br�BjBiyBgmBe`BcTBbNB`BB_;B[#BZBZBXBVBQ�BL�BN�BJ�BK�BH�BB�BE�BD�BD�B@�BA�BA�BB�BB�B=qB?}B=qB=qB=qBB�BG�BO�BR�BVB^5BdZBe`BdZBdZBe`BffBffBbNBbNB8RB6FB>wB?}BG�BM�BT�BaHBffBp�Bt�Bz�B�B�oB��BŢB��B�HB�mB��B��B	DB	�B	(�B	6FB	N�B	VB	s�B	�B	�=B	�{B	��B	��B	��B	�B	�9B	�qB	ĜB	��B	��B	�B	�B	�;B	�ZB	�`B	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
%B
1B

=B
JB
\B
hB
{B
{B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
%�B
&�B
(�B
+B
,B
/B
0!B
2-B
49B
5?B
6FB
7LB
8RB
:^B
9XB
;dB
=qB
>wB
>wB
@�B
A�B
B�B
A�B
D�B
D�B
D�B
G�B
G�B
I�B
I�B
I�B
J�B
K�B
M�B
M�B
O�B
P�B
Q�B
R�B
T�B
T�B
W
B
W
B
XB
YB
[#B
\)B
\)B
[#B
\)B
_;B
]/B
`BB
aHB
aHB
cTB
cTB
dZB
e`B
ffB
gmB
hsB
jB
iyB
k�B
k�B
m�B
m�B
m�B
n�B
o�B
p�B
p�B
q�B
q�B
s�B
s�B
s�B
t�B
t�B
u�B
v�B
w�B
x�B
x�B
x�B
y�B
z�B
z�B
|�B
|�B
|�B
|�B
}�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�%B
�+B
�=B
�DB
�=B
�JB
�PB
�bB
�bB
�oB
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�!B
�!B
�-B
�3B
�3B
�?B
�?B
�FB
�LB
�LB
�RB
�RB
�RB
�XB
�XB
�XB
�^B
�^B
�^B
�^B
�dB
�dB
�dB
�dB
�dB
�^B
�dB
�dB
�dB
�dB
�jB
�jB
�dB
�dB
�jB
�jB
�jB
��B
��B
��B
��B
��B
��B
��B
�}B
��B
��B
B
��B
��B
��B
��B
��B
�qB
�}B
��B
��B
�}B
��B
��B
��B
��B
�}B
��B
��B
��B
�}B
��B
�}B
�}B
��B
��B
��B
��B
��B
��B
��B
��B
�}B
��B
�}B
��B
��B
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
��B
��B
��B
��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    B
��B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
�wB
�}B
�wB
�wB
�}B
��B
��B
�}B
��B
ɺB
�BE�B�B�;BB�BM�BL�BJ�BJ�BF�BA�B>wB;dB:^B5?BA�BL�BO�BP�BO�BO�BK�BK�BG�B@�BJ�BJ�BO�BT�BYBW
B\)BdZBk�Bn�B�B�=B�+B�+B�bB�uB��B��B��B�VB�hB�PB�+B�%B�B� Bz�Bv�Bk�BdZB^5BR�BM�BN�BXBQ�BG�B:^B33B0!B.B#�BbBhBVBB��B�
B�B�JB}�Bm�BZBI�B=qB,B(�B!�B�B�B�BhB
��B
�B
ŢB
ĜB
�}B
�RB
�'B
��B
��B
��B
��B
�PB
�+B
s�B
\)B
O�B
,B
%B
B	�B	�fB	�NB	�B	��B	�dB	�'B	�B	��B	��B	��B	�1B	s�B	\)B	M�B	C�B	>wB	6FB	#�B	�B		7B	B	B��B��B�B�B�5B�#B�BȴB�^B�FB�RB�LB�FB�?B�9B�3B�9B�!B��B��B��B��B��B��B��B��B��B�{B�uB�uB�oB�hB�bB�\B�VB�\B�VB�bB�VB�hB�{B��B��B��B�{B�bB�VB�PB�=B�PB�7B�B�B~�Bz�Bx�Bw�Bv�Bu�Bs�Bq�BiyBhsBffBdZBbNBaHB_;B^5BZBYBYBW
BT�BP�BK�BM�BI�BJ�BG�BA�BD�BC�BC�B?}B@�B@�BA�BA�B<jB>wB<jB<jB<jBA�BF�BN�BQ�BT�B]/BcTBdZBcTBcTBdZBe`Be`BaHBaHB7LB5?B=qB>wBF�BL�BS�B`BBe`Bo�Bs�By�B�B�hB��BĜB��B�BB�fB�B��B	
=B	�B	'�B	5?B	M�B	T�B	r�B	�B	�7B	�uB	��B	��B	��B	�B	�9B	�qB	ĜB	��B	��B	�B	�B	�;B	�ZB	�`B	�B	�B	�B	�B	��B	��B	��B	��B
  B
B
B
%B
1B

=B
JB
\B
hB
{B
{B
�B
�B
�B
�B
�B
�B
�B
!�B
"�B
%�B
&�B
(�B
+B
,B
/B
0!B
2-B
49B
5?B
6FB
7LB
8RB
:^B
9XB
;dB
=qB
>wB
>wB
@�B
A�B
B�B
A�B
D�B
D�B
D�B
G�B
G�B
I�B
I�B
I�B
J�B
K�B
M�B
M�B
O�B
P�B
Q�B
R�B
T�B
T�B
W
B
W
B
XB
YB
[#B
\)B
\)B
[#B
\)B
_;B
^5B
aHB
bNB
bNB
dZB
dZB
e`B
ffB
gmB
hsB
iyB
k�B
jB
l�B
l�B
n�B
n�B
n�B
o�B
p�B
q�B
q�B
r�B
r�B
t�B
t�B
t�B
u�B
u�B
v�B
w�B
x�B
y�B
y�B
y�B
z�B
{�B
{�B
}�B
}�B
}�B
}�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�DB
�JB
�DB
�PB
�VB
�hB
�hB
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
�B
�B
�B
�B
�B
�B
�!B
�'B
�-B
�-B
�9B
�?B
�FB
�RB
�RB
�XB
�^B
�^B
�dB
�dB
�dB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�wB
�}B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
�}B
��B
��B
�}B
�}B
��B
��B
��B
��B
��B
��B
�}B
��B
��B
�}B
�wB
��B
�}B
��B
�}B
��B
�}B
�}B
�}B
�jB
�wB
�}B
��B
�wB
��B
�}B
�}B
�}B
�wB
�}B
�}B
�}B
�wB
�}B
�wB
�wB
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�}B
�wB
�}B
�wB
�}B
�}B
�wB
�wB
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
�}B
�}G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201807242202332021061413521120210614135211202106141746202021061417462020210614174620201807242202332021061413521120210614135211202106141746202021061417462020210614174620PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.001 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018072422023320180724220233  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023320180724220233QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018072422023320180724220233QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216014920210722160149IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                