CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-08-03T13:35:00Z creation      
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
resolution        =���     �  f   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  v�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 8  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   4   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    <   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        \   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        d   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       l   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20180803133500  20210722160151  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL                  DD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�t[����@�t[����11  @�t[��]`@�t[��]`@6�����?@6�����?�c�>�'�!�c�>�'�!11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  >���?�  @ff@@  @�33@�  @���@�33A��A��A&ffA@  Aa��A���A���A���A�  A���A�ffAᙚA���B   BffB33B��B ffB(ffB0ffB8ffB@ffBHffBP��BX��B`ffBh  Bp  Bx  B�33B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�33B�  B�  B�33B���B�  B���B�  B�ffB�33B�  B���B���B�ffB�ffB�33B�33B�33B�  B�  C   C  C  C  C�C
33CL�C�fC  C�C�C33C33C33C33C33C �C"  C#�fC&  C'�fC)�fC+�fC.L�C0L�C2  C4  C5�fC8�C:L�C<33C>  C@33CB�CC�fCF  CG�fCI��CL  CN�CPL�CR33CS�fCV�CXffCZ�C[�fC^  C`33Cb33Cd�Ce�fCh  Cj33Cl�Cm��Cp�Cq�fCs��Cv�Cw�fCy�fC|  C~  C�fC�  C�&fC��C�  C�  C��fC��C��3C��3C��C�  C��3C��C��C��3C�  C��C�&fC��C��3C��C�&fC��C��3C�  C��C�&fC�&fC�  C�ٚC�ٚC��fC��fC��3C�  C��C��C�&fC��C�  C��C��3C��3C�  C��C��C�&fC�  C��fC��3C��3C�  C��C�&fC�&fC�&fC��C��fC��3C�  C��C�&fC�33C��C�  C��C�  C��fC�  C��C�  C��3C��C��C�  C�  C��3C��C�  C��3C��C�  C��fC��C��C��3C��C��C�  C�  C��fC��C�  C��3C�  C��3C��fC��C�  C��3C��C��C��C�  C�  C�  C��3C��C�&fC��C��C�  C��C�  C��3C��C�  C��fC�  C��3C�ٚC�  D� DfD��D
�D��DfDs3D��DL�D�fD,�D� D &fD"�3D%Y�D(fD*��D-9�D/ٚD2� D5&fD7�fD:ffD=fD?�3DB33DD��DGS3DI��DL� DOfDQ�fDT&fDV�3DY@ D[�fD^L�D`��DcY�De�fDhs3Dj�3Dm� Do��Drl�DtٚDw9�Dy� D{��D}�3D�,�D�L�D�s3D��fD���D��3D��D�<�D�s3D�� D��3D�3D�9�D�p D��3D��fD��D�Y�D���D���D��D�P D�� D���D�	�D�<�D�� D�� D���D�@ D�y�D��fD��fD�0 D�s3D���D��D�&fD�` D���D��3D�fD�@ D�p D�� D��fD��3D��D�P D�y�D�� D��3D�� D���D��D�33D�I�D�ffDŃ3Dƣ3Dǹ�D�� D��D���D� D��D�0 D�@ D�L�D�S3D�\�D�l�D�|�DՉ�D֓3Dנ Dج�Dٰ DڶfD۹�D��fD�� D���D��3D�ٚD��3D��fD�� D���D��D��D�&fD�6fD�I�D�c3D�y�D�3D��D��3D�� D��fD�fD�9�D�S3D�p D���D���D��3D���D��3D��fD���D�fE  E ��E33E� EL�EٚEffE�fE{3E�fE�3E�3E�E
�3E�3E�fE� E��E�fE�3EI�EVfEd�E�3E�Ed�ET�E�3E3EffE�fE � E!��E#�E$X E%��E&�fE(I�E)�3E*��E+� E-` E.P E/� E1$�E2fE3� E4�fE5��E7@ E8��E9�fE:�3E<@ E?t�EB��EEњEH�3EK�fENٚERA�EUC3EXNfE[d�E^x Ea� Ed�Eh$�Ek1�En3Eq@ Et��Ewc3Ez�3E}�3E�3E��E���E�*fE�� E�:fE�� E�%�E�h�E���E��E�Y�E��fE��E�C3E���E�� E�H E���E�� E�3E�w3E�� E�3E�a�E��fE�
fE�\�E���E�� E�JfE��3E��E�.fE�{3E�ɚE�0�E���E���E�3E�ffE��3E��E�X�E�� E��f>���>���>���>���>���?   >���>���?   >���>���>���>���?   ?��?��?333?L��?�  ?���?���?�33?�  ?�ff@   @ff@   @&ff@9��@L��@Y��@s33@�  @���@�ff@���@���@�33@���@ə�@�ff@�33@�  @���A��A33A��A  A   A$��A,��A1��A8  A@  AH  ANffAT��A^ffAc33Al��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414414444114111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ?fff?�  @&ff@`  @�33@�  @���@�33A	��A��A.ffAH  Ai��A���A���A���A�  A���A�ffA噚A���B  B
ffB33B��B"ffB*ffB2ffB:ffBBffBJffBR��BZ��BbffBj  Br  Bz  B�33B�33B�  B�  B�  B�  B���B���B�  B�  B�  B�33B�33B�  B�  B�33B���B�  B���B�  B�ffB�33B�  B���B���B�ffB�ffB�33B�33B�33B�  B�  C � C� C� C� C��C
�3C��CffC� C��C��C�3C�3C�3C�3C�3C ��C"� C$ffC&� C(ffC*ffC,ffC.��C0��C2� C4� C6ffC8��C:��C<�3C>� C@�3CB��CDffCF� CHffCJL�CL� CN��CP��CR�3CTffCV��CX�fCZ��C\ffC^� C`�3Cb�3Cd��CfffCh� Cj�3Cl��CnL�Cp��CrffCtL�Cv��CxffCzffC|� C~� C�33C�@ C�ffC�Y�C�@ C�@ C�&fC�L�C�33C�33C�Y�C�@ C�33C�L�C�L�C�33C�@ C�L�C�ffC�L�C�33C�L�C�ffC�Y�C�33C�@ C�Y�C�ffC�ffC�@ C��C��C�&fC�&fC�33C�@ C�Y�C�Y�C�ffC�L�C�@ C�L�C�33C�33C�@ C�L�C�Y�C�ffC�@ C�&fC�33C�33C�@ C�L�C�ffC�ffC�ffC�L�C�&fC�33C�@ C�L�C�ffC�s3C�Y�C�@ C�Y�C�@ C�&fC�@ C�L�C�@ C�33C�Y�C�L�C�@ C�@ C�33C�L�C�@ C�33C�L�C�@ C�&fC�L�C�L�C�33C�Y�C�L�C�@ C�@ C�&fC�L�C�@ C�33C�@ C�33C�&fC�L�C�@ C�33C�Y�C�Y�C�L�C�@ C�@ C�@ C�33C�Y�C�ffC�Y�C�L�C�@ C�Y�C�@ C�33C�L�C�@ C�&fC�@ C�33C��C�@ D� D&fD��D
9�D��D&fD�3D��Dl�D�fDL�D� D FfD"�3D%y�D(&fD*��D-Y�D/��D2� D5FfD7�fD:�fD=&fD?�3DBS3DD��DGs3DJ�DL� DO&fDQ�fDTFfDV�3DY` D[�fD^l�D`��Dcy�DffDh�3Dk3Dm� Dp�Dr��Dt��DwY�Dy� D{��D~3D�<�D�\�D��3D��fD�ɚD��3D��D�L�D��3D�� D��3D�3D�I�D�� D��3D��fD�)�D�i�D���D���D�,�D�` D�� D���D��D�L�D�� D�� D��D�P D���D��fD�fD�@ D��3D���D���D�6fD�p D���D��3D�fD�P D�� D�� D��fD�3D�,�D�` D���D�� D��3D�� D�	�D�)�D�C3D�Y�D�vfDœ3DƳ3D�ɚD�� D���D��D�  D�,�D�@ D�P D�\�D�c3D�l�D�|�DԌ�Dՙ�D֣3Dװ Dؼ�D�� D��fD�ɚD��fD�� D���D��3D��D��3D��fD�  D�	�D��D�,�D�6fD�FfD�Y�D�s3D쉚D��3D��D��3D�� D�fD�&fD�I�D�c3D�� D���D���D��3D���D��3D��fD��D�&fE   E ��E;3E� ET�E�EnfE�fE�3E�fE�3E�3E�E
�3E�3E�fE� E��E�fE�3EQ�E^fEl�E�3E�El�E\�E�3E3EnfE�fE � E!��E#�E$` E%��E&�fE(Q�E)�3E*��E,  E-h E.X E/� E1,�E2&fE3� E4�fE5��E7H E8��E:fE:�3E<H E?|�EB��EEٚEH�3EK�fEN�ERI�EUK3EXVfE[l�E^� Ea� Ed��Eh,�Ek9�En3EqH Et��Ewk3Ez�3E}�3E��3E��E���E�.fE�� E�>fE�� E�)�E�l�E���E��E�]�E��fE��E�G3E���E�� E�L E���E�� E�3E�{3E�� E�3E�e�E��fE�fE�`�E���E�  E�NfE��3E��E�2fE�3E�͚E�4�E���E���E�3E�jfE��3E��E�\�E�� E��fG�O�G�O�G�O�G�O�?L��G�O�G�O�?fffG�O�G�O�G�O�G�O�?fff?�  G�O�?���?���?�ff?�  ?���?ٙ�?�33@   @33@   @&ff@@  @Fff@Y��@l��@y��@���@�  @���@�ff@���@���@�33@���@ٙ�@�ff@�33A   A��A��A33A��A   A(  A,��A4��A9��A@  AH  AP  AVffA\��AfffAk33At��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111444414414444114111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                @ �@ �@ V@ {@ �@ ""@ (G@ 0x@ 7L@ >@ F�@ Q�@ `B@ m�@ |?@ �7@ �0@ ��@ ��@ ��@ �|@ �t@ ��@ ��@�@�@g@-@:�@H]@V@dZ@r@~�@��@��@�A@��@�>@�7@��@�@�,@%@�@""@/�@=q@K�@Yn@ff@t@�d@��@�@��@�R@�W@�O@�H@�@@��@�@B@&;@3�@A�@N�@\)@i�@ww@�@��@�@�r@��@ȴ@�
@�`@�@^@@�@*S@7�@D�@Q�@^�@m:@z3@��@��@��@��@�&@��@��@��@��@�@@ @-@9X@G�@T�@a�@p�@~�@��@��@��@��@�J@��@�/@�@��@1@*@!s@/�@>�@K�@Wb@g@s_@�W@�@�U@��@�R@��@��@�H@��@��@
=@�@$.@3�@@,@M�@]�@i�@v�@��@�u@��@�@�k@�o@׹@�@�@@@�@(�@7�@FQ@S�@_�@k.@x�@�+@��@�(@�~@��@�*@܀@��@��@	@	b@	
@	,`@	:�@	I@	Wb@	b�@	oF@	}�@	�D@	��@	��@	��@	Ĝ@	�C@	ލ@	�(@	�~@
�@
*@
$.@
2�@
>�@
K@
Z@
ff@
r�@
��@
�@
�@
��@
��@
ƨ@
Ӡ@
�H@
�@@
�E@
=@6@&;@33@?}@O0@\�@i!@x�@��@��@�m@��@�k@�c@�[@�@�@��@V@O@(G@7�@E�@R�@_�@m:@z�@��@��@��@��@��@��@��@�@�@@@[@,`@9X@E�@UU@�@0x@uk@�@��@A�@��@��@1@K�@�P@�7@*@Z�@�(@�4@2�@z3@��@
=@R�@�H@��@)�@oF@��@��@B�@�D@�7@*@Yn@�@�@,`@qS@�F@��@@,@��@�o@�@UU@��@��@
@^�@�m@�\@�@X@��@�O@o@P�@�@ψ@b@R�@��@�O@*@Wb@��@�#@�@a�@��@��@ /@ s_@ ��@ �,@!<�@!�W@!��@"�@"K@"��@"Ӡ@#�@#Z@#�a@#�H@$&;@$i!@$��@$�L@%33@%v@%�@%��@&=q@&~K@&�&@&��@'>@'}�@'�&@'��@(=q@({�@(�R@(�e@)1�@)m�@)�M@)�@*"�@*`A@*��@*�
@+@+M�@+��@+��@+��@,6�@,o�@,��@,��@-�@-T�@-��@-ƨ@.  @.9X@.p�@.��@.��@/B@/Q�@/��@/��@/��@00x@0g�@0�m@0�@1�@1M�@1�|@1��@1�9@27L@2r�@2��@2��@3&;@3b�@3�a@3��@4�@4V@4��@4��@5	�@5D�@5}�@5�@5�@6/@6k.@6�A@6�T@7 @7\)@7�<@7�O@8b@8Ji@8�|@8�Q@9x�@9�L@:j@; @;��@<b@<��@=:�@=��@>#�@>׹@?Ji@?��@@dZ@@��@Ar�@A�@Bn�@C�@C��@D!s@D~K@E�@E�i@F�@F�M@G9X@G�|@H`�@Hƨ@I_�@I�,@J_�@J�,@K��@K�Q@L��@M2�@M�<@N/�@NĜ@O[z@O��@PQ�@Q�!@SJ@Tg@U��@V��@XA�@Y��@Z��@\Ji@]��@^�@`@�@a��@cJ@dYn@e�#@f��@hS�@i�P@k]@l>�@m��@n�<@pE�@q�y@r��@t?}@u��@u�@v�@vV@v�r@v�y@w$.@wx�@w��@x�@x=q@x�\@x�>@y@yC�@y�@y�H@z�@zZ@z�y@z�'@{0x@{ul@{�R@{�8@|<�@||?@|��@|�Q@}B8@}�I@}ލ@~&;@~X@~�y@~��@0x@qS@�~@��G�O�G�O�G�O�G�O�@ G�O�G�O�@ �G�O�G�O�G�O�G�O�@ �@ jG�O�@ @ �@ v@ �@ �@ 1@ 	�@ 
=@ J@ �@ V@ @ �@ �@ �@ 6@ �@ O@ 
@  @ !s@ $.@ &;@ (G@ +@ -�@ 0x@ 33@ 5?@ 8�@ ;d@ >@ @�@ DD@ FQ@ I�@ K�@ N�@ Q�@ UU@ X@ Z�@ ^�@ `�@ e	G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aя\Aљ�Aљ�Aћ�Aћ�Aћ�Aћ�Aѕ�Aї�Aї�Aї�AѓuAѓuAѓuAћ�Aї�Aѕ�Aѕ�AѓuA�l�A�I�A�7LA��A��A��A��A��A��A��A�{A�bA�VA�A�l�A���A�bA�=qA���A���A�A�$�A�hsA�^5A��!A��`A�v�A��`A���A���A�Q�A�hsA�=qA�  A��;A���A�r�A�A�A��A�XA�bA���A��7A�jA��A��hA��\A��
A���A�=qA���A�I�A��A�1A��+A��A��
A�hsA�A���A�33A��A�-A���A�z�A�Q�A��mA��PA�$�A���A�E�A�|�A�l�A��7A��DA�+A�ƨA��TA�K�A��A��A�C�A�VA���A�XA���A��A�t�A�-A�;dA�ƨA�  A�A�A��mA��/A���A�x�A�XA��/A�  A��!A��A��^A�VA��9A}�
Az��AzQ�Ay�;Aw��Au;dAtI�Aq+Ao�FAn{AjM�Ah�HAh�\Ag�-Ae�Aa��A`ȴA_�hA[�AY�PAW�7AVA�AU7LAS7LAQK�AM��AL�jAJ�\AI"�AG�
AF~�AE\)AB�9A@��A@{A>JA:��A8�A6~�A4��A4A�A3��A21'A0�jA/?}A.�\A.  A.A�A. �A.bA-��A,1'A*M�A((�A%��A$�HA$�/A$bNA$(�A#��A!�A!K�A �DA   AXAZAXAjA��Ax�A�DA`BA�A��A�A��AI�AI�AVA��A+A�\A��A�7A�RAM�A
=A-A?}A�RA1'A"�A	�#A	"�AffA�AS�A�A�A��AƨAA��At�A+A�
A�wAG�A ĜA ��@��
@�^5@��@���@��@�1'@�
=@�v�@���@��@�7L@���@�P@���@��@�p�@�O�@���@�v�@�9@�S�@߅@��@�\)@� �@ϥ�@��y@�b@�%@�^5@�1'@��R@�ȴ@��j@�1@��D@�+@��@��@���@�33@�?}@��u@��@�o@�M�@��@���@���@���@���@���@��@��@�O�@��9@��@��#@��@�"�@���@���@���@�@�hs@��@�K�@��+@�ff@�p�@|�@|��@y��@x��@z-@w|�@w\)@v�@uO�@s@q�#@pA�@nE�@l9X@jM�@i��@iG�@fȴ@eV@c��@a&�@`b@_\)@^�@]p�@[��@Z=q@X�u@V��@U�-@Sƨ@Q�^@P�9@O\)@M`B@L(�@I��@H  @F�y@Ep�@D�@B��@Ax�@?�@=��@<��@;��@:�!@9��@8��@8bN@7�P@6ff@5O�@4I�@3�
@2n�@1�@0��@/�@/\)@.5?@.@-�T@,�@,�D@+t�@*��@)��@(Ĝ@'��@'\)@&@%��@$�/@#��@#t�@"~�@!x�@ �u@l�@5?@��@�j@t�@n�@��@�u@��@�y@��@ff@�T@�/@z�@��@~�@J@%@��@r�@  @+@��@�T@�@��@j@(�@�
@
�@	��@�u@ �@��@��@ff@{@�@��@1@dZ@�!@��@ Ĝ?�5??��?���?��#?�Q�?�
=?�F?�t�?�M�?�%??��?�V?�"�?�x�?�1'?�?��
?�7?�\)?�{?�dZ?�x�?��?�E�?�`B?�o?Ұ!?�M�?���?Ѓ?� �?� �?�\)?�5??��?�C�?��H?��#?�7L?�Q�?���?�
=?���?�Z?�33?�-?�&�?���?���?�{?��h?�p�?�j?�1?�"�?�C�?�dZ?�dZ?�?�"�?�?�?�"�?���?�(�?�(�?��?�V?��-?�5??�V?���?���?��R?��?���?��?�\)?�\)?�|�?���?��w?�  ?�  ?�A�?�A�?��?���?���?��`?��`?�&�?�G�?��7?��7?���?��?�J?�-?�M�?\?\?���?�o?�o?�33?�S�?Õ�?Õ�Aя\AёhAёhAёhAя\AэPAѓuAёhAэPAыDAыDAэPAэPAэPAёhAљ�Aћ�Aѝ�Aћ�Aћ�Aї�Aљ�Aљ�Aљ�Aљ�Aљ�Aљ�Aћ�Aћ�Aћ�Aћ�Aћ�Aћ�Aљ�Aћ�Aѝ�Aћ�Aћ�Aћ�Aљ�Aѕ�Aѕ�Aї�Aї�Aѕ�Aї�Aї�Aѕ�Aљ�Aљ�Aѕ�Aѕ�AѓuAѓuAѓuAёhAѓuAёhAёhAѓuG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                Aя\Aљ�Aљ�Aћ�Aћ�Aћ�Aћ�Aѕ�Aї�Aї�Aї�AѓuAѓuAѓuAћ�Aї�Aѕ�Aѕ�AѓuA�l�A�I�A�7LA��A��A��A��A��A��A��A�{A�bA�VA�A�l�A���A�bA�=qA���A���A�A�$�A�hsA�^5A��!A��`A�v�A��`A���A���A�Q�A�hsA�=qA�  A��;A���A�r�A�A�A��A�XA�bA���A��7A�jA��A��hA��\A��
A���A�=qA���A�I�A��A�1A��+A��A��
A�hsA�A���A�33A��A�-A���A�z�A�Q�A��mA��PA�$�A���A�E�A�|�A�l�A��7A��DA�+A�ƨA��TA�K�A��A��A�C�A�VA���A�XA���A��A�t�A�-A�;dA�ƨA�  A�A�A��mA��/A���A�x�A�XA��/A�  A��!A��A��^A�VA��9A}�
Az��AzQ�Ay�;Aw��Au;dAtI�Aq+Ao�FAn{AjM�Ah�HAh�\Ag�-Ae�Aa��A`ȴA_�hA[�AY�PAW�7AVA�AU7LAS7LAQK�AM��AL�jAJ�\AI"�AG�
AF~�AE\)AB�9A@��A@{A>JA:��A8�A6~�A4��A4A�A3��A21'A0�jA/?}A.�\A.  A.A�A. �A.bA-��A,1'A*M�A((�A%��A$�HA$�/A$bNA$(�A#��A!�A!K�A �DA   AXAZAXAjA��Ax�A�DA`BA�A��A�A��AI�AI�AVA��A+A�\A��A�7A�RAM�A
=A-A?}A�RA1'A"�A	�#A	"�AffA�AS�A�A�A��AƨAA��At�A+A�
A�wAG�A ĜA ��@��
@�^5@��@���@��@�1'@�
=@�v�@���@��@�7L@���@�P@���@��@�p�@�O�@���@�v�@�9@�S�@߅@��@�\)@� �@ϥ�@��y@�b@�%@�^5@�1'@��R@�ȴ@��j@�1@��D@�+@��@��@���@�33@�?}@��u@��@�o@�M�@��@���@���@���@���@���@��@��@�O�@��9@��@��#@��@�"�@���@���@���@�@�hs@��@�K�@��+@�ff@�p�@|�@|��@y��@x��@z-@w|�@w\)@v�@uO�@s@q�#@pA�@nE�@l9X@jM�@i��@iG�@fȴ@eV@c��@a&�@`b@_\)@^�@]p�@[��@Z=q@X�u@V��@U�-@Sƨ@Q�^@P�9@O\)@M`B@L(�@I��@H  @F�y@Ep�@D�@B��@Ax�@?�@=��@<��@;��@:�!@9��@8��@8bN@7�P@6ff@5O�@4I�@3�
@2n�@1�@0��@/�@/\)@.5?@.@-�T@,�@,�D@+t�@*��@)��@(Ĝ@'��@'\)@&@%��@$�/@#��@#t�@"~�@!x�@ �u@l�@5?@��@�j@t�@n�@��@�u@��@�y@��@ff@�T@�/@z�@��@~�@J@%@��@r�@  @+@��@�T@�@��@j@(�@�
@
�@	��@�u@ �@��@��@ff@{@�@��@1@dZ@�!@��@ Ĝ?�5??��?���?��#?�Q�?�
=?�F?�t�?�M�?�%??��?�V?�"�?�x�?�1'?�?��
?�7?�\)?�{?�dZ?�x�?��?�E�?�`B?�o?Ұ!?�M�?���?Ѓ?� �?� �?�\)?�5??��?�C�?��H?��#?�7L?�Q�?���?�
=?���?�Z?�33?�-?�&�?���?���?�{?��h?�p�?�j?�1?�"�?�C�?�dZ?�dZ?�?�"�?�?�?�"�?���?�(�?�(�?��?�V?��-?�5??�V?���?���?��R?��?���?��?�\)?�\)?�|�?���?��w?�  ?�  ?�A�?�A�?��?���?���?��`?��`?�&�?�G�?��7?��7?���?��?�J?�-?�M�?\?\?���?�o?�o?�33?�S�?Õ�?Õ�Aя\AёhAёhAёhAя\AэPAѓuAёhAэPAыDAыDAэPAэPAэPAёhAљ�Aћ�Aѝ�Aћ�Aћ�Aї�Aљ�Aљ�Aљ�G�O�G�O�Aљ�Aћ�Aћ�Aћ�Aћ�Aћ�Aћ�Aљ�Aћ�Aѝ�Aћ�Aћ�Aћ�Aљ�Aѕ�Aѕ�Aї�Aї�Aѕ�Aї�Aї�Aѕ�Aљ�Aљ�Aѕ�Aѕ�AѓuAѓuAѓuAёhAѓuAёhAёhAѓuG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BBB
��BBBBBBBBBBBBBBBBB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B  B	7BVBr�B�B�BoB?}BN�BXBcTBm�BT�BYBR�BA�B;dB5?B0!B;dB:^B9XB:^B9XB;dB>wBC�BF�BH�BH�BJ�BJ�BQ�BZBZBXBZB^5B[#BB�B>wB:^B9XB;dB>wBA�BG�BK�BaHBm�Bk�Br�Bq�Bt�By�Br�Bn�BcTBF�BE�B(�BoB��B��B�LB��B�=B�Bz�B�%B�B� B�uB�=BffB;dBB�B+B
��B
�B
�ZB
��B
�B
�
B
�}B
��B
��B
�PB
y�B
o�B
l�B
gmB
]/B
8RB
)�B
 �B
�B	��B	�fB	�HB	ɺB	�}B	�B	��B	�bB	�JB	�B	q�B	Q�B	L�B	?}B	 �B	bB		7B	B��B�yB�BB�
B��B�FB�?B��B��B��B�bB�JB�%By�BgmBaHB_;B[#B[#BW
BW
BS�BR�BQ�B\)Bp�Bw�Bw�Bt�Bm�BdZBW
BR�BR�BS�B[#BbNBe`BgmBffBgmBhsBiyBffBgmBffBe`BdZB`BB[#B^5BP�BL�BI�BH�BM�BR�BR�BR�BP�BP�BN�BM�BL�BK�BJ�BH�BH�BD�BC�BA�B@�B=qB?}B>wB<jB<jB;dB:^B9XB9XB7LB6FB,B.B,B+B+B&�B(�B(�B'�B'�B&�B'�B&�B&�B$�B'�B'�B(�B'�B(�B(�B'�B$�B'�B'�B(�B:^BN�BO�BjB��B��B�?B��B��B�B�B��B	DB	
=B	bB	�B	�B	"�B	)�B	N�B	VB	\)B	dZB	hsB	r�B	y�B	|�B	�JB	�uB	��B	��B	��B	�B	�B	�-B	�3B	�FB	�FB	�^B	�wB	ƨB	��B	��B	�B	�/B	�)B	�;B	�ZB	�fB	�mB	�TB	�`B	�`B	��B	��B	��B
B
%B
1B
1B
	7B
JB
VB
bB
hB
uB
�B
�B
�B
�B
�B
 �B
"�B
$�B
&�B
(�B
+B
-B
.B
/B
1'B
2-B
33B
5?B
5?B
6FB
7LB
9XB
:^B
;dB
<jB
>wB
?}B
A�B
B�B
C�B
D�B
E�B
E�B
E�B
G�B
I�B
I�B
K�B
K�B
L�B
K�B
M�B
N�B
N�B
P�B
P�B
O�B
P�B
Q�B
S�B
S�B
T�B
VB
W
B
W
B
XB
YB
YB
[#B
[#B
\)B
]/B
^5B
^5B
`BB
`BB
aHB
bNB
dZB
dZB
ffB
ffB
gmB
gmB
hsB
hsB
iyB
jB
l�B
k�B
l�B
m�B
m�B
n�B
n�B
o�B
p�B
p�B
r�B
r�B
s�B
t�B
t�B
t�B
v�B
x�B
w�B
y�B
y�B
y�B
y�B
z�B
z�B
{�B
|�B
|�B
~�B
� B
�B
�B
�B
�%B
�1B
�1B
�=B
�1B
�=B
�PB
�VB
�bB
�bB
�hB
�oB
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
�B
�B
�B
�!B
�'B
�-B
�3B
�3B
�?B
�?B
�LB
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
�dB
�dB
�dB
�^B
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�^B
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�dB
�jB
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
�dB
�^B
�dB
�dB
�dB
�dB
�dBBBBBBBBBBBBBBB%BBBBBBBBBBB
��BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                BBB
��BBBBBBBBBBBBBBBB  B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B+BS�Bp�B��B�BbB=qBL�BVBaHBk�BR�BW
BP�B?}B9XB33B.B9XB8RB7LB8RB7LB9XB<jBA�BD�BF�BF�BH�BH�BO�BXBXBVBXB\)BYB@�B<jB8RB7LB9XB<jB?}BE�BI�B_;Bk�BiyBp�Bo�Br�Bw�Bp�Bl�BaHBD�BC�B&�BbB��B��B�?B��B�1B~�Bx�B�B�B}�B�hB�1BdZB9XB@�B(�B
��B
�B
�NB
��B
�yB
��B
�qB
��B
��B
�DB
w�B
m�B
jB
e`B
[#B
6FB
'�B
�B
�B	�B	�ZB	�;B	ǮB	�qB	��B	��B	�\B	�=B	�B	o�B	P�B	K�B	>wB	�B	\B	1B	  B��B�sB�;B�B��B�?B�9B��B��B��B�\B�DB�Bx�BffB`BB^5BZBZBVBVBR�BQ�BP�B[#Bo�Bv�Bv�Bs�Bl�BcTBVBQ�BQ�BR�BZBaHBdZBffBe`BffBgmBhsBe`BffBe`BdZBcTB_;BZB]/BO�BK�BH�BG�BL�BQ�BQ�BQ�BO�BO�BM�BL�BK�BJ�BI�BG�BG�BC�BB�B@�B?}B<jB>wB=qB;dB;dB:^B9XB8RB8RB6FB5?B+B-B+B)�B)�B%�B'�B'�B&�B&�B%�B&�B%�B%�B#�B&�B&�B'�B&�B'�B'�B&�B#�B&�B&�B'�B9XBM�BN�BiyB��B��B�9B��B��B�
B�B��B	
=B		7B	\B	�B	�B	!�B	(�B	M�B	T�B	[#B	cTB	gmB	q�B	x�B	{�B	�DB	�oB	��B	��B	��B	��B	�B	�'B	�-B	�?B	�?B	�XB	�qB	ŢB	ɺB	��B	�B	�)B	�#B	�5B	�TB	�`B	�fB	�NB	�ZB	�ZB	��B	��B	��B
B
B
+B
+B
1B
DB
PB
\B
bB
oB
�B
�B
�B
�B
�B
 �B
"�B
$�B
&�B
(�B
+B
-B
.B
/B
1'B
2-B
33B
5?B
5?B
6FB
7LB
9XB
:^B
;dB
<jB
>wB
?}B
A�B
B�B
C�B
D�B
E�B
E�B
E�B
G�B
I�B
I�B
K�B
K�B
L�B
K�B
M�B
N�B
N�B
P�B
P�B
O�B
P�B
Q�B
S�B
S�B
T�B
VB
W
B
W
B
XB
YB
YB
[#B
[#B
\)B
]/B
^5B
^5B
`BB
`BB
aHB
bNB
dZB
dZB
ffB
ffB
gmB
gmB
hsB
hsB
iyB
jB
l�B
k�B
l�B
m�B
m�B
n�B
n�B
o�B
p�B
p�B
r�B
r�B
s�B
t�B
t�B
t�B
v�B
x�B
w�B
y�B
y�B
y�B
z�B
{�B
{�B
|�B
}�B
}�B
� B
�B
�B
�B
�%B
�+B
�7B
�7B
�DB
�7B
�DB
�VB
�\B
�hB
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
�B
�B
�B
�!B
�!B
�'B
�-B
�3B
�9B
�?B
�?B
�LB
�LB
�XB
�XB
�XB
�^B
�dB
�dB
�jB
�jB
�qB
�qB
�qB
�wB
�wB
�wB
�qB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�wB
�qB
�wB
�wB
�wB
�}B
�wB
�wB
�wB
�}B
�wB
�wB
�wB
�wB
�}B
�wB
�wB
�wB
�}B
�}B
�wB
�}B
�}B
�}B
�}B
��B
�}B
�wB
�}B
�}B
�}B
�}B
�}BBBBBBBBBBBBBBBBBBBBBBBBG�O�G�O�BBBBBBBBBBBBBBBBBBBBB  BBBBBBBBBBBBBG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�201808031335002021061413522520210614135225202106141746312021061417463120210614174631201808031335002021061413522520210614135225202106141746312021061417463120210614174631PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 1 (+/-0), vertically averaged dS = -0.002 (+/-0)                                                                                                                                                                                                         Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2018080313350020180803133500  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080313350020180803133500QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018080313350020180803133500QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216015120210722160151IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                