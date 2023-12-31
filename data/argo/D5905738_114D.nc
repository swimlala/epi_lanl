CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  +   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-09-09T11:00:39Z creation      
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
resolution        =���   axis      Z        )X  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  e8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )X  o�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     )X  �@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X  ̘   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X  H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X )�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X 3�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X ]P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 
X �X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     )X İ   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                      HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        T   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        \   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  � �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  � �      � �Argo profile    3.1 1.2 19500101000000  20200909110039  20210722160209  5905738 5905738 US ARGO PROJECT                                                 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              GREGORY C. JOHNSON                                              PRES            TEMP            PSAL            PRES            TEMP            PSAL               r   rDD  AOAO7218                            7218                            2C  2C  DD  SOLO_D_MRV                      SOLO_D_MRV                      12001                           12001                           SBE603 01Dec16                  SBE603 01Dec16                  874 874 @�1�Jf@�1�Jf11  @�1홙�`@�1홙�`@2<	��B@2<	��B�cg`A�7L�cg`A�7L11  GPS     GPS     Primary sampling: averaged [1Hz sampling by SBE-41CP averaged in 2-dbar bins]                                                                                                                                                                                   Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  >���?�  ?�33@9��@�  @�33@�  @�  A   A  A$��AA��A`  A�  A�33A�33A�  A���A���A���A���B   B  B33B��B ffB(ffB0  B8ffB@��BHffBP  BX  B`ffBh��BpffBxffB�  B���B���B�  B�33B�33B�33B�33B�33B�  B�33B�ffB�33B�  B�  B�  B�ffB�33BǙ�B�33B�ffB���B�33B�33Bߙ�B���B�33B�33B�ffB�ffB���B�  C �C�C�fCL�CL�C
33C33C33C33C  C�fC��C�C  C�fC33C �C"  C$33C&�C'�fC*�C,�C-�fC0�C2L�C4L�C633C8�C:�C<�C>  C@  CB  CC�fCE��CH33CJ�CL�CN  CP  CR  CT  CV  CX  CZ  C\  C^  C_�fCb33Cd�Cf�Ch�Cj  ClL�Cn33Cp�Cq�fCs�fCv33Cx�Cz  C|  C}��C�fC��C�  C��3C�  C��C��C��C�  C��C�  C��fC�  C��C�&fC��C��3C��C�  C��fC�  C�&fC��C��3C��C��C�  C�  C��3C��C��C��C�  C�  C��C�&fC��C��C��C��C�  C��C��C��C��C��C��C��C��C��C��C��C�  C��C��C��C�  C��fC�  C�  C��fC��C�  C��3C��C��C�  C��C�  C�  C��C��C�  C��C�  C��fC��C�  C��fC�  C�&fC��C��C��3C��fC��C�  C��3C��C��C��fC��C��C�  C�&fC��C�  C��3C��fC�  C�&fC��C�  C��3C��3C��C�  C��3C��C��C��3C��C�&fC��C��C�&fC��C��3C��C�  C��3C�  C��3C���C��3C�  C�� C��3D fD ffD ��D��D�3D�fD�Ds3D  D��D��D� D�Dl�D  D�fD�3D��D	  D	� D
3D
� D
��D�3D  Dy�D�D� D  D�3D,�D��D��D� D3Ds3DfD��D�3D� D�Dl�D��D�fD�D��D�3D� DfD�3D�Ds3D��D� D�D��D��D� DfD�3D�Ds3DfD�fD�fD s3D ��D!� D"fD"ffD"��D#y�D$3D$��D$��D%�fD&3D&l�D&��D'��D'� D(l�D(�3D)� D*  D*�fD+3D+l�D+��D,� D-�D-��D-��D.y�D/fD/�3D0  D0y�D1fD1��D23D2��D2�3D3y�D3��D4� D5fD5�fD6�D6�3D73D7l�D7��D8y�D9  D9� D:3D:��D:�fD;s3D;�3D<� D=  D=��D>3D>ffD>��D?y�D@  D@�fDA3DA��DA��DBy�DC  DC�3DD�DDs3DE  DE� DF�DF��DF��DGs3DH  DH�fDI�DIy�DJfDJ��DK3DK� DK��DL�fDM�DMl�DM��DNy�DOfDO��DO�3DPy�DQfDQ��DR�DRy�DSfDS��DS�fDTy�DU  DU�fDV3DVl�DV�3DWy�DXfDX�fDY�DY��DY�3DZy�DZ��D[�fD\�D\l�D\�3D]� D^�D^��D_  D_�fD`�D`y�DafDa�3Da�3Db� Dc3Dcs3Dd  Dd��Dd�3De� Df3Dfl�Dg  Dg��Dg��Dh� Di3Dis3DjfDj��Dj��Dk��Dl�Dly�Dm�Dm� DnfDn�3Do&fDo� Dp3Dp� Dq  Dq��Dr�Dr� Ds�Ds��Ds��Dt�3Du  Du� Dv3Dv�fDwfDwffDw��Dx�fDx�fDy� Dz3Dzy�D{�D{� D|  D|�3D}&fD}��D}�fD~�fD�Dy�D�fD�L�D�|�D��fD�3D�C3D�s3D���D�fD�33D�|�D��3D�fD�L�D�y�D��3D�	�D�33D�|�D��3D�	�D�P D�|�D��3D�	�D�<�D��3D�ɚD���D�@ D���D���D�  D�C3D���D�� D���D�C3D���D��fD�  D�C3D���D���D���D�C3D��fD��3D���D�C3D���D��3D�  D�C3D���D��fD���D�@ D��3D��fD��fD�<�D�� D��3D�	�D�6fD�|�D�� D�3D�I�D�vfD���D���D�C3D���D��fD�  D�@ D���D�� D���D�@ D��3D��fD�fD�L�D���D���D�  D�C3D��fD��fD��3D�9�D�|�D��3D�3D�I�D�� D���D�3D�I�D�vfD���D�  D�C3D��3D��fD�	�D�6fD�� D��3D�	�D�L�D�|�D��3D�fD�9�D�|�D��3D�fD�I�D�vfD���D���D�<�D�� D��3D�fD�I�D���D���D�  D�@ D�� D��3D�3D�FfD���D���D��fD�9�D�y�D���D���D�<�D�|�D���D���D�<�D�|�D�� D���D�<�D�|�D���D���D�<�D�� D��3D�  D�C3D��3D��fD�3D�FfD���D��fD�3D�FfD��3D��3D�  D�@ D�|�D�� D���D�9�D���D���D�	�D�FfD��fD���D���D�<�D�y�D��fD��fD�L�D���D�ɚD�3D�C3D�|�D�� D���D�6fD�s3D���D�	�D�I�D���D��fD�3D�@ D��3D��3D�  D�C3D�� D���D���D�<�D�y�D���D�	�D�FfD��fD��3D�3D�C3D�|�D���D�	�D�C3D��3D���D���D�9�D�vfD�ɚD�3D�@ D�� D���D�	�D�FfD��3D���D���D�9�D�y�D�ɚD�fD�C3D�� D�� D� D�P D�y�D���D���D�<�D�|�D���D���D�9�D�y�D���D���D�<�D�vfD��fD��D�I�D�D�ɚD�	�D�I�DÉ�D��3D�3D�FfDĆfD��fD�3D�C3DŃ3D��3D�3D�C3Dƃ3D��3D�fD�C3DǆfD�ɚD�	�D�I�Dȉ�D�ɚD��D�L�D�vfDɶfD�	�D�I�DʆfD��fD�3D�C3DˆfD��3D�fD�C3D̃3D�� D�  D�<�D̀ D�� D���D�@ D�|�D���D�	�D�I�DφfD��fD�3D�C3DЃ3D��3D�  D�C3D�y�Dѹ�D��D�I�D҆fD��3D���D�9�D�vfDӶfD���D�I�DԆfD��fD�  D�@ D�|�Dռ�D��D�FfDփ3D��3D�  D�<�D�|�D׹�D���D�9�D�|�DضfD��fD�I�Dى�D��fD�  D�@ D�y�Dڹ�D�	�D�FfDۃ3Dۼ�D�  D�<�D�y�D���D�	�D�I�D݃3D��3D�fD�FfDރ3D�� D�  D�9�D�s3D߶fD�fD�C3D��3D��3D�  D�<�D�y�D���D�fD�FfD�fD�� D���D�<�D��D�ɚD�fD�C3D�|�D���D�	�D�I�D�3D��fD�  D�@ D�y�D�fD�3D�@ D�|�D���D�3D�C3D�y�D�fD�	�D�FfD�3D�� D���D�FfD�fD�� D���D�6fD뉚D��fD�3D�@ D� D�� D���D�L�D��D�ɚD�3D�C3D�y�DD�	�D�FfD�3D�� D���D�L�D���D��fD�3D�C3D�y�D�fD�	�D�C3D�|�D�D���D�I�D�fD��fD�fD�C3D� D���D���D�9�D���D���D�	�D�I�D���D��fD�3D�@ D�� D��3D�  D�<�D�y�D���D��fD�L�D��fD��fD��D��3D��3D�� D�fD�Y�E L�E � E��E�E��EK3E��E��E�E��EX E��E� E4�E��E	l�E
3E
� EA�E��Et�E�E��E6fE�fE�E�fE,�E��EVfE�3E|�E�E��E+3E��EFfE` E�3Et�E  E� E3E$�E� E+3E�3E8 EFfE��ED�E��E C3E!H E!� E"@ E#>fE#�fE$6fE$� E%��E&0 E&�3E'  E(�E(�fE)�E)� E*i�E*�fE+њE,A�E-4�E-��E.�E/3E/�fE/�fE0�E1h E1�3E2\�E3T�E3�3E4D�E5C3E5� E633E70 E7��E8)�E8�fE9�3E:�E:� E;�fE<�E<��E=	�E>fE>�3E?�E?��E@� EA3EA� EBfEC�EC� ED�ED��EE#3EF0 EF�fEG.fEG��EH��EI6fEI�fEJ9�EJ��EKɚELFfEL�3EM@ EN>fEN��EO.fEP$�EP�fEQfER	�ER~fER�3ES� ETX ET� EU�fEV.fEV�3EW�fEX�EX�3EYt�EY�3EZc3E[T�E[ɚE\A�E]33E]� E^��E^�fE_q�E`S3E`��Ea�fEb Eb��Eci�Ec� Ed�fEe+3Ee�3Eft�Ef�fEg�3Eh+3EifEip EjH Ej��Ek� Ek��El��Em�Em�3EnY�Eo33Eo��Eph Eq;3Eq�fErs3Er�3Es� Et�Et�3Eu<�EvfEv� Ew;3ExfExs3Ey<�Ey��Ezd�E{0 E{��E|d�E|�3E}� E}� E~�3E�3E�E�ZfE�� E��fE�RfE��fE���E�JfE�zfE���E��E�nfE��3E�� E�\�E��fE�� E�D�E�� E��fE�T�E���E�� E�4�E�� E�� E�fE�q�E��fE��fE�T E���E��E�a�E���E��E�BfE��3E�� E�3E�x E�њE�*fE��fE���E�fE�_3E��3E� E�8�E���E��3E�<�E��3E���E�?3E�jfE���E�3E�l�E�� E�fE�d�E��3E�
fE�\ E���E���E�NfE���E�� E�;3E�� E��fE�#3E�p�E���E�3E�T E���E�� E�U�E���E��E�&fE�� E�њE��E�3E�� E�3E�o3E��fE���E�g3E���E��3E�8 E�� E��3E�(�E���E��3E�33E�s3E�� E�f>���>L��>L��>���>���>���>���>L��>���>���>���>���?   ?��?��?L��?fff?fff?���?�33?�  ?���?ٙ�?�ff@��@��@,��@333@L��@fff@s33@�ff@�  @���@���@�ff@�  @���@ٙ�@�ff@�33A��AffA��A33A��A   A(  A0  A6ffA<��AFffAL��AT��A\��Ac33Al��As33A{33A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414141141141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ?L��?�  @��@Y��@�  @�33@�  @�  A  A  A,��AI��Ah  A�  A�33A�33A�  A���A���A���A���B  B
  B33B��B"ffB*ffB2  B:ffBB��BJffBR  BZ  BbffBj��BrffBzffB�  B���B���B�  B�33B�33B�33B�33B�33B�  B�33B�ffB�33B�  B�  B�  B�ffB�33Bș�B�33B�ffB���B�33B�33B���B���B�33B�33B�ffB�ffB���B�  C ��C��CffC��C��C
�3C�3C�3C�3C� CffCL�C��C� CffC�3C ��C"� C$�3C&��C(ffC*��C,��C.ffC0��C2��C4��C6�3C8��C:��C<��C>� C@� CB� CDffCFL�CH�3CJ��CL��CN� CP� CR� CT� CV� CX� CZ� C\� C^� C`ffCb�3Cd��Cf��Ch��Cj� Cl��Cn�3Cp��CrffCtffCv�3Cx��Cz� C|� C~L�C�33C�L�C�@ C�33C�@ C�L�C�Y�C�L�C�@ C�Y�C�@ C�&fC�@ C�L�C�ffC�L�C�33C�L�C�@ C�&fC�@ C�ffC�Y�C�33C�Y�C�L�C�@ C�@ C�33C�Y�C�Y�C�L�C�@ C�@ C�Y�C�ffC�Y�C�Y�C�Y�C�L�C�@ C�L�C�Y�C�L�C�L�C�Y�C�Y�C�Y�C�L�C�Y�C�L�C�L�C�@ C�Y�C�L�C�L�C�@ C�&fC�@ C�@ C�&fC�L�C�@ C�33C�L�C�L�C�@ C�Y�C�@ C�@ C�Y�C�L�C�@ C�L�C�@ C�&fC�L�C�@ C�&fC�@ C�ffC�L�C�L�C�33C�&fC�L�C�@ C�33C�L�C�L�C�&fC�L�C�L�C�@ C�ffC�Y�C�@ C�33C�&fC�@ C�ffC�Y�C�@ C�33C�33C�L�C�@ C�33C�L�C�L�C�33C�L�C�ffC�Y�C�L�C�ffC�L�C�33C�L�C�@ C�33C�@ C�33C��C�33C�@ C�  C�33D &fD �fD�D��D3D�fD,�D�3D  D��D�D� D,�D��D  D�fD3D��D	@ D	� D
33D
� D�D�3D@ D��D,�D� D  D�3DL�D��D�D� D33D�3D&fD��D3D� D,�D��D�D�fD,�D��D3D� D&fD�3D9�D�3D�D� D,�D��D�D� D&fD�3D9�D�3D&fD�fD fD �3D!�D!� D"&fD"�fD#�D#��D$33D$��D%�D%�fD&33D&��D'�D'��D(  D(��D)3D)� D*  D*�fD+33D+��D,�D,� D-,�D-��D.�D.��D/&fD/�3D0@ D0��D1&fD1��D233D2��D33D3��D4�D4� D5&fD5�fD6,�D6�3D733D7��D8�D8��D9  D9� D:33D:��D;fD;�3D<3D<� D=  D=��D>33D>�fD?�D?��D@  D@�fDA33DA��DB�DB��DC  DC�3DD9�DD�3DE  DE� DF,�DF��DG�DG�3DH  DH�fDI9�DI��DJ&fDJ��DK33DK� DL�DL�fDM,�DM��DN�DN��DO&fDO��DP3DP��DQ&fDQ��DR9�DR��DS&fDS��DTfDT��DU  DU�fDV33DV��DW3DW��DX&fDX�fDY,�DY��DZ3DZ��D[�D[�fD\,�D\��D]3D]� D^,�D^��D_  D_�fD`9�D`��Da&fDa�3Db3Db� Dc33Dc�3Dd  Dd��De3De� Df33Df��Dg  Dg��Dh�Dh� Di33Di�3Dj&fDj��Dk�Dk��Dl9�Dl��Dm,�Dm� Dn&fDn�3DoFfDo� Dp33Dp� Dq  Dq��Dr9�Dr� Ds,�Ds��Dt�Dt�3Du@ Du� Dv33Dv�fDw&fDw�fDx�Dx�fDyfDy� Dz33Dz��D{,�D{� D|  D|�3D}FfD}��D~fD~�fD9�D��D�fD�\�D���D��fD�#3D�S3D��3D���D�fD�C3D���D��3D�fD�\�D���D��3D��D�C3D���D��3D��D�` D���D��3D��D�L�D��3D�ٚD�	�D�P D���D�ɚD� D�S3D���D�� D��D�S3D���D��fD� D�S3D���D���D��D�S3D��fD��3D��D�S3D���D��3D� D�S3D���D��fD��D�P D��3D��fD�fD�L�D�� D��3D��D�FfD���D�� D�3D�Y�D��fD�ɚD�	�D�S3D���D��fD� D�P D���D�� D��D�P D��3D��fD�fD�\�D���D�ɚD� D�S3D��fD��fD�3D�I�D���D��3D�3D�Y�D�� D�ɚD�3D�Y�D��fD�ɚD� D�S3D��3D��fD��D�FfD�� D��3D��D�\�D���D��3D�fD�I�D���D��3D�fD�Y�D��fD�ɚD�	�D�L�D�� D��3D�fD�Y�D���D���D� D�P D�� D��3D�3D�VfD���D���D�fD�I�D���D���D��D�L�D���D���D��D�L�D���D�� D��D�L�D���D���D�	�D�L�D�� D��3D� D�S3D��3D��fD�3D�VfD���D��fD�3D�VfD��3D��3D� D�P D���D�� D�	�D�I�D���D���D��D�VfD��fD���D��D�L�D���D��fD�fD�\�D���D�ٚD�3D�S3D���D�� D�	�D�FfD��3D�ɚD��D�Y�D���D��fD�3D�P D��3D��3D� D�S3D�� D���D��D�L�D���D���D��D�VfD��fD��3D�3D�S3D���D�ɚD��D�S3D��3D���D��D�I�D��fD�ٚD�3D�P D�� D�ɚD��D�VfD��3D���D��D�I�D���D�ٚD�fD�S3D�� D�� D�  D�` D���D�ɚD�	�D�L�D���D���D��D�I�D���D���D��D�L�D��fD��fD��D�Y�D�D�ٚD��D�Y�DÙ�D��3D�3D�VfDĖfD��fD�3D�S3Dœ3D��3D�3D�S3DƓ3D��3D�fD�S3DǖfD�ٚD��D�Y�Dș�D�ٚD��D�\�DɆfD��fD��D�Y�DʖfD��fD�3D�S3D˖fD��3D�fD�S3D̓3D�� D� D�L�D͐ D�� D��D�P DΌ�D���D��D�Y�DϖfD��fD�3D�S3DГ3D��3D� D�S3Dщ�D�ɚD��D�Y�DҖfD��3D��D�I�DӆfD��fD�	�D�Y�DԖfD��fD� D�P DՌ�D���D��D�VfD֓3D��3D� D�L�D׌�D�ɚD��D�I�D،�D��fD�fD�Y�Dٙ�D��fD� D�P Dډ�D�ɚD��D�VfDۓ3D���D� D�L�D܉�D���D��D�Y�Dݓ3D��3D�fD�VfDޓ3D�� D� D�I�D߃3D��fD�fD�S3D��3D��3D� D�L�DቚD���D�fD�VfD�fD�� D��D�L�D��D�ٚD�fD�S3D��D���D��D�Y�D�3D��fD� D�P D扚D��fD�3D�P D��D���D�3D�S3D艚D��fD��D�VfD�3D�� D�	�D�VfD�fD�� D��D�FfD뙚D��fD�3D�P D� D�� D��D�\�D��D�ٚD�3D�S3DD�ɚD��D�VfD�3D�� D�	�D�\�D�D��fD�3D�S3D�D��fD��D�S3D��D�ɚD�	�D�Y�D�fD��fD�fD�S3D�� D���D��D�I�D���D���D��D�Y�D���D��fD�3D�P D�� D��3D� D�L�D���D�ɚD�fD�\�D��fD��fD�)�D��3D��3D�� D�&fD�i�E T�E � E��E!�E��ES3E��E��E$�E��E` E�E� E<�E��E	t�E
3E
� EI�E��E|�E�E��E>fE�fE�E�fE4�E��E^fE�3E��E�E��E33E��ENfEh E�3E|�E E� E3E,�E� E33E�3E@ ENfE��EL�E��E K3E!P E!� E"H E#FfE#�fE$>fE$� E%��E&8 E&�3E'( E(!�E(�fE)	�E*  E*q�E*�fE+ٚE,I�E-<�E-��E.$�E/3E/�fE0fE0��E1p E1�3E2d�E3\�E3�3E4L�E5K3E5� E6;3E78 E7��E81�E8�fE9�3E:!�E:� E;�fE<�E<��E=�E>fE>�3E?�E?��E@� EA3EA� EBfEC!�EC� ED!�ED��EE+3EF8 EF�fEG6fEG��EH��EI>fEI�fEJA�EJɚEKњELNfEL�3EMH ENFfEN��EO6fEP,�EP�fEQfER�ER�fER�3ES� ET` ET� EU�fEV6fEV�3EW�fEX�EX�3EY|�EY�3EZk3E[\�E[њE\I�E];3E]� E^��E_fE_y�E`[3E`ɚEa�fEb Eb��Ecq�Ec� Ed�fEe33Ee�3Ef|�Ef�fEg�3Eh33EifEix EjP Ej��Ek� Ek��El��Em$�Em�3Ena�Eo;3Eo��Epp EqC3Eq�fEr{3Er�3Es� Et�Et�3EuD�EvfEv� EwC3ExfEx{3EyD�Ey��Ezl�E{8 E{��E|l�E|�3E}� E~  E~�3E�3E�E�^fE�� E��fE�VfE��fE���E�NfE�~fE���E��E�rfE��3E�  E�`�E��fE�� E�H�E�� E��fE�X�E���E�� E�8�E�� E�� E�fE�u�E��fE��fE�X E���E�	�E�e�E���E��E�FfE��3E�� E�#3E�| E�՚E�.fE��fE���E�
fE�c3E��3E� E�<�E���E��3E�@�E��3E���E�C3E�nfE�ŚE�3E�p�E�� E�fE�h�E��3E�fE�` E���E��E�RfE���E�� E�?3E�� E��fE�'3E�t�E���E�3E�X E���E�� E�Y�E���E��E�*fE�� E�՚E��E��3E�� E�3E�s3E��fE��E�k3E���E��3E�< E�� E��3E�,�E���E��3E�73E�w3E�� E�fG�O�G�O�?333G�O�G�O�G�O�G�O�?333G�O�?L��G�O�?fff?�  G�O�?���?�ffG�O�?�33?ٙ�?�33@   @ff@��@33@,��@9��@L��@S33@l��@�33@���@�ff@�  @���@���@�ff@�  @���@陚@�ffA��A	��AffA��A33A!��A(  A0  A8  A>ffAD��ANffAT��A\��Ad��Ak33At��A{33A���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441444414141141141111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 @ @ �@ �@ �@ O@ "�@ (�@ /�@ 6�@ =q@ FQ@ R�@ _�@ m:@ z3@ ��@ �0@ ��@ �-@ ��@ �|@ �t@ �@ ��@�@�@g@,`@:�@I@V@b�@p�@~�@�P@�H@��@��@��@ψ@��@�4@��@�@*@"�@/�@>@Lu@Yn@ff@t@��@��@��@�M@�@�W@��@��@�@�9@	�@�@&;@4�@B8@P�@\)@j@x&@�p@��@�z@�r@�@��@�h@�@�@��@V@O@(G@7�@D�@Q�@`�@m�@z3@�7@��@�(@�-@�2@��@��@��@�q@@@�@,`@9X@FQ@V�@c�@qS@~K@��@��@�A@��@@�7@��@�@�~@1@*@"�@0x@=q@M$@Z@g@s_@�@��@��@��@�R@Ĝ@��@��@��@��@
=@�@&�@3�@@�@O�@\)@hs@ww@��@��@�@�f@�k@�c@խ@�@�e@^@�@�@)�@6�@DD@Q=@`�@n�@{�@��@�0@�5@��@��@�*@��@��@��@	@	o@	g@	-@	;d@	I@	V�@	c�@	r@	~�@	��@	��@	��@	��@	�>@	�7@	܀@	�@	�,@
v@
*@
""@
/@
>@
K�@
X�@
g�@
t@
��@
��@
��@
��@
�@
��@
�C@
��@
��@
�9@
=@�@&;@3�@@,@M$@\�@i�@v�@��@�u@�@��@�k@�c@�@�@�Y@�Q@J@O@+@7�@DD@Q=@^�@m�@z�@��@��@��@��@��@��@��@��@��@@b@g@,`@9X@G�@T�@`B@o�@~K@��@��@��@�-@��@є@܀@�4@��@v@{@#�@-�@=q@Lu@V�@ff@t�@�W@��@�m@��@�^@�c@��@�T@�Y@��@�@O@%�@5?@E�@O�@Z@i�@y�@��@�u@��@��@��@��@��@�@�@^@b@�@(�@7L@FQ@T�@^5@l�@z�@��@��@��@�~@��@��@�/@�@�q@@V@[@+�@:@H]@R�@bN@o�@�W@��@��@��@��@��@ψ@�;@�@� @v@{@""@0x@?}@I@X@ff@uk@�@�P@�U@�Y@�^@�c@��@��@�L@��@�@�@$�@2�@@�@O0@\�@k.@y�@�+@��@��@�f@��@�c@�@�@�@��@J@O@(�@7�@FQ@O0@^�@l�@z�@�7@�<@�5@�r@�w@��@܀@��@�e@j@@ @-�@7�@FQ@UU@c�@s_@}�@��@��@�M@�R@��@��@�;@�y@�~@%@*@$�@.l@<�@K�@Z@i!@s_@�d@��@�H@��@�R@ƨ@խ@�;@�@��@
�@�@&�@5�@?}@M�@[z@j@x�@�@�h@�m@�r@�w@�c@׹@�m@�@ �@�@�@(�@8�@B�@Q�@`�@k�@z�@��@�$@��@��@�@��@܀@�@�q@%@b@ @/@9X@I@X�@c�@r�@�d@��@��@��@��@��@��@��@��@��@%@�@%�@/�@?}@O0@Yn@c�@s_@�d@��@�@��@��@�W@�
@�H@��@ �@�@*@&;@5�@@,@O�@^�@i!@x�@�7@�u@��@�f@�@ƨ@�\@�`@�@�@J@�@+@3�@C�@R�@a�@p�@z3@�7@�<@�(@�-@�2@�o@�t@�(@�e@j@�@ �@/�@9X@H]@Wb@`�@p�@~�@��@�U@��@��@��@�|@�/@�4@�9@
�@{@"�@1�@;d@Ji@X�@g@uk@�@��@�@�Y@�^@��@��@�H@�@��@1@�@$/@3�@B�@Lu@\)@i�@y�@��@�@�m@��@�@��@��@�m@��@   @ V@ �@ *S@ 3�@ B�@ Q=@ `A@ m�@ |�@ ��@ ��@ ��@ ��@ �@ �o@ �t@ ��@ �q@!�@!@!�@!,`@!:�@!I�@!X@!bN@!qS@!�@!��@!��@!��@!�F@!Ĝ@!�*@!܀@!�(@!�~@"�@"*@"#�@"1�@"@,@"Ji@"X�@"ff@"t@"�d@"�@"�a@"��@"�@"��@"�C@"��@"�@@"��@#	�@#6@#$�@#2�@#@,@#M�@#\)@#i!@#v�@#�p@#�@#�@#�f@#��@#�@#�
@#�`@#�@$]@$V@$�@$+@$7�@$D�@$SI@$`A@$m�@$z�@$��@$��@$��@$�!@$��@$��@$�/@$�(@$� @%�@%b@%
@%+�@%8�@%E�@%SI@%e�@%r�@%�W@%��@%�H@%��@%��@%�2@%�*@%�#@%�(@%�9@&�@&�@&#�@&0x@&=q@&K�@&Yn@&ff@&t�@&��@&��@&�U@&��@&��@&ȴ@&խ@&�@&�L@&�E@'
�@'�@'$�@'1�@'B�@'O0@'\�@'i!@'v�@'��@'��@'�z@'��@'��@'�c@'խ@'�@'�@( �@(�@(�@('�@(5@@(FQ@(SI@(`A@(m:@(z�@(��@(��@(�z@(�!@(��@(�@(��@(�m@(�@)@)�@)
@)+�@)9X@)E�@)SI@)e�@)r�@)�W@)��@)��@)�M@)��@)�>@)��@)�;@)��@)��@*�@**@*"�@*0x@*>@*K�@*Yn@*g@*uk@*�d@*��@*�@*��@*�^@*�@*խ@*�@*�@*��@+1@+�@+'�@+4�@+B8@+O0@+\�@+k.@+x&@+�|@+�u@+�@+�@+��@+ȴ@+�
@+�@+�@,  @,�@,
@,+@,8�@,E�@,SI@,`A@,m�@,{�@,�7@,�0@,��@,�!@,��@,ψ@,܀@,�y@,�q@-�@-�@-�@-*S@-8�@-I�@-V�@-dZ@-p�@-~K@-�D@-��@-��@-�F@-�>@-��@-��@-��@-�~@.v@.�@. �@./@.;d@.I@.Z�@.hs@.uk@.��@.�\@.��@.�M@.�^@.�W@.�O@.��@.��@.��@/�@/�@/'�@/5@@/A�@/O0@/]�@/k.@/x&@/�@/��@/�@/�Y@/��@/��@/׹@/�`@/�@0  @0�@0�@0+�@07�@0E�@0SI@0_�@0l�@0z3@0�D@0�<@0�4@0�-@0�w@0ψ@0܀@0�(@0�q@1�@1@1�@1+@17�@1H]@1UU@1bN@1s_@1~�@1��@1�<@1�4@1��@1��@1��@1��@1�(@1��@21@2{@2!s@2-�@2?}@2Lu@2Yn@2ff@2t@2��@2��@2��@2�f@2�^@2ƨ@2�O@2��@2�@2��@3�@3�@3%�@31�@3C�@3P�@3]�@3j@3x&@3��@3��@3�z@3��@3�@3�@3խ@3�@3�@4]@4@4�@4(�@45�@4C�@4P�@4bN@4o�@4|�@4��@4�<@4�4@4�-@4�&@4��@4�#@4�@4�@5@5�@5�@5/@5;d@5I@5Z�@5qS@5�d@5�Y@64�@6y�@6��@7  @7A�@7�d@7Ĝ@8�@8FQ@8�7@8�o@9V@9Q�@9��@9�t@:[@:a�@:�z@:�@;(�@;j@;��@;�@<.l@<oF@<�f@<�@@=r�@=��@=��@>1�@>o�@>�r@>�@?+@?hs@?�4@?��@@	@@�0@@є@AJ@AG�@A��@A�@B1�@Bi�@B��@B��@C{@C��@C��@C�e@D+@D`�@D�7@Ej@E:@E��@E��@Fb@FG�@F��@F�@G�@GN�@G�@G��@H�@H�@H��@H�l@IO1@I~�@I�@J�@JI�@J��@J�@K6@K~�@K�~@K�@L�@L��@L�F@L�(@MV�@M��@M�@N(�@N\�@N�u@Nȴ@O4�@Og@O�@P	�@P<@Pr�@P��@Q6@QLu@Q��@Q�^@R,`@R`�@R��@Rψ@S> @St@S�Z@S�T@T�@T�\@T�J@T��@U3�@U��@U��@Vb@VH]@V�d@V�@W(G@W]�@W��@W�Q@X1�@Xe�@X��@X�Q@Y2�@Y��@Yψ@Z]@Zi�@Z��@Z��@[2�@[bN@[�#@[��@\.l@\`�@\�@\��@]-�@]��@]ƨ@]��@^`�@^�\@^�e@_$�@_V@_�F@_�`@`F�@`t@`��@a�@a6�@a��@a�W@a� @bS�@b�@b�@c@cl�@c��@c��@d �@dz�@d��@e �@e+@e�|@e�-@f@f8�@f��@f��@g6@gr@g��@g��@h �@hx�@h�z@h��@iQ�@i|?@i�[@j]@jWb@j~�@jխ@k,`@kV@k�!@k�h@l33@l\)@l��@m1@m0x@m�+@m�@nv@nZ�@n��@n�#@o.l@oWa@o�Z@o�O@p'�@pz2@p�m@p�@qB�@qi�@q�@r�@rS�@r�@rƨ@s{@s`B@s�@sӠ@t �@tn�@t�^@t��@u/�@u|?@u�W@v�@v:�@v��@vլ@w!s@wm:@w�@w��@x*T@xv@x�1@x�@y1�@y}�@yȴ@z{@z7L@z�d@z�@{*@{^�@{��@{�@|�@|`�@|��@|�@}:@}�W@}ƨ@~�@~S�@~��@~ލ@#�@hr@�Z@��@�B@�:@�[z@�|?@��[@�� @��;@��K@��@�=q@�l4@���@���@��J@��@�V@�+@�Xh@�u�@��o@���@��/@���@�(�@�FQ@�dZ@���@��@���@��s@�u@�/r@�Z@�uk@��@��XG�O�G�O�@ ^G�O�G�O�G�O�G�O�@ ^G�O�@ G�O�@ �@ jG�O�@ @ vG�O�@ %@ 1@ 	�@ 
=@ 
�@ �@ J@ @ b@ o@ @ �@ �@ �@ �@ �@ !s@ $.@ &�@ (�@ +�@ .l@ 1'@ 3�@ 7L@ 9X@ <@ >�@ A�@ DD@ G�@ K@ M�@ P�@ T�@ Wb@ Z�@ ^5@ `�@ e	@ g�@ k.@ n�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ƨA���A���A���A���A��
A��;A��/A��;A��yA��`A��;A��TA��TA��`A��mA��yA��mA��`A��`A��TA��A�1A��A� �A�$�A�$�A�$�A� �A��A��A��A��A�oA�  A��;AԴ9Aԙ�A�p�A�9XA�A��
Aӧ�AӉ7A�l�A�ZA� �A���A��
Aҩ�A�1'A���A��A�%A̗�A�&�Aǧ�A�bA��Aã�A�9XA�A�ȴA�A���A�~�A���A�$�A��yA��HA�1'A��hA���A�K�A���A�ĜA�1'A�t�A�ĜA��^A��hA��A�v�A�
=A�ffA�^5A��yA�\)A���A�^5A��A�bNA�?}A��-A�/A��A��wA���A�$�A���A��mA�ffA���A��9A��A��9A�XA��9A��RA�oA��A~~�A~$�A}��A|bNAx��As\)Ap$�AkS�Ai�Ah  Ae�Ab�!A`JA[�AV��AR�+AQ�AOt�ANbNAM�PAL �AJ�AIdZAF�\ACƨA?��A<v�A:�!A9VA7��A5�hA3�A2(�A0�A/XA.�A,Q�A+�A(A�A'K�A&Q�A%�hA$��A$v�A$VA$-A$�A#�FA#K�A#l�A"��A"=qA"bA!�A ��A A�AJA�AC�A+A(�A��A33A��A1A��AVAz�AQ�A  A�TA33Av�A
=A�^A�A��A9XA��A
�A"�AƨA�/A7LA`BA��AA%A?}A�yA�A��A\)A
�A
�+A	S�A�`A~�A�A`BA�+A  A��AdZA
=AjA�^AdZAȴAA�A�A�#AƨA�PAC�A�A ��A ��A �`A �jA �jA (�@�/@���@�t�@�V@��T@���@�`B@�bN@���@�33@��j@�@��@��@�h@�A�@��m@�P@��y@��@�O�@�Ĝ@�j@��m@��@�$�@�Q�@�l�@柾@��@�%@�@�|�@�n�@�M�@�@��@�hs@�j@�
=@��@ݡ�@��`@���@۾w@ڸR@�@� �@��@֧�@�5?@պ^@�O�@��@Լj@�z�@�Q�@� �@�  @ӝ�@�l�@�;d@�"�@��y@ҏ\@�M�@ѡ�@���@��@��@·+@�@��#@��T@��@�M�@�hs@�?}@�O�@ͺ^@�~�@�^5@Ͳ-@́@�`B@��`@�o@�E�@�=q@�@ɩ�@�x�@�G�@��/@ȴ9@�1'@�b@ǍP@�o@Ƨ�@��@�-@��@�`B@�/@��@��/@���@î@�@���@�G�@��9@���@���@�z�@���@��P@�t�@���@�@��@���@��D@�A�@��
@�;d@���@�~�@�=q@��#@��@���@�j@�1'@��@��@���@�ff@�{@��@��^@�O�@�Ĝ@� �@��;@���@���@���@�~�@�v�@�5?@��@�&�@��D@�1@��F@�;d@�@��@�^5@�5?@��@�X@�&�@���@���@���@�~�@�ff@�=q@�p�@��/@��@�(�@� �@�(�@��;@�S�@�S�@�o@��@��@���@���@��@��@��/@��D@�9X@�|�@�C�@�"�@��R@���@��+@�v�@�^5@�J@���@��@���@��D@�Z@�ƨ@�C�@���@��@���@�5?@��@�G�@���@��@��@���@�K�@�;d@�
=@�~�@�M�@�=q@�E�@�=q@��T@�&�@�%@���@���@��@��/@��u@�z�@�I�@�  @���@�K�@��y@���@�ff@�-@�J@���@�/@�%@�Ĝ@�I�@�b@�ƨ@�l�@�S�@�+@�@���@��@���@�^5@���@���@��^@��-@��@�p�@�X@�G�@�?}@��@���@� �@���@��;@��@�l�@��@�v�@�V@�$�@��-@���@��7@�x�@�p�@�V@���@���@�bN@� �@�  @��m@��w@���@�|�@�dZ@�C�@��@��!@��\@�^5@�5?@�J@���@��#@��^@���@�hs@�X@�G�@���@���@���@�Q�@�(�@��w@�dZ@�;d@�"�@��y@���@��\@�v�@�@��-@��@�X@�/@�%@���@��@��u@�j@�Z@�A�@�I�@�9X@�b@��@�ƨ@�dZ@�K�@�@�ȴ@��R@���@���@�v�@�ff@�5?@��T@���@�G�@�V@�%@���@��`@��u@�(�@��@�1@�  @�@�@~�+@}/@|��@|Z@{�
@{t�@{@z�!@z^5@z�@y�^@y��@yX@y&�@x��@x�@x �@w\)@w�@v�@vE�@u@t��@t�D@tj@t(�@s�F@sC�@r�H@q�#@qx�@qx�@qx�@qhs@qG�@p��@p  @o��@ol�@o
=@nff@nE�@n{@m�@m��@m�@mV@l�@l��@k��@k�@k33@ko@j�!@j^5@j�@i��@iX@iG�@h��@h�@g�w@g�P@gl�@f��@f�y@f��@f{@e��@e�h@e/@d�j@d�D@d1@c��@cS�@b��@b-@aG�@`�9@` �@_��@_\)@^�R@^ff@^$�@]��@]O�@\z�@\1@[�m@[33@Z�@Z�@Z��@Zn�@ZJ@Yx�@X��@X1'@W��@WK�@VV@U�@U�@T�@T�D@TZ@T�@S��@St�@So@R^5@Q��@Q�7@Q&�@Q7L@Q�@P�u@P  @O�P@Ol�@O�@O
=@N�y@N�+@Nv�@N5?@M��@M��@MV@M�@Lz�@K��@Kƨ@L9X@L1@Kƨ@K��@K�@K��@K"�@J�@J�@J�H@J��@J�!@J��@J~�@HĜ@H �@G��@G��@GK�@F��@FV@F{@E@E`B@E?}@D�@D��@D�@C��@Cƨ@C33@B��@A��@@��@@Q�@@A�@?��@?��@?l�@?;d@>�y@>ȴ@>��@>E�@>{@>@=�T@=�-@=p�@<��@<��@<j@<1@;��@;dZ@;o@:~�@:^5@:=q@:=q@:-@:�@9�@9��@9�^@9&�@8bN@81'@8b@7|�@7�@6�y@6��@5�@5�@5p�@5O�@5O�@4�j@4j@4(�@41@3�m@3dZ@3C�@3C�@2�@2~�@2^5@2=q@1&�@1�@1%@1%@0Ĝ@0Ĝ@0�9@0�9@0��@0bN@/�;@/|�@.�y@.��@.ff@-@-�@,�/@,z�@+��@+�m@+�
@+ƨ@+��@+t�@*~�@)�#@)�^@)��@)7L@)%@(�`@(r�@(bN@(Q�@(A�@(A�@(b@'��@'|�@'\)@'�@&�R@&��@&��@&V@&@%��@%�-@%�h@%�h@%p�@%?}@%�@$�@$�j@$�j@$��@$�D@$j@$9X@#S�@"�H@"n�@"=q@"�@"J@!��@!%@ �9@ �u@ bN@ A�@  �@�@�;@�w@�@��@K�@+@��@�@��@V@�T@�@p�@p�@O�@?}@�@V@�/@�@z�@I�@1@��@�m@�
@�F@33@�@��@�\@~�@M�@M�@�@��@�#@��@��@�7@x�@%@��@r�@Q�@ �@�@�@ȴ@��@��@ff@@��@��@�h@p�@�@�/@z�@I�@9X@��@��@S�@"�@�@��@�!@M�@=q@��@��@��@�7@x�@x�@hs@X@7L@7L@&�@�`@��@�@Q�@�@�@��@�P@;d@
=@ȴ@��@�+@E�@�@�T@�T@�T@��@@��@�h@�@�@�@�@p�@p�@O�@�/@��@��@
��@
^5@
J@	%@�@�@ȴ@$�@��@`B@�/@j@�@�!@�^@ Ĝ@ A�@   ?��?�{?�/?�V?�I�?�dZ?��?�X?��9?�ff?�?�9X?�S�?�!?�J?�G�?�Ĝ?�A�?�|�??��-?�j?��?��H?�^5?�X?�9?�
=?��?�?�j?�F?�S�?�!?��?�%?� �?޸R?ޗ�?ݲ-?ۥ�?�"�?�"�?�^5?�x�?���?�r�?�r�?�
=?֧�?�E�?�$�?�?�?}?ԛ�?�z�?ӶF?�33?�33?�J?��?���?�G�?��`?� �?Ͼw?ϝ�?�|�?��?�{?Ͳ-?�p�?���?�I�?�1?�1?�"�?�C�?�~�?���?��#?��?���?�Q�?�Q�?�Q�?�1'?�
=?Ƈ+?�E�?�ff?��T?š�?�?š�?��?���?ě�?�z�?�9X?���?���?���?���?��
?Õ�?�t�?�S�?�o?\?�M�?��?���?���?���?���?��7?�hs?��`?�Ĝ?�A�?� �?��;?�  ?�  ?��w?��w?���?�\)?�\)?��?��?��?��?��?��R?��R?��R?�V?�5??�{?�{?�{?���?���?�p�?�p�?�p�?�p�?�O�?�p�?�/?��?���?���?��?�j?�I�?�I�?�I�?�(�?��m?��m?�ƨ?���?��?��?�dZ?��?�dZ?�dZ?�dZ?�dZ?�C�?�"�?�"�?�C�?�C�?�dZ?�C�?�C�?�C�?�dZ?��?���?�ƨ?�ƨ?�ƨ?��m?�ƨ?��m?��m?��m?��m?��m?��m?��m?�1?�1?�1?�(�?�I�?�(�?�I�?��D?�j?�j?��D?�j?�I�?�j?�j?�j?��D?��D?��?��?��?��?���?��?���?��?���?��?��?��?�V?�/?�O�?�O�?�p�?�p�?��h?��-?��-?���?���?��?�{?�5??�V?�5??�v�?�v�?�v�?�v�?��R?��R?��R?��R?��?���?��?��?�;d?��?��?�;d?�;d?�\)?�|�?���?�|�?�|�?���?���?��w?��;?�  ?��;?�  ?�  ?�  ?� �?�A�?�A�?�A�?�bN?�bN?�bN?�bN?�bN?��?���?��?��?��?��?� �?��?��?���?�;d?�;d?�|�AԾwAԾwAԾwA�A���A�ȴA�ȴA���A���A�ȴA�ƨA�A�ȴA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A���A���A���A��
A��
A��
A��/A��;A��;A��/A��#A��/A��/A��`A��mA��yA��yA��mA��TA��TA��;A��HA��/A��/A��;A��TA��`A��TA��TA��HA��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 A�ƨA���A���A���A���A��
A��;A��/A��;A��yA��`A��;A��TA��TA��`A��mA��yA��mA��`A��`A��TA��A�1A��A� �A�$�A�$�A�$�A� �A��A��A��A��A�oA�  A��;AԴ9Aԙ�A�p�A�9XA�A��
Aӧ�AӉ7A�l�A�ZA� �A���A��
Aҩ�A�1'A���A��A�%A̗�A�&�Aǧ�A�bA��Aã�A�9XA�A�ȴA�A���A�~�A���A�$�A��yA��HA�1'A��hA���A�K�A���A�ĜA�1'A�t�A�ĜA��^A��hA��A�v�A�
=A�ffA�^5A��yA�\)A���A�^5A��A�bNA�?}A��-A�/A��A��wA���A�$�A���A��mA�ffA���A��9A��A��9A�XA��9A��RA�oA��A~~�A~$�A}��A|bNAx��As\)Ap$�AkS�Ai�Ah  Ae�Ab�!A`JA[�AV��AR�+AQ�AOt�ANbNAM�PAL �AJ�AIdZAF�\ACƨA?��A<v�A:�!A9VA7��A5�hA3�A2(�A0�A/XA.�A,Q�A+�A(A�A'K�A&Q�A%�hA$��A$v�A$VA$-A$�A#�FA#K�A#l�A"��A"=qA"bA!�A ��A A�AJA�AC�A+A(�A��A33A��A1A��AVAz�AQ�A  A�TA33Av�A
=A�^A�A��A9XA��A
�A"�AƨA�/A7LA`BA��AA%A?}A�yA�A��A\)A
�A
�+A	S�A�`A~�A�A`BA�+A  A��AdZA
=AjA�^AdZAȴAA�A�A�#AƨA�PAC�A�A ��A ��A �`A �jA �jA (�@�/@���@�t�@�V@��T@���@�`B@�bN@���@�33@��j@�@��@��@�h@�A�@��m@�P@��y@��@�O�@�Ĝ@�j@��m@��@�$�@�Q�@�l�@柾@��@�%@�@�|�@�n�@�M�@�@��@�hs@�j@�
=@��@ݡ�@��`@���@۾w@ڸR@�@� �@��@֧�@�5?@պ^@�O�@��@Լj@�z�@�Q�@� �@�  @ӝ�@�l�@�;d@�"�@��y@ҏ\@�M�@ѡ�@���@��@��@·+@�@��#@��T@��@�M�@�hs@�?}@�O�@ͺ^@�~�@�^5@Ͳ-@́@�`B@��`@�o@�E�@�=q@�@ɩ�@�x�@�G�@��/@ȴ9@�1'@�b@ǍP@�o@Ƨ�@��@�-@��@�`B@�/@��@��/@���@î@�@���@�G�@��9@���@���@�z�@���@��P@�t�@���@�@��@���@��D@�A�@��
@�;d@���@�~�@�=q@��#@��@���@�j@�1'@��@��@���@�ff@�{@��@��^@�O�@�Ĝ@� �@��;@���@���@���@�~�@�v�@�5?@��@�&�@��D@�1@��F@�;d@�@��@�^5@�5?@��@�X@�&�@���@���@���@�~�@�ff@�=q@�p�@��/@��@�(�@� �@�(�@��;@�S�@�S�@�o@��@��@���@���@��@��@��/@��D@�9X@�|�@�C�@�"�@��R@���@��+@�v�@�^5@�J@���@��@���@��D@�Z@�ƨ@�C�@���@��@���@�5?@��@�G�@���@��@��@���@�K�@�;d@�
=@�~�@�M�@�=q@�E�@�=q@��T@�&�@�%@���@���@��@��/@��u@�z�@�I�@�  @���@�K�@��y@���@�ff@�-@�J@���@�/@�%@�Ĝ@�I�@�b@�ƨ@�l�@�S�@�+@�@���@��@���@�^5@���@���@��^@��-@��@�p�@�X@�G�@�?}@��@���@� �@���@��;@��@�l�@��@�v�@�V@�$�@��-@���@��7@�x�@�p�@�V@���@���@�bN@� �@�  @��m@��w@���@�|�@�dZ@�C�@��@��!@��\@�^5@�5?@�J@���@��#@��^@���@�hs@�X@�G�@���@���@���@�Q�@�(�@��w@�dZ@�;d@�"�@��y@���@��\@�v�@�@��-@��@�X@�/@�%@���@��@��u@�j@�Z@�A�@�I�@�9X@�b@��@�ƨ@�dZ@�K�@�@�ȴ@��R@���@���@�v�@�ff@�5?@��T@���@�G�@�V@�%@���@��`@��u@�(�@��@�1@�  @�@�@~�+@}/@|��@|Z@{�
@{t�@{@z�!@z^5@z�@y�^@y��@yX@y&�@x��@x�@x �@w\)@w�@v�@vE�@u@t��@t�D@tj@t(�@s�F@sC�@r�H@q�#@qx�@qx�@qx�@qhs@qG�@p��@p  @o��@ol�@o
=@nff@nE�@n{@m�@m��@m�@mV@l�@l��@k��@k�@k33@ko@j�!@j^5@j�@i��@iX@iG�@h��@h�@g�w@g�P@gl�@f��@f�y@f��@f{@e��@e�h@e/@d�j@d�D@d1@c��@cS�@b��@b-@aG�@`�9@` �@_��@_\)@^�R@^ff@^$�@]��@]O�@\z�@\1@[�m@[33@Z�@Z�@Z��@Zn�@ZJ@Yx�@X��@X1'@W��@WK�@VV@U�@U�@T�@T�D@TZ@T�@S��@St�@So@R^5@Q��@Q�7@Q&�@Q7L@Q�@P�u@P  @O�P@Ol�@O�@O
=@N�y@N�+@Nv�@N5?@M��@M��@MV@M�@Lz�@K��@Kƨ@L9X@L1@Kƨ@K��@K�@K��@K"�@J�@J�@J�H@J��@J�!@J��@J~�@HĜ@H �@G��@G��@GK�@F��@FV@F{@E@E`B@E?}@D�@D��@D�@C��@Cƨ@C33@B��@A��@@��@@Q�@@A�@?��@?��@?l�@?;d@>�y@>ȴ@>��@>E�@>{@>@=�T@=�-@=p�@<��@<��@<j@<1@;��@;dZ@;o@:~�@:^5@:=q@:=q@:-@:�@9�@9��@9�^@9&�@8bN@81'@8b@7|�@7�@6�y@6��@5�@5�@5p�@5O�@5O�@4�j@4j@4(�@41@3�m@3dZ@3C�@3C�@2�@2~�@2^5@2=q@1&�@1�@1%@1%@0Ĝ@0Ĝ@0�9@0�9@0��@0bN@/�;@/|�@.�y@.��@.ff@-@-�@,�/@,z�@+��@+�m@+�
@+ƨ@+��@+t�@*~�@)�#@)�^@)��@)7L@)%@(�`@(r�@(bN@(Q�@(A�@(A�@(b@'��@'|�@'\)@'�@&�R@&��@&��@&V@&@%��@%�-@%�h@%�h@%p�@%?}@%�@$�@$�j@$�j@$��@$�D@$j@$9X@#S�@"�H@"n�@"=q@"�@"J@!��@!%@ �9@ �u@ bN@ A�@  �@�@�;@�w@�@��@K�@+@��@�@��@V@�T@�@p�@p�@O�@?}@�@V@�/@�@z�@I�@1@��@�m@�
@�F@33@�@��@�\@~�@M�@M�@�@��@�#@��@��@�7@x�@%@��@r�@Q�@ �@�@�@ȴ@��@��@ff@@��@��@�h@p�@�@�/@z�@I�@9X@��@��@S�@"�@�@��@�!@M�@=q@��@��@��@�7@x�@x�@hs@X@7L@7L@&�@�`@��@�@Q�@�@�@��@�P@;d@
=@ȴ@��@�+@E�@�@�T@�T@�T@��@@��@�h@�@�@�@�@p�@p�@O�@�/@��@��@
��@
^5@
J@	%@�@�@ȴ@$�@��@`B@�/@j@�@�!@�^@ Ĝ@ A�@   ?��?�{?�/?�V?�I�?�dZ?��?�X?��9?�ff?�?�9X?�S�?�!?�J?�G�?�Ĝ?�A�?�|�??��-?�j?��?��H?�^5?�X?�9?�
=?��?�?�j?�F?�S�?�!?��?�%?� �?޸R?ޗ�?ݲ-?ۥ�?�"�?�"�?�^5?�x�?���?�r�?�r�?�
=?֧�?�E�?�$�?�?�?}?ԛ�?�z�?ӶF?�33?�33?�J?��?���?�G�?��`?� �?Ͼw?ϝ�?�|�?��?�{?Ͳ-?�p�?���?�I�?�1?�1?�"�?�C�?�~�?���?��#?��?���?�Q�?�Q�?�Q�?�1'?�
=?Ƈ+?�E�?�ff?��T?š�?�?š�?��?���?ě�?�z�?�9X?���?���?���?���?��
?Õ�?�t�?�S�?�o?\?�M�?��?���?���?���?���?��7?�hs?��`?�Ĝ?�A�?� �?��;?�  ?�  ?��w?��w?���?�\)?�\)?��?��?��?��?��?��R?��R?��R?�V?�5??�{?�{?�{?���?���?�p�?�p�?�p�?�p�?�O�?�p�?�/?��?���?���?��?�j?�I�?�I�?�I�?�(�?��m?��m?�ƨ?���?��?��?�dZ?��?�dZ?�dZ?�dZ?�dZ?�C�?�"�?�"�?�C�?�C�?�dZ?�C�?�C�?�C�?�dZ?��?���?�ƨ?�ƨ?�ƨ?��m?�ƨ?��m?��m?��m?��m?��m?��m?��m?�1?�1?�1?�(�?�I�?�(�?�I�?��D?�j?�j?��D?�j?�I�?�j?�j?�j?��D?��D?��?��?��?��?���?��?���?��?���?��?��?��?�V?�/?�O�?�O�?�p�?�p�?��h?��-?��-?���?���?��?�{?�5??�V?�5??�v�?�v�?�v�?�v�?��R?��R?��R?��R?��?���?��?��?�;d?��?��?�;d?�;d?�\)?�|�?���?�|�?�|�?���?���?��w?��;?�  ?��;?�  ?�  ?�  ?� �?�A�?�A�?�A�?�bN?�bN?�bN?�bN?�bN?��?���?��?��?��?��?� �?��?��?���?�;d?�;d?�|�AԾwAԾwAԾwA�A���A�ȴA�ȴA���A���A�ȴA�ƨA�A�ȴA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A�ȴA���A���A���A���A���A��
A��
A��
A��/A��;A��;A��/A��#A��/A��/A��`A��mA��yA��yA��mA��TA��TA��;A��HA��/A��/A��;A��TA��`A��TA��TA��HA��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�1B	�1B	�1B	�1B	�7B	�=B	�DB	�DB	�JB	�VB	�JB	�DB	�JB	�JB	�JB	�DB	�DB	�=B	�=B	�7B	�=B	�VB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�uB	��B	��B	��B	��B	��B	�B	�dB	�wB	�?B	�B	�B	��B	�dB	�NB	�TB	��B	ŢB	�-B	��B	�^B	�)B
VB
'�B
.B
&�B
:^B
cTB
�B
ƨB
�yB
��BB%B$�B1'B;dBI�BC�BM�B2-B(�B/B(�B-B33B5?BA�B!�B
��B
�mB
ÖB
��B
�uB
�B
x�B
��B
��B
�uB
�B
�B
R�B	�B	�ZB	�B	�B	ĜB	��B	��B	�B	�B	�B	�#B	�!B	��B	�B	}�B	k�B	\)B	H�B	33B	�B	JB	�B	bB	JB	DB	%B��B��B�B��B�B�B��B��B�
B�
B�B�/B�5B�BB�5B��B��BȴBƨBŢBƨBǮB�B�HB�sB�B��B��B��B	�B	�B	�B	�B	�B	$�B	&�B	(�B	%�B	�B		7B	PB	B��B�B��B��B�B��B��B��B��B	B	B	%B	B��B��B��B��B��B		7B	bB	!�B	8RB	>wB	F�B	W
B	\)B	\)B	k�B	s�B	r�B	q�B	q�B	p�B	t�B	r�B	q�B	p�B	o�B	k�B	iyB	gmB	iyB	hsB	gmB	dZB	bNB	bNB	aHB	aHB	e`B	ffB	k�B	m�B	l�B	k�B	o�B	u�B	w�B	w�B	{�B	z�B	v�B	u�B	t�B	u�B	v�B	y�B	y�B	z�B	� B	}�B	u�B	w�B	�B	�B	�B	�B	�%B	�1B	�1B	�=B	�=B	�JB	�VB	�VB	�PB	�VB	�=B	�7B	�+B	�DB	�=B	�7B	�+B	�B	�B	�B	�B	�+B	�%B	�B	�B	�7B	�+B	�7B	�1B	�B	�%B	� B	�B	�B	�%B	�1B	�7B	�DB	�PB	�bB	�hB	�oB	�oB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�FB	�9B	�FB	�FB	�^B	B	ĜB	ÖB	ĜB	ŢB	ĜB	��B	�}B	��B	��B	B	ĜB	ƨB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�
B	�B	�
B	�B	�B	�B	�#B	�B	�#B	�)B	�)B	�)B	�/B	�)B	�)B	�5B	�5B	�5B	�5B	�5B	�5B	�/B	�)B	�5B	�5B	�;B	�;B	�5B	�/B	�BB	�BB	�HB	�NB	�TB	�TB	�NB	�NB	�TB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�TB	�TB	�NB	�BB	�HB	�HB	�NB	�ZB	�`B	�mB	�fB	�fB	�sB	�B	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
%B
B
B
%B
%B
+B
1B
+B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B

=B
DB
DB
DB
DB
DB
DB
DB
JB
JB
JB
PB
PB
PB
PB
JB
PB
VB
VB
VB
VB
VB
VB
VB
\B
VB
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
hB
oB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
!�B
!�B
"�B
!�B
!�B
"�B
!�B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
$�B
%�B
%�B
$�B
$�B
%�B
%�B
$�B
%�B
%�B
&�B
%�B
%�B
'�B
&�B
'�B
(�B
(�B
)�B
)�B
+B
+B
+B
,B
+B
+B
+B
,B
,B
,B
-B
-B
/B
/B
/B
/B
/B
/B
/B
0!B
0!B
1'B
0!B
1'B
2-B
2-B
2-B
2-B
33B
33B
49B
33B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
8RB
7LB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
;dB
:^B
<jB
<jB
<jB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
A�B
@�B
@�B
@�B
>wB
@�B
?}B
@�B
?}B
?}B
?}B
?}B
?}B
>wB
>wB
>wB
>wB
=qB
?}B
>wB
?}B
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
B�B
C�B
E�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
I�B
H�B
J�B
K�B
J�B
J�B
J�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
P�B
O�B
O�B
P�B
O�B
O�B
P�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
N�B
O�B
O�B
N�B
O�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
Q�B
R�B
R�B
S�B
R�B
S�B
S�B
S�B
R�B
R�B
S�B
S�B
T�B
T�B
VB
W
B
W
B
XB
XB
XB
XB
XB
XB
XB
XB
XB
XB
YB
XB
XB
YB
XB
XB
XB
XB
XB
XB
XB
XB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
\)B
\)B
\)B
^5B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
e`B
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
e`B
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
gmB
gmB
hsB
iyB
iyB
jB
jB
jB
iyB
jB
k�B
k�B
k�B
k�B
k�B
k�B
l�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
l�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
p�B
o�B
o�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
q�B
s�B
s�B
s�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
u�B
v�B
v�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
y�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
z�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
}�B
~�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�%B
�+B
�1B
�+B
�1B
�1B
�=B
�=B
�JB
�JB
�JB
�JB
�PB
�PB
�VB
�VB
�VB
�\B
�bB
�bB
�hB
�oB
�oB
�oB
�{B
��B
��B
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
��B
��B
��B
��B
��B
��B
��B
��B
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
��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�!B
�!B
�!B
�!B
�'B
�!B
�!B
�'B
�-B
�'B
�-B
�-B
�3B
�9B
�9B
�9B
�?B
�?B
�?B
�FB
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�?B
�FB
�FB
�LB
�FB
�FB
�LB
�FB
�LB
�LB
�LB
�RB
�LB
�RB
�LB
�LB
�RB
�RB
�RB
�XB
�RB
�XB
�XB
�XB
�^B
�XB
�^B
�^B
�^B
�XB
�^B
�XB
�^B
�dB
�dB
�dB
�dB
�dB
�jB
�dB
�dB
�dB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�jB
�qB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�qB
�wB
�wB
�wB
�qB
�wB
�qB
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
�}B
�wB
�}B
�}B
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
B
B
B
B
��B
��B
B
��B
B
B
B
B
B
B
��B
ÖB
B
ÖB
B
B
B
B
B
B
ÖB
ÖB
ÖB
ÖB
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ÖB
ĜB
ƨB
ŢB
ŢB
ĜB
ŢB
ŢB	�1B	�+B	�1B	�7B	�1B	�1B	�7B	�1B	�+B	�+B	�1B	�1B	�+B	�1B	�1B	�+B	�1B	�1B	�1B	�1B	�1B	�7B	�7B	�1B	�1B	�+B	�7B	�1B	�1B	�1B	�7B	�7B	�7B	�7B	�DB	�DB	�JB	�DB	�DB	�DB	�=B	�DB	�VB	�VB	�PB	�PB	�JB	�DB	�=B	�JB	�DB	�DB	�DB	�JB	�JB	�JB	�JB	�DB	�JB	�DG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�%B	�B	�%B	�%B	�%B	�B	�B	�B	�B	�B	�B	�1B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�uB	�hB	�VB	�PB	�\B	�bB	��B	��B	��B	��B	�?B	�RB	�B	��B	��B	��B	�?B	�)B	�/B	��B	�}B	�B	��B	�9B	�B
1B
!�B
'�B
 �B
49B
]/B
��B
��B
�TB
�B
��B  B�B+B5?BC�B=qBG�B,B"�B(�B"�B&�B-B/B;dB�B
��B
�HB
�qB
��B
�PB
z�B
r�B
��B
��B
�PB
|�B
{�B
L�B	�fB	�5B	��B	��B	�wB	ĜB	��B	��B	�B	�B	��B	��B	��B	|�B	w�B	e`B	VB	B�B	-B	�B	%B	bB	
=B	%B	B	  B��B�B�sB�B�yB��B��B��B��B��B��B�
B�B�#B�B��BɺBÖB��B��B��BB��B�)B�TB�B��B�B��B	hB	{B	�B	�B	�B	�B	!�B	#�B	 �B	oB	B	1B��B�B�B�B�B�B�B��B��B��B��B��B	B��B��B�B�B��B��B	B	DB	�B	33B	9XB	A�B	Q�B	W
B	W
B	ffB	n�B	m�B	l�B	l�B	k�B	o�B	m�B	l�B	k�B	jB	ffB	dZB	bNB	dZB	cTB	bNB	_;B	]/B	]/B	\)B	\)B	`BB	aHB	ffB	hsB	gmB	ffB	jB	p�B	r�B	r�B	v�B	u�B	q�B	p�B	o�B	p�B	q�B	t�B	t�B	u�B	z�B	x�B	p�B	r�B	{�B	}�B	|�B	� B	�B	�B	�B	�B	�B	�+B	�7B	�7B	�1B	�7B	�B	�B	�B	�%B	�B	�B	�B	� B	~�B	� B	~�B	�B	�B	}�B	|�B	�B	�B	�B	�B	� B	�B	z�B	|�B	� B	�B	�B	�B	�%B	�1B	�DB	�JB	�PB	�PB	�VB	�\B	�\B	�\B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�B	�'B	�'B	�?B	�qB	�}B	�wB	�}B	��B	�}B	�dB	�^B	�dB	�jB	�qB	�}B	��B	ÖB	ĜB	ŢB	ĜB	ŢB	ŢB	ƨB	ƨB	ǮB	ǮB	ǮB	ȴB	ɺB	ɺB	ɺB	��B	ɺB	��B	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�
B	�
B	�
B	�B	�
B	�
B	�B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�5B	�/B	�/B	�5B	�;B	�;B	�;B	�;B	�BB	�BB	�5B	�5B	�/B	�#B	�)B	�)B	�/B	�;B	�BB	�NB	�HB	�HB	�TB	�`B	�ZB	�ZB	�`B	�fB	�mB	�sB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
1B
1B
+B
1B
	7B
	7B
	7B
	7B
	7B
	7B
	7B

=B
	7B

=B

=B
DB
DB
DB
JB
JB
JB
JB
PB
PB
JB
PB
PB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
hB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
 �B
 �B
�B
�B
 �B
 �B
�B
 �B
 �B
!�B
 �B
 �B
"�B
!�B
"�B
$�B
$�B
$�B
%�B
&�B
&�B
&�B
'�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
+B
+B
+B
+B
+B
+B
+B
,B
,B
-B
,B
-B
.B
.B
.B
.B
/B
/B
0!B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
33B
33B
49B
33B
49B
49B
49B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
7LB
6FB
8RB
8RB
8RB
9XB
:^B
:^B
;dB
;dB
;dB
<jB
<jB
=qB
<jB
<jB
<jB
:^B
<jB
;dB
<jB
;dB
;dB
;dB
;dB
;dB
:^B
:^B
:^B
:^B
9XB
;dB
:^B
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
=qB
=qB
>wB
?}B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
E�B
D�B
F�B
G�B
F�B
F�B
F�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
L�B
K�B
K�B
L�B
K�B
K�B
L�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
J�B
K�B
K�B
J�B
K�B
L�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
N�B
O�B
N�B
O�B
O�B
O�B
N�B
N�B
O�B
O�B
P�B
P�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
S�B
S�B
T�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
XB
XB
XB
ZB
YB
YB
YB
YB
YB
YB
ZB
YB
YB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
dZB
cTB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
e`B
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
hsB
hsB
hsB
iyB
iyB
hsB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
l�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
m�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
r�B
r�B
q�B
q�B
r�B
r�B
r�B
s�B
s�B
s�B
r�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
u�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
v�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
y�B
y�B
y�B
z�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{�B
{�B
|�B
}�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�+B
�+B
�7B
�7B
�7B
�7B
�=B
�=B
�DB
�DB
�DB
�JB
�PB
�PB
�VB
�\B
�\B
�\B
�hB
�oB
�oB
�hB
�oB
�oB
�uB
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
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�!B
�B
�!B
�!B
�'B
�-B
�-B
�-B
�3B
�3B
�3B
�9B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�3B
�9B
�9B
�?B
�9B
�9B
�?B
�9B
�?B
�?B
�?B
�FB
�?B
�FB
�?B
�?B
�FB
�FB
�FB
�LB
�FB
�LB
�LB
�LB
�RB
�LB
�RB
�RB
�RB
�LB
�RB
�LB
�RB
�XB
�XB
�XB
�XB
�XB
�^B
�XB
�XB
�XB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
�dB
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
�qB
�qB
�qB
�jB
�qB
�jB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�qB
�wB
�qB
�wB
�qB
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
��B
�}B
��B
��B
��B
�}B
�}B
��B
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
B
B
B
B
��B
��B
B
��B
B
B
B
B
B
B
��B
ÖB
B
ÖB
B
B
B
B
B
B
ÖB
ÖB
ÖB
ÖB
B
ÖB
B
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ÖB
ĜB
ĜB
ÖB
ĜB
ƨB
ŢB
ŢB
ĜB
ŢB
ŢB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�B	�B	�B	�B	�B	�1B	�1B	�+B	�+B	�%B	�B	�B	�%B	�B	�B	�B	�%B	�%B	�%B	�%B	�B	�%B	�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�202009091100392021061413543020210614135430202106141748132021061417481320210614174813202009091100392021061413543020210614135430202106141748132021061417481320210614174813PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            new conductivity = original conductivity * (1 + delta*TEMP + CPcor_SBE*PRES) / (1 + delta*TEMP_ADJUSTED + CPcor_new*PRES_ADJUSTED)                                                                                                                              none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS                                                                                                                                                                                                                                       surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.006 (+/-0)                                                                                                                                                                                                    surface_pressure=-0.50 dbar                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CPcor_new = -1.25e-07; CPcor_SBE = -9.57e-08; delta = 3.25e-06                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            OW r = 0.9999 (+/-0), vertically averaged dS = -0.006 (+/-0)                                                                                                                                                                                                    Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                      none                                                                                                                                                                                                                                                            New conductivity computed by using a different CPcor value from that provided by Sea-Bird.                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Significant salinity sensor drift detected. OW least squares fit adopted.                                                                                                                                                                                       PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            AO  AO  ARCAARCAADJPADJP                                                                                                                                        2020090911003920200909110039  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020090911003920200909110039QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020090911003920200909110039QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               PM  PM  ARSQARSQCOWGCOWGV1.1V1.1                                                                                                                                2021072216020920210722160209IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                