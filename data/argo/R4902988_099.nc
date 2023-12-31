CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-30T12:51:47Z creation;2022-06-30T12:51:48Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220630125147  20220630125730  4902988                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               cA   JA                                  2B  A   APEX                            8600                            2.11.3                          846 @���b:g�1   @��ޟۗS@<O�;dZ�c�ě��T1   GPS     A   A   F   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @���@�  A   A   A@  A`  A�  A���A���A�  A�  A�  A���A�  B   B  B  B  B   B(  B/��B7��B?��BG��BP  BX  B`  Bh  Bo��Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C  C�fC   C"�C$�C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��3C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��D fD � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D	� D
  D
� D  D� D  D� D  D� D  D� DfD� D  D� DfD� D  D�fD  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D   D y�D!  D!� D"  D"� D#  D#� D$  D$y�D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-�fD.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4fD4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDDfDD� DD��DE� DF  DF� DG  DG�fDH  DH� DI  DI�fDJfDJ� DJ��DK� DLfDL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� DbfDb� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dg��Dhy�Di  Di� Dj  Dj� Dk  Dky�Dk��Dl� DmfDm� Dn  Dn� Do  Do� Dp  Dp� Dp��Dq� Dr  Dr� Ds  Ds�fDtfDt�fDufDu� Du��Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~�fDfD� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D���D���D�@ D�� D�� D���D�<�D�|�D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D���D�<�DĀ D�� D�  D�@ Dŀ D��3D�  D�@ Dƀ D�� D���D�<�Dǀ D�� D�  D�@ D�|�Dȼ�D�  D�<�D�|�D�� D�  D�@ Dʀ D�� D���D�<�D�|�D˼�D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ D�|�DѼ�D�  D�@ DҀ DҼ�D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڃ3D�� D�  D�C3Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�C3D�3D�� D�  D�C3D� D�� D�  D�<�D�|�D��D�  D�@ D�3D��3D�  D�@ D� D��3D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��D���D�@ D� D�� D�  D�<�D�|�D��D���D�@ D� D�� D�  D�@ D� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�c3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�G�@��@��A�
A;�
A[�
A{�
A��RA��RA��A��A��A޸RA��A��B��B��B��B��B&��B.�]B6�]B>�]BF�]BN��BV��B^��Bf��Bn�]Bv��B~��B�z�B�z�B�G�B�z�B�z�B�z�B�z�B�z�B�G�B�z�B�z�B��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B��B�z�B�z�C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�qC�qC�C�qC��C�qC!�C#�C%�qC'�qC)�qC+�C-�qC/�qC1�qC3�qC5�qC7�qC9�C;�qC=�qC?�qCA��CC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�Cq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C��C�޸C���C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C���C�޸C�޸C���C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C��C��C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C���C�޸C�޸C�޸C���C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C��C�޸C�޸C�޸C�޸C�޸C��C��D o\D �\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D��Dh�D��D	o\D	�\D
o\D
�\Do\D�\Do\D�\Do\D�\Do\D��Do\D�\Do\D��Do\D�\Du�D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Du�D�\Do\D�\Do\D�\Do\D�\Dh�D�\Do\D�\Do\D�\Do\D�\D h�D �\D!o\D!�\D"o\D"�\D#o\D#�\D$h�D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)��D*o\D*�\D+o\D+�\D,o\D,�\D-u�D-�\D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3��D4u�D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCu�DC��DDo\DD��DEo\DE�\DFo\DF�\DGu�DG�\DHo\DH�\DIu�DI��DJo\DJ��DKo\DK��DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ��DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da��Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df��Dgo\Dg��Dhh�Dh�\Dio\Di�\Djo\Dj�\Dkh�Dk��Dlo\Dl��Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp��Dqo\Dq�\Dro\Dr�\Dsu�Ds��Dtu�Dt��Duo\Du��Dvo\Dv�\Dwo\Dw�\Dxo\Dx�\Dyo\Dy�\Dzo\Dz�\D{o\D{�\D|o\D|�\D}o\D}�\D~u�D~��Do\D�\D�:�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�t{D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D��{D�4{D�t{D��{D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D��{D�7�D�w�D���D���D�7�D�t{D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�z�D���D���D�7�D�w�D��{D��{D�7�D�w�D���D��{D�4{D�t{D���D���D�7�D�w�D���D���D�4{D�w�D���D���D�7�D�w�D��{D��{D�7�D�w�D���D���D�7�D�w�D���D���D�4{D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D��{D���D�:�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D·�D���D�7�D�w�D÷�D��{D�4{D�w�Dķ�D���D�7�D�w�Dź�D���D�7�D�w�DƷ�D��{D�4{D�w�DǷ�D���D�7�D�t{Dȴ{D���D�4{D�t{Dɷ�D���D�7�D�w�Dʷ�D��{D�4{D�t{D˴{D���D�7�D�w�D̷�D���D�7�D�t{Dͷ�D���D�7�D�w�Dη�D���D�7�D�w�DϷ�D���D�7�D�w�Dз�D���D�7�D�t{DѴ{D���D�7�D�w�DҴ{D���D�7�D�w�Dӷ�D���D�7�D�w�DԷ�D���D�7�D�w�Dշ�D���D�7�D�w�Dַ�D���D�7�D�w�D׷�D���D�7�D�w�Dط�D���D�7�D�w�Dٷ�D���D�7�D�z�Dڷ�D���D�:�D�w�D۷�D���D�7�D�w�Dܷ�D���D�7�D�w�Dݷ�D���D�7�D�w�D޷�D���D�7�D�w�D߷�D���D�7�D�w�D෮D���D�7�D�w�DᷮD���D�7�D�w�DⷮD���D�7�D�w�D㷮D���D�:�D�z�D䷮D���D�:�D�w�D差D���D�4{D�t{D�{D���D�7�D�z�D��D���D�7�D�w�D��D���D�4{D�w�D鷮D���D�7�D�w�D귮D���D�7�D�w�D뷮D���D�7�D�w�D췮D���D�7�D�w�D���D��{D�7�D�w�DD���D�7�D�w�D﷮D���D�7�D�w�D�{D��{D�7�D�w�D�D���D�4{D�t{D�{D��{D�7�D�w�D�D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D��{D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�Z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AӅ�AӇ�AӇ�Aӈ�AӅ�AӃA�}�A�~�A��A�|A�aHA�ٴA�s�A˦LAȯAǬ�A��A�8RA���A���AŪ�A�>AĐ.A�NpAÎ�A�:A�A�>wA�	�A�{A�WsA��TA���A�xA��%A�_A���A��A���A�[�A���A��>A���A���A��A�HKA��.A�<A��A���A��)A�w�A�@�A���A���A�WsA�sMA��A��A��4A�ĜA�1[A��@A��A��"A�E�A��iA��A���A�CaA��A�S�A�}�A�ƨA���A�.IA�cTA�AA�8�A��DA�sMA��A���A�8�A�8A��A�:�A��MA��iA���A�~]A���A���A�A��	A�%FA��A�c�A�>wA���A��A�-A��fA���A�XyA��A��kA�.A�P}A�t�A��LA�.IA��(A��&A���A���AOA}˒A|�A|�6Az.IAv�4As�YAq�!ApjAm�EAmi�Al��Ajy>AiL0Ahk�AguAeu�Acz�A`�A^�uA\(�AY�AWMAU�DAT�eATxATSAS�zAR�AQ�-AO�&AM��AL�5AL�AK��AJ�eAI�UAI"hAHp;AH�AG,�AEȴAD��AD�ABu�AA��A@�|A@�A>�rA<�MA;�)A;��A9�jA8A6��A6 �A5|�A4��A3�+A3V�A2�A1��A1 �A0iDA/خA/�rA.�9A.I�A-%�A+��A*1�A)&�A(��A'<6A&�A&TaA%�A$%FA#�4A"��A!��A!
=A�3Ap�AjA�2A�5AU�A��A�uAZ�A7A��A��AR�A�UA��A�A�+A\)A��A#:A�Ap;A
�A>BA�OA�HAc�A:*A��A�5AA�
A6A
��A
�A	�bA	g�A�YA4�A��A�XA��A?}A�uA�AP�A��A �A %@���@��@��@���@��@���@�^5@��@��@�@��@�"h@��@�J#@��8@�m]@�/�@�4@���@�@�-w@迱@�!�@�h@�ݘ@�4�@��/@�6@�`�@�rG@�G�@�($@ߋ�@���@ܜx@�F@ڰ�@�C�@�F@�s�@�=�@�l�@��m@�J�@���@��;@ӯ�@�
=@�@Е�@��@�ں@̈́M@̅�@�E�@ˡ�@�Z�@��@�S�@��@�7L@�G�@�@�J�@��@@��p@�	�@���@���@��@��v@�@�خ@�Mj@���@��@��v@��#@��	@�RT@���@�Q@�:*@���@�&�@��@��@��E@���@�?@��T@���@�7L@���@��@�6@�33@��@�(�@���@�p;@�E�@�H@�C�@�,=@��@�!�@�S�@�Ov@���@�T�@��.@�I�@�~@��&@��@���@�Ft@�~@���@��@���@�{J@�	l@���@��2@�ϫ@�!-@���@���@�C-@�e@��]@��#@���@�[W@���@�#:@���@�6z@���@���@�N�@��@�}�@���@�E�@�
�@���@��K@���@���@�x�@�Z�@�!�@���@�Ov@��f@�Ĝ@��U@��\@�i�@�K^@�$@��
@�?}@��|@�J�@�	�@��@�8�@�c@���@��)@��4@�~�@�H�@��Z@��z@��@�j@�J�@�!�@�S@��[@�	@��7@��/@��.@�j@�6�@��}@���@�w2@�f�@�Y�@�33@�o@�@���@��O@��@���@��'@��H@���@��H@��@��K@��H@��$@�z@��@��	@�ߤ@��9@���@�G�@��c@���@�H@�*�@�G@��}@���@��@�iD@�q@��B@�~�@��p@��6@�q�@qv@}�@|�_@{�@{K�@z�M@z�!@y�o@yV@x@w��@v}V@v�@u�N@u��@u`B@u�@u	l@t�p@t�@s{J@sU�@s"�@r�@rC�@q�M@p��@o��@o~�@o,�@n�<@m��@m��@l��@ll"@l7@k)_@jں@j��@j�1@j{�@ji�@j_�@j#:@i�j@i�@iG�@h��@h�e@hV�@g��@g�P@g>�@f��@f��@f�}@fOv@eԕ@e��@e��@e��@e�H@e��@e��@e�)@e��@e��@e�-@e�-@e��@eIR@e�@d�P@d�5@d�/@d�)@d��@d�@c�{@cA�@c$t@c�@b��@b��@b�r@b+k@a�@a�@a��@aO�@`�I@_��@^�"@]��@\�f@\r�@\"h@\1@\�@[�@[a@[@Z��@Z��@Z@Y�9@Y�"@Y5�@Y;@X�$@X��@X�@X��@X�U@XĜ@X�z@X��@Xoi@X�@X  @W�F@W�4@WRT@V�8@V��@VQ@U�)@U�@U�t@U�@U/@T��@T�5@T��@T�@T�@S�0@S��@Sa@R\�@Q�H@Q+@Q�@Qq@Q�@Q@@P��@Pی@P��@P��@PD�@O�@Ob�@O\)@OW?@O�@N�\@Np;@M��@M�n@M-w@L�@L�`@L�p@L��@L��@L�e@L��@LM@K�P@K"�@Jߤ@J�'@J�F@J	@J@I�H@I|@H��@H%�@G�W@G��@G�$@Gb�@F�B@F�+@Fh
@F=q@E�T@E��@EV@DĜ@D��@D��@C��@Cqv@CA�@C4�@CY@B�H@B��@B�@B�}@B�b@B	@Ax�@A(�@@�v@@��@@z�@@'R@?��@?�@?�:@?H�@>�@>��@>��@>?@=�@=@=Q�@=*0@<�K@<Ɇ@<��@<u�@;�A@;��@;K�@;/�@;�@:ȴ@:V@:�@9��@9&�@8��@92a@9G�@8�	@8��@8oi@8�u@8w�@8S�@8?�@8*�@8@7��@7خ@7�*@7K�@7o@6�H@6��@6�<@6�@6=q@5�@5��@5��@5zx@5s�@5k�@5Q�@5q@4ی@4��@4��@4*�@3�W@3�}@3��@3t�@3@2��@1ԕ@1`B@1�@0�@0�)@0�@0�@0��@0Z@0'R@0�@/�@/�[@/��@/\)@/S@.��@.��@.�@.�y@.��@.��@.ȴ@.��@.e@-��@-��@-k�@-c�@-S&@,��@,��@+� @+a@+�@*�8@*�M@*�X@*s�@*;�@*#:@)��@)zx@(�K@(��@(�O@(��@(2�@'��@'��@'\)@'>�@']�@'F�@&��@&�<@&��@&Z�@%�@%�9@%�@%`B@$��@$�4@$oi@$A�@#��@#�@#�4@#Z�@#@O@#!-@"�h@"�@"
�@!�o@!�@!2a@! \@ ��@ �e@ �Y@ [�@ I�@ 7�@ %�@ @��@��@g�@�@��@l�@.�@u@��@�@�@ԕ@�H@��@�M@*0@�@g8@7@��@x@�H@��@�A@R�@:*@O@�o@u�@�@�@��@Ft@�@��@�@�0@.I@ں@�b@q�@c @J�@�@��@�t@��@\�@Q�@+�@�@�E@��@m�@�W@خ@|�@&@�@��@z@h
@^5@L0@�@�^@u�@?}@�@Ɇ@��@g8@S�@Ft@7�@$@�@��@b�@4�@o@�@�@�M@�@s�@B[@4@��@�@�^@hs@*0@!�@�@�@h�@g8@bN@C-@�@�+@�m@�Q@ƨ@iD@F�@@
�"@
�s@
��@
}V@
ff@
@�@
($@
	@
�@
4@

�@	�t@	Dg@	�@�@�)@|�@>B@�@� @�q@��@�Q@�W@ݘ@�Q@ƨ@�k@�4@\)@'�@Y@�@��@�6@��@d�@;�@+k@�@�#@�z@�-@��@c@zx@f�@F@#�@@�K@��@�$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AӅ�AӇ�AӇ�Aӈ�AӅ�AӃA�}�A�~�A��A�|A�aHA�ٴA�s�A˦LAȯAǬ�A��A�8RA���A���AŪ�A�>AĐ.A�NpAÎ�A�:A�A�>wA�	�A�{A�WsA��TA���A�xA��%A�_A���A��A���A�[�A���A��>A���A���A��A�HKA��.A�<A��A���A��)A�w�A�@�A���A���A�WsA�sMA��A��A��4A�ĜA�1[A��@A��A��"A�E�A��iA��A���A�CaA��A�S�A�}�A�ƨA���A�.IA�cTA�AA�8�A��DA�sMA��A���A�8�A�8A��A�:�A��MA��iA���A�~]A���A���A�A��	A�%FA��A�c�A�>wA���A��A�-A��fA���A�XyA��A��kA�.A�P}A�t�A��LA�.IA��(A��&A���A���AOA}˒A|�A|�6Az.IAv�4As�YAq�!ApjAm�EAmi�Al��Ajy>AiL0Ahk�AguAeu�Acz�A`�A^�uA\(�AY�AWMAU�DAT�eATxATSAS�zAR�AQ�-AO�&AM��AL�5AL�AK��AJ�eAI�UAI"hAHp;AH�AG,�AEȴAD��AD�ABu�AA��A@�|A@�A>�rA<�MA;�)A;��A9�jA8A6��A6 �A5|�A4��A3�+A3V�A2�A1��A1 �A0iDA/خA/�rA.�9A.I�A-%�A+��A*1�A)&�A(��A'<6A&�A&TaA%�A$%FA#�4A"��A!��A!
=A�3Ap�AjA�2A�5AU�A��A�uAZ�A7A��A��AR�A�UA��A�A�+A\)A��A#:A�Ap;A
�A>BA�OA�HAc�A:*A��A�5AA�
A6A
��A
�A	�bA	g�A�YA4�A��A�XA��A?}A�uA�AP�A��A �A %@���@��@��@���@��@���@�^5@��@��@�@��@�"h@��@�J#@��8@�m]@�/�@�4@���@�@�-w@迱@�!�@�h@�ݘ@�4�@��/@�6@�`�@�rG@�G�@�($@ߋ�@���@ܜx@�F@ڰ�@�C�@�F@�s�@�=�@�l�@��m@�J�@���@��;@ӯ�@�
=@�@Е�@��@�ں@̈́M@̅�@�E�@ˡ�@�Z�@��@�S�@��@�7L@�G�@�@�J�@��@@��p@�	�@���@���@��@��v@�@�خ@�Mj@���@��@��v@��#@��	@�RT@���@�Q@�:*@���@�&�@��@��@��E@���@�?@��T@���@�7L@���@��@�6@�33@��@�(�@���@�p;@�E�@�H@�C�@�,=@��@�!�@�S�@�Ov@���@�T�@��.@�I�@�~@��&@��@���@�Ft@�~@���@��@���@�{J@�	l@���@��2@�ϫ@�!-@���@���@�C-@�e@��]@��#@���@�[W@���@�#:@���@�6z@���@���@�N�@��@�}�@���@�E�@�
�@���@��K@���@���@�x�@�Z�@�!�@���@�Ov@��f@�Ĝ@��U@��\@�i�@�K^@�$@��
@�?}@��|@�J�@�	�@��@�8�@�c@���@��)@��4@�~�@�H�@��Z@��z@��@�j@�J�@�!�@�S@��[@�	@��7@��/@��.@�j@�6�@��}@���@�w2@�f�@�Y�@�33@�o@�@���@��O@��@���@��'@��H@���@��H@��@��K@��H@��$@�z@��@��	@�ߤ@��9@���@�G�@��c@���@�H@�*�@�G@��}@���@��@�iD@�q@��B@�~�@��p@��6@�q�@qv@}�@|�_@{�@{K�@z�M@z�!@y�o@yV@x@w��@v}V@v�@u�N@u��@u`B@u�@u	l@t�p@t�@s{J@sU�@s"�@r�@rC�@q�M@p��@o��@o~�@o,�@n�<@m��@m��@l��@ll"@l7@k)_@jں@j��@j�1@j{�@ji�@j_�@j#:@i�j@i�@iG�@h��@h�e@hV�@g��@g�P@g>�@f��@f��@f�}@fOv@eԕ@e��@e��@e��@e�H@e��@e��@e�)@e��@e��@e�-@e�-@e��@eIR@e�@d�P@d�5@d�/@d�)@d��@d�@c�{@cA�@c$t@c�@b��@b��@b�r@b+k@a�@a�@a��@aO�@`�I@_��@^�"@]��@\�f@\r�@\"h@\1@\�@[�@[a@[@Z��@Z��@Z@Y�9@Y�"@Y5�@Y;@X�$@X��@X�@X��@X�U@XĜ@X�z@X��@Xoi@X�@X  @W�F@W�4@WRT@V�8@V��@VQ@U�)@U�@U�t@U�@U/@T��@T�5@T��@T�@T�@S�0@S��@Sa@R\�@Q�H@Q+@Q�@Qq@Q�@Q@@P��@Pی@P��@P��@PD�@O�@Ob�@O\)@OW?@O�@N�\@Np;@M��@M�n@M-w@L�@L�`@L�p@L��@L��@L�e@L��@LM@K�P@K"�@Jߤ@J�'@J�F@J	@J@I�H@I|@H��@H%�@G�W@G��@G�$@Gb�@F�B@F�+@Fh
@F=q@E�T@E��@EV@DĜ@D��@D��@C��@Cqv@CA�@C4�@CY@B�H@B��@B�@B�}@B�b@B	@Ax�@A(�@@�v@@��@@z�@@'R@?��@?�@?�:@?H�@>�@>��@>��@>?@=�@=@=Q�@=*0@<�K@<Ɇ@<��@<u�@;�A@;��@;K�@;/�@;�@:ȴ@:V@:�@9��@9&�@8��@92a@9G�@8�	@8��@8oi@8�u@8w�@8S�@8?�@8*�@8@7��@7خ@7�*@7K�@7o@6�H@6��@6�<@6�@6=q@5�@5��@5��@5zx@5s�@5k�@5Q�@5q@4ی@4��@4��@4*�@3�W@3�}@3��@3t�@3@2��@1ԕ@1`B@1�@0�@0�)@0�@0�@0��@0Z@0'R@0�@/�@/�[@/��@/\)@/S@.��@.��@.�@.�y@.��@.��@.ȴ@.��@.e@-��@-��@-k�@-c�@-S&@,��@,��@+� @+a@+�@*�8@*�M@*�X@*s�@*;�@*#:@)��@)zx@(�K@(��@(�O@(��@(2�@'��@'��@'\)@'>�@']�@'F�@&��@&�<@&��@&Z�@%�@%�9@%�@%`B@$��@$�4@$oi@$A�@#��@#�@#�4@#Z�@#@O@#!-@"�h@"�@"
�@!�o@!�@!2a@! \@ ��@ �e@ �Y@ [�@ I�@ 7�@ %�@ @��@��@g�@�@��@l�@.�@u@��@�@�@ԕ@�H@��@�M@*0@�@g8@7@��@x@�H@��@�A@R�@:*@O@�o@u�@�@�@��@Ft@�@��@�@�0@.I@ں@�b@q�@c @J�@�@��@�t@��@\�@Q�@+�@�@�E@��@m�@�W@خ@|�@&@�@��@z@h
@^5@L0@�@�^@u�@?}@�@Ɇ@��@g8@S�@Ft@7�@$@�@��@b�@4�@o@�@�@�M@�@s�@B[@4@��@�@�^@hs@*0@!�@�@�@h�@g8@bN@C-@�@�+@�m@�Q@ƨ@iD@F�@@
�"@
�s@
��@
}V@
ff@
@�@
($@
	@
�@
4@

�@	�t@	Dg@	�@�@�)@|�@>B@�@� @�q@��@�Q@�W@ݘ@�Q@ƨ@�k@�4@\)@'�@Y@�@��@�6@��@d�@;�@+k@�@�#@�z@�-@��@c@zx@f�@F@#�@@�K@��@�$111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B#�B#�B#�B#�B#�B#TB#:B# B#�B#:B�B�B��B�TBϫB�SB��B�cB�]B��B�B�-B�=B��B҉B�&BևB�B�	B�VB�`B��B�&B�sB�B�}B�B�
B��B��B�0B��B�@B�BܬBյB�)B�(B�qBB�MBĶBĜB�'B�B��B��B��B�|B��B�yB��B�=B��B}<By�Bv+Bq�BlBi*Be�B^�BU�BHfBEmB=�B3�BKB�B�XB��B̈́BªB��B�oB��B�BuBmwB]�BT{BIRB9�B"4B��B�4B�2B��B�B�.B�TB��B�B�!B��B�+Bs�Bh�B^BE�B8lB0oB-�B*B%,B�BxB�B
��B
��B
��B
̳B
��B
��B
��B
��B
�pB
�^B
}�B
v�B
o�B
h�B
\�B
Q�B
CaB
5�B
'8B
�B
�B
	�B
�B
�B
�B
MB
 iB	��B	��B	�mB	��B	��B	�QB	�`B	޸B	�=B	�$B	��B	�.B	�~B	�KB	�YB	�}B	�DB	�zB	��B	��B	�B	�B	��B	�<B	~�B	v�B	shB	o�B	jB	fLB	ffB	e�B	a-B	]�B	\CB	\)B	[	B	YKB	V�B	S�B	M�B	J�B	E�B	C{B	BuB	<�B	;dB	9�B	6B	4B	1�B	/OB	,"B	+B	'�B	$tB	#TB	"NB	 \B	;B	�B	�B	/B	�B	WB	_B	�B	�B	�B	 B	�B	<B	�B	jB	�B	�B	
�B		�B	�B	%B	B	�B	-B	�B��B��B�VB��B��B�0B��B�2B��B��B�9B��B�'B��B�B�OB� B��B�KB�B�yB��B�B�XB�mB�RB�sB��B�B�B�B��B��B�`B�&B�FB�tB�tB�B�B�2B�zB�tB�B�tB�B��B��B�$B�B�B��B�B�8B��B�B�yB��B�B��B��B�B��B��B��B�IB�B��B�B�B�-B��B��B��B�B�AB�B��B�-B��B�B�5B�iB�3B�B��B�^B��B�.B	 OB�<B	 B	mB	�B		�B	"B	�B	�B	\B	B	�B	 B	�B	�B	�B	uB	�B	B	�B	�B	sB	�B	�B	�B	YB	�B	]B	B	�B	 �B	 �B	 �B	 'B	!-B	%zB	(�B	(�B	1vB	9rB	>�B	@B	@4B	@4B	A�B	C-B	E�B	FtB	H1B	IB	IB	IB	J=B	O�B	VB	Z�B	]/B	_�B	`'B	b4B	bhB	b�B	b�B	eFB	g�B	j�B	m�B	pUB	r�B	uB	v`B	x8B	z^B	|�B	�iB	��B	�B	��B	��B	�EB	��B	�KB	�B	�#B	�jB	��B	��B	��B	�mB	�yB	�1B	��B	��B	�xB	��B	�HB	��B	�,B	�,B	�DB	�oB	�GB	��B	�hB	�tB	�RB	��B	�B	�qB	�wB	�cB	�OB	��B	��B	�zB	ʦB	��B	��B	�\B	�B	�TB	�@B	ӏB	ӏB	��B	�FB	�B	�aB	��B	֡B	�1B	�#B	�dB	�\B	��B	�sB	�B	��B	�B	�B	�]B	��B	��B	��B	�|B	��B	��B	��B	�B	��B	�B	��B	�B	��B	��B	�JB	��B
 �B
�B
�B
�B
�B
�B
�B
KB
�B
VB
.B
�B
�B
B
eB
	B
 B
"4B
%,B
(sB
)*B
+�B
.�B
1�B
4�B
:*B
="B
>wB
?B
@�B
AoB
C�B
EmB
GEB
H1B
HfB
HfB
G�B
I�B
K�B
M�B
Q�B
R�B
S@B
T�B
VSB
VmB
V�B
W�B
X+B
X�B
ZB
[=B
\CB
]~B
_�B
`BB
a�B
b�B
b�B
c�B
eFB
g�B
hXB
iyB
jKB
k�B
m�B
n�B
o�B
s3B
tB
t�B
utB
v`B
xB
x�B
y�B
y�B
{�B
~B
B
}B
��B
�B
��B
�B
��B
�mB
��B
��B
��B
��B
�mB
�B
��B
��B
��B
��B
��B
�mB
��B
�?B
�tB
�EB
��B
��B
��B
��B
��B
��B
�VB
��B
�\B
�bB
�hB
��B
��B
��B
�$B
��B
�_B
��B
��B
��B
�IB
�OB
��B
�BB
�B
�B
� B
�nB
��B
�@B
�B
�`B
��B
��B
�fB
�mB
�mB
�RB
�8B
��B
��B
�B
�"B
�WB
��B
�wB
�IB
�cB
�cB
��B
�B
�!B
�|B
��B
�9B
�FB
��B
�B
��B
��B
��B
�jB
��B
��B
�"B
�"B
�<B
�qB
��B
��B
�B
��B
��B
�[B
��B
�-B
ðB
�gB
�zB
ȚB
�lB
��B
�	B
ʦB
̘B
̈́B
��B
��B
ΊB
ΊB
��B
��B
�B
�bB
��B
�@B
�[B
�[B
ӏB
өB
��B
�B
�FB
�aB
�B
�sB
רB
ؓB
��B
�eB
��B
�7B
ڠB
ڠB
�qB
��B
�B
�]B
�/B
��B
�B
ߊB
߾B
�BB
�vB
��B
�B
��B
�B
�TB
�nB
�nB
�tB
�B
��B
�B
�>B
�sB
��B
��B
�B
�B
�B
�WB
�B
�]B
�B
��B
��B
�/B
�IB
�B
�5B
�B
�B
�B
�B
�oB
��B
��B
�B
�-B
�aB
�aB
�|B
�B
��B
�MB
�B
��B
�nB
��B
��B
�B
�tB
��B
�FB
�8B
��B
�$B
��B
��B
��B
��B
�B
�^B
��B
��B
�B
�dB
�dB
�B
��B
��B
��B
��B
��B
��B
��B
��B
�<B
��B
�B
��B
��B
��B
��B
�}B
��B BUB�B�B�BAB�BGBaB�B�BSB�BmBmB�B�B�B�BYB�B�B�B�B�B	RB	�B
#B
=B
XB
#B
	B	�B	�B
XB
�B
�B)BDB^B�BJBJB�BjB�BB�B�BBHB.B.BBHB�B�B B�BTBBB�B�B�B�BB@B�B�B{B�BgB�B�B�BBmB�B+B+BEB_B�B�B�BBeB�BBBkBqB�B�B)B]BCB�BdB~B~B�B�BB5BjB�B!B�BpB BB \B \B �B �B!-B!B!-B!�B"4B"�B"�B#B#B#:B#�B#�B#�B#�B$B$tB$�B%`B%�B%�B%�B%�B%�B&LB&�B&�B&�B&�B&�B&�B'B'�B'�B'�B(XB(�B)B)*B)_B)yB)�B)�B)�B)�B*�B*�B+B+6B+�B,B,B,WB,�B,�B,�B,�B,�B,�B-]B-wB-�B-�B.B.�B/B/iB/�B/�B/�B0;B0�B0�B0�B0�B0�B0�B0�B1B1'B1vB1�B1�B1�B2-B2�B2�B2�B2�B33B33B3MB3hB3MB3�B3�B3�B4B4nB4nB4�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   B#�B#�B#�B#�B#�B#TB#:B# B#�B#:B�B�B��B�TBϫB�SB��B�cB�]B��B�B�-B�=B��B҉B�&BևB�B�	B�VB�`B��B�&B�sB�B�}B�B�
B��B��B�0B��B�@B�BܬBյB�)B�(B�qBB�MBĶBĜB�'B�B��B��B��B�|B��B�yB��B�=B��B}<By�Bv+Bq�BlBi*Be�B^�BU�BHfBEmB=�B3�BKB�B�XB��B̈́BªB��B�oB��B�BuBmwB]�BT{BIRB9�B"4B��B�4B�2B��B�B�.B�TB��B�B�!B��B�+Bs�Bh�B^BE�B8lB0oB-�B*B%,B�BxB�B
��B
��B
��B
̳B
��B
��B
��B
��B
�pB
�^B
}�B
v�B
o�B
h�B
\�B
Q�B
CaB
5�B
'8B
�B
�B
	�B
�B
�B
�B
MB
 iB	��B	��B	�mB	��B	��B	�QB	�`B	޸B	�=B	�$B	��B	�.B	�~B	�KB	�YB	�}B	�DB	�zB	��B	��B	�B	�B	��B	�<B	~�B	v�B	shB	o�B	jB	fLB	ffB	e�B	a-B	]�B	\CB	\)B	[	B	YKB	V�B	S�B	M�B	J�B	E�B	C{B	BuB	<�B	;dB	9�B	6B	4B	1�B	/OB	,"B	+B	'�B	$tB	#TB	"NB	 \B	;B	�B	�B	/B	�B	WB	_B	�B	�B	�B	 B	�B	<B	�B	jB	�B	�B	
�B		�B	�B	%B	B	�B	-B	�B��B��B�VB��B��B�0B��B�2B��B��B�9B��B�'B��B�B�OB� B��B�KB�B�yB��B�B�XB�mB�RB�sB��B�B�B�B��B��B�`B�&B�FB�tB�tB�B�B�2B�zB�tB�B�tB�B��B��B�$B�B�B��B�B�8B��B�B�yB��B�B��B��B�B��B��B��B�IB�B��B�B�B�-B��B��B��B�B�AB�B��B�-B��B�B�5B�iB�3B�B��B�^B��B�.B	 OB�<B	 B	mB	�B		�B	"B	�B	�B	\B	B	�B	 B	�B	�B	�B	uB	�B	B	�B	�B	sB	�B	�B	�B	YB	�B	]B	B	�B	 �B	 �B	 �B	 'B	!-B	%zB	(�B	(�B	1vB	9rB	>�B	@B	@4B	@4B	A�B	C-B	E�B	FtB	H1B	IB	IB	IB	J=B	O�B	VB	Z�B	]/B	_�B	`'B	b4B	bhB	b�B	b�B	eFB	g�B	j�B	m�B	pUB	r�B	uB	v`B	x8B	z^B	|�B	�iB	��B	�B	��B	��B	�EB	��B	�KB	�B	�#B	�jB	��B	��B	��B	�mB	�yB	�1B	��B	��B	�xB	��B	�HB	��B	�,B	�,B	�DB	�oB	�GB	��B	�hB	�tB	�RB	��B	�B	�qB	�wB	�cB	�OB	��B	��B	�zB	ʦB	��B	��B	�\B	�B	�TB	�@B	ӏB	ӏB	��B	�FB	�B	�aB	��B	֡B	�1B	�#B	�dB	�\B	��B	�sB	�B	��B	�B	�B	�]B	��B	��B	��B	�|B	��B	��B	��B	�B	��B	�B	��B	�B	��B	��B	�JB	��B
 �B
�B
�B
�B
�B
�B
�B
KB
�B
VB
.B
�B
�B
B
eB
	B
 B
"4B
%,B
(sB
)*B
+�B
.�B
1�B
4�B
:*B
="B
>wB
?B
@�B
AoB
C�B
EmB
GEB
H1B
HfB
HfB
G�B
I�B
K�B
M�B
Q�B
R�B
S@B
T�B
VSB
VmB
V�B
W�B
X+B
X�B
ZB
[=B
\CB
]~B
_�B
`BB
a�B
b�B
b�B
c�B
eFB
g�B
hXB
iyB
jKB
k�B
m�B
n�B
o�B
s3B
tB
t�B
utB
v`B
xB
x�B
y�B
y�B
{�B
~B
B
}B
��B
�B
��B
�B
��B
�mB
��B
��B
��B
��B
�mB
�B
��B
��B
��B
��B
��B
�mB
��B
�?B
�tB
�EB
��B
��B
��B
��B
��B
��B
�VB
��B
�\B
�bB
�hB
��B
��B
��B
�$B
��B
�_B
��B
��B
��B
�IB
�OB
��B
�BB
�B
�B
� B
�nB
��B
�@B
�B
�`B
��B
��B
�fB
�mB
�mB
�RB
�8B
��B
��B
�B
�"B
�WB
��B
�wB
�IB
�cB
�cB
��B
�B
�!B
�|B
��B
�9B
�FB
��B
�B
��B
��B
��B
�jB
��B
��B
�"B
�"B
�<B
�qB
��B
��B
�B
��B
��B
�[B
��B
�-B
ðB
�gB
�zB
ȚB
�lB
��B
�	B
ʦB
̘B
̈́B
��B
��B
ΊB
ΊB
��B
��B
�B
�bB
��B
�@B
�[B
�[B
ӏB
өB
��B
�B
�FB
�aB
�B
�sB
רB
ؓB
��B
�eB
��B
�7B
ڠB
ڠB
�qB
��B
�B
�]B
�/B
��B
�B
ߊB
߾B
�BB
�vB
��B
�B
��B
�B
�TB
�nB
�nB
�tB
�B
��B
�B
�>B
�sB
��B
��B
�B
�B
�B
�WB
�B
�]B
�B
��B
��B
�/B
�IB
�B
�5B
�B
�B
�B
�B
�oB
��B
��B
�B
�-B
�aB
�aB
�|B
�B
��B
�MB
�B
��B
�nB
��B
��B
�B
�tB
��B
�FB
�8B
��B
�$B
��B
��B
��B
��B
�B
�^B
��B
��B
�B
�dB
�dB
�B
��B
��B
��B
��B
��B
��B
��B
��B
�<B
��B
�B
��B
��B
��B
��B
�}B
��B BUB�B�B�BAB�BGBaB�B�BSB�BmBmB�B�B�B�BYB�B�B�B�B�B	RB	�B
#B
=B
XB
#B
	B	�B	�B
XB
�B
�B)BDB^B�BJBJB�BjB�BB�B�BBHB.B.BBHB�B�B B�BTBBB�B�B�B�BB@B�B�B{B�BgB�B�B�BBmB�B+B+BEB_B�B�B�BBeB�BBBkBqB�B�B)B]BCB�BdB~B~B�B�BB5BjB�B!B�BpB BB \B \B �B �B!-B!B!-B!�B"4B"�B"�B#B#B#:B#�B#�B#�B#�B$B$tB$�B%`B%�B%�B%�B%�B%�B&LB&�B&�B&�B&�B&�B&�B'B'�B'�B'�B(XB(�B)B)*B)_B)yB)�B)�B)�B)�B*�B*�B+B+6B+�B,B,B,WB,�B,�B,�B,�B,�B,�B-]B-wB-�B-�B.B.�B/B/iB/�B/�B/�B0;B0�B0�B0�B0�B0�B0�B0�B1B1'B1vB1�B1�B1�B2-B2�B2�B2�B2�B33B33B3MB3hB3MB3�B3�B3�B4B4nB4nB4�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220630125038  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220630125147  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220630125148  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220630125148                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220630215153  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220630215153  QCF$                G�O�G�O�G�O�            8000JA  ARUP                                                                        20220630125730                      G�O�G�O�G�O�                