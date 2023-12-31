CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:04Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               .A   AO  20111130140118  20190522121825  1727_5046_046                   2C  D   APEX                            2143                            040306                          846 @�Q��@1   @�QDD@@7^5?|��c�fffff1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  Dy�D��D� D  D� DfD� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA��DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_fD_� D`  D`� Da  Da� Db  Db� Dc  Dc�fDd  Ddy�De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @333@y��@���@���AffA>ffA^ffA~ffA�ffA�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���BǙ�B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC  C�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Ds3D�3Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;s3D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA�3DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D_  D_y�D_��D`y�D`��Day�Da��Dby�Db��Dc� Dc��Dds3Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhs3Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A���A���A���A�  A�A�A�A�A���A���A�%A�
=A�A�A�A�%A�A�
=A�JA�JA�bA�bA�oA�{A��A�{A��A��A��A��A��A��A��A��A��A� �A� �A� �A�"�A�$�A�$�A�&�A�&�A�&�A�&�A�(�A���Ař�A��A��A���A�A��9A��FA��^A��9A�t�A���A��
A���A���A�n�A��\A��#A��\A��mA��+A���A�M�A��A��A�l�A�G�A�S�A���A���A�oA�;dA���A��/A�7LA��A��HA��A�x�A���A�$�A�S�A��A�~�A�G�A���A���A�?}A�(�A�C�A���A�bA��;A�v�A�I�A�"�A���A�{A���A�&�A�A�A7LA~�!A~r�A}�A{�Ax�/AwdZAv�AuS�At~�AtE�At  As�-As�hAq�-Ap  Ao�7Ao;dAnn�Am7LAlbAi�AiG�Ag�7Ae��Ae��Ae�Ad�Ad �Ac
=A`I�A^I�A\��A[�AZ��AY��AXĜAW�AV��AV~�AV�AT�yAR��AP�uAO�wAO�AN��AM�
AMx�AL �AJ��AI�TAG��AF  AE
=ADbNAB�RAAp�A?l�A>�A>��A>A�A=��A<I�A;�A:ffA9�PA8ĜA8�DA7�;A7G�A6jA5p�A4~�A3��A1�;A/�#A.I�A,�RA+ƨA+%A*VA)"�A(�A'&�A%�A#�-A"�\A!�A!��A!�PA!|�A ~�A��AXA%A��A��AQ�A/An�A�A&�A�DAƨA
=A�DA`BAȴA��A��A��A�`A�DAA;dA�A
=AM�A�hA
�/A	x�A�\AA��A5?A�yA�AA�AA�
A�uA`BA z�A $�@��@���@���@��@��
@���@���@��@���@�l�@�@�@��m@�t�@ꗍ@��@���@�$�@�1@�\@���@ߥ�@��H@��#@�p�@��@�I�@�33@�E�@��`@�l�@���@� �@Ӯ@��@Ѓ@�ƨ@ΰ!@�?}@��`@˅@��H@��`@�"�@ŉ7@� �@�~�@�V@��@�ff@���@�`B@��9@���@��+@�{@���@�r�@�t�@�5?@�&�@�Z@��w@�;d@��@�`B@���@��@��R@�=q@�O�@��9@�Q�@�+@�$�@�hs@�r�@�ƨ@�C�@��y@�n�@���@��h@��@���@�M�@�5?@�p�@��@��@���@�ƨ@�33@�o@��!@�=q@�@��h@��`@�r�@�1@���@�ƨ@�t�@�33@��H@��R@�n�@��T@��@�{@���@��h@��@�p�@�p�@�/@���@��/@���@��D@�Q�@��
@�S�@�ff@���@�@��@��@��/@���@���@��D@��@��@��y@��@��@��@��@���@�p�@���@���@�%@�V@��@���@��@�bN@�b@��
@�l�@��@���@�|�@��P@�|�@�33@�+@�"�@�o@�
=@�
=@�@��H@��R@���@�v�@�@���@���@�?}@���@��`@��9@��D@�j@�1'@�  @���@���@�t�@�C�@�"�@��y@�n�@��@��@��-@���@�X@���@��j@�z�@���@��;@���@���@�|�@��@�l�@�33@���@���@�J@���@��h@��7@��h@��h@��7@�%@��D@�z�@�Z@�bN@�Z@�1'@��@��
@�ƨ@��w@���@��@��R@�E�@��@���@��-@���@��7@��@�`B@�7L@�/@�&�@�%@���@�Ĝ@���@��u@��u@|9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A���A���A���A�  A�A�A�A�A���A���A�%A�
=A�A�A�A�%A�A�
=A�JA�JA�bA�bA�oA�{A��A�{A��A��A��A��A��A��A��A��A��A� �A� �A� �A�"�A�$�A�$�A�&�A�&�A�&�A�&�A�(�A���Ař�A��A��A���A�A��9A��FA��^A��9A�t�A���A��
A���A���A�n�A��\A��#A��\A��mA��+A���A�M�A��A��A�l�A�G�A�S�A���A���A�oA�;dA���A��/A�7LA��A��HA��A�x�A���A�$�A�S�A��A�~�A�G�A���A���A�?}A�(�A�C�A���A�bA��;A�v�A�I�A�"�A���A�{A���A�&�A�A�A7LA~�!A~r�A}�A{�Ax�/AwdZAv�AuS�At~�AtE�At  As�-As�hAq�-Ap  Ao�7Ao;dAnn�Am7LAlbAi�AiG�Ag�7Ae��Ae��Ae�Ad�Ad �Ac
=A`I�A^I�A\��A[�AZ��AY��AXĜAW�AV��AV~�AV�AT�yAR��AP�uAO�wAO�AN��AM�
AMx�AL �AJ��AI�TAG��AF  AE
=ADbNAB�RAAp�A?l�A>�A>��A>A�A=��A<I�A;�A:ffA9�PA8ĜA8�DA7�;A7G�A6jA5p�A4~�A3��A1�;A/�#A.I�A,�RA+ƨA+%A*VA)"�A(�A'&�A%�A#�-A"�\A!�A!��A!�PA!|�A ~�A��AXA%A��A��AQ�A/An�A�A&�A�DAƨA
=A�DA`BAȴA��A��A��A�`A�DAA;dA�A
=AM�A�hA
�/A	x�A�\AA��A5?A�yA�AA�AA�
A�uA`BA z�A $�@��@���@���@��@��
@���@���@��@���@�l�@�@�@��m@�t�@ꗍ@��@���@�$�@�1@�\@���@ߥ�@��H@��#@�p�@��@�I�@�33@�E�@��`@�l�@���@� �@Ӯ@��@Ѓ@�ƨ@ΰ!@�?}@��`@˅@��H@��`@�"�@ŉ7@� �@�~�@�V@��@�ff@���@�`B@��9@���@��+@�{@���@�r�@�t�@�5?@�&�@�Z@��w@�;d@��@�`B@���@��@��R@�=q@�O�@��9@�Q�@�+@�$�@�hs@�r�@�ƨ@�C�@��y@�n�@���@��h@��@���@�M�@�5?@�p�@��@��@���@�ƨ@�33@�o@��!@�=q@�@��h@��`@�r�@�1@���@�ƨ@�t�@�33@��H@��R@�n�@��T@��@�{@���@��h@��@�p�@�p�@�/@���@��/@���@��D@�Q�@��
@�S�@�ff@���@�@��@��@��/@���@���@��D@��@��@��y@��@��@��@��@���@�p�@���@���@�%@�V@��@���@��@�bN@�b@��
@�l�@��@���@�|�@��P@�|�@�33@�+@�"�@�o@�
=@�
=@�@��H@��R@���@�v�@�@���@���@�?}@���@��`@��9@��D@�j@�1'@�  @���@���@�t�@�C�@�"�@��y@�n�@��@��@��-@���@�X@���@��j@�z�@���@��;@���@���@�|�@��@�l�@�33@���@���@�J@���@��h@��7@��h@��h@��7@�%@��D@�z�@�Z@�bN@�Z@�1'@��@��
@�ƨ@��w@���@��@��R@�E�@��@���@��-@���@��7@��@�`B@�7L@�/@�&�@�%@���@�Ĝ@���@��u@��u@|9X1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BǮB�jB�dB��B�wBĜBĜBŢBŢBĜB�jB�?B�-B�?B�B��B��B�+B{�Bp�B]/BO�B;dB'�B�B	7B��B�B�5B�BǮB�wB�-B��B�B{�Bq�BcTBZBQ�BH�B=qB,B�B{BhBJB
��B
�B
�B
�fB
�NB
�/B
�B
��B
�}B
�^B
�'B
��B
�uB
|�B
x�B
t�B
gmB
`BB
L�B
C�B
>wB
:^B
6FB
33B
2-B
0!B
-B
#�B
�B
�B
�B
uB
bB

=B
B
  B	��B	�B	�B	�B	�sB	�ZB	�#B	��B	ƨB	��B	�^B	�?B	�!B	�B	��B	��B	��B	��B	�{B	�%B	w�B	q�B	o�B	k�B	dZB	dZB	_;B	W
B	N�B	C�B	:^B	49B	0!B	)�B	"�B	�B	�B	�B	oB	\B		7B	B	B��B��B��B��B��B�B�B�mB�TB�/B��B��B��BȴBŢB��B�jB�LB�-B��B��B��B��B��B��B��B��B��B��B��B��B�hB�JB�DB�=B�7B�DB�7B�B�B�B}�B|�By�Bv�Br�Bq�Bo�Bl�BiyBgmBdZBbNB`BB]/B[#BYBVBT�BQ�BP�BO�BN�BM�BK�BH�BF�BE�BE�BC�BA�B?}B<jB:^B9XB8RB5?B5?B33B33B2-B2-B1'B0!B/B.B-B-B-B+B,B+B+B+B)�B)�B(�B'�B&�B&�B%�B&�B%�B&�B&�B&�B&�B'�B'�B(�B'�B,B.B0!B1'B49B0!B2-B8RB:^B:^B;dB<jB@�BC�BE�BJ�BH�BN�BN�BQ�BT�BW
BT�BS�BS�BT�BYBZB\)B`BBbNBbNBcTBdZBgmBiyBl�Bn�Bp�Bq�Br�Bz�B� B�B�B�%B�+B�+B�=B�JB�VB�bB�oB�{B��B��B��B��B��B��B��B�B�B�3B�FB�jB��BÖBǮB��B��B��B��B��B�B�)B�/B�;B�;B�HB�`B�B�B�B�B�B��B��B	B	B	
=B	hB	bB	\B	oB	{B	�B	�B	 �B	&�B	+B	-B	33B	>wB	D�B	F�B	I�B	L�B	P�B	R�B	VB	[#B	aHB	dZB	ffB	gmB	jB	l�B	m�B	n�B	n�B	n�B	n�B	o�B	p�B	q�B	q�B	u�B	v�B	x�B	{�B	}�B	~�B	�B	�B	�B	�B	�%B	�1B	�7B	�DB	�JB	�PB	�\B	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�9B	�FB	�LB	�RB	�RB	�RB	�RB	�XB	�^B	�^B	�dB	�jB	�}B	ÖB	ÖB	ĜB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BŢB�wBĜBĜBŢBƨBǮB��B�^B�qB�dB�LB��B��B�=B�Bx�BcTBT�BA�B,B�BPB��B��B�HB�B��BB�wB��B�1B�By�BgmB^5BW
BL�BD�B8RB�B�BuBbBB
��B
�B
�mB
�ZB
�5B
�5B
��B
ÖB
�jB
�9B
�-B
��B
}�B
y�B
x�B
k�B
iyB
P�B
G�B
@�B
<jB
7LB
49B
33B
1'B
33B
(�B
�B
�B
�B
�B
{B
hB
%B
%B	��B	�B	�B	�B	�B	�sB	�ZB	�
B	��B	ĜB	�wB	�RB	�9B	�B	��B	��B	��B	��B	��B	�VB	z�B	r�B	q�B	n�B	e`B	gmB	bNB	ZB	S�B	G�B	<jB	6FB	5?B	.B	'�B	�B	�B	�B	{B	uB	JB	+B	%B	B��B	  B��B��B��B�B�B�yB�TB�B��B��B��BȴBŢB��B�dB�XB�B��B��B��B��B��B��B��B��B��B��B��B��B�bB�VB�JB�PB�PB�JB�+B�B�B� B~�B|�Bz�Bt�Br�Bq�Bn�Bm�BjBffBdZBbNBaHB^5B\)BZBW
BVBR�BP�BO�BN�BO�BL�BI�BF�BF�BG�BC�BB�BA�B=qB;dB;dB8RB8RB6FB5?B33B33B33B2-B2-B1'B-B-B.B/B.B+B,B,B,B,B+B)�B(�B(�B'�B'�B(�B(�B'�B(�B(�B(�B)�B)�B+B/B0!B2-B49B5?B2-B6FB9XB;dB;dB=qB>wBA�BD�BG�BL�BJ�BP�BO�BR�BVBYBVBT�BT�BW
BZB\)B]/BaHBdZBdZBdZBffBhsBjBm�Bo�Bq�Br�Bu�B{�B�B�B�B�+B�+B�1B�DB�PB�VB�hB�uB��B��B��B��B��B��B��B��B�B�!B�3B�LB�qB��BÖBȴB��B��B��B��B�B�B�)B�5B�;B�BB�NB�fB�B�B�B�B�B��B��B	B	B	
=B	oB	oB	bB	oB	{B	�B	�B	!�B	'�B	+B	-B	33B	>wB	E�B	F�B	J�B	M�B	Q�B	S�B	VB	[#B	bNB	dZB	ffB	hsB	jB	l�B	m�B	n�B	n�B	n�B	n�B	o�B	p�B	q�B	r�B	u�B	v�B	y�B	|�B	}�B	~�B	�B	�B	�B	�B	�%B	�1B	�7B	�DB	�JB	�VB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�9B	�LB	�LB	�XB	�XB	�XB	�RB	�XB	�^B	�^B	�dB	�jB	��B	ÖB	ÖB	ĜB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446502012010314465020120103144650  AO  ARGQ                                                                        20111130140118  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140118  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144650  IP                  G�O�G�O�G�O�                