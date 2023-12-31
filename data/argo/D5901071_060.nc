CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:07Z UW 3.1 conversion   
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kt   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mp   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  U\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]H   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _D   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g0   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i,   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �0   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �8   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �@   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               <A   AO  20111130140442  20190522121826  1727_5046_060                   2C  D   APEX                            2143                            040306                          846 @�c36< 1   @�c3�À@77
=p���c�V�u1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH�CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM�fDN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DSy�DT  DT�fDUfDU�fDVfDV� DW  DW� DXfDX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  Dey�Df  Df� Dg  Dg� Dg��Dhy�Dh��Di� Dj  Dj� Dk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy��D�0 D�l�D���D��D�)�D�vfD�� D��3D��D�\�D��3D��3D��D�P Dڜ�D�3D�&fD�i�D�D�&f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B33B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCH  CI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCd  Ce�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC|  C}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C�  C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D�3Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D*  D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DM� DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSs3DS��DT� DU  DU� DV  DVy�DV��DWy�DX  DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Des3De��Dfy�Df��Dgy�Dg�3Dhs3Dh�3Diy�Di��Djy�Dj��Dks3Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy�3D�,�D�i�D���D��fD�&fD�s3D���D�� D��D�Y�D�� D�� D�fD�L�Dڙ�D� D�#3D�ffD�fD�#3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A�A�  A�A�A�%A�A�A�A�A�A�
=A�
=A�JA�JA�JA�VA�bA�{A�oA�bA�VA�$�A�n�A�n�A�l�A�ffA�`BA�O�A��A�bA�%A���A��A���A��RA��A���A�jA�5?A�$�A��A��yA��!A��DA�t�A�XA�M�A�E�A�33A� �A�VA�A��yA��/A��
A���A���A���A�&�A�A�ƨA��hA��uA��uA��uA���A���A��PA�/A�ZA��uA��yA�=qA�{A�$�A��^A��A���A�1'A���A�A�A�{A�-A�%A�/A��A��A�A���A��wA�n�A��9A�z�A� �A���A�5?A�jA�S�A��jA� �A��FA�bNA�ƨA�p�A���A��wA�A�C�A���A�l�A�-A�bNA��A�p�A�r�A�S�A�jA�
=A���A�G�A���A�9XA��9A�1A��`A�{A�z�A��AC�A}S�A{�7Ay��Ax��AwG�AvE�Au�PArjAoG�Am��Am
=Al�DAj�/AjJAiVAg�;AgoAf��Ad��Ad �Ab�RAa
=A_x�A^��A]oAZ1'AXM�AW��AU�#AT{ASC�AR^5AQ��AP�DAN�AN�+AL��AKhsAJI�AI+AF�AFffAFI�AEXAD�HADZAC�AB��AAA@ �A>�/A=|�A=&�A<VA;�wA:�`A8^5A7|�A4�A3�A2^5A/A/A.(�A,�9A+�7A*��A)�#A)�^A)�A(r�A'�-A't�A& �A$(�A#hsA!��A �DA �A�mA��A��A�A-AĜA~�A�mA`BA33A33A�wAȴAZAM�A-AA�^A �A��A��A��AE�A �AA
 �A
1Av�A?}A(�A�A�A��A n�@�{@��@��
@��H@���@�ff@��@��#@�n�@�M�@�?}@��@�^5@�o@��y@�1@��@��@�5?@��@��@�ƨ@�o@�$�@�%@�+@ڟ�@�Z@�M�@�%@Ӆ@��T@��m@���@�ȴ@�-@̋D@���@��`@�b@�dZ@�
=@��H@Ƨ�@ģ�@�+@�$�@���@�@� �@�C�@�v�@��7@��@��D@��@�9X@��@�l�@�;d@�;d@�=q@��@���@�V@�(�@�n�@���@�@��7@�j@���@�X@��`@�I�@�l�@�"�@��!@�J@�@��h@��@�Z@�1'@���@�ȴ@�~�@��#@�x�@�x�@�@��F@���@�1'@�5?@�X@���@���@��h@��#@�K�@�b@���@���@���@�?}@�V@�r�@���@���@�
=@��@���@�  @�%@�r�@��T@���@��@�33@���@��@��@��w@��m@�z�@�1@�z�@���@��@���@�n�@���@��^@���@�x�@�hs@�`B@�`B@��@���@�9X@��
@��@���@���@���@�@���@���@�x�@��@��/@��u@���@�\)@��@���@�dZ@�+@��@���@�p�@�X@�?}@�Ĝ@��@��w@���@���@��@�;d@��y@��\@���@��+@�v�@�z�@��@���@�bN@� �@��m@��F@�t�@�S�@�
=@��R@�E�@�p�@�/@��@��@��@�V@��/@��j@���@��@�1'@��@���@�S�@��@���@��+@�~�@�v�@�V@�5?@��@��#@���@���@��h@��h@�x�@�X@��@��@��D@�bN@�I�@���@���@���@�l�@�C�@�"�@�@��+@�hs@�G�@�?}@�G�@�X@�hs@�p�@�p�@�hs@�x�@~v�@s"�@g
=@`��@[ƨ@T�@H�`@Ahs@<Z@8�u@2-@0�@,9X@(��@"�@?}@�u@�/@�#@�-@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�A�A�  A�A�A�%A�A�A�A�A�A�
=A�
=A�JA�JA�JA�VA�bA�{A�oA�bA�VA�$�A�n�A�n�A�l�A�ffA�`BA�O�A��A�bA�%A���A��A���A��RA��A���A�jA�5?A�$�A��A��yA��!A��DA�t�A�XA�M�A�E�A�33A� �A�VA�A��yA��/A��
A���A���A���A�&�A�A�ƨA��hA��uA��uA��uA���A���A��PA�/A�ZA��uA��yA�=qA�{A�$�A��^A��A���A�1'A���A�A�A�{A�-A�%A�/A��A��A�A���A��wA�n�A��9A�z�A� �A���A�5?A�jA�S�A��jA� �A��FA�bNA�ƨA�p�A���A��wA�A�C�A���A�l�A�-A�bNA��A�p�A�r�A�S�A�jA�
=A���A�G�A���A�9XA��9A�1A��`A�{A�z�A��AC�A}S�A{�7Ay��Ax��AwG�AvE�Au�PArjAoG�Am��Am
=Al�DAj�/AjJAiVAg�;AgoAf��Ad��Ad �Ab�RAa
=A_x�A^��A]oAZ1'AXM�AW��AU�#AT{ASC�AR^5AQ��AP�DAN�AN�+AL��AKhsAJI�AI+AF�AFffAFI�AEXAD�HADZAC�AB��AAA@ �A>�/A=|�A=&�A<VA;�wA:�`A8^5A7|�A4�A3�A2^5A/A/A.(�A,�9A+�7A*��A)�#A)�^A)�A(r�A'�-A't�A& �A$(�A#hsA!��A �DA �A�mA��A��A�A-AĜA~�A�mA`BA33A33A�wAȴAZAM�A-AA�^A �A��A��A��AE�A �AA
 �A
1Av�A?}A(�A�A�A��A n�@�{@��@��
@��H@���@�ff@��@��#@�n�@�M�@�?}@��@�^5@�o@��y@�1@��@��@�5?@��@��@�ƨ@�o@�$�@�%@�+@ڟ�@�Z@�M�@�%@Ӆ@��T@��m@���@�ȴ@�-@̋D@���@��`@�b@�dZ@�
=@��H@Ƨ�@ģ�@�+@�$�@���@�@� �@�C�@�v�@��7@��@��D@��@�9X@��@�l�@�;d@�;d@�=q@��@���@�V@�(�@�n�@���@�@��7@�j@���@�X@��`@�I�@�l�@�"�@��!@�J@�@��h@��@�Z@�1'@���@�ȴ@�~�@��#@�x�@�x�@�@��F@���@�1'@�5?@�X@���@���@��h@��#@�K�@�b@���@���@���@�?}@�V@�r�@���@���@�
=@��@���@�  @�%@�r�@��T@���@��@�33@���@��@��@��w@��m@�z�@�1@�z�@���@��@���@�n�@���@��^@���@�x�@�hs@�`B@�`B@��@���@�9X@��
@��@���@���@���@�@���@���@�x�@��@��/@��u@���@�\)@��@���@�dZ@�+@��@���@�p�@�X@�?}@�Ĝ@��@��w@���@���@��@�;d@��y@��\@���@��+@�v�@�z�@��@���@�bN@� �@��m@��F@�t�@�S�@�
=@��R@�E�@�p�@�/@��@��@��@�V@��/@��j@���@��@�1'@��@���@�S�@��@���@��+@�~�@�v�@�V@�5?@��@��#@���@���@��h@��h@�x�@�X@��@��@��D@�bN@�I�@���@���@���@�l�@�C�@�"�@�@��+@�hs@�G�@�?}@�G�@�X@�hs@�p�@�p�@�hs@�x�@~v�@s"�@g
=@`��@[ƨ@T�@H�`@Ahs@<Z@8�u@2-@0�@,9X@(��@"�@?}@�u@�/@�#@�-@
�\111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBS�BR�BR�BS�BS�BS�BS�BR�BS�BT�BT�BVBVBVBVBVBVBVBVBW
BXBW
BVBVB_;B�B�JB�\B�hB�hB�oB�uB�uB�oB�oB�{B��B��B��B��B��B��B��B��B�B�?B�qBÖBǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�B�
B�
B��BɺB�}B�jB�RB�?B�FB�FB�'B�B�B��B��B�\B�+B{�Bq�Be`BR�BE�B>wB6FB2-BD�BD�B>wB5?B.B%�B�B1B�B�wB�9B�'B��B��B��Bo�Be`BW
BF�B:^B,B"�B�B
=B
��B
��B
�B
�HB
�B
��B
��B
ÖB
�3B
��B
�DB
�B
r�B
gmB
\)B
R�B
K�B
C�B
=qB
6FB
$�B
�B
PB

=B
%B	��B	��B	��B	�B	�mB	�ZB	�)B	�B	��B	ȴB	��B	�^B	�'B	��B	��B	�{B	�7B	�B	|�B	w�B	s�B	l�B	ffB	bNB	\)B	VB	N�B	G�B	>wB	>wB	?}B	?}B	>wB	<jB	8RB	33B	-B	&�B	 �B	�B	�B	�B	uB	VB	%B	  B��B�B�sB�BB�)B�
B��B��BȴBŢBĜBB�qB�dB�RB�-B�B��B��B��B��B��B��B��B�uB�VB�PB�JB�=B�uB��B��B��B��B��B��B��B�oB{�Bq�Br�Bx�Bz�Bu�Bo�Bo�Bn�Bm�Bk�BgmBbNB\)BO�BL�BI�BG�BC�BC�BC�BD�BE�BF�BF�BL�BW
Bv�Bt�Bp�BhsBXBT�BB�B;dB@�BC�BC�BF�BF�BD�BD�B@�B5?B.B+B+B)�B-B1'B7LB9XB8RB:^B;dB>wB?}B@�BA�B@�B?}BA�BD�BH�BJ�BI�BI�BJ�BJ�BL�BN�BQ�BYB[#B`BBgmBjBjBiyBgmBgmBgmBhsBhsBo�Bq�Br�Bv�Bw�Bx�Bx�By�B{�B{�B{�B}�B�B�B�B�%B�%B�B�B�%B�%B�7B�DB�\B��B��B�-B�wB��BÖBɺB��B��B�B�5B�/B�#B�B�TB�sB�mB�B�B��B��B	
=B	�B	!�B	�B	�B	
=B	%B	B	1B		7B	JB	�B	�B	"�B	!�B	\B	hB	uB	�B	�B	�B	�B	�B	 �B	#�B	$�B	%�B	(�B	+B	,B	33B	?}B	D�B	F�B	F�B	F�B	G�B	H�B	I�B	J�B	J�B	I�B	K�B	S�B	YB	ZB	[#B	]/B	^5B	aHB	e`B	ffB	ffB	ffB	iyB	l�B	o�B	p�B	s�B	v�B	y�B	{�B	�B	�B	�B	�B	�B	�1B	�1B	�1B	�1B	�1B	�7B	�7B	�7B	�DB	�DB	�PB	�\B	�\B	�\B	�\B	�bB	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�LB	�^B	�dB	�qB	�qB	��B	��B	B	B	ÖB	ÖB	ÖB	ĜB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�/B	��B
PB
�B
�B
&�B
1'B
:^B
@�B
J�B
O�B
Q�B
W
B
ZB
aHB
ffB
jB
m�B
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BS�BR�BR�BS�BS�BS�BS�BR�BS�BT�BT�BVBVBVBVBVBVBVBVBW
BXBW
BVBVB^5B�B�JB�\B�hB�hB�uB�uB�uB�oB�oB��B��B��B��B��B��B��B��B��B�B�FB�wBĜBǮBǮBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B�
B�B�B�B�B�B�B�B�B��BB�qB�jB�dB�dB�dB�3B�!B�'B�B��B��B�JB�By�Bl�B[#BK�BF�B:^B49BF�BG�B@�B8RB2-B'�B�BVB��B��B�FB�3B�B��B��Br�Bk�B]/BJ�BA�B0!B(�B�B\BB
��B
��B
�`B
�)B
�
B
��B
ȴB
�^B
��B
�VB
�7B
w�B
k�B
aHB
T�B
N�B
F�B
?}B
>wB
-B
�B
\B
JB

=B	��B	��B	��B	�B	�yB	�yB	�5B	�)B	�B	��B	ÖB	�}B	�XB	��B	��B	��B	�VB	�B	� B	y�B	w�B	q�B	gmB	gmB	`BB	ZB	R�B	N�B	@�B	?}B	B�B	A�B	@�B	>wB	<jB	6FB	2-B	+B	$�B	�B	�B	�B	�B	{B	1B	+B��B�B�B�NB�5B�#B��B��B��BƨBŢBŢB�}B�jB�jB�LB�B�B��B��B��B��B��B��B��B�uB�VB�VB�JB�{B��B��B��B��B��B��B��B��B�Bu�Bs�By�B|�B|�Bs�Br�Bo�Br�Bn�BjBe`BaHBR�BP�BM�BI�BD�BE�BD�BE�BF�BG�BE�BL�BR�Bx�Bw�Bv�Bn�B]/BT�BF�B<jBA�BD�BE�BG�BH�BF�BH�BG�B9XB1'B-B.B-B0!B33B8RB:^B;dB=qB>wB@�B@�BA�BB�BA�BC�BD�BF�BM�BM�BL�BJ�BK�BK�BM�BO�BR�BYB\)BaHBgmBjBl�BjBhsBhsBiyBk�BiyBp�Br�Bt�By�By�By�By�B{�B|�B|�B|�B~�B�B�B�%B�+B�+B�+B�%B�+B�+B�7B�=B�JB��B��B�-B��BBBɺB��B��B�B�;B�;B�5B�B�NB�B�B�B�B�B��B	%B	�B	#�B	$�B	�B	JB	+B	%B	1B		7B	DB	�B	�B	#�B	,B	bB	oB	uB	�B	�B	 �B	�B	�B	 �B	#�B	$�B	&�B	)�B	,B	-B	6FB	?}B	D�B	F�B	F�B	F�B	G�B	H�B	J�B	K�B	K�B	J�B	L�B	S�B	YB	[#B	\)B	_;B	_;B	aHB	e`B	ffB	ffB	hsB	iyB	l�B	p�B	p�B	t�B	w�B	z�B	{�B	�B	�B	�B	�B	�B	�1B	�1B	�1B	�7B	�7B	�7B	�7B	�=B	�JB	�JB	�PB	�\B	�\B	�\B	�\B	�bB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�LB	�^B	�dB	�qB	�wB	��B	��B	B	B	ÖB	ÖB	ĜB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�/B	��B
PB
�B
�B
&�B
1'B
:^B
@�B
J�B
O�B
Q�B
W
B
ZB
aHB
ffB
k�B
m�B
r�B
v�B
y�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446552012010314465520120103144655  AO  ARGQ                                                                        20111130140442  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140442  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144655  IP                  G�O�G�O�G�O�                