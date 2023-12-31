CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:01Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               &A   AO  20111130135922  20190522121825  1727_5046_038                   2C  D   APEX                            2143                            040306                          846 @�F�j��1   @�F���@7,�C���c�1&�y1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	�fD
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<�fD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DV��DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Dfy�Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dky�Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsl�Dy331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B�  B�  B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B�  B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	� D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D�3Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D2  D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<� D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV�3DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfs3Df��Dgy�Dg��Dhy�Dh��Diy�Di��Dj� Dj��Dks3Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��DsffDy,�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A�A�=qA���AЛ�A�;dA��AϬA�v�A�^5A�I�A�A�A�A�A�M�A�VA�`BA�S�A��A�%A���A��AΧ�A΋DA΁A�x�A�n�A�ffA�\)A�I�A�?}A�1'A� �A�A��A��A��A��mA���A͡�A�5?A���A̋DAʑhA�VA�n�A��A�M�A�33A�;dA��A��-A��A�r�A�7LA��A��`A���A�bNA�x�A���A��mA��RA�ffA�$�A�5?A�bNA�ƨA�%A�ffA�A��\A��mA��!A�`BA�A���A�I�A��mA��A��uA�9XA��`A�-A�dZA��A�ZA��9A���A���A�l�A��^A�jA���A�p�A�Q�A��hA��7A�p�A�&�A�VA��mA��RA���A��A�JA�\)A�7LA;dA}��A|1'Ay�FAv��Au`BAsK�Ar(�Aot�AnZAl�RAkhsAj  Ahv�AgO�AeVAc�Ac��Ab�A`A�A_�A]��A[\)AZr�AY�^AX��AW��AV�AT��AS33AR�9AQ��AQ?}AP�HANM�AL�yAJ�\AJr�AI|�AIdZAI/AH�yAH�+AH1'AGdZAFbNAE��AD�\AC%AA��AA"�A@ZA?��A=;dA;��A9�TA8 �A7%A6�DA5dZA3��A2��A1��A0bNA/XA-�;A-
=A,~�A+ƨA*E�A)�wA);dA(9XA&ȴA&v�A&A%�PA%O�A$��A$M�A#�#A#33A"jA �`A�Ax�A�A�A��A�Ar�AO�Ar�A&�A��A��A~�A�^A&�A�mA+A �AO�A��A1'A�
A�A�#A��A`BAK�A&�A�DA��At�A;dA33A"�A�A
�9A �A?}A�+Av�A�;A&�A�AA�7A�AĜA�mA I�@���@�\)@���@�^5@��@��-@���@�9X@���@���@��;@�x�@��@�\)@�
=@�+@�n�@�$�@�h@�b@��H@��@��@�"�@��@�bN@�|�@��H@݁@��/@��
@ڰ!@�$�@��@�/@�z�@��y@պ^@��;@�x�@ϥ�@�M�@�&�@��/@��@�@�@��@�1'@�S�@őh@� �@���@Å@��@� �@�S�@�K�@�J@��u@���@��-@���@�-@��@�|�@�ff@�J@���@�O�@�bN@���@�S�@���@�I�@��F@�|�@�33@��@���@�5?@��@�V@�r�@�ƨ@�M�@���@��@�  @���@���@�|�@���@�@��j@�r�@�9X@�b@��
@�\)@���@�E�@��@���@��@�1'@�z�@�`B@�`B@�7L@���@���@���@�\)@�@��y@���@�V@���@���@�hs@�O�@���@�1'@���@���@���@�=q@��@��@�%@�V@��@�V@�&�@��h@�p�@�7L@���@��`@���@���@��P@��
@�|�@�C�@��
@�z�@��@�1@�  @���@�v�@�\)@�M�@�+@�C�@�S�@��@��R@�~�@��+@���@��\@�~�@��T@��-@�`B@���@��j@���@�j@�1@�ƨ@�l�@��H@��+@�-@��#@��h@�7L@��@�%@��j@�Q�@� �@�  @���@���@���@�l�@���@��y@���@�E�@�J@���@��#@���@�x�@�G�@�%@���@���@�1'@�1@���@��m@��;@��
@���@��w@��w@��w@�ƨ@�ƨ@���@��w@���@�dZ@��y@��R@��!@��\@�v�@�E�@�@��T@��7@�`B@���@���@�I�@��w@���@��P@�dZ@�o@�@���@�@�ȴ@��R@���@�ff@�5?@�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�A�A�A�A�=qA���AЛ�A�;dA��AϬA�v�A�^5A�I�A�A�A�A�A�M�A�VA�`BA�S�A��A�%A���A��AΧ�A΋DA΁A�x�A�n�A�ffA�\)A�I�A�?}A�1'A� �A�A��A��A��A��mA���A͡�A�5?A���A̋DAʑhA�VA�n�A��A�M�A�33A�;dA��A��-A��A�r�A�7LA��A��`A���A�bNA�x�A���A��mA��RA�ffA�$�A�5?A�bNA�ƨA�%A�ffA�A��\A��mA��!A�`BA�A���A�I�A��mA��A��uA�9XA��`A�-A�dZA��A�ZA��9A���A���A�l�A��^A�jA���A�p�A�Q�A��hA��7A�p�A�&�A�VA��mA��RA���A��A�JA�\)A�7LA;dA}��A|1'Ay�FAv��Au`BAsK�Ar(�Aot�AnZAl�RAkhsAj  Ahv�AgO�AeVAc�Ac��Ab�A`A�A_�A]��A[\)AZr�AY�^AX��AW��AV�AT��AS33AR�9AQ��AQ?}AP�HANM�AL�yAJ�\AJr�AI|�AIdZAI/AH�yAH�+AH1'AGdZAFbNAE��AD�\AC%AA��AA"�A@ZA?��A=;dA;��A9�TA8 �A7%A6�DA5dZA3��A2��A1��A0bNA/XA-�;A-
=A,~�A+ƨA*E�A)�wA);dA(9XA&ȴA&v�A&A%�PA%O�A$��A$M�A#�#A#33A"jA �`A�Ax�A�A�A��A�Ar�AO�Ar�A&�A��A��A~�A�^A&�A�mA+A �AO�A��A1'A�
A�A�#A��A`BAK�A&�A�DA��At�A;dA33A"�A�A
�9A �A?}A�+Av�A�;A&�A�AA�7A�AĜA�mA I�@���@�\)@���@�^5@��@��-@���@�9X@���@���@��;@�x�@��@�\)@�
=@�+@�n�@�$�@�h@�b@��H@��@��@�"�@��@�bN@�|�@��H@݁@��/@��
@ڰ!@�$�@��@�/@�z�@��y@պ^@��;@�x�@ϥ�@�M�@�&�@��/@��@�@�@��@�1'@�S�@őh@� �@���@Å@��@� �@�S�@�K�@�J@��u@���@��-@���@�-@��@�|�@�ff@�J@���@�O�@�bN@���@�S�@���@�I�@��F@�|�@�33@��@���@�5?@��@�V@�r�@�ƨ@�M�@���@��@�  @���@���@�|�@���@�@��j@�r�@�9X@�b@��
@�\)@���@�E�@��@���@��@�1'@�z�@�`B@�`B@�7L@���@���@���@�\)@�@��y@���@�V@���@���@�hs@�O�@���@�1'@���@���@���@�=q@��@��@�%@�V@��@�V@�&�@��h@�p�@�7L@���@��`@���@���@��P@��
@�|�@�C�@��
@�z�@��@�1@�  @���@�v�@�\)@�M�@�+@�C�@�S�@��@��R@�~�@��+@���@��\@�~�@��T@��-@�`B@���@��j@���@�j@�1@�ƨ@�l�@��H@��+@�-@��#@��h@�7L@��@�%@��j@�Q�@� �@�  @���@���@���@�l�@���@��y@���@�E�@�J@���@��#@���@�x�@�G�@�%@���@���@�1'@�1@���@��m@��;@��
@���@��w@��w@��w@�ƨ@�ƨ@���@��w@���@�dZ@��y@��R@��!@��\@�v�@�E�@�@��T@��7@�`B@���@���@�I�@��w@���@��P@�dZ@�o@�@���@�@�ȴ@��R@���@�ff@�5?@�  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�mB�mB�mB�B�B�B�B�B�sB�sB�yB�yB�B�B�B�B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�B�B�HB�B��BÖB�?B��B��B�\B�Bx�Bp�BcTB[#BS�BM�BC�B5?B,B�B1B  B��B�B�yB�;B�#B�B�B�/B�/B�)B�
BB��B��B��B�{B�PB�\B�JB{�BiyB[#BL�BC�B?}B7LB33B/B"�BoBB
�B
�B
�B
�)B
��B
B
��B
�oB
�B
u�B
hsB
[#B
H�B
8RB
.B
$�B
�B
VB
1B
B
B
  B	��B	��B	�yB	�TB	�;B	�B	��B	ÖB	�^B	�B	��B	��B	��B	��B	�\B	�+B	�+B	�B	y�B	u�B	r�B	e`B	XB	J�B	H�B	H�B	S�B	R�B	R�B	P�B	M�B	J�B	F�B	B�B	>wB	7LB	0!B	-B	'�B	"�B	�B	uB		7B	B��B��B�B�B�fB�NB�)B�B��B��BȴBĜB��B�wB�dB�FB�?B�9B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B��B�uB�bB�PB�DB�=B�=B�7B�1B�B�B�+B|�By�Bw�Bv�Bu�Br�Bp�Bn�Bm�Bm�Bl�Bk�BiyBgmBffBffBe`Be`BcTB]/B[#BYBXBW
BT�BS�BQ�BO�BN�BL�BJ�BF�BB�B@�B?}B?}B?}B>wB=qB=qB<jB:^B7LB5?B33B2-B2-B2-B2-B1'B1'B/B/B.B-B,B+B+B)�B(�B'�B'�B&�B&�B'�B'�B'�B'�B&�B)�B)�B)�B0!B1'B2-B33B2-B49B5?B8RB9XB8RB7LB7LB:^B:^B7LB7LB:^B>wB>wB=qBA�BB�BC�BB�BD�BK�BL�BQ�BR�BR�BS�BW
BW
BW
BYB[#B]/B]/B_;B_;B_;BaHBaHBcTBe`BffBk�Bl�Br�Bu�Bw�Bx�Bz�B{�B}�B�B�B�B�%B�+B�7B�VB�\B��B��B��B��B��B�'B�3B�9B�LB�XB�jBBƨBǮB��B��B��B��B�B�B�
B�)B�;B�;B�5B�/B�NB�yB�B�B�B�B��B	B	1B	
=B	PB	bB	oB	�B	�B	�B	�B	"�B	(�B	-B	,B	/B	2-B	2-B	/B	%�B	&�B	0!B	33B	5?B	49B	49B	49B	7LB	F�B	P�B	XB	[#B	\)B	^5B	^5B	_;B	`BB	bNB	dZB	ffB	hsB	k�B	m�B	p�B	r�B	t�B	w�B	x�B	z�B	|�B	�B	�B	�B	�B	�+B	�+B	�1B	�JB	�JB	�PB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�RB	�RB	�XB	�^B	�dB	�qB	�qB	��B	��B	B	ĜB	ĜB	ĜB	ĜB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�mB�mB�sB�B�B�B�B�B�sB�sB�yB�yB�B�B�B�B��B��B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BDBDBB�B�`B�5B�)B��B�qB�!B��B��B�1B}�Bx�BgmB^5BW
BR�BH�B:^B0!B%�BDBB��B�B�B�NB�)B�)B�)B�;B�;B�5B�)B��B��B��B��B��B�\B�uB��B�Bo�BcTBP�BE�BC�B8RB49B49B(�B�B	7B
�B
�B
�B
�BB
��B
ɺB
�!B
��B
�+B
x�B
m�B
aHB
O�B
<jB
33B
'�B
&�B
hB
PB
	7B
%B
B
B	��B	�B	�ZB	�ZB	�)B	��B	ǮB	B	�'B	�B	��B	��B	��B	��B	�PB	�7B	�B	|�B	w�B	{�B	jB	`BB	K�B	K�B	I�B	T�B	S�B	T�B	R�B	P�B	N�B	I�B	F�B	D�B	<jB	2-B	0!B	)�B	(�B	�B	�B	VB	B��B��B��B�B�sB�fB�;B�B��B��B��BȴBB��B�wB�^B�FB�FB�9B�-B�-B�!B�B�B��B��B��B��B��B��B��B��B��B�{B�bB�\B�DB�=B�=B�=B�+B�B�7B� B{�By�Bw�Bv�Bu�Br�Bo�Bn�Bm�Bm�Bm�Bk�BhsBgmBffBe`Be`Be`Be`B^5B[#BYBYBW
BVBS�BQ�BP�BM�BM�BL�BG�BD�B@�B@�B@�B?}B>wB>wB=qB<jB<jB:^B9XB7LB33B33B2-B2-B2-B2-B1'B0!B/B0!B.B.B,B+B+B(�B(�B(�B(�B(�B'�B)�B(�B,B-B-B33B33B49B49B33B6FB7LB9XB:^B9XB:^B9XB;dB:^B;dB8RB;dB>wB@�B@�BD�BD�BE�BF�BG�BL�BN�BR�BS�BS�BVBXBXBZB\)B\)B^5B^5B_;B`BB`BBbNBcTBdZBffBiyBl�Bn�Bs�Bu�Bw�By�B{�B|�B~�B�B�B�B�%B�1B�=B�\B�bB��B��B��B��B��B�'B�3B�?B�LB�^B�qBÖBƨBȴB��B��B��B��B�B�
B�B�5B�BB�;B�;B�5B�TB�yB�B�B�B�B��B	B		7B	DB	PB	bB	oB	�B	�B	�B	�B	!�B	'�B	.B	,B	/B	33B	49B	33B	'�B	%�B	0!B	33B	6FB	49B	49B	49B	49B	D�B	P�B	YB	[#B	]/B	_;B	^5B	_;B	`BB	cTB	e`B	gmB	iyB	l�B	n�B	q�B	s�B	u�B	w�B	x�B	{�B	}�B	�B	�B	�B	�B	�+B	�+B	�7B	�JB	�JB	�VB	�oB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�9B	�FB	�RB	�XB	�^B	�^B	�dB	�wB	�qB	��B	B	ÖB	ĜB	ĜB	ĜB	ŢB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�H1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<ě�<T��<T��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446472012010314464720120103144647  AO  ARGQ                                                                        20111130135922  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135922  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144647  IP                  G�O�G�O�G�O�                