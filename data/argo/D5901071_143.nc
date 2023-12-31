CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:31Z UW 3.1 conversion   
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
_FillValue                 �  A8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T8   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  eP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g8   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xP   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �h   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142420  20190522121827  1727_5046_143                   2C  D   APEX                            2143                            040306                          846 @��zS�1   @��z��@6ؓt�j�c���v�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,�C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D;��D<y�D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC�fDD  DD� DEfDE�fDF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY�fDZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC,  C-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D  D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'� D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;�3D<s3D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DC� DC��DDy�DE  DE� DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DY� DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dss3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��FA���A�S�A�?}A�&�A�
=A���A��A��A��A��A��A��yA��yA��mA��mA��`A��HA��;A��A��
A���A���A���A��wA��uA�+A���A�Q�A��\A���A��A��9A���A�n�A�C�A�&�A��A��A�ĜA�v�A��A��/A���A���A�^5A��7A��A��\A�oA�A���A���A���A��A�ffA�{A�^5A��A�9XA�v�A�A�A��hA�v�A�K�A�1A�A���A��`A��A��
A�r�A��A���A�-A�ȴA�%A��DA��A��^A�|�A���A�5?A��A�E�A���A���A���A�VA���A�{A�oA�  A���A�33A��A��-A��TA��!A�{A��`A��9A���A��PA�jA�(�A��9A���A�bNA���A�O�A�bNA�=qA��/A�%A�(�A��7A���A���A�-A�n�A�x�A�/A�\)A�v�A�^A~n�A{`BAz(�Ay\)AwdZAu�hAt��Aq��AqoAohsAl��Aj��AhM�Af��Aep�Ad�!Ac�Ab�RA`VA_��A]K�A[|�AZM�AX��AX�AWhsAUAT�AT-AS�AQ"�AP1'AO"�AMdZAI�TAI�AHE�AH(�AG��AG�hAG|�AG�AF1'AB(�A>�A=t�A;��A;
=A9�FA8ĜA7��A6�RA6VA5ƨA5dZA5l�A5O�A5A4A2�A0�yA.�\A-O�A,�9A,=qA+�
A+t�A+7LA+
=A*ĜA)��A(��A(z�A(E�A(bA';dA$��A#�A"��A �AO�A��A(�A
=A��Az�A��A�AƨA�uA1A;dA
=A9XA�A��Az�A9XA�9A�7A"�A�HA��A�+AZA?}A
�A	��A�A�7A%A��A��A �A?}@��@��#@���@���@��/@���@��H@���@�|�@�\@�/@��
@�o@�v�@��-@���@�+@��@�\)@�@���@��@�5?@ٺ^@��`@�9X@׾w@֧�@�J@�p�@�&�@�r�@ӥ�@���@�E�@�hs@�Z@��
@·+@́@�7L@̛�@̓u@�bN@˥�@�;d@��y@�E�@��`@Ǖ�@�ff@�-@��@�x�@�/@��/@�A�@�"�@�hs@��/@��m@��@�5?@�p�@��@��;@���@���@� �@��m@�C�@�n�@���@��`@��@�ȴ@�J@�7L@�A�@�K�@�ȴ@�@�O�@�Ĝ@�|�@��@�@��@��!@��@�Q�@���@�^5@��#@�&�@���@���@��u@��u@� �@�t�@���@��\@���@�hs@�G�@��/@�1'@��F@��P@�|�@�l�@�S�@�@��+@�V@�5?@��@���@�x�@�G�@��D@���@�+@���@�E�@�{@�@��@��-@�G�@���@�Q�@�  @��w@��@�K�@�o@���@�v�@�E�@�^5@�n�@�V@��@��7@��`@�bN@�1'@��@�  @��
@���@���@�dZ@�"�@�o@��y@��H@��@���@��@�@���@�
=@�33@�"�@�o@�33@�l�@��y@��@�?}@���@���@�X@���@��F@�33@�n�@�$�@�=q@�5?@�$�@�J@�@�p�@�?}@��@��j@���@�ƨ@�\)@�C�@�C�@�C�@�;d@�"�@�@��@��R@��R@�K�@� �@�b@�ƨ@�ƨ@���@�K�@���@�V@�M�@��+@��\@�v�@�~�@�n�@�E�@�5?@��@�@��7@�hs@�`B@�`B@�O�@�G�@�7L@��@��@�V@���@��`@��@���@���@�9X@��
@�@���@�E�@�@�p�@��@�r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��FA���A�S�A�?}A�&�A�
=A���A��A��A��A��A��A��yA��yA��mA��mA��`A��HA��;A��A��
A���A���A���A��wA��uA�+A���A�Q�A��\A���A��A��9A���A�n�A�C�A�&�A��A��A�ĜA�v�A��A��/A���A���A�^5A��7A��A��\A�oA�A���A���A���A��A�ffA�{A�^5A��A�9XA�v�A�A�A��hA�v�A�K�A�1A�A���A��`A��A��
A�r�A��A���A�-A�ȴA�%A��DA��A��^A�|�A���A�5?A��A�E�A���A���A���A�VA���A�{A�oA�  A���A�33A��A��-A��TA��!A�{A��`A��9A���A��PA�jA�(�A��9A���A�bNA���A�O�A�bNA�=qA��/A�%A�(�A��7A���A���A�-A�n�A�x�A�/A�\)A�v�A�^A~n�A{`BAz(�Ay\)AwdZAu�hAt��Aq��AqoAohsAl��Aj��AhM�Af��Aep�Ad�!Ac�Ab�RA`VA_��A]K�A[|�AZM�AX��AX�AWhsAUAT�AT-AS�AQ"�AP1'AO"�AMdZAI�TAI�AHE�AH(�AG��AG�hAG|�AG�AF1'AB(�A>�A=t�A;��A;
=A9�FA8ĜA7��A6�RA6VA5ƨA5dZA5l�A5O�A5A4A2�A0�yA.�\A-O�A,�9A,=qA+�
A+t�A+7LA+
=A*ĜA)��A(��A(z�A(E�A(bA';dA$��A#�A"��A �AO�A��A(�A
=A��Az�A��A�AƨA�uA1A;dA
=A9XA�A��Az�A9XA�9A�7A"�A�HA��A�+AZA?}A
�A	��A�A�7A%A��A��A �A?}@��@��#@���@���@��/@���@��H@���@�|�@�\@�/@��
@�o@�v�@��-@���@�+@��@�\)@�@���@��@�5?@ٺ^@��`@�9X@׾w@֧�@�J@�p�@�&�@�r�@ӥ�@���@�E�@�hs@�Z@��
@·+@́@�7L@̛�@̓u@�bN@˥�@�;d@��y@�E�@��`@Ǖ�@�ff@�-@��@�x�@�/@��/@�A�@�"�@�hs@��/@��m@��@�5?@�p�@��@��;@���@���@� �@��m@�C�@�n�@���@��`@��@�ȴ@�J@�7L@�A�@�K�@�ȴ@�@�O�@�Ĝ@�|�@��@�@��@��!@��@�Q�@���@�^5@��#@�&�@���@���@��u@��u@� �@�t�@���@��\@���@�hs@�G�@��/@�1'@��F@��P@�|�@�l�@�S�@�@��+@�V@�5?@��@���@�x�@�G�@��D@���@�+@���@�E�@�{@�@��@��-@�G�@���@�Q�@�  @��w@��@�K�@�o@���@�v�@�E�@�^5@�n�@�V@��@��7@��`@�bN@�1'@��@�  @��
@���@���@�dZ@�"�@�o@��y@��H@��@���@��@�@���@�
=@�33@�"�@�o@�33@�l�@��y@��@�?}@���@���@�X@���@��F@�33@�n�@�$�@�=q@�5?@�$�@�J@�@�p�@�?}@��@��j@���@�ƨ@�\)@�C�@�C�@�C�@�;d@�"�@�@��@��R@��R@�K�@� �@�b@�ƨ@�ƨ@���@�K�@���@�V@�M�@��+@��\@�v�@�~�@�n�@�E�@�5?@��@�@��7@�hs@�`B@�`B@�O�@�G�@�7L@��@��@�V@���@��`@��@���@���@�9X@��
@�@���@�E�@�@�p�@��@�r�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�!B�!B�!B�'B�!B�!B�!B�!B�!B�!B�!B�B�B�B��B�B�wBȴB�B�;B�ZB�fB�mB�mB�sB�B��B��B��B��B��B��B��B��B�B�B�B��BBBPB�B�B �B#�B!�B�B�B�B\B�B2-B9XB;dB<jB>wBD�BE�BD�BA�B:^B2-B)�B�BuBbBDB%B��B�ZB�
BŢB�B��B��Bz�BhsB^5BM�B1'B�BDB%BB��B�ZB�BɺB�XB�!B�B��B��B�bB�Bq�BgmB\)BK�BH�BA�B1'B!�BDBB
��B
�yB
��B
��B
�\B
�B
x�B
q�B
hsB
T�B
M�B
G�B
7LB
-B
%�B
�B
hB
PB	�B	�BB	��B	ĜB	�jB	�LB	�-B	�B	��B	��B	�1B	{�B	u�B	o�B	iyB	cTB	[#B	VB	Q�B	J�B	A�B	<jB	49B	(�B	�B	�B	{B	uB	oB	hB	bB	PB	B�B�TB�)B�B�B�B�B�B�B�B�B�;B�;B�BB�BB�HB�BB�)B��B��B��B��B��B��B��B��B��B��B��B��BɺBǮBÖB�jB�9B�B��B��B��B��B�hB�VB�JB�=B�1B�B�+B� Bx�Bt�Br�Bp�Bq�Bo�Bn�Bk�BiyBhsBgmBiyBjBm�BjBiyBgmBdZBcTB`BB[#BYBXBVBT�BS�BQ�BN�BL�BL�BK�BJ�BJ�BH�BH�BF�BF�BD�BA�B?}B>wB>wB@�BA�BB�BE�BE�BE�BF�BG�BG�BH�BH�BH�BH�BH�BH�BK�BP�BQ�BQ�BR�BR�BQ�BQ�BS�BVBXBZB[#B[#B\)B\)B]/B_;B`BB_;BcTBe`BffBe`Be`Bm�Bp�Bt�Bv�Bw�By�B{�B{�B|�B}�B� B� B~�B�B�B�%B�1B�7B�=B�DB�VB�hB�oB�{B��B��B��B��B��B��B��B��B��B�'B�3B�3B�FB�RB�XB�^B�XB�dB�}BĜBȴB��B��B��B��B�
B�/B�5B�5B�;B�;B�HB�fB�mB�sB�yB�B�B�B�B��B��B	  B	B	B	B	%B	+B	
=B	\B	oB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	"�B	%�B	(�B	-B	1'B	2-B	33B	49B	5?B	7LB	8RB	:^B	=qB	>wB	@�B	A�B	B�B	C�B	F�B	J�B	N�B	Q�B	W
B	\)B	_;B	bNB	m�B	q�B	n�B	o�B	r�B	v�B	v�B	t�B	r�B	r�B	t�B	w�B	z�B	{�B	�B	�B	�B	�B	� B	� B	� B	� B	�B	�B	�+B	�7B	�7B	�=B	�DB	�DB	�JB	�VB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�9B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�LB	�^B	�^B	�jB	�}B	��B	ÖB	ÖB	ŢB	ƨB	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�!B�!B�!B�!B�!B�!B�!B�'B�!B�!B�!B�!B�!B�!B�!B�!B�!B�-B�B�3B�wBȴB�B�BB�`B�mB�sB�mB�yB�B��B��B��B��B��B��B  B��B�B�B�B��BBBVB�B�B#�B%�B%�B�B�B�BbB�B33B9XB;dB=qB@�BH�BG�BF�BD�B=qB49B.B�B�BhBPBDBB�sB�/B��B�'B��B��B}�BjBcTBXB;dB�BJB1B%B��B�sB�;B��B�wB�-B�B��B��B��B�1Bv�Bo�BaHBL�BJ�BI�B49B'�BVBB
��B
�B
�B
�'B
�uB
�1B
{�B
t�B
p�B
XB
O�B
L�B
<jB
0!B
-B
�B
�B
{B	��B	�mB	��B	ȴB	�wB	�^B	�?B	�-B	��B	��B	�VB	� B	z�B	q�B	k�B	hsB	^5B	XB	S�B	N�B	C�B	>wB	8RB	1'B	�B	�B	{B	{B	uB	hB	hB	bB	VB��B�mB�HB�B�B�B�#B�#B�B�)B�#B�;B�BB�HB�TB�ZB�fB�TB�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB�9B�B��B��B��B��B�hB�VB�JB�DB�1B�7B�B~�Bv�Bt�Br�Br�Bp�Br�Bn�BjBiyBhsBjBk�Bp�Bl�Bl�Bk�BgmBe`BdZBcTB\)B[#B[#BXBVBT�BQ�BN�BN�BN�BL�BL�BJ�BJ�BG�BG�BF�BF�BF�BD�BD�BC�BC�BE�BG�BF�BF�BG�BH�BI�BI�BI�BI�BI�BI�BI�BL�BQ�BS�BR�BT�BT�BQ�BQ�BS�BW
BYB[#B\)B\)B^5B^5B_;B`BBaHB`BBdZBffBgmBgmBhsBn�Br�Bv�Bw�Bx�Bz�B}�B}�B~�B�B�B�B�B�B�%B�7B�7B�DB�JB�PB�bB�oB�{B��B��B��B��B��B��B��B��B��B�B�-B�3B�?B�LB�XB�XB�^B�^B�jB��BŢBɺB��B��B��B��B�B�/B�5B�5B�;B�BB�NB�fB�mB�yB�B�B�B�B��B��B��B	B	B	B	B	+B	1B	DB	bB	uB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	"�B	#�B	&�B	)�B	.B	1'B	2-B	33B	49B	5?B	7LB	9XB	;dB	=qB	>wB	@�B	A�B	B�B	C�B	F�B	J�B	N�B	Q�B	W
B	\)B	_;B	bNB	n�B	s�B	n�B	o�B	r�B	w�B	w�B	v�B	s�B	s�B	u�B	w�B	z�B	{�B	�B	�B	�B	�B	�B	� B	� B	�B	�B	�B	�+B	�7B	�7B	�=B	�DB	�DB	�JB	�VB	�VB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�-B	�3B	�3B	�?B	�?B	�FB	�FB	�FB	�FB	�FB	�FB	�LB	�^B	�^B	�jB	�}B	��B	ĜB	ĜB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447242012010314472420120103144724  AO  ARGQ                                                                        20111130142420  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142420  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144724  IP                  G�O�G�O�G�O�                