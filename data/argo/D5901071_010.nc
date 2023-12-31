CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:39:53Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               
A   AO  20111130135525  20190522121825  1727_5046_010                   2C  D   APEX                            2143                            040306                          846 @�$3_@	1   @�$4F?�@7�5?|��c�hr� �1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B_��Bg��Bp  Bx  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cy�fC|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	fD	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DQ��DR� DS  DS� DT  DT� DU  DUy�DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_�fD`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� DkfDk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  DsffDy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���AffA>ffA^ffA~ffA�33A�  A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_33Bg33Bo��Bw��B��B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B㙚B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy��C{�fC}�fC�fC��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D	  D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�DA  DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ�3DRy�DR��DSy�DS��DTy�DT��DUs3DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_� D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dk  Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Ds` Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�K�A�I�A�C�A�E�A�K�A�M�A�C�A�A�A�=qA�5?A�9XA�;dA�&�A��yAɩ�A��A�%A��PA�ffA�jA��9A���A���A��PA�5?A�5?A�A�A��A�E�A��/A��uA�
=A��A�  A��A�VA��A���A�"�A��#A���A�O�A��A��wA�VA��/A��A�~�A�ZA�&�A��A�=qA��A���A��HA���A���A�hsA��PA��A�"�A���A�Q�A��A��A�hsA�%A���A���A���A���A�l�A���A���A�l�A��A���A���A�
=A���A��A��PA�A�+A�ƨA��uA�ffA���A�\)A�{A�ffA�%A�-A��A�C�A�oA�t�A��-A��TA�ȴA��jA�z�A���A�M�A�v�A��A�x�A���A���A���A�\)A�hsA��!A��TA��A}�hA{|�Az�AxVAv�!As��Aq�mAp�RAo/AmoAj�yAi�Ai�Ai"�Ag33Af5?AbȴAal�Aa"�A_;dA]C�A[G�AZ-AZVAZ�\AZ  AYAW�
AUt�AT�DASoAOAO&�AO�hAO�^AOdZAN�HANQ�AKK�AI��AIx�AG�AF��AE�FAD�RAC�PAB��AA��A@�A?��A?
=A>�A<��A;K�A:��A9��A8{A7��A7;dA5�
A4jA2�`A1XA0��A/�mA.E�A,�A+oA)�;A)33A(�jA';dA&��A&M�A&  A%�A#�mA#%A"�A �DAn�A  AbNA��A\)A�A��AffAt�A�A�-A�A��A��A�A�AC�A�
Av�AG�A1'A��A��A�7A
E�A	�A�AjAJAp�AS�AK�A�`AjAA%A�AVA ^5A �@�
=@��@�"�@���@�5?@��^@�7L@�r�@���@�ff@�(�@�@�F@�^5@���@�V@�"�@�^@�D@㝲@�o@�@�bN@�M�@�I�@�|�@ڰ!@���@ى7@��@��@��T@�&�@�b@�C�@��@���@��@ҸR@҇+@щ7@�%@�Ĝ@�Q�@�(�@�dZ@Η�@�G�@���@�Z@��@�{@�@ɡ�@�p�@ț�@�
=@�M�@�?}@�Z@��@þw@��@�|�@��\@���@�r�@��@�o@���@���@��+@�n�@�-@�x�@�Q�@�C�@��@�~�@�E�@�O�@�%@��u@�K�@��F@���@��P@�dZ@�+@�@���@�M�@�x�@��/@��`@�"�@�A�@��@��j@���@��u@�(�@�n�@��@�G�@��
@���@��@��H@�{@�X@��@��@�?}@���@���@�"�@��^@�hs@��7@��h@�x�@�?}@�&�@�7L@�bN@�  @�|�@�
=@�o@��@�@��y@��!@�V@��-@��9@�A�@���@���@�C�@�"�@�@��@��\@�v�@�=q@�-@��@��^@��7@��@���@�5?@�$�@��T@���@�7L@���@�Q�@���@���@�\)@���@�V@��@�p�@�G�@�G�@�X@�?}@��`@��D@�Q�@�A�@���@���@�ȴ@�V@�@���@��h@��@��@�bN@�Q�@�Q�@�Q�@�I�@�9X@� �@�(�@��@��@���@���@��@�@��#@���@���@���@��h@�G�@�r�@��m@��F@�t�@��;@��@�1@��;@��;@���@���@��F@�S�@��+@�1@��@�ȴ@�ff@�v�@�E�@��@���@�/@��D@�I�@�  @��@���@��
@�1@�9X@�9X@�b@���@�^5@���@��^@���@��/@��@��`@�|�@��@��@���@��R@�ȴ@�r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�K�A�I�A�C�A�E�A�K�A�M�A�C�A�A�A�=qA�5?A�9XA�;dA�&�A��yAɩ�A��A�%A��PA�ffA�jA��9A���A���A��PA�5?A�5?A�A�A��A�E�A��/A��uA�
=A��A�  A��A�VA��A���A�"�A��#A���A�O�A��A��wA�VA��/A��A�~�A�ZA�&�A��A�=qA��A���A��HA���A���A�hsA��PA��A�"�A���A�Q�A��A��A�hsA�%A���A���A���A���A�l�A���A���A�l�A��A���A���A�
=A���A��A��PA�A�+A�ƨA��uA�ffA���A�\)A�{A�ffA�%A�-A��A�C�A�oA�t�A��-A��TA�ȴA��jA�z�A���A�M�A�v�A��A�x�A���A���A���A�\)A�hsA��!A��TA��A}�hA{|�Az�AxVAv�!As��Aq�mAp�RAo/AmoAj�yAi�Ai�Ai"�Ag33Af5?AbȴAal�Aa"�A_;dA]C�A[G�AZ-AZVAZ�\AZ  AYAW�
AUt�AT�DASoAOAO&�AO�hAO�^AOdZAN�HANQ�AKK�AI��AIx�AG�AF��AE�FAD�RAC�PAB��AA��A@�A?��A?
=A>�A<��A;K�A:��A9��A8{A7��A7;dA5�
A4jA2�`A1XA0��A/�mA.E�A,�A+oA)�;A)33A(�jA';dA&��A&M�A&  A%�A#�mA#%A"�A �DAn�A  AbNA��A\)A�A��AffAt�A�A�-A�A��A��A�A�AC�A�
Av�AG�A1'A��A��A�7A
E�A	�A�AjAJAp�AS�AK�A�`AjAA%A�AVA ^5A �@�
=@��@�"�@���@�5?@��^@�7L@�r�@���@�ff@�(�@�@�F@�^5@���@�V@�"�@�^@�D@㝲@�o@�@�bN@�M�@�I�@�|�@ڰ!@���@ى7@��@��@��T@�&�@�b@�C�@��@���@��@ҸR@҇+@щ7@�%@�Ĝ@�Q�@�(�@�dZ@Η�@�G�@���@�Z@��@�{@�@ɡ�@�p�@ț�@�
=@�M�@�?}@�Z@��@þw@��@�|�@��\@���@�r�@��@�o@���@���@��+@�n�@�-@�x�@�Q�@�C�@��@�~�@�E�@�O�@�%@��u@�K�@��F@���@��P@�dZ@�+@�@���@�M�@�x�@��/@��`@�"�@�A�@��@��j@���@��u@�(�@�n�@��@�G�@��
@���@��@��H@�{@�X@��@��@�?}@���@���@�"�@��^@�hs@��7@��h@�x�@�?}@�&�@�7L@�bN@�  @�|�@�
=@�o@��@�@��y@��!@�V@��-@��9@�A�@���@���@�C�@�"�@�@��@��\@�v�@�=q@�-@��@��^@��7@��@���@�5?@�$�@��T@���@�7L@���@�Q�@���@���@�\)@���@�V@��@�p�@�G�@�G�@�X@�?}@��`@��D@�Q�@�A�@���@���@�ȴ@�V@�@���@��h@��@��@�bN@�Q�@�Q�@�Q�@�I�@�9X@� �@�(�@��@��@���@���@��@�@��#@���@���@���@��h@�G�@�r�@��m@��F@�t�@��;@��@�1@��;@��;@���@���@��F@�S�@��+@�1@��@�ȴ@�ff@�v�@�E�@��@���@�/@��D@�I�@�  @��@���@��
@�1@�9X@�9X@�b@���@�^5@���@��^@���@��/@��@��`@�|�@��@��@���@��R@�ȴ@�r�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB+B)�B+B,B+B+B+B+B,B,B,B,B)�B(�B'�B �BDB�B�B	7B�B�B#�B/BA�BM�B^5Bs�Bs�Bw�B}�B�DB��B��B��B��B�RBĜBȴB��B��B��B��B��B��B��B��B��B��B��B�NB�B�B��B��BBuB�B%�B2-B33B9XB5?B6FB9XB49B(�B�BDB�yB�sB��B��B��B�B�B��B��B��B�hB�%Bm�BW
BH�B1'B.B+B�BB�B�B�}B�-B��B�DBx�BjBZBF�B&�B�B  B
�ZB
��B
B
�3B
��B
��B
�hB
�DB
�+B
o�B
Q�B
9XB
#�B
oB
B	��B	�B	�NB	��B	ĜB	�jB	�-B	��B	��B	��B	��B	��B	�bB	�B	w�B	u�B	q�B	cTB	R�B	M�B	Q�B	XB	^5B	]/B	dZB	aHB	S�B	H�B	5?B	�B	�B	5?B	=qB	?}B	=qB	6FB	)�B	�B	�B	uB	uB	JB	1B	B��B��B��B��B�B�B�fB�ZB�NB�5B��B�B�B��B��BB�qB�^B�9B�!B�B��B��B��B��B��B��B��B��B��B��B��B�oB�PB�%B�B}�B|�B{�Bz�By�Bw�Bt�Br�Bo�Bm�Bk�BjBhsBiyBe`BcTBaHB_;B`BB_;B^5B\)B[#BZBYBXBXBW
BW
BVBT�BS�BR�BO�BL�BM�BM�BL�BK�BK�BK�BK�BK�BJ�BJ�BI�BI�BG�BG�BG�BG�BF�BF�BE�BG�BG�BH�BI�BJ�BI�BI�BJ�BM�BM�BO�BP�BP�BS�BVBYB[#B`BBffBffBgmBgmBgmBgmBhsBiyBiyBl�Bn�Br�Bx�B{�B{�B{�B}�B� B� B� B~�B|�B}�B|�Bz�Bz�By�Bx�Bv�Bz�B{�B|�B� B�B�B�B�B�%B�%B�+B�1B�\B�hB�bB�\B�bB�{B��B��B��B��B�'B�3B�FB�XB�qBǮBȴBɺB��B��B�`B�B�B��B��B��B��B�B��B��B�B�B�mB�TB�NB�HB�mB�B��B�B�B��B�B��B��B��B��B��B��B��B	B		7B	VB	oB	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	"�B	%�B	'�B	(�B	)�B	+B	,B	-B	/B	1'B	2-B	33B	5?B	6FB	=qB	?}B	A�B	D�B	F�B	G�B	H�B	J�B	M�B	O�B	R�B	S�B	VB	[#B	_;B	bNB	dZB	ffB	gmB	ffB	ffB	ffB	e`B	dZB	ffB	hsB	l�B	n�B	n�B	n�B	p�B	s�B	t�B	u�B	u�B	v�B	w�B	y�B	{�B	{�B	}�B	}�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�%B	�%B	�+B	�+B	�+B	�1B	�JB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�?B	�^B	�wB	��B	��B	�}B	�qB	�qB	�qB	�wB	�}B	��B	��B	�}B	�}B	�}B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B+B)�B+B,B+B+B+B+B,B,B,B,B+B)�B)�B(�B �B��B�BJB�B �B%�B1'BA�BM�B_;Bw�Bu�By�B�B�VB��B��B��B��B�jBƨBɺB��B��B��B��B��B��B��B��B��B��B��B�ZB�B��B��B��BB{B{B%�B49B49B;dB6FB6FB;dB6FB)�B�BVB�B�B��B�B�B�B�B��B��B��B�uB�DBu�B\)BO�B2-B/B.B$�BDB��B�/BÖB�dB��B�hB{�Bn�B_;BP�B+B�B1B
�B
�
B
ǮB
�RB
��B
��B
�uB
�PB
�bB
w�B
VB
>wB
(�B
�B
1B	��B	�B	�yB	�B	ǮB	��B	�RB	�B	��B	��B	��B	��B	�uB	�\B	{�B	v�B	w�B	iyB	YB	P�B	Q�B	XB	`BB	^5B	hsB	ffB	VB	L�B	<jB	�B	�B	5?B	>wB	@�B	?}B	=qB	.B	 �B	 �B	�B	�B	\B	DB	B	B��B��B��B�B�B�B�fB�`B�NB��B�B�5B�
B��BƨB�}B�qB�XB�FB�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�1B�7B�B}�B|�B{�B{�B{�Bx�Bv�Bv�Br�Bo�Bm�Bl�Bl�Bk�BhsBffBcTBaHB`BB`BBaHB^5B\)B[#BZBZBXBW
BXBW
BT�BVBT�BR�BO�BN�BN�BN�BN�BL�BL�BL�BK�BK�BJ�BK�BK�BK�BJ�BI�BI�BJ�BJ�BJ�BI�BJ�BJ�BL�BL�BM�BN�BO�BO�BP�BQ�BS�BW
BXB[#B]/BbNBgmBffBgmBgmBhsBiyBiyBjBjBl�Bo�Bs�Bz�B|�B|�B}�B� B�B� B�B� B~�B~�B~�B{�B{�Bz�Bz�B{�B|�B}�B~�B�B�B�B�B�B�%B�+B�1B�=B�hB�oB�bB�bB�oB��B��B��B��B��B�'B�3B�LB�XB�wBȴBɺB��B��B��B�TB�B�B��B��B��B��B��B��B��B��B�B�yB�ZB�TB�NB�mB�B��B��B��B��B��B��B��B��B��B��B��B��B	%B	
=B	\B	oB	�B	�B	�B	�B	�B	�B	!�B	"�B	#�B	#�B	&�B	'�B	(�B	)�B	,B	,B	.B	/B	2-B	2-B	33B	6FB	5?B	=qB	?}B	A�B	E�B	G�B	H�B	I�B	K�B	M�B	P�B	S�B	T�B	W
B	\)B	_;B	bNB	dZB	ffB	hsB	gmB	ffB	ffB	ffB	ffB	ffB	iyB	m�B	n�B	o�B	o�B	q�B	s�B	t�B	u�B	u�B	v�B	w�B	y�B	{�B	{�B	}�B	}�B	� B	� B	� B	�B	�B	�B	�B	�B	�+B	�+B	�1B	�+B	�+B	�+B	�JB	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�-B	�?B	�^B	�wB	��B	��B	B	�wB	�qB	�}B	�wB	�}B	��B	ÖB	��B	�}B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�1<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446372012010314463720120103144637  AO  ARGQ                                                                        20111130135525  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135525  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144637  IP                  G�O�G�O�G�O�                