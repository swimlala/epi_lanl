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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               #A   AO  20111130135838  20190522121825  1727_5046_035                   2C  D   APEX                            2143                            040306                          846 @�B�_À1   @�B���@7Q��R�c���+1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A���B   B��B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� DfD� D��D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D   D � D!  D!� D"  D"� D#  D#y�D$  D$� D%  D%y�D%��D&y�D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,y�D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA�fDBfDB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�ffA�33A�  A�33B33B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D�3Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D� D  Dy�D��D y�D ��D!y�D!��D"y�D"��D#s3D#��D$y�D$��D%s3D%�3D&s3D&��D'� D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,s3D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D3  D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�DA  DA� DB  DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWs3DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Dy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��/A��/A��;A��TA��HA��HA��HA��`A��mA��mA��mA��`A��`A��mA��yA��mA��mA��yA��yA��A��A��A��HA��;A��#A��#A��
A���Aԙ�A�ZA�E�A�$�A��AӉ7A�r�Aщ7A�K�A�1'A�$�A�XAƃA��A�A���A���A���A��^A���A�
=A�VA��A��!A�hsA��A��mA�I�A�=qA��A�hsA��A�Q�A��A�ȴA���A�VA���A���A��TA��7A��A�t�A�=qA�A�A��A�|�A�-A�hsA���A�oA��^A��PA��PA��PA���A�O�A�r�A�oA��A�C�A��`A�I�A��DA���A��A���A�(�A��!A�ffA�bA��uA���A��A�/A���A�JA�1A��TA���A��A���A�
=A�ȴA��A�n�A�-A�bA�x�A��wA�jA~=qA|�A{33AxȴAwO�Av��Au��As33Aq��Aq&�Ao�-Am��Am7LAl��Ak�mAjv�AhZAg��Ag+AfbNAe��Ac�^Aa�7Aa&�A_33A\�A[AY"�AX �AW��AV��AV  AU�
AUl�AT-AS/AQ�AQC�AP��AP$�AO�7AOAN5?AMdZAMG�AL�AJ�AHv�AGdZAF�uAES�AD�AC��AA�A@r�A?�mA>ffA=��A=O�A;A9�7A9&�A8��A7|�A6��A4��A41'A3"�A0�/A.�+A-ƨA-7LA+�TA+K�A*�!A)��A(�!A't�A%�A$ȴA$�\A$E�A#��A!�^A �yA�7A��A��Ar�AK�A(�A��AE�A�TAĜA�DA1'A��A�yA�+A��A%A�+A�A�yA(�A�A�!AQ�A�TAx�A�A
z�A	�A	�A��A�;A�A��A�yAz�A9XA�
AO�A�HAr�A��A�AdZA?}A ĜA ^5@�~�@���@��h@���@���@���@�^5@��@�K�@�p�@�r�@�@�p�@���@�z�@�=q@�I�@�\)@��T@�j@߮@ޏ\@ܬ@���@�n�@؃@�E�@�bN@ҸR@Ѓ@�"�@�=q@́@ˮ@�{@�b@�ff@��T@�hs@ģ�@��;@�`B@�%@���@��j@���@�Z@�@���@�&�@��
@�K�@�o@���@���@�V@���@�~�@�-@�@���@��@�ȴ@�E�@��-@���@��-@��@��@�l�@�t�@�\)@��H@��u@��!@�Q�@�1'@�9X@�A�@�I�@��!@���@�ȴ@�x�@��@�A�@��@�dZ@�ff@���@��@���@��P@��@���@���@�J@��7@�p�@�@���@��P@���@���@�|�@��@��@�`B@��j@�A�@��w@���@���@�-@��@��#@���@�7L@��@���@��9@��@�A�@���@�;d@���@�v�@�M�@�$�@���@�X@���@��m@�l�@��@���@��@��R@���@��+@��@�`B@��@�r�@�9X@��;@���@�C�@���@�^5@�5?@�{@�hs@��@�%@�Ĝ@��D@�bN@�(�@��w@�t�@�;d@��@�ff@��#@���@��h@��h@��h@��h@��h@��7@�hs@��@��/@��j@�Q�@�b@���@�
=@�v�@�E�@��@��T@���@��^@���@��@���@���@��j@�Z@�9X@�  @�ƨ@�t�@���@��@�ȴ@���@��R@�ff@��@���@�`B@��j@�z�@�A�@���@��@�K�@�o@��y@��\@�M�@�5?@�$�@�{@���@���@�O�@���@���@��;@�ƨ@��F@�t�@�K�@�33@��R@���@���@��+@�n�@��/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��/A��/A��;A��TA��HA��HA��HA��`A��mA��mA��mA��`A��`A��mA��yA��mA��mA��yA��yA��A��A��A��HA��;A��#A��#A��
A���Aԙ�A�ZA�E�A�$�A��AӉ7A�r�Aщ7A�K�A�1'A�$�A�XAƃA��A�A���A���A���A��^A���A�
=A�VA��A��!A�hsA��A��mA�I�A�=qA��A�hsA��A�Q�A��A�ȴA���A�VA���A���A��TA��7A��A�t�A�=qA�A�A��A�|�A�-A�hsA���A�oA��^A��PA��PA��PA���A�O�A�r�A�oA��A�C�A��`A�I�A��DA���A��A���A�(�A��!A�ffA�bA��uA���A��A�/A���A�JA�1A��TA���A��A���A�
=A�ȴA��A�n�A�-A�bA�x�A��wA�jA~=qA|�A{33AxȴAwO�Av��Au��As33Aq��Aq&�Ao�-Am��Am7LAl��Ak�mAjv�AhZAg��Ag+AfbNAe��Ac�^Aa�7Aa&�A_33A\�A[AY"�AX �AW��AV��AV  AU�
AUl�AT-AS/AQ�AQC�AP��AP$�AO�7AOAN5?AMdZAMG�AL�AJ�AHv�AGdZAF�uAES�AD�AC��AA�A@r�A?�mA>ffA=��A=O�A;A9�7A9&�A8��A7|�A6��A4��A41'A3"�A0�/A.�+A-ƨA-7LA+�TA+K�A*�!A)��A(�!A't�A%�A$ȴA$�\A$E�A#��A!�^A �yA�7A��A��Ar�AK�A(�A��AE�A�TAĜA�DA1'A��A�yA�+A��A%A�+A�A�yA(�A�A�!AQ�A�TAx�A�A
z�A	�A	�A��A�;A�A��A�yAz�A9XA�
AO�A�HAr�A��A�AdZA?}A ĜA ^5@�~�@���@��h@���@���@���@�^5@��@�K�@�p�@�r�@�@�p�@���@�z�@�=q@�I�@�\)@��T@�j@߮@ޏ\@ܬ@���@�n�@؃@�E�@�bN@ҸR@Ѓ@�"�@�=q@́@ˮ@�{@�b@�ff@��T@�hs@ģ�@��;@�`B@�%@���@��j@���@�Z@�@���@�&�@��
@�K�@�o@���@���@�V@���@�~�@�-@�@���@��@�ȴ@�E�@��-@���@��-@��@��@�l�@�t�@�\)@��H@��u@��!@�Q�@�1'@�9X@�A�@�I�@��!@���@�ȴ@�x�@��@�A�@��@�dZ@�ff@���@��@���@��P@��@���@���@�J@��7@�p�@�@���@��P@���@���@�|�@��@��@�`B@��j@�A�@��w@���@���@�-@��@��#@���@�7L@��@���@��9@��@�A�@���@�;d@���@�v�@�M�@�$�@���@�X@���@��m@�l�@��@���@��@��R@���@��+@��@�`B@��@�r�@�9X@��;@���@�C�@���@�^5@�5?@�{@�hs@��@�%@�Ĝ@��D@�bN@�(�@��w@�t�@�;d@��@�ff@��#@���@��h@��h@��h@��h@��h@��7@�hs@��@��/@��j@�Q�@�b@���@�
=@�v�@�E�@��@��T@���@��^@���@��@���@���@��j@�Z@�9X@�  @�ƨ@�t�@���@��@�ȴ@���@��R@�ff@��@���@�`B@��j@�z�@�A�@���@��@�K�@�o@��y@��\@�M�@�5?@�$�@�{@���@���@�O�@���@���@��;@�ƨ@��F@�t�@�K�@�33@��R@���@���@��+@�n�@��/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�RB��B�mB�B�)B��B�B�B�B�B�sB�B�B�sB�;B�)B�)B�B��B��BƨB�dB�qB�FB�FB�XB�jB��B�^B�3B�B��B��B�VBz�B^5BVBG�B8RB/B$�BPBB��B�B�B�sB�)B��BȴB�XB�B��B�JB�Bx�Bm�B`BBI�B8RB+B#�B�B�BVB+B
��B
�B
�/B
��B
ƨB
�?B
��B
�7B
{�B
x�B
� B
��B
��B
��B
��B
�{B
�JB
�B
}�B
p�B
hsB
^5B
Q�B
J�B
E�B
?}B
2-B
)�B
%�B
�B
�B
uB
VB
	7B
  B	��B	�B	�B	�B	�sB	�NB	�
B	��B	ĜB	�'B	��B	��B	��B	��B	��B	��B	��B	��B	�{B	�PB	�%B	�B	|�B	y�B	v�B	r�B	n�B	jB	hsB	cTB	W
B	J�B	C�B	>wB	8RB	49B	,B	#�B	�B	�B	VB	
=B	+B��B��B��B�B�B�sB�NB�5B�B��BȴBƨBĜB��B��B�qB�^B�RB�?B�3B�'B�!B�B��B��B��B��B��B��B��B��B�hB�\B�PB�DB�7B�B� B}�B{�By�Bw�Bt�Br�Bp�Bn�Bk�BjBhsBgmBffBe`BcTBaHB_;B]/B[#BW
BT�BQ�BO�BN�BM�BK�BJ�BI�BH�BG�BG�BG�BE�BD�BB�BA�BA�B?}B=qB;dB:^B9XB6FB5?B49B33B0!B0!B.B,B(�B)�B(�B(�B(�B'�B&�B'�B&�B&�B%�B%�B%�B&�B&�B(�B+B,B.B/B2-B49B5?B6FB6FB6FB=qB>wB>wB@�BA�BC�BL�BN�BN�BO�BO�BP�BR�BVBVBYB]/B^5B_;B^5B]/B]/BbNBcTBhsBjBjBm�Bn�Bo�Br�Bw�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B�FB�jB��BĜBƨB��B��B��B��B��B�5B�NB�fB�fB�yB�B�B�B�B�B��B��B��B��B��B��B��B	%B	+B	1B	
=B	PB	VB	\B	{B	�B	�B	�B	�B	�B	�B	�B	%�B	'�B	(�B	)�B	+B	+B	+B	+B	.B	0!B	49B	49B	5?B	5?B	5?B	5?B	7LB	8RB	8RB	9XB	<jB	>wB	>wB	@�B	A�B	B�B	D�B	H�B	J�B	K�B	M�B	Q�B	VB	YB	YB	YB	YB	YB	YB	YB	ZB	[#B	]/B	^5B	aHB	bNB	dZB	jB	l�B	o�B	q�B	r�B	r�B	s�B	s�B	t�B	y�B	{�B	{�B	�B	�B	�B	�%B	�7B	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�FB	�RB	�XB	�^B	�^B	�dB	�qB	�}B	ÖB	ǮB	ǮB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�qB��B�B�B�fB�;B��B��B�B�B�B��B��B�B�TB�5B�/B�;B�B��B��B��BÖB�qB�dB�}B��BĜB�}B�?B�!B��B��B�uB�B`BB\)BL�B<jB49B,BhBB��B�B�B�B�HB�B��B�wB�9B��B�hB�B{�Bq�BiyBO�B<jB-B%�B�B�BbBJB
��B
�B
�BB
�B
��B
�dB
�B
�\B
|�B
|�B
�B
��B
��B
��B
��B
��B
�bB
�%B
�B
t�B
m�B
dZB
VB
L�B
G�B
F�B
6FB
,B
)�B
%�B
�B
�B
bB
VB
%B	��B	��B	�B	�B	�B	�yB	�B	�B	��B	�FB	�!B	��B	��B	��B	��B	��B	��B	��B	��B	�bB	�1B	�B	}�B	{�B	x�B	t�B	p�B	k�B	jB	iyB	]/B	M�B	F�B	B�B	9XB	7LB	1'B	'�B	�B	�B	bB	DB	JB	B��B��B��B�B�B�`B�NB�BB�
B��BȴBȴBÖBB��B�wB�jB�^B�LB�-B�'B�'B�!B�B��B��B��B��B��B��B�{B�bB�VB�VB�VB�B�B� B|�B{�By�Bu�Bs�Bs�Bp�Bn�Bk�BiyBhsBgmBffBe`BdZBaHB^5B^5BZBXBT�BQ�BO�BN�BM�BL�BK�BJ�BH�BH�BH�BG�BF�BF�BB�BB�BC�B@�B?}B=qB;dB9XB8RB6FB5?B49B5?B2-B0!B,B)�B,B(�B+B)�B)�B)�B(�B)�B(�B'�B'�B)�B(�B)�B,B.B0!B2-B49B5?B6FB7LB7LB:^B>wB>wB?}B@�BB�BF�BN�BO�BQ�BP�BP�BQ�BT�BXBYB[#B^5B_;BaHBaHB_;B^5BbNBcTBiyBm�Bk�Bm�Bn�Bo�Bs�Bs�B�B�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�FB�jB��BĜBƨB��B��B��B�B��B�)B�NB�fB�mB�yB�B�B�B�B�B��B��B��B��B��B��B��B	%B	+B		7B	
=B	VB	\B	bB	�B	�B	�B	�B	�B	�B	�B	 �B	%�B	'�B	(�B	)�B	+B	+B	+B	,B	/B	1'B	49B	49B	6FB	6FB	6FB	6FB	8RB	8RB	8RB	:^B	=qB	>wB	?}B	@�B	A�B	C�B	E�B	I�B	K�B	L�B	N�B	R�B	W
B	YB	YB	YB	YB	YB	YB	YB	[#B	\)B	]/B	_;B	bNB	bNB	e`B	k�B	l�B	o�B	q�B	r�B	r�B	s�B	s�B	u�B	y�B	{�B	|�B	�B	�B	�B	�+B	�=B	�PB	�VB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�?B	�LB	�RB	�XB	�^B	�^B	�jB	�wB	��B	ĜB	ǮB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	�/1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446462012010314464620120103144646  AO  ARGQ                                                                        20111130135838  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135838  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144646  IP                  G�O�G�O�G�O�                