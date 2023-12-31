CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:18Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               bA   AO  20111130141403  20190522121826  1727_5046_098                   2C  D   APEX                            2143                            040306                          846 @ԔW��1   @ԔX��@6��\)�c�?|�h1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D�fDfD�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@fD@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� DefDe� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr�fDs  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B33B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B�  B���B���B���B���B���B���B���B���B���B���B���B�  B�  B���B���B���B���Bߙ�B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC��C�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCl  Cn  Co�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C�  C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��D� D  D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D � D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D@  D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE�3DFy�DF��DGy�DG��DHy�DH��DIy�DJ  DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�De  Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dr� Dr��Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�G�A�G�A�I�A�I�A�K�A�M�A�E�A�G�A�A�A�A�A�C�A�C�A�A�A�=qA�=qA�9XA�5?A��A��A��/A��TA��/AӮAӟ�AӉ7A�n�A�I�A�VA��AҸRA�n�A�x�A���A�S�AȰ!AƸRA�I�A�dZA�
=Ağ�A�XA�S�A�C�A��A�;dA��A�C�A���A��-A�v�A�&�A���A���A�  A�;dA�ĜA��A�ƨA�+A�33A��HA�|�A�
=A��A���A�S�A�(�A�
=A��A���A�^5A��A��!A�ZA�oA��TA��DA�?}A�C�A��/A��A�  A� �A��A��A�`BA��+A��7A�|�A�l�A�\)A���A�`BA�M�A���A�E�A�ffA�33A��HA��+A���A�r�A�1A���A�VA�ĜA��A�"�A���A���A�$�A���A���A�5?A��-A��RA�`BA���A��A��TA�ffA�ĜA��A���A��yA�?}A�oA�^5A��A}?}A{�Az�Aw�TAvv�Au�^As�Aq�Ao�;AnffAljAk��Aj�!Ai�wAg��Ael�Ac�PAa�#A`JA^�A[��AX�/AV�`AU��AT  AS?}AQ&�AOp�AM��AM��AM?}AL^5AKt�AIt�AF�AE�AD^5AC��AB��ABr�AA��A?�A>(�A=
=A<A�A;%A:bNA9\)A7VA6�A4Q�A2��A2�A2�+A1��A1?}A0��A0M�A/\)A.^5A.1'A-�A*�A)�A(9XA'�;A'?}A&�jA&$�A$~�A#�A#�FA#�PA"��A"ffA!t�A �\A�
AXA�HA�uA9XA33A�!A�A��A{A�TAp�A%AM�A?}A��A�AZA\)A��A�A��A�AȴA
�!A	�#A	��A�/Av�A��AA�7A��A�A ~�@��m@��F@�@�Ĝ@���@��
@��T@�D@�F@�$�@�F@�v�@�h@��@�(�@�~�@���@�G�@��@���@�@�E�@��@�l�@��H@�{@�&�@�A�@�ff@�-@�M�@�7L@׶F@�t�@�ȴ@�G�@Ӯ@��@�ȴ@���@Ұ!@�$�@Ѳ-@��/@�1'@�C�@Η�@�  @̓u@˶F@�$�@�n�@�33@�t�@�hs@���@��-@�G�@�hs@��@�Q�@�+@��@���@�~�@���@��@��m@��!@�@�%@�bN@��
@�K�@��H@��\@�$�@�p�@�  @�\)@�~�@�/@��
@���@�|�@�dZ@��@��@��@�X@��9@�ƨ@�"�@�E�@��@�x�@���@��j@�(�@� �@���@�dZ@�+@�ȴ@��@���@�@��@��P@�n�@�E�@��@���@�hs@�O�@�7L@�%@�(�@��y@��@�$�@�J@�=q@���@��@��@��y@��R@��#@�Ĝ@���@�Z@� �@��@���@��@�n�@�V@�5?@��@���@���@�z�@���@�&�@�&�@���@��u@� �@���@�+@�
=@��y@�ȴ@��R@��R@�ff@�V@�$�@�{@��^@��@��@�%@���@���@��@�Ĝ@��`@��@�A�@�dZ@��y@���@��@���@�x�@�hs@�`B@�/@��u@�Z@�(�@�t�@�+@�
=@�ȴ@�M�@�{@�@���@���@�x�@�O�@��@��@��9@���@�Z@�bN@�bN@�9X@��@�K�@�C�@��H@���@��7@�X@��@�V@��`@���@��\@�n�@�V@�5?@�J@���@��/@�1@�b@�(�@�  @�1@���@�S�@�S�@�l�@�t�@�t�@�t�@�t�@�t�@�l�@�\)@�33@��@��@��R@��+@�E�@���@�O�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�G�A�G�A�I�A�I�A�K�A�M�A�E�A�G�A�A�A�A�A�C�A�C�A�A�A�=qA�=qA�9XA�5?A��A��A��/A��TA��/AӮAӟ�AӉ7A�n�A�I�A�VA��AҸRA�n�A�x�A���A�S�AȰ!AƸRA�I�A�dZA�
=Ağ�A�XA�S�A�C�A��A�;dA��A�C�A���A��-A�v�A�&�A���A���A�  A�;dA�ĜA��A�ƨA�+A�33A��HA�|�A�
=A��A���A�S�A�(�A�
=A��A���A�^5A��A��!A�ZA�oA��TA��DA�?}A�C�A��/A��A�  A� �A��A��A�`BA��+A��7A�|�A�l�A�\)A���A�`BA�M�A���A�E�A�ffA�33A��HA��+A���A�r�A�1A���A�VA�ĜA��A�"�A���A���A�$�A���A���A�5?A��-A��RA�`BA���A��A��TA�ffA�ĜA��A���A��yA�?}A�oA�^5A��A}?}A{�Az�Aw�TAvv�Au�^As�Aq�Ao�;AnffAljAk��Aj�!Ai�wAg��Ael�Ac�PAa�#A`JA^�A[��AX�/AV�`AU��AT  AS?}AQ&�AOp�AM��AM��AM?}AL^5AKt�AIt�AF�AE�AD^5AC��AB��ABr�AA��A?�A>(�A=
=A<A�A;%A:bNA9\)A7VA6�A4Q�A2��A2�A2�+A1��A1?}A0��A0M�A/\)A.^5A.1'A-�A*�A)�A(9XA'�;A'?}A&�jA&$�A$~�A#�A#�FA#�PA"��A"ffA!t�A �\A�
AXA�HA�uA9XA33A�!A�A��A{A�TAp�A%AM�A?}A��A�AZA\)A��A�A��A�AȴA
�!A	�#A	��A�/Av�A��AA�7A��A�A ~�@��m@��F@�@�Ĝ@���@��
@��T@�D@�F@�$�@�F@�v�@�h@��@�(�@�~�@���@�G�@��@���@�@�E�@��@�l�@��H@�{@�&�@�A�@�ff@�-@�M�@�7L@׶F@�t�@�ȴ@�G�@Ӯ@��@�ȴ@���@Ұ!@�$�@Ѳ-@��/@�1'@�C�@Η�@�  @̓u@˶F@�$�@�n�@�33@�t�@�hs@���@��-@�G�@�hs@��@�Q�@�+@��@���@�~�@���@��@��m@��!@�@�%@�bN@��
@�K�@��H@��\@�$�@�p�@�  @�\)@�~�@�/@��
@���@�|�@�dZ@��@��@��@�X@��9@�ƨ@�"�@�E�@��@�x�@���@��j@�(�@� �@���@�dZ@�+@�ȴ@��@���@�@��@��P@�n�@�E�@��@���@�hs@�O�@�7L@�%@�(�@��y@��@�$�@�J@�=q@���@��@��@��y@��R@��#@�Ĝ@���@�Z@� �@��@���@��@�n�@�V@�5?@��@���@���@�z�@���@�&�@�&�@���@��u@� �@���@�+@�
=@��y@�ȴ@��R@��R@�ff@�V@�$�@�{@��^@��@��@�%@���@���@��@�Ĝ@��`@��@�A�@�dZ@��y@���@��@���@�x�@�hs@�`B@�/@��u@�Z@�(�@�t�@�+@�
=@�ȴ@�M�@�{@�@���@���@�x�@�O�@��@��@��9@���@�Z@�bN@�bN@�9X@��@�K�@�C�@��H@���@��7@�X@��@�V@��`@���@��\@�n�@�V@�5?@�J@���@��/@�1@�b@�(�@�  @�1@���@�S�@�S�@�l�@�t�@�t�@�t�@�t�@�t�@�l�@�\)@�33@��@��@��R@��+@�E�@���@�O�@�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�mB�ZB�TB�TB�TB�HB�BB�5B�/B�B�
B�B��B��B�RB�B@�B'�B�BhB�B�B"�B>wBO�Br�B�DB�{B��BŢB�ZB�B��B1B�B=qB]/Bx�B�PB�VB�=B�VB�JB�hB�hB��B��B��B��B��B��B�B�B�B�!B�-B�dB�qB�wBĜBǮBȴBƨBŢBB�jB�dB�wBĜB��B�RB�B��B}�BffBM�B;dB1'B(�BhB��B�B�sB�/B��B�B��B��B�=Br�B\)BD�B&�BVBB
��B
�B
�HB
��B
�FB
�B
��B
��B
��B
� B
y�B
s�B
k�B
cTB
XB
P�B
L�B
=qB
/B
'�B
�B
uB
VB
B	��B	�B	�HB	�
B	��B	��B	ĜB	�XB	�'B	��B	��B	�oB	�7B	{�B	jB	]/B	Q�B	G�B	C�B	9XB	-B	$�B	"�B	�B	�B	oB	JB��B��B�B�B�B�mB�HB�
B��B��BƨB��B��B�wB�RB�3B�!B�FB�wBB��B�}B�qB�^B�RB�FB�9B�B��B��B��B��B��B��B��B�hB�hB�hB�bB�bB�VB�\B�=B�7B�1B�%B�B�B�B|�Bw�Bs�Bs�Br�Bp�Bn�Bl�BjBhsBffBl�Bm�Bl�Bk�BjBhsBcTBbNB`BB_;BXBP�BM�BH�BH�BH�BE�BE�BD�BC�B@�B<jB;dB9XB9XB8RB7LB49B49B49B33B33B2-B5?B6FB5?B9XB;dB49B1'B9XB?}BA�B@�B=qB8RB<jBA�BD�BG�BJ�BO�BP�BL�BJ�BK�BK�BK�BL�BO�BS�BR�BR�BO�BO�BM�BYB^5B]/BS�BO�BW
BVB\)B_;BbNBgmBgmB`BB\)BZBYBYBXBW
BVBS�BR�BR�BT�BW
BXBYBYBYBZB^5B`BBaHBe`BjBk�Bk�Bk�Bl�Bl�Bp�Br�Bu�Bz�B�B�1B�7B�=B�DB�bB�{B��B��B��B��B��B��B�B�B�B�B�!B�'B�LB�qB��B��BBĜBƨBɺB��B��B�
B�B�#B�/B�/B�5B�;B�;B�TB�TB�TB�TB�ZB�yB�B��B��B�B�B��B��B��B��B	B	B	+B		7B	VB	oB	{B	{B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	 �B	"�B	$�B	%�B	&�B	(�B	+B	,B	/B	1'B	49B	7LB	;dB	<jB	?}B	@�B	@�B	@�B	@�B	B�B	B�B	B�B	H�B	K�B	L�B	N�B	R�B	T�B	T�B	VB	XB	YB	[#B	]/B	^5B	bNB	hsB	q�B	x�B	x�B	y�B	{�B	|�B	|�B	{�B	{�B	~�B	� B	�B	�B	�7B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�?B	�?B	�?B	�LB	�^B	�dB	�dB	�dB	�jB	�jB	�jB	�qB	�}B	��B	B	B	ÖB	ÖB	ƨB	ɺB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�sB�ZB�TB�TB�ZB�HB�BB�5B�5B�#B�B�
B��B��B��B��BF�B0!B�BuB�B�B&�BB�BT�Bv�B�JB��B��BǮB�`B�B��B	7B�B?}B_;B}�B�\B�\B�JB�oB�PB�uB�uB��B��B��B��B��B�B�B�B�B�'B�9B�qB�wB��BƨB��B��BȴBȴBǮB�wB�}BBɺBƨB�qB�'B��B�Bm�BR�B=qB49B2-B�B%B�B�B�`BǮB�B��B��B�hBz�BdZBP�B1'BhB1B
��B
�B
�B
��B
�^B
�B
��B
��B
��B
�B
{�B
w�B
n�B
iyB
\)B
S�B
R�B
C�B
2-B
.B
�B
�B
{B
1B	��B	�B	�mB	�B	��B	��B	��B	��B	�LB	�B	��B	��B	�hB	�%B	q�B	bNB	XB	K�B	J�B	@�B	33B	%�B	$�B	#�B	�B	�B	�B	B��B��B�B�B�B�B�#B��B��B��BÖBƨBǮB�jB�^B�?B�FB��BŢB��B��B�}B�qB�dB�LB�LB�?B�B��B��B��B��B��B��B�oB�oB�oB�oB�oB�hB�oB�JB�DB�=B�+B�B�%B�B�B}�Bv�Bt�Bt�Br�Bp�Bo�Bl�Bk�Bk�Bo�Bo�Bo�Bn�Bl�Bl�BiyBdZBaHBbNB_;BS�BR�BJ�BJ�BK�BI�BG�BE�BE�BD�BA�B>wB<jB;dB:^B:^B8RB6FB6FB49B5?B5?B6FB6FB49B:^BD�B:^B0!B9XB@�BC�BC�BG�B;dB=qBA�BG�BJ�BK�BQ�BS�BO�BL�BL�BK�BK�BL�BO�BS�BS�BT�BP�BR�BM�BZB`BBbNBXBO�BZBVB\)B`BBbNBjBk�BbNB]/B[#BYBZBZBYBXBT�BT�BS�BVBXBYBZBZB[#B]/B^5BbNBdZBhsBk�Bk�Bk�Bl�Bm�Bn�Bq�Bt�Bw�B{�B�B�7B�=B�DB�DB�hB�{B��B��B��B��B��B��B�B�B�-B�B�!B�-B�RB�wB��B��BBŢBȴBɺB��B��B�
B�B�#B�/B�/B�;B�HB�HB�TB�ZB�ZB�ZB�`B�B�B��B��B��B�B��B��B��B��B	B	%B	1B	
=B	\B	uB	{B	{B	�B	�B	�B	�B	�B	�B	�B	!�B	 �B	 �B	"�B	$�B	&�B	&�B	(�B	+B	,B	0!B	2-B	5?B	8RB	<jB	=qB	?}B	@�B	@�B	@�B	A�B	B�B	B�B	C�B	I�B	K�B	M�B	O�B	R�B	T�B	T�B	VB	XB	YB	[#B	]/B	^5B	bNB	iyB	q�B	x�B	x�B	z�B	{�B	|�B	}�B	}�B	|�B	~�B	�B	�B	�B	�+B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�FB	�FB	�?B	�LB	�^B	�dB	�dB	�dB	�jB	�jB	�jB	�qB	�}B	��B	B	B	ĜB	ĜB	ƨB	ɺB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447082012010314470820120103144708  AO  ARGQ                                                                        20111130141403  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141403  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144708  IP                  G�O�G�O�G�O�                