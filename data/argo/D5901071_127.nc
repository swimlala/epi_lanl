CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:26Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130142047  20190522121827  1727_5046_127                   2C  D   APEX                            2143                            040306                          846 @Թ��@1   @Թͻ���@7���v��d1&�x�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�C�C33C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� DfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dh  Dh� Di  Di� Dj  Djy�Dk  Dk� Dk��Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dr� Ds  Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @y��@���@���AffA>ffA^ffA|��A�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG33BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC  C  C�C�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7��C9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCR  CS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C�  C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D� D��Dy�D��D� D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D  Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'� D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Dg  Dgy�Dg��Dhy�Dh��Diy�Di��Djs3Dj��Dky�Dk�3Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqs3Dq�3Dry�Dr��Dss3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A��yA���A��
A���A���A���A���A���A�bNA�`BA�^5A�^5A�\)A�\)A�ZA�ZA�XA�S�A�S�A�S�A�S�A�Q�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�M�A�O�A�O�A�O�A�M�A�M�A�M�A�K�A�I�A�I�A�E�A�E�A�C�A�A�A�A�A�A�A�A�A�;dA�=qA�;dA�;dA�7LA�33A�-A��A�A�E�A�dZA�G�A�r�A���A���A��A�dZA�33A��A���A���A�C�A�A���A�l�A�?}A�1'A���A�$�A��A�7LA��9A�hsA�bA�v�A�ĜA���A�l�A� �A��HA���A��-A�p�A�dZA�\)A�E�A�?}A�;dA�33A�33A�=qA�33A�&�A��A��A�A��A��;A��-A���A���A���A��7A�x�A�l�A�XA�A�ȴA���A�t�A�-A��^A�hsA�;dA�|�A��hA��A�+A���A�{A�hsA�/A�A�I�A���A���A���A��/A���A��mA��A���A�ƨA���A��A� �A�oA|I�Ay�Av(�At  Aqx�Am�^Al{Aj�Ai;dAg�
Afz�Ad��AbVA^r�AU��AM�#AL�RAJ��AI��AH�`AH�AG
=AEx�AB�yAB �AB$�ABbA@-A=�A:�`A9�A9l�A8�A7&�A6�A5��A3��A1oA/��A.��A.5?A-��A-;dA+��A)�A(E�A&�DA$��A#t�A"�jA!�A Q�AdZA�\AffAVA(�A��AoA�PAC�A�HAr�A��A�RA�^AG�A9XA��AG�AZAt�A
r�A	��A	&�A�/A-A�hA�+AK�A�DAffA�^A�RA�A/A �yA �RA j@���@���@�33@���@���@�M�@���@�@��/@��@��m@�@�@�E�@�{@�$�@�ff@�~�@�!@��@�7@��
@�;d@�!@�j@�ff@��@�5?@�ƨ@�+@�v�@���@؛�@��@�5?@���@�9X@�^5@щ7@�Z@ϕ�@θR@�$�@���@�K�@�^5@ɩ�@�p�@�&�@��m@��H@Ə\@�ff@ŉ7@�  @���@�5?@��@�x�@�7L@���@�bN@�1@���@��F@���@���@�E�@���@�G�@�%@���@�j@���@��P@�C�@�33@�33@�C�@�v�@�x�@�%@�z�@� �@�ƨ@�;d@���@��@���@��@��@�(�@�ƨ@���@��@�K�@�@��@��@�I�@�ƨ@�l�@�@���@�V@�J@��@�`B@�&�@���@��@���@�@���@�O�@���@��u@� �@���@���@��@�S�@��@�ff@��^@�X@���@�Q�@�\)@�V@�O�@���@�Ĝ@�j@�1@��@��@�K�@�
=@��@�ȴ@�~�@�E�@���@�x�@�hs@���@��u@��@�K�@���@��\@�-@�@��T@���@�@�x�@���@��9@�j@� �@��
@��P@���@�M�@�5?@�-@�$�@�$�@���@�x�@�O�@�?}@�%@�Ĝ@��D@�9X@��
@�|�@�\)@�C�@�+@�o@�ȴ@���@���@��\@�~�@�^5@�M�@��@���@��@�p�@�hs@�hs@�7L@�%@��`@��@�Z@� �@�b@���@��@��;@�t�@�;d@�o@�+@�+@���@��H@�ff@�5?@�{@��@��#@�7L@�9X@�S�@�S�@�l�@�t�@�;d@�~�@�V@�-@���@��@�A�@�A�@�j@��@��j@��u@�r�@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��yA���A��
A���A���A���A���A���A�bNA�`BA�^5A�^5A�\)A�\)A�ZA�ZA�XA�S�A�S�A�S�A�S�A�Q�A�O�A�O�A�O�A�M�A�M�A�M�A�M�A�M�A�O�A�M�A�O�A�M�A�M�A�M�A�O�A�O�A�O�A�O�A�M�A�O�A�O�A�O�A�M�A�M�A�M�A�K�A�I�A�I�A�E�A�E�A�C�A�A�A�A�A�A�A�A�A�;dA�=qA�;dA�;dA�7LA�33A�-A��A�A�E�A�dZA�G�A�r�A���A���A��A�dZA�33A��A���A���A�C�A�A���A�l�A�?}A�1'A���A�$�A��A�7LA��9A�hsA�bA�v�A�ĜA���A�l�A� �A��HA���A��-A�p�A�dZA�\)A�E�A�?}A�;dA�33A�33A�=qA�33A�&�A��A��A�A��A��;A��-A���A���A���A��7A�x�A�l�A�XA�A�ȴA���A�t�A�-A��^A�hsA�;dA�|�A��hA��A�+A���A�{A�hsA�/A�A�I�A���A���A���A��/A���A��mA��A���A�ƨA���A��A� �A�oA|I�Ay�Av(�At  Aqx�Am�^Al{Aj�Ai;dAg�
Afz�Ad��AbVA^r�AU��AM�#AL�RAJ��AI��AH�`AH�AG
=AEx�AB�yAB �AB$�ABbA@-A=�A:�`A9�A9l�A8�A7&�A6�A5��A3��A1oA/��A.��A.5?A-��A-;dA+��A)�A(E�A&�DA$��A#t�A"�jA!�A Q�AdZA�\AffAVA(�A��AoA�PAC�A�HAr�A��A�RA�^AG�A9XA��AG�AZAt�A
r�A	��A	&�A�/A-A�hA�+AK�A�DAffA�^A�RA�A/A �yA �RA j@���@���@�33@���@���@�M�@���@�@��/@��@��m@�@�@�E�@�{@�$�@�ff@�~�@�!@��@�7@��
@�;d@�!@�j@�ff@��@�5?@�ƨ@�+@�v�@���@؛�@��@�5?@���@�9X@�^5@щ7@�Z@ϕ�@θR@�$�@���@�K�@�^5@ɩ�@�p�@�&�@��m@��H@Ə\@�ff@ŉ7@�  @���@�5?@��@�x�@�7L@���@�bN@�1@���@��F@���@���@�E�@���@�G�@�%@���@�j@���@��P@�C�@�33@�33@�C�@�v�@�x�@�%@�z�@� �@�ƨ@�;d@���@��@���@��@��@�(�@�ƨ@���@��@�K�@�@��@��@�I�@�ƨ@�l�@�@���@�V@�J@��@�`B@�&�@���@��@���@�@���@�O�@���@��u@� �@���@���@��@�S�@��@�ff@��^@�X@���@�Q�@�\)@�V@�O�@���@�Ĝ@�j@�1@��@��@�K�@�
=@��@�ȴ@�~�@�E�@���@�x�@�hs@���@��u@��@�K�@���@��\@�-@�@��T@���@�@�x�@���@��9@�j@� �@��
@��P@���@�M�@�5?@�-@�$�@�$�@���@�x�@�O�@�?}@�%@�Ĝ@��D@�9X@��
@�|�@�\)@�C�@�+@�o@�ȴ@���@���@��\@�~�@�^5@�M�@��@���@��@�p�@�hs@�hs@�7L@�%@��`@��@�Z@� �@�b@���@��@��;@�t�@�;d@�o@�+@�+@���@��H@�ff@�5?@�{@��@��#@�7L@�9X@�S�@�S�@�l�@�t�@�;d@�~�@�V@�-@���@��@�A�@�A�@�j@��@��j@��u@�r�@�j111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB]/B\)B\)B\)B\)B[#B[#BZBZBZBZBYBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBYBXBYBYBXBYBXBXBXBYBYBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBYBYB[#B�DBÖBǮB��B��B��B��B��B��B��B��BȴBǮBƨBĜBB��B�qB�^B�RB�FB�?B�-B�B�B�B�B�B�3B�dB�qB��BŢBƨBɺB��B��B��B�#B�BB�`B�yB�B�B�B�B��B��B��B��B��B��B  B  BBB��B��B��B�B�B�ZB�)B��BB�B��B�7Be`BC�B �B��B�)B��B�^B��B�oB�B_;B(�BB
�HB
��B
�B
�PB
`BB
B�B
&�B
bB	��B	�mB	�B	ĜB	�^B	�3B	�B	��B	��B	�JB	w�B	S�B	�B	B�B�fB�BB�)B�B�B�B��B��B��B�B�yB�/B��B��B��B��BɺBǮBÖB�dB�9B�!B�B�B��B��B��B��B��B�\B�7B�B�B}�B|�Bz�Bw�Bv�Bu�Bs�Bp�Bk�Bl�Bk�Bk�BiyBgmBe`BcTBaHB^5B[#BYBXBW
BVBT�BS�BR�BQ�BO�BM�BM�BM�BL�BJ�BI�BI�BH�BH�BG�BE�BD�BD�BD�BE�BE�BG�BF�BF�BF�BH�BG�BH�BL�BO�BP�BQ�BS�BT�BVBVBVBXBYBW
BT�BR�BR�BM�BN�BO�BO�BO�BO�BP�BR�BT�BXBZBXBVBVBXBXBYBVB[#B_;B`BB`BBcTBgmBgmBgmBjBs�B{�B� B� B�B�B�B�+B�=B�JB�VB�\B�oB��B��B��B��B��B��B��B��B��B��B��B��B�B�3B�?B�RB�XB�^B�dBBÖBȴB��B�B�5B�HB�HB�NB�NB�HB�NB�`B�yB�B�B�B�B��B��B��B	  B��B		7B	JB	JB	JB	JB	VB	bB	oB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	,B	/B	49B	5?B	5?B	7LB	;dB	<jB	=qB	?}B	B�B	B�B	D�B	E�B	F�B	G�B	I�B	I�B	J�B	L�B	N�B	Q�B	W
B	YB	YB	[#B	\)B	]/B	]/B	_;B	bNB	cTB	e`B	ffB	hsB	k�B	p�B	t�B	u�B	u�B	u�B	u�B	w�B	|�B	}�B	}�B	�B	�B	�B	�1B	�DB	�PB	�PB	�PB	�VB	�\B	�bB	�hB	�oB	�oB	�oB	�uB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�'B	�'B	�3B	�-B	�'B	�-B	�?B	�FB	�XB	�qB	��B	ÖB	ǮB	ɺ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B]/B\)B\)B\)B\)B[#B[#B[#BZBZBZBZBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBYBXBYBYBXBYBXBXBXBYBYBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBXBZB\)B_;B��BƨBɺB��B��B��B��B��B��B��B��BɺBɺBǮBŢBÖBÖB��B�wB�^B�XB�LB�9B�3B�-B�B�B�B�9B�jB�wBBŢBƨB��B��B��B��B�#B�BB�`B�yB�B�B�B��B��B��B��B��B��B��B  B  BBB��B��B��B��B�B�fB�/B�BǮB�3B��B�\Bk�BJ�B)�B��B�;B��B�wB��B��B�+Bl�B33BPB
�mB
�B
�FB
��B
k�B
M�B
/B
�B	��B	�B	�NB	ɺB	�qB	�FB	�B	��B	��B	�bB	~�B	e`B	'�B	B��B�yB�NB�5B�)B�5B��B��B��B��B��B�B�TB��B��B��B��B��BɺB��BÖB�RB�-B�!B�B�B�B��B��B��B�{B�VB�+B�B�B� B}�B~�B{�By�By�Bu�Bp�Bm�Bl�Bl�Bk�BjBhsBdZBdZBaHB_;B\)B[#BZBXBW
BT�BT�BS�BR�BQ�BO�BN�BN�BM�BL�BK�BI�BI�BH�BI�BH�BG�BG�BI�BH�BJ�BJ�BI�BH�BI�BK�BM�BN�BP�BP�BQ�BS�BT�BVBXBYBYBZB[#BYBVBXBQ�BO�BP�BP�BQ�BQ�BQ�BT�BVB[#B[#BZBW
BW
BYBZB\)BXB\)B`BBaHBbNBe`BhsBhsBgmBm�Bu�B|�B� B�B�B�B�B�1B�DB�JB�VB�hB�{B��B��B��B��B��B��B��B��B��B��B��B��B�B�9B�FB�XB�^B�dB�wBBĜBȴB��B�#B�;B�HB�HB�TB�TB�TB�ZB�mB�B�B�B�B��B��B��B��B	B	B	
=B	JB	JB	JB	PB	\B	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	'�B	.B	1'B	5?B	5?B	6FB	8RB	;dB	=qB	>wB	@�B	B�B	C�B	E�B	F�B	G�B	H�B	I�B	J�B	K�B	M�B	O�B	R�B	W
B	ZB	YB	[#B	\)B	]/B	^5B	`BB	bNB	dZB	ffB	gmB	iyB	l�B	q�B	t�B	u�B	u�B	u�B	u�B	x�B	|�B	}�B	}�B	�B	�B	�%B	�7B	�JB	�PB	�PB	�PB	�VB	�bB	�bB	�hB	�oB	�oB	�oB	�uB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�'B	�3B	�3B	�3B	�3B	�?B	�FB	�XB	�qB	��B	ÖB	ǮB	ɺ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
<#�
<#�
<#�
<#�
<�o<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447182012010314471820120103144718  AO  ARGQ                                                                        20111130142047  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142047  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144718  IP                  G�O�G�O�G�O�                