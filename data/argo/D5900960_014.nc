CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:26:49Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  AH   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C<   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  T�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  fT   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  hH   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  p   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  w�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  y�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �`   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �0   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �4   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �8   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �<Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130103309  20190523124443  1514_5041_014                   2C  D   APEX                            2041                            062805                          846 @�؋�Q�1   @�؋�Q�@6�C��%�c(bM��1   GPS     Primary sampling: mixed [deeper than nominal 1000dbar: discrete; nominal 1000dbar to surface: 2dbar-bin averaged]                                                                                                                                                  A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@���@���AffA>ffA^ffA~ffA�33A�33A�33A�33A�33A�33A�33A�33B��B��B��B��B'��B/��B7��B?��BG��BO��BW��B_��Bg��Bo��Bw��B��B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C�fC�fC�fC�fC	�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC�fC!�fC#�fC%�fC'�fC)�fC+�fC-�fC/�fC1�fC3�fC5�fC7�fC9�fC;�fC=�fC?�fCA�fCC�fCE�fCG�fCI�fCK�fCM�fCO�fCQ�fCS�fCU�fCW�fCY�fC[�fC]�fC_�fCa�fCc�fCe�fCg�fCi�fCk�fCm�fCo�fCq�fCs�fCu�fCw�fCy�fC{�fC}�fC�fC��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3C��3D y�D ��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D	y�D	��D
y�D
��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��Dy�D��D y�D ��D!y�D!��D"y�D"��D#y�D#��D$y�D$��D%y�D%��D&y�D&��D'y�D'��D(y�D(��D)y�D)��D*y�D*��D+y�D+��D,y�D,��D-y�D-��D.y�D.��D/y�D/��D0y�D0��D1y�D1��D2y�D2��D3y�D3��D4y�D4��D5y�D5��D6y�D6��D7y�D7��D8y�D8��D9y�D9��D:y�D:��D;y�D;��D<y�D<��D=y�D=��D>y�D>��D?y�D?��D@y�D@��DAy�DA��DBy�DB��DCy�DC��DDy�DD��DEy�DE��DFy�DF��DGy�DG��DHy�DH��DIy�DI��DJy�DJ��DKy�DK��DLy�DL��DMy�DM��DNy�DN��DOy�DO��DPy�DP��DQy�DQ��DRy�DR��DSy�DS��DTy�DT��DUy�DU��DVy�DV��DWy�DW��DXy�DX��DYy�DY��DZy�DZ��D[y�D[��D\y�D\��D]y�D]��D^y�D^��D_y�D_��D`y�D`��Day�Da��Dby�Db��Dcy�Dc��Ddy�Dd��Dey�De��Dfy�Df��Dgy�Dg��Dhy�Dh��Diy�Di��Djy�Dj��Dky�Dk��Dly�Dl��Dmy�Dm��Dny�Dn��Doy�Do��Dpy�Dp��Dqy�Dq��Dry�Dr��Dsy�Ds��Dty�Dt��Duy�Du��Dvy�Dv��Dwy�Dw��Dxy�Dx��Dyy�Dy��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�|�A�7LA�VA��A���AμjAΧ�AΑhA�x�A�n�A�^5A�7LA�5?A�K�A�E�A�1'A� �A��A��A��A�
=Aʹ9A�$�A��TA�1A�1'A�^5A�\)A���A�A�A���A���A��A��hA�\)A��yA�
=A��A��;A�$�A��HA�S�A���A��A��TA��!A��A�VA���A��\A��A�l�A�"�A�XA��HA��RA��PA�r�A�7LA�M�A��7A�jA�1'A�;dA��uA��A���A��A���A���A���A�~�A�=qA�{A�7LA�ĜA�;dA�&�A��A��A���A���A�K�A�XA�dZA��A�G�A�9XA�ffA�S�A�-A�"�A�?}A�  A�$�A��FA�-A��A�&�A�A���A�p�A���A�jA��!A�z�A���A��#A��A~^5A}��A{7LAx5?Av=qAtbAst�As�^At�As�Aql�Aq�ApA�Ao�AoAn=qAm�mAm;dAl��Aj�yAi�Ag��Ae�Adz�Ac�PAb�!Aa��AaoA_��A^�\A]�-A]A\I�A[p�AZ��AX�`AWK�AU��AT�HAS�AR��AQt�AP�RAOC�AMƨAL�+AKO�AJ�HAJ1AHȴAH  AFĜAE\)AD��ADr�AD9XACƨAB��AA��A@��A?�7A>�\A=A<I�A;�A:ȴA9%A8�A6jA5ƨA5G�A4v�A3\)A2~�A1&�A/��A.�`A.��A-A,9XA*��A)hsA($�A'p�A'%A&9XA$�A#�-A"bNA ��A^5A��At�A33A1'A�DAG�A~�A��A;dAjA��A`BA�A�PA�HAVA�-Al�AffA�^A��A�A��A��A
�+A	�A	+A�AG�AQ�A+A9XAS�AVA ĜA �@�C�@��#@�bN@�|�@��T@��D@��@�=q@��@�n�@�`B@��;@�=q@��@���@��@�hs@���@��@�^@�@�@��@ᙚ@�Z@ߍP@ޏ\@��@�"�@�^5@؃@�hs@�I�@�
=@���@�t�@���@�%@��@�@���@��@���@�o@š�@�{@�`B@��j@�(�@��H@���@��/@��@��@�$�@�hs@�ƨ@���@��@�Z@�@���@���@��\@�@�%@���@�bN@���@���@���@��@��@��w@��@�5?@���@�x�@���@���@�\)@�M�@���@�7L@��9@�(�@��@��P@�+@�n�@���@���@�bN@�I�@�(�@�b@�ƨ@�S�@���@��R@���@���@�Q�@�1@���@�dZ@��@��R@�=q@��-@�V@��D@�(�@��m@��P@�S�@�\)@�l�@��m@���@��P@�o@�5?@�1@�n�@�@��T@�{@�M�@�ff@�n�@�J@���@�{@��y@�\)@�r�@�$�@�~�@�v�@��@�o@���@�V@���@��;@�|�@��@�  @�Q�@�bN@�I�@�(�@�9X@���@���@��@�G�@�p�@��@��h@��^@��-@��-@��T@��T@��#@���@���@���@�hs@�X@�G�@��@��j@�j@��@���@��!@���@�ff@�ff@��@��^@�p�@��@�r�@�Z@�bN@�j@�r�@��@���@�  @� �@�  @��@�33@��@��y@��+@�E�@�-@��@���@�x�@�`B@�G�@�7L@�%@��/@��9@���@��@�Q�@�b@��
@��w@��@�l�@�;d@��@��R@�~�@�-@��@��h@��@��/@�Ĝ@�Z@��@�;d@�"�@�@��@��@���@�~�@�$�@���@���@��h@�`B@�7L@��@�V@���@���@��@��@�A�@���@�ƨ@�l�@�
=@��R@��+@�ff@�E�@�5?@�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�|�A�7LA�VA��A���AμjAΧ�AΑhA�x�A�n�A�^5A�7LA�5?A�K�A�E�A�1'A� �A��A��A��A�
=Aʹ9A�$�A��TA�1A�1'A�^5A�\)A���A�A�A���A���A��A��hA�\)A��yA�
=A��A��;A�$�A��HA�S�A���A��A��TA��!A��A�VA���A��\A��A�l�A�"�A�XA��HA��RA��PA�r�A�7LA�M�A��7A�jA�1'A�;dA��uA��A���A��A���A���A���A�~�A�=qA�{A�7LA�ĜA�;dA�&�A��A��A���A���A�K�A�XA�dZA��A�G�A�9XA�ffA�S�A�-A�"�A�?}A�  A�$�A��FA�-A��A�&�A�A���A�p�A���A�jA��!A�z�A���A��#A��A~^5A}��A{7LAx5?Av=qAtbAst�As�^At�As�Aql�Aq�ApA�Ao�AoAn=qAm�mAm;dAl��Aj�yAi�Ag��Ae�Adz�Ac�PAb�!Aa��AaoA_��A^�\A]�-A]A\I�A[p�AZ��AX�`AWK�AU��AT�HAS�AR��AQt�AP�RAOC�AMƨAL�+AKO�AJ�HAJ1AHȴAH  AFĜAE\)AD��ADr�AD9XACƨAB��AA��A@��A?�7A>�\A=A<I�A;�A:ȴA9%A8�A6jA5ƨA5G�A4v�A3\)A2~�A1&�A/��A.�`A.��A-A,9XA*��A)hsA($�A'p�A'%A&9XA$�A#�-A"bNA ��A^5A��At�A33A1'A�DAG�A~�A��A;dAjA��A`BA�A�PA�HAVA�-Al�AffA�^A��A�A��A��A
�+A	�A	+A�AG�AQ�A+A9XAS�AVA ĜA �@�C�@��#@�bN@�|�@��T@��D@��@�=q@��@�n�@�`B@��;@�=q@��@���@��@�hs@���@��@�^@�@�@��@ᙚ@�Z@ߍP@ޏ\@��@�"�@�^5@؃@�hs@�I�@�
=@���@�t�@���@�%@��@�@���@��@���@�o@š�@�{@�`B@��j@�(�@��H@���@��/@��@��@�$�@�hs@�ƨ@���@��@�Z@�@���@���@��\@�@�%@���@�bN@���@���@���@��@��@��w@��@�5?@���@�x�@���@���@�\)@�M�@���@�7L@��9@�(�@��@��P@�+@�n�@���@���@�bN@�I�@�(�@�b@�ƨ@�S�@���@��R@���@���@�Q�@�1@���@�dZ@��@��R@�=q@��-@�V@��D@�(�@��m@��P@�S�@�\)@�l�@��m@���@��P@�o@�5?@�1@�n�@�@��T@�{@�M�@�ff@�n�@�J@���@�{@��y@�\)@�r�@�$�@�~�@�v�@��@�o@���@�V@���@��;@�|�@��@�  @�Q�@�bN@�I�@�(�@�9X@���@���@��@�G�@�p�@��@��h@��^@��-@��-@��T@��T@��#@���@���@���@�hs@�X@�G�@��@��j@�j@��@���@��!@���@�ff@�ff@��@��^@�p�@��@�r�@�Z@�bN@�j@�r�@��@���@�  @� �@�  @��@�33@��@��y@��+@�E�@�-@��@���@�x�@�`B@�G�@�7L@�%@��/@��9@���@��@�Q�@�b@��
@��w@��@�l�@�;d@��@��R@�~�@�-@��@��h@��@��/@�Ĝ@�Z@��@�;d@�"�@�@��@��@���@�~�@�$�@���@���@��h@�`B@�7L@��@�V@���@���@��@��@�A�@���@�ƨ@�l�@�
=@��R@��+@�ff@�E�@�5?@�{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�#B�B�B�B�B��B��B��BBBB��B��B�B�B�/B�B�)B�sB�sB�B�B�B�B�B��B��B��B��B��B��B��B��B�B�B�B�B�fB�TB�BB�#B�B�
B��B��B��BɺBÖBB�}B�RB�3B�B��B�hB�+B{�Bt�Bo�Bl�BhsBYBH�BA�B@�B<jB6FB6FB5?B1'B%�B�BPBB��B�sB�)BƨB�RB��B�PBz�B_;B=qB�B
��B
ǮB
��B
{�B
m�B
VB
K�B
@�B
6FB
'�B
�B
VB
  B	�B	�NB	�/B	�#B	�NB	�B	��B	��B	�B	�B	�B	��B
B
  B
B	��B	��B	�B	�5B	��B	ŢB	�jB	�9B	�B	��B	��B	��B	��B	��B	��B	�VB	�7B	�B	�B	t�B	o�B	hsB	e`B	^5B	VB	P�B	I�B	C�B	B�B	9XB	8RB	2-B	,B	-B	%�B	 �B	�B	�B	�B	�B	�B	oB	\B	PB		7B	+B��B��B��B�B�sB�TB�HB�HB�5B�B��B��BƨB��B�}B�dB�XB�9B�B��B��B��B��B��B�\B�=B�B}�Bx�Bw�Bu�Bz�Bq�Bp�Bq�Bp�Bs�BjBr�Bm�BjBe`BgmBdZB_;B^5B]/BZBXBS�BO�BR�BJ�BI�BI�BE�BB�BA�B>wB<jB:^B7LB6FB5?B49B2-B0!B/B.B,B+B+B)�B'�B&�B%�B%�B$�B#�B"�B"�B!�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{BoBoBuB{B�B�B{BuB{B�BoB�B�B�B#�B$�B#�B$�B$�B$�B#�B"�B$�B'�B-B2-B1'B5?B;dB<jB>wBB�BE�BF�BF�BG�BK�BO�BS�BT�BXBYBZBZB[#BaHB`BBffBhsBiyBm�Bm�Bn�Bp�Bo�Bt�By�By�B{�B{�B}�B~�B�B�B�%B�+B�7B�VB�\B�hB��B��B��B��B��B�!B�?B�LB�RB�XB�dB�wB��BBȴB�;B�sB�mB�ZB�fB�ZB�`B�B�B��B��B��B	  B	B	1B	VB	�B	"�B	0!B	49B	6FB	:^B	<jB	@�B	B�B	H�B	F�B	G�B	H�B	J�B	Q�B	W
B	\)B	^5B	`BB	dZB	hsB	l�B	p�B	s�B	v�B	z�B	}�B	� B	�B	�7B	�DB	�JB	�PB	�PB	�\B	�hB	�hB	�oB	�{B	��B	��B	��B	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�3B	�9B	�FB	�LB	�LB	�XB	�dB	�jB	��B	��B	��B	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�;B	�HB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�`B	�fB	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�)B�B�B�B�B��B��B��BB1B%B%B��B�B�B�HB�B�HB�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�yB�`B�ZB�/B�B�B�B��B�B��BĜBÖBÖB�dB�FB�'B��B�{B�DB~�Bv�Bp�Bl�Bl�B_;BJ�BB�BA�B@�B8RB6FB6FB6FB)�B�BbB+B��B�B�NB��B�qB��B�hB�Be`BD�B �BB
��B
��B
}�B
r�B
YB
O�B
D�B
9XB
+B
�B
bB
B	�B	�mB	�NB	�/B	�HB	�B
B	��B	�B	�B	�B	��B
B
B
B	��B	��B	�B	�NB	�
B	ɺB	�}B	�FB	�!B	��B	��B	��B	��B	��B	��B	�bB	�DB	�1B	�+B	w�B	q�B	jB	gmB	aHB	XB	S�B	M�B	F�B	E�B	:^B	:^B	5?B	.B	0!B	(�B	!�B	�B	�B	�B	�B	�B	�B	oB	\B	DB	
=B��B��B��B�B�B�`B�TB�TB�HB�#B�B��BȴBB��B�}B�qB�RB�!B��B��B��B��B��B�oB�VB�=B� By�Bw�Bw�B~�Bt�Br�Bs�Bq�Bv�Bl�Br�Bp�Bl�BffBhsBe`B`BB`BB^5B\)BYBW
BQ�BVBL�BK�BL�BG�BD�BD�B@�B>wB=qB;dB8RB6FB6FB49B2-B1'B0!B/B-B.B,B)�B(�B(�B(�B&�B&�B$�B#�B#�B#�B!�B �B�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�B�B�B�B�B�B�B�B �B$�B%�B$�B&�B&�B%�B$�B$�B&�B(�B.B5?B2-B6FB<jB=qB?}BB�BE�BG�BH�BI�BL�BP�BT�BVBYBZB[#B[#B]/BbNBbNBgmBiyBjBn�Bn�Bo�Bq�Bp�Bv�Bz�Bz�B{�B|�B~�B� B�B�%B�+B�1B�DB�\B�\B�oB��B��B��B��B��B�'B�FB�RB�RB�^B�jB�wB��BBŢB�5B�yB�sB�mB�yB�`B�`B�B�B��B��B��B	B	B	+B	PB	uB	�B	0!B	49B	5?B	:^B	=qB	@�B	C�B	K�B	G�B	G�B	H�B	J�B	Q�B	XB	]/B	^5B	`BB	dZB	hsB	l�B	p�B	s�B	v�B	z�B	}�B	� B	�B	�7B	�JB	�JB	�PB	�VB	�\B	�hB	�oB	�oB	�{B	��B	��B	��B	�{B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�3B	�?B	�FB	�LB	�LB	�XB	�dB	�qB	��B	��B	B	ĜB	ƨB	ƨB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�BB	�HB	�NB	�NB	�NB	�NB	�TB	�TB	�ZB	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<T��<D��<49X<49X<49X<49X<D��<49X<#�
<49X<#�
<49X<D��<49X<49X<#�
<49X<49X<49X<#�
<#�
<#�
<#�
<49X<49X<49X<49X<#�
<49X<49X<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<49X<49X<49X<49X<49X<49X<49X<49X<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<49X<#�
<#�
<#�
<49X<49X<49X<49X<D��<49X<D��<D��<D��<49X<D��<49X<D��<D��<D��<49X<D��<T��<49X<#�
<49X<#�
<49X<49X<#�
<49X<49X<#�
<49X<D��<49X<49X<#�
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
<49X<49X<49X<49X<49X<#�
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
<49X<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<49X<49X<49X<#�
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
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.1 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181657252011121816572520111218165725  AO  ARGQ                                                                        20111130103309  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130103309  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165725  IP                  G�O�G�O�G�O�                