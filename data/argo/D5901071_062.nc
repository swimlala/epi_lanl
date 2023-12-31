CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:08Z UW 3.1 conversion   
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
_FillValue                 �  A0   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �x   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               >A   AO  20111130140522  20190522121826  1727_5046_062                   2C  D   APEX                            2143                            040306                          846 @�eż�	1   @�e�b�� @7F$�/��c�G�z�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dry�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D/��D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D7��D8� D9fD9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DW��DX� DY  DY� DZ  DZ� DZ��D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df�fDg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dq��Dry�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�t�A�t�A�t�A�v�A�v�A�r�A�t�A�x�A�~�A�x�A�hsA�M�A�{A���A�bNA��\A���A��/A�hsA�VA�  A��
A���A���A�5?A�{A���A���A�r�A�?}A�"�A��A��HA��jA���A��PA�|�A�n�A�~�A�t�A�hsA�bNA�XA�O�A�E�A�9XA�(�A��A��A��A��A��A��/A��A�\)A�G�A�=qA��A�A��A�ffA�A�A��`A�x�A�7LA�VA��HA�ffA��/A��+A���A��A�$�A���A�=qA�?}A��jA�K�A��^A�5?A��A�A�n�A��-A��uA��\A��!A��DA�"�A�I�A��+A�I�A�
=A�hsA�&�A��A�A���A�jA��wA��RA�ȴA���A���A�9XA���A�p�A���A�A�A�S�A��A�n�A�"�A��9A�A���A���A�G�A��#A��A���A��mA�ƨA�ȴA��^A�(�A��A�S�A~(�A{��Ax��Ax$�AwC�Av��Au�wAtJAp  An�`Al��Ak�AiXAg��Af5?Aa"�A]�A\��A\�uA\n�A[�A[
=AZjAXffAW"�AV�!AVQ�AV  AU�;AUdZAT�!ATr�AT1AS�PAS\)AP�ANZAN1AL�jAJ�RAJ-AIƨAIAF�/AD�AC�mAB1A?�A>v�A> �A=&�A=
=A<ȴA<�!A;�A:�A:�A9�hA8�`A8$�A7�A6�DA5�A4Q�A3��A3�A3;dA2�HA2{A0�A-oA*�!A)�
A)��A)�PA(��A(1A't�A&{A%�wA%/A$�A$ZA$A�A#XA" �A ��A��A�A9XA��A�AC�A��AZAdZA1AZA�AO�A�AM�A��A�A��A-A�mA|�A&�A
�!A	�mA	G�A��AC�A�AO�AQ�AC�A��@�33@��7@�\)@�"�@�$�@���@��@�V@�w@�"�@�\@���@�5?@�A�@��@⟾@��@���@ᙚ@�r�@�^5@�;d@��@�^5@��#@�J@�x�@�ƨ@�o@ҏ\@Ѻ^@�V@�dZ@��@�%@�r�@�  @��@��
@�V@�{@���@ģ�@�;d@���@�@��@�@��@�j@�dZ@���@�^5@��-@�Q�@�dZ@��!@�v�@�$�@���@��P@�o@�-@���@��@��@��w@�S�@���@�V@���@�X@���@��@�bN@�O�@�p�@���@���@��w@�l�@���@���@��@�?}@�A�@���@���@�v�@�C�@���@�7L@�v�@��-@��9@��h@�O�@�$�@��H@���@�K�@��^@���@�E�@��#@���@��u@��P@���@�1'@�1@�|�@�ff@��h@� �@�33@�ff@�@���@��@���@���@���@�/@��j@�1'@�dZ@��@��\@���@��@�X@���@���@�V@�X@�X@��@��`@���@��u@��@�ƨ@��@��@���@�ff@��-@�`B@��/@�Z@���@�o@�~�@�n�@�p�@��@�b@��m@��m@��m@��m@��m@��m@��;@��;@��F@�+@��y@�n�@�ȴ@���@���@�~�@�ff@�M�@��@�O�@��@��@��/@���@��D@�G�@�hs@��7@�X@�G�@��j@�|�@�=q@�5?@�n�@��T@���@���@��@�x�@�`B@�O�@�7L@�%@��/@��u@�j@�j@�Z@�A�@�9X@�1'@��@��
@���@��P@��P@��@�|�@�|�@�t�@�K�@�+@�o@��@��y@��H@�ȴ@�ff@�{@��T@��^@���@���@���@���@�/@�V@��`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�t�A�t�A�t�A�v�A�v�A�r�A�t�A�x�A�~�A�x�A�hsA�M�A�{A���A�bNA��\A���A��/A�hsA�VA�  A��
A���A���A�5?A�{A���A���A�r�A�?}A�"�A��A��HA��jA���A��PA�|�A�n�A�~�A�t�A�hsA�bNA�XA�O�A�E�A�9XA�(�A��A��A��A��A��A��/A��A�\)A�G�A�=qA��A�A��A�ffA�A�A��`A�x�A�7LA�VA��HA�ffA��/A��+A���A��A�$�A���A�=qA�?}A��jA�K�A��^A�5?A��A�A�n�A��-A��uA��\A��!A��DA�"�A�I�A��+A�I�A�
=A�hsA�&�A��A�A���A�jA��wA��RA�ȴA���A���A�9XA���A�p�A���A�A�A�S�A��A�n�A�"�A��9A�A���A���A�G�A��#A��A���A��mA�ƨA�ȴA��^A�(�A��A�S�A~(�A{��Ax��Ax$�AwC�Av��Au�wAtJAp  An�`Al��Ak�AiXAg��Af5?Aa"�A]�A\��A\�uA\n�A[�A[
=AZjAXffAW"�AV�!AVQ�AV  AU�;AUdZAT�!ATr�AT1AS�PAS\)AP�ANZAN1AL�jAJ�RAJ-AIƨAIAF�/AD�AC�mAB1A?�A>v�A> �A=&�A=
=A<ȴA<�!A;�A:�A:�A9�hA8�`A8$�A7�A6�DA5�A4Q�A3��A3�A3;dA2�HA2{A0�A-oA*�!A)�
A)��A)�PA(��A(1A't�A&{A%�wA%/A$�A$ZA$A�A#XA" �A ��A��A�A9XA��A�AC�A��AZAdZA1AZA�AO�A�AM�A��A�A��A-A�mA|�A&�A
�!A	�mA	G�A��AC�A�AO�AQ�AC�A��@�33@��7@�\)@�"�@�$�@���@��@�V@�w@�"�@�\@���@�5?@�A�@��@⟾@��@���@ᙚ@�r�@�^5@�;d@��@�^5@��#@�J@�x�@�ƨ@�o@ҏ\@Ѻ^@�V@�dZ@��@�%@�r�@�  @��@��
@�V@�{@���@ģ�@�;d@���@�@��@�@��@�j@�dZ@���@�^5@��-@�Q�@�dZ@��!@�v�@�$�@���@��P@�o@�-@���@��@��@��w@�S�@���@�V@���@�X@���@��@�bN@�O�@�p�@���@���@��w@�l�@���@���@��@�?}@�A�@���@���@�v�@�C�@���@�7L@�v�@��-@��9@��h@�O�@�$�@��H@���@�K�@��^@���@�E�@��#@���@��u@��P@���@�1'@�1@�|�@�ff@��h@� �@�33@�ff@�@���@��@���@���@���@�/@��j@�1'@�dZ@��@��\@���@��@�X@���@���@�V@�X@�X@��@��`@���@��u@��@�ƨ@��@��@���@�ff@��-@�`B@��/@�Z@���@�o@�~�@�n�@�p�@��@�b@��m@��m@��m@��m@��m@��m@��;@��;@��F@�+@��y@�n�@�ȴ@���@���@�~�@�ff@�M�@��@�O�@��@��@��/@���@��D@�G�@�hs@��7@�X@�G�@��j@�|�@�=q@�5?@�n�@��T@���@���@��@�x�@�`B@�O�@�7L@�%@��/@��u@�j@�j@�Z@�A�@�9X@�1'@��@��
@���@��P@��P@��@�|�@�|�@�t�@�K�@�+@�o@��@��y@��H@�ȴ@�ff@�{@��T@��^@���@���@���@���@�/@�V@��`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB{�B}�B� B�B�B�B�%B�1B�JB�\B�oB�uB��B��B��B��B�FBĜB�RB�?B�dB�wB�B'�BR�Bn�B�+B�hB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B�B�B�B�B�3B�?B�dB�}BBÖBÖBǮBɺB��B��B��B��B��B��B��B��BƨBŢBB��BÖB��B�qB�qB�dB�^B�FB�LB�B��B��B��B��B��B�\B�B{�Bw�Bm�BaHBF�B6FB1'B,B)�B'�B%�B �B�B%B��B�B�5B��B��B�^B�B��B��B|�BjBe`B]/BP�B8RBuB
�B
�B
��B
�wB
�B
��B
�B
t�B
jB
e`B
ZB
H�B
9XB
'�B
#�B
�B
�B
oB
%B	�B	�B	�HB	�#B	��B	ŢB	�dB	�B	��B	�uB	�{B	�{B	�{B	�oB	�=B	� B	z�B	y�B	x�B	w�B	w�B	x�B	w�B	u�B	y�B	|�B	v�B	YB	L�B	O�B	J�B	?}B	=qB	:^B	49B	(�B	�B	 �B	�B	+B	B	JB	\B	oB	hB	\B	bB	\B	JB		7B	%B	B	  B��B�B�B�B�B�B�yB�NB��B�FB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�JB�%B|�Bz�B|�B�B�B�B�B�B�B�B�B�%B�B�+B�B�B|�By�Bu�Bn�B`BB\)BVBK�BI�BO�BE�B<jB;dB<jB;dB:^B;dB;dB;dB;dB<jBA�BC�BD�B;dB6FB5?B5?B5?B9XB7LB49B2-B1'B1'B/B/B0!B1'B2-B2-B33B6FB8RB7LB6FB9XB<jB>wBD�BD�BG�BH�BL�BN�BQ�BQ�BT�BYB\)B]/B]/B]/B_;BcTBdZBffBgmBiyBn�Bp�Bq�Br�Bs�Bt�Bv�By�B{�B�JB��B��B�uB�bB�hB�oB��B��B��B��B��B�B�B�?B�}B��B��B�/B�#B�B�fB��B	B	DB	�B	�B	{B	uB	bB	oB	{B	uB	oB	�B	"�B	"�B	#�B	$�B	"�B	!�B	%�B	&�B	(�B	+B	.B	0!B	49B	5?B	7LB	;dB	E�B	J�B	M�B	L�B	M�B	L�B	L�B	K�B	N�B	T�B	YB	`BB	aHB	hsB	iyB	jB	m�B	q�B	t�B	u�B	u�B	u�B	v�B	v�B	v�B	w�B	w�B	w�B	y�B	x�B	w�B	y�B	}�B	� B	� B	� B	� B	� B	� B	� B	� B	� B	�B	�+B	�PB	�bB	�hB	�oB	�oB	��B	��B	�uB	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�?B	�LB	�^B	�dB	�dB	�dB	�jB	�qB	�qB	�wB	��B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B{�B}�B� B�B�B�B�%B�1B�JB�\B�oB�{B��B��B��B��B�LBƨB�^B�?B�jB�}B�yB&�BS�Bo�B�1B�uB��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B��B��B�B�B�B�B�3B�FB�jB��BBÖBŢBȴB��B��B��B��B��B��B��B��B��BɺBǮBŢBŢBǮBB��B��B�qB�dB�dB�jB�B��B��B��B��B��B�uB�7B}�B{�Bq�Bl�BM�B9XB2-B-B+B(�B&�B$�B�BJB  B�B�NB�B��B�wB�'B�B��B�Bl�BgmB`BBW
BB�B�B
��B
�)B
��B
ÖB
�9B
��B
�+B
w�B
k�B
iyB
aHB
O�B
A�B
)�B
&�B
�B
�B
�B
hB	��B	�B	�fB	�;B	��B	ȴB	ŢB	�9B	��B	�{B	�{B	��B	��B	�{B	�\B	�B	{�B	z�B	y�B	w�B	x�B	z�B	x�B	v�B	z�B	}�B	}�B	_;B	M�B	R�B	O�B	A�B	>wB	<jB	:^B	.B	!�B	%�B	�B	
=B	%B	\B	\B	uB	oB	oB	uB	hB	\B	DB		7B	B	B��B��B�B�B�B�B�B�sB�/B�wB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�1B�B� B�B�B�B�%B�B�B�B�B�+B�1B�+B�DB�7B�B� B}�Bw�Bx�BcTB`BB\)BM�BK�BVBK�B>wB<jB=qB=qB@�B>wB<jB=qB<jB<jBB�BE�BL�B?}B7LB6FB6FB5?B:^B9XB5?B33B2-B2-B2-B1'B2-B2-B33B5?B6FB8RB9XB7LB8RB<jB?}BB�BH�BG�BG�BK�BN�BP�BR�BR�BXB[#B]/B^5B^5B`BBaHBdZBffBgmBhsBk�Bo�Bp�Br�Bs�Bt�Bu�Bw�Bz�B|�B�DB��B��B��B�hB�oB�uB��B��B��B��B��B�B�B�9B�qB��B��B�5B�/B�B�BB��B	B	
=B	�B	�B	�B	�B	bB	uB	�B	�B	uB	uB	"�B	#�B	%�B	&�B	%�B	#�B	'�B	'�B	(�B	+B	.B	0!B	49B	6FB	8RB	<jB	G�B	K�B	N�B	M�B	N�B	L�B	M�B	L�B	M�B	T�B	YB	aHB	aHB	iyB	iyB	k�B	n�B	q�B	u�B	v�B	u�B	v�B	w�B	w�B	w�B	x�B	x�B	x�B	y�B	z�B	x�B	z�B	}�B	� B	� B	� B	� B	� B	� B	� B	� B	�B	�B	�1B	�PB	�bB	�hB	�oB	�oB	��B	��B	�{B	�uB	�{B	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�!B	�B	�B	�B	�!B	�'B	�-B	�-B	�3B	�?B	�RB	�^B	�dB	�dB	�dB	�jB	�qB	�qB	�}B	��B	ÖB	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ȴB	ȴB	ɺB	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446552012010314465520120103144655  AO  ARGQ                                                                        20111130140522  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140522  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144655  IP                  G�O�G�O�G�O�                