CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:20Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               jA   AO  20111130141555  20190522121826  1727_5046_106                   2C  D   APEX                            2143                            040306                          846 @Ԟ����1   @Ԟ�Q��@6Z��vȴ�c�?|�h1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cm�fCo�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cm�fCo�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D.��D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da�fDb  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��A��A���A��A��A��;A���Aϰ!Aϛ�A�x�A�p�A�l�A�hsA�ffA�bNA�bNA�\)A�ZA�ZA�XA�XA�VA�XA�VA�VA�VA�S�A�S�A�S�A�O�A�I�A�A�A�9XA�&�AξwA�A˅Aǉ7AĲ-A��DA��A�dZA���A�dZA���A��9A���A�r�A���A�JA�Q�A��9A��#A��jA�(�A�7LA��RA�|�A�XA��hA��`A�9XA���A��A��A�p�A�;dA���A��-A��PA��mA�;dA�t�A�?}A��A�hsA�ƨA�&�A�1A���A�A�A�
=A�7LA�-A�ƨA���A��jA��uA�=qA�jA��A�O�A��jA�;dA���A�"�A���A�hsA��^A��A��A�bA�VA�E�A�XA�M�A�hsA��`A�dZA�O�A�ƨA���A�-A��HA��RA���A�ffA��A�ZAG�A~�/A}�Ax$�Au��As�PAoXAmt�Al��Ak��AkAi"�Ac��Ab�Ab��Aa�mA_��AYO�AWp�AT�!AQ"�AOx�AN^5AMO�AK��AJ�AI�PAH�AG�^AFn�AEx�ADv�AC�#AC�hABr�A@~�A>I�A;S�A8�HA8-A5��A4Q�A2��A2VA1��A0n�A/O�A.(�A-VA+��A*�+A)|�A(�9A(5?A&�HA&bA%�hA$��A$r�A#��A"�9A"9XA!S�A ffA��A��A�A��A�HAVA�TAhsA��A�DA�FA��A��A��A��AXAbNA�hA�DA(�A��A=qA?}AAjA��A��A7LA%A
��A
ĜA	�7A�\AdZA�uA�AVA�wA(�AO�A��A{AS�A j@���@���@��F@�C�@�p�@��@��7@�O�@��@�p�@�Z@��
@�l�@�+@�+@��@�D@�ȴ@���@�X@���@�o@�R@�J@�z�@�o@�ff@��`@�+@�{@�1'@� �@�;d@և+@��`@ҸR@���@��;@���@͡�@̼j@�K�@�$�@Ǖ�@�-@��@�$�@�^5@Ə\@ư!@��H@��@�
=@�
=@�o@��@�o@��y@�ȴ@�V@�x�@�ƨ@�Z@�@�-@��h@���@���@�Z@�"�@�o@��;@�  @�G�@��@���@�A�@���@�ȴ@���@�~�@��/@��-@���@�A�@�dZ@�V@���@��@��;@��@�l�@�$�@� �@�S�@���@���@�~�@�^5@��^@���@��/@��j@��@�Q�@���@�b@���@���@��@�j@�(�@��@�Z@�I�@���@��@�bN@��D@��@�V@�7L@�$�@�n�@��+@�33@�\)@��y@�V@��-@���@��T@�x�@�`B@�`B@�/@�Q�@��@�V@���@�^5@��y@�V@��@��P@�O�@�@���@���@���@�|�@�-@��^@���@��/@�z�@�dZ@�+@�~�@�{@�J@�-@��T@�G�@��@�/@�/@�V@���@���@�Q�@��m@�S�@���@�=q@��@�X@��@�/@��@��u@��u@��9@��9@�Q�@�bN@�r�@��D@���@��j@��@���@���@�A�@��@���@�ƨ@��F@���@��@��@���@�t�@�33@���@�ȴ@���@�ȴ@���@�v�@�n�@���@��@�o@�o@�
=@��y@��@�
=@���@��H@���@�{@��@�M�@�=q@�J@�@��-@���@��D@�`B@�x�@�X@��@��@�33@��+@��@��@�(�@�P@+@~�R@~ff@~$�@}�@}�-@}O�@|z�@|I�@|9X@|9X@|(�@{��@{ƨ@{ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A���A��A��A��;A���Aϰ!Aϛ�A�x�A�p�A�l�A�hsA�ffA�bNA�bNA�\)A�ZA�ZA�XA�XA�VA�XA�VA�VA�VA�S�A�S�A�S�A�O�A�I�A�A�A�9XA�&�AξwA�A˅Aǉ7AĲ-A��DA��A�dZA���A�dZA���A��9A���A�r�A���A�JA�Q�A��9A��#A��jA�(�A�7LA��RA�|�A�XA��hA��`A�9XA���A��A��A�p�A�;dA���A��-A��PA��mA�;dA�t�A�?}A��A�hsA�ƨA�&�A�1A���A�A�A�
=A�7LA�-A�ƨA���A��jA��uA�=qA�jA��A�O�A��jA�;dA���A�"�A���A�hsA��^A��A��A�bA�VA�E�A�XA�M�A�hsA��`A�dZA�O�A�ƨA���A�-A��HA��RA���A�ffA��A�ZAG�A~�/A}�Ax$�Au��As�PAoXAmt�Al��Ak��AkAi"�Ac��Ab�Ab��Aa�mA_��AYO�AWp�AT�!AQ"�AOx�AN^5AMO�AK��AJ�AI�PAH�AG�^AFn�AEx�ADv�AC�#AC�hABr�A@~�A>I�A;S�A8�HA8-A5��A4Q�A2��A2VA1��A0n�A/O�A.(�A-VA+��A*�+A)|�A(�9A(5?A&�HA&bA%�hA$��A$r�A#��A"�9A"9XA!S�A ffA��A��A�A��A�HAVA�TAhsA��A�DA�FA��A��A��A��AXAbNA�hA�DA(�A��A=qA?}AAjA��A��A7LA%A
��A
ĜA	�7A�\AdZA�uA�AVA�wA(�AO�A��A{AS�A j@���@���@��F@�C�@�p�@��@��7@�O�@��@�p�@�Z@��
@�l�@�+@�+@��@�D@�ȴ@���@�X@���@�o@�R@�J@�z�@�o@�ff@��`@�+@�{@�1'@� �@�;d@և+@��`@ҸR@���@��;@���@͡�@̼j@�K�@�$�@Ǖ�@�-@��@�$�@�^5@Ə\@ư!@��H@��@�
=@�
=@�o@��@�o@��y@�ȴ@�V@�x�@�ƨ@�Z@�@�-@��h@���@���@�Z@�"�@�o@��;@�  @�G�@��@���@�A�@���@�ȴ@���@�~�@��/@��-@���@�A�@�dZ@�V@���@��@��;@��@�l�@�$�@� �@�S�@���@���@�~�@�^5@��^@���@��/@��j@��@�Q�@���@�b@���@���@��@�j@�(�@��@�Z@�I�@���@��@�bN@��D@��@�V@�7L@�$�@�n�@��+@�33@�\)@��y@�V@��-@���@��T@�x�@�`B@�`B@�/@�Q�@��@�V@���@�^5@��y@�V@��@��P@�O�@�@���@���@���@�|�@�-@��^@���@��/@�z�@�dZ@�+@�~�@�{@�J@�-@��T@�G�@��@�/@�/@�V@���@���@�Q�@��m@�S�@���@�=q@��@�X@��@�/@��@��u@��u@��9@��9@�Q�@�bN@�r�@��D@���@��j@��@���@���@�A�@��@���@�ƨ@��F@���@��@��@���@�t�@�33@���@�ȴ@���@�ȴ@���@�v�@�n�@���@��@�o@�o@�
=@��y@��@�
=@���@��H@���@�{@��@�M�@�=q@�J@�@��-@���@��D@�`B@�x�@�X@��@��@�33@��+@��@��@�(�@�P@+@~�R@~ff@~$�@}�@}�-@}O�@|z�@|I�@|9X@|9X@|(�@{��@{ƨ@{ƨ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BȴB�qB�9B�'BÖBȴB��B��B��B�LB�?B�9B�3B�3B�-B�FB�?B�jB�qB��B�qB�jB�dB�qB�wB�wB�wB�jB�jB�XB�^B�qB�jB�qB�dB�?B��B��B�oB�PB�Bx�Bo�Bl�BhsBbNB]/BR�BD�B0!B&�B$�B&�B#�B�B\BB�B�NB�#B��B�dB��B�oBw�BgmBW
BF�B,B{B
��B
��B
�XB
��B
�DB
� B
o�B
S�B
L�B
I�B
G�B
C�B
9XB
"�B
�B
hB
B	�5B	ɺB	�LB	��B	�\B	�1B	�B	z�B	k�B	P�B	J�B	G�B	A�B	2-B	\B	B�B�BB�B��B��BŢB�wB�XB�9B�B��B��B��B��B�oB�JB� Br�BffBcTB_;B]/B]/B]/B]/B\)B\)B\)B\)B\)B\)B]/B^5B`BB`BBaHBhsBq�Bn�Bo�Bp�Bn�Bp�Bu�Bu�Bt�B}�B�B�B�B�B~�B|�Bz�By�Bv�Bw�Bs�Br�Bq�Bo�Bn�Bl�Bk�BiyBhsBffBffBe`BdZBcTBbNBbNBaHBaHB^5B\)BZBYBYBYBVBT�BS�BR�BQ�BP�BN�BL�BJ�BJ�BI�BH�BF�BF�BG�BG�BI�BK�BK�BL�BL�BL�BL�BO�BN�BN�BO�BN�BP�BQ�BQ�BP�BQ�BR�BQ�BQ�BQ�BP�BO�BR�BR�BR�BQ�BT�BW
BW
BXBZBZBZBXB_;BffBiyBl�Bp�Bs�Bu�Bx�Bx�Bz�B{�B|�B{�B|�B~�B�B�+B�1B}�Bz�B�+B�JB�=B�B|�Bw�Bv�B�B�JB�\B��B�!B�LB�jB�wB�wB�RB�'B�B��B��B��B��B��B��B�{B�hB�PB�PB�bB��B��B��B��B��B��B�B�3B�?B�FB�LB�dBÖB��B��B�B�)B�BB�fB�yB�B��B	B	B	%B	+B	DB	\B	oB	�B	�B	#�B	)�B	,B	+B	(�B	&�B	&�B	,B	0!B	33B	33B	2-B	1'B	/B	.B	/B	5?B	9XB	;dB	E�B	E�B	?}B	7LB	7LB	;dB	<jB	<jB	:^B	:^B	:^B	<jB	A�B	F�B	F�B	E�B	D�B	D�B	F�B	H�B	I�B	J�B	N�B	VB	XB	XB	ZB	[#B	\)B	^5B	^5B	_;B	_;B	bNB	gmB	l�B	l�B	k�B	l�B	m�B	n�B	x�B	}�B	~�B	� B	�B	�B	�%B	�+B	�7B	�7B	�JB	�bB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�RB	�XB	�XB	�XB	�^B	�wB	��B	��B	��B	��B	ŢB	ǮB	��B	�B	�B	�#B	�)B	�)B	�B	�B	�B	�
B	�
B	�
B	�B	�
B	�B	�B	�B	�B	�#B	�;B	�HB	�HB	�HB	�HB	�NB	�TB	�T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�
B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BɺB�^B�FBǮB��B��B��B��B�}B�XB�?B�FB�XB�XB�XB�XB��BŢBŢB��B�qB�jBBB��B��B�wB��B�jB�dB�wB�qB�wB�wB�RB��B��B�uB�\B�%B{�Bp�Bm�Bk�BcTBaHBXBK�B49B'�B%�B(�B(�B$�BuBVB�B�ZB�;B��BÖB��B��B}�Bl�B[#BL�B1'B�B+B
�5B
B
��B
�\B
�+B
y�B
VB
M�B
J�B
I�B
I�B
?}B
&�B
�B
{B
bB	�TB	��B	��B	��B	�hB	�=B	�B	~�B	x�B	R�B	K�B	I�B	F�B	A�B	{B	JB��B�`B�)B�B��B��B��B�jB�LB�'B��B��B��B��B��B�uB�1B|�Bn�BffBgmBbNBaHB_;B_;B_;B_;B_;B_;B_;B`BB`BB`BBbNBdZBcTBjBs�Bo�Bq�Bs�Bp�Bs�Bx�Bx�Bw�B� B�B�%B�B�B� B}�B{�B{�Bx�B|�Bw�Bu�Bs�Br�Bp�Bo�Bl�Bl�BjBiyBgmBgmBe`BdZBcTBcTBaHBbNBbNB_;B^5B\)BYBZBXBZBW
BT�BT�BS�BQ�BP�BM�BL�BJ�BK�BJ�BH�BH�BJ�BM�BM�BL�BM�BM�BM�BN�BP�BQ�BP�BP�BP�BQ�BR�BR�BS�BS�BS�BT�BT�BS�BS�BO�BS�BS�BT�BT�BXBXBYBZB\)B\)B\)B\)BaHBffBiyBl�Bp�Bs�Bu�Bx�Bx�Bz�B{�B|�B{�B|�B~�B�B�7B�bB�Bx�B�+B�PB�PB�%B~�Bw�Bv�B�B�JB�PB��B�'B�RB�qB�}B��B�qB�9B�'B��B��B��B��B��B��B��B�{B�hB�\B�{B��B��B��B��B��B��B�B�3B�?B�FB�RB�jBÖB��B��B�B�)B�BB�fB�yB�B��B	B	B	%B	%B	DB	\B	hB	�B	�B	"�B	)�B	-B	,B	)�B	&�B	&�B	-B	0!B	33B	49B	49B	33B	1'B	/B	.B	49B	:^B	8RB	F�B	I�B	E�B	9XB	7LB	=qB	=qB	>wB	;dB	:^B	;dB	=qB	C�B	G�B	G�B	F�B	D�B	D�B	G�B	I�B	I�B	J�B	N�B	VB	XB	XB	[#B	\)B	]/B	^5B	_;B	_;B	`BB	bNB	gmB	l�B	m�B	k�B	l�B	m�B	o�B	x�B	}�B	~�B	� B	�B	�B	�%B	�+B	�=B	�=B	�JB	�bB	�hB	�hB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�9B	�RB	�XB	�XB	�^B	�^B	�wB	��B	��B	B	��B	ǮB	ǮB	ɺB	�B	�B	�)B	�5B	�/B	�#B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�B	�B	�)B	�;B	�HB	�HB	�HB	�HB	�NB	�TB	�T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447112012010314471120120103144711  AO  ARGQ                                                                        20111130141555  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141555  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144711  IP                  G�O�G�O�G�O�                