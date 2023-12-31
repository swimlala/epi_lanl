CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:09Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               CA   AO  20111130140634  20190522121826  1727_5046_067                   2C  D   APEX                            2143                            040306                          846 @�l>��1   @�l?m�?�@7F$�/��c�|�hs1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DFy�DF��DG� DH  DH�fDIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D��D� D  D� D  D�fD  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)y�D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3y�D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DE��DFy�DF��DG� DH  DH�fDIfDI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�  A��/A���A���A���A��uA��PA��7A��A�|�A�x�A�t�A�r�A�jA�bNA�XA�E�A��#A�ffA��A��-A�|�A�1'A�{A�A�ĜA��A���A�x�A�VA��FA�XA�A��A��A�^5A��!A�S�A���A�bNA��`A�z�A�ZA�%A��A��A�G�A�oA��^A�1'A���A�VA�z�A��jA�5?A��A���A���A�r�A���A�I�A�VA��7A���A���A�t�A�+A�?}A��A�x�A�JA���A���A��A�ĜA�G�A��#A���A�l�A�\)A�C�A���A� �A�{A�A��FA�O�A�A���A��uA�-A�-A�VA�A�A�A�M�A�(�A�r�A�Q�A��/A�O�A���A��A��jA��A�(�A�E�A��RA���A�ȴA�33A��`AhsA|��A{%Ay�hAx�Avv�At9XAr�+Aq�hApbNAn��Al��Ai�wAi�Ah��Ag�wAe�hAbbA`9XA_;dA^1'A\ZA[�^AZ�AY|�AW�AT9XARVAOC�AM+AK�AJ��AJ�+AJQ�AJJAI��AIS�AH��AH�!AH9XAFĜAD��AC�TAC�mAC�wAB�yAAK�A@�HA?K�A>�A=��A=/A<�A<bNA<1'A<{A;+A9�hA8�RA7��A6�RA5�
A5XA4�uA3��A2M�A1/A0��A.~�A-+A,n�A,v�A+l�A*��A)��A)�7A)�A(��A(��A(~�A(Q�A'��A&�uA&�A#�-A"�DA!�A!�-A!hsA��A��A��A��Al�A33A�+A��A7LA�A�/A�!A�A�7AVAbNA�A-A�-AG�AjAƨA�A^5AA�`A{AVAA�AXA
ZA	+A�AA�AA�A��A/A�A��A�A�#Ax�A33A �uA 9XA {@�M�@��@�1@��@��y@�G�@�S�@�hs@�b@�@��@�@�$�@�u@�b@�o@��@�F@��@㕁@���@���@ܴ9@��H@ش9@���@�@�&�@��/@�I�@�1'@ҧ�@�p�@�?}@�x�@�5?@��@Ͼw@�M�@�Q�@ʏ\@�S�@Ƨ�@���@���@ċD@�;d@\@��@�x�@��j@�b@�l�@���@��@�V@��`@�Ĝ@��w@��@�^5@�=q@�-@�J@�Ĝ@�(�@��@�{@���@���@�%@�^5@�"�@�;d@��@���@��;@��+@���@��@�J@��/@���@���@�p�@��@��-@�x�@�p�@�Q�@��!@��R@�5?@��@��@��+@��@��@���@�9X@�(�@�33@�=q@���@���@�1'@�ff@��#@���@���@���@�~�@�^5@�{@�{@�J@�Q�@���@�M�@���@���@��@�`B@���@���@��@�j@���@�|�@�ff@��@���@�X@��9@�(�@��@���@��@��@��@��-@�hs@�/@��/@�I�@���@�+@�M�@���@��7@�%@���@�z�@�b@���@��F@���@��P@�l�@�S�@��@���@�~�@��@���@�O�@��@��/@��D@��F@�C�@���@�v�@�5?@�-@��T@���@��/@�j@�(�@�  @���@��P@�K�@���@��H@���@��!@��\@�V@�-@�J@��@���@���@���@�hs@��@�%@��`@��9@���@��u@��@�A�@��F@�\)@�33@�"�@�o@�
=@��@��H@��H@�ȴ@��!@�V@�E�@�=q@�-@�-@�J@���@���@�Z@��@��
@��w@��F@���@�33@�
=@��R@�v�@�5?@�{@��T@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�  A��/A���A���A���A��uA��PA��7A��A�|�A�x�A�t�A�r�A�jA�bNA�XA�E�A��#A�ffA��A��-A�|�A�1'A�{A�A�ĜA��A���A�x�A�VA��FA�XA�A��A��A�^5A��!A�S�A���A�bNA��`A�z�A�ZA�%A��A��A�G�A�oA��^A�1'A���A�VA�z�A��jA�5?A��A���A���A�r�A���A�I�A�VA��7A���A���A�t�A�+A�?}A��A�x�A�JA���A���A��A�ĜA�G�A��#A���A�l�A�\)A�C�A���A� �A�{A�A��FA�O�A�A���A��uA�-A�-A�VA�A�A�A�M�A�(�A�r�A�Q�A��/A�O�A���A��A��jA��A�(�A�E�A��RA���A�ȴA�33A��`AhsA|��A{%Ay�hAx�Avv�At9XAr�+Aq�hApbNAn��Al��Ai�wAi�Ah��Ag�wAe�hAbbA`9XA_;dA^1'A\ZA[�^AZ�AY|�AW�AT9XARVAOC�AM+AK�AJ��AJ�+AJQ�AJJAI��AIS�AH��AH�!AH9XAFĜAD��AC�TAC�mAC�wAB�yAAK�A@�HA?K�A>�A=��A=/A<�A<bNA<1'A<{A;+A9�hA8�RA7��A6�RA5�
A5XA4�uA3��A2M�A1/A0��A.~�A-+A,n�A,v�A+l�A*��A)��A)�7A)�A(��A(��A(~�A(Q�A'��A&�uA&�A#�-A"�DA!�A!�-A!hsA��A��A��A��Al�A33A�+A��A7LA�A�/A�!A�A�7AVAbNA�A-A�-AG�AjAƨA�A^5AA�`A{AVAA�AXA
ZA	+A�AA�AA�A��A/A�A��A�A�#Ax�A33A �uA 9XA {@�M�@��@�1@��@��y@�G�@�S�@�hs@�b@�@��@�@�$�@�u@�b@�o@��@�F@��@㕁@���@���@ܴ9@��H@ش9@���@�@�&�@��/@�I�@�1'@ҧ�@�p�@�?}@�x�@�5?@��@Ͼw@�M�@�Q�@ʏ\@�S�@Ƨ�@���@���@ċD@�;d@\@��@�x�@��j@�b@�l�@���@��@�V@��`@�Ĝ@��w@��@�^5@�=q@�-@�J@�Ĝ@�(�@��@�{@���@���@�%@�^5@�"�@�;d@��@���@��;@��+@���@��@�J@��/@���@���@�p�@��@��-@�x�@�p�@�Q�@��!@��R@�5?@��@��@��+@��@��@���@�9X@�(�@�33@�=q@���@���@�1'@�ff@��#@���@���@���@�~�@�^5@�{@�{@�J@�Q�@���@�M�@���@���@��@�`B@���@���@��@�j@���@�|�@�ff@��@���@�X@��9@�(�@��@���@��@��@��@��-@�hs@�/@��/@�I�@���@�+@�M�@���@��7@�%@���@�z�@�b@���@��F@���@��P@�l�@�S�@��@���@�~�@��@���@�O�@��@��/@��D@��F@�C�@���@�v�@�5?@�-@��T@���@��/@�j@�(�@�  @���@��P@�K�@���@��H@���@��!@��\@�V@�-@�J@��@���@���@���@�hs@��@�%@��`@��9@���@��u@��@�A�@��F@�\)@�33@�"�@�o@�
=@��@��H@��H@�ȴ@��!@�V@�E�@�=q@�-@�-@�J@���@���@�Z@��@��
@��w@��F@���@�33@�
=@��R@�v�@�5?@�{@��T@���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�}B��B��B��B��B��B��B��B��B��B��B��B��BĜBǮB��B��B�B�5B�ZB�`B�yB�B��B��B��BB%B	7B1BB  B��B��B�B�B�B�B�mB�NB�HB�B�B�B�B�yB�mB�`B�`B�`B�TB�BB�B�
B��B��B��B�FB�B��B��B��B�\B�7B�+B�B� Bw�Bp�Bk�BgmBaHBZBW
BT�BR�BQ�BP�BO�BN�BL�BE�B@�B>wB<jB7LB1'B,B&�B#�B�BB�B��B�-B��B�VBn�B`BBH�B%�B\B%BB
��B
�NB
��B
��B
ÖB
�B
��B
y�B
iyB
T�B
G�B
>wB
49B
'�B
�B
VB
1B	��B	��B	�yB	�B	��B	��B	��B	�}B	��B	��B	��B	��B	�PB	�7B	� B	v�B	gmB	S�B	F�B	@�B	/B	)�B	)�B	33B	7LB	7LB	6FB	6FB	6FB	6FB	49B	/B	(�B	'�B	&�B	%�B	 �B	�B	�B	hB	JB	
=B	1B	%B	B	B	B��B��B��B�B�B�B�B�yB�ZB�B��B��BȴBB�wB�qB�XB�FB�9B�?B�FB�LB�XB�RB�LB�FB�3B�B�B��B��B��B��B��B��B��B��B�{B�oB�bB�PB�JB�=B�7B�1B�+B�B�B�B}�B|�B{�By�Bx�Bw�Bu�Bt�Bq�Bo�Bl�BiyBgmBe`BbNB`BB_;B]/B\)BZBW
BT�BT�BS�BR�BR�BR�BQ�BQ�BQ�BO�BO�BN�BN�BN�BM�BN�BP�BT�BW
BW
BXB[#B\)B_;B^5B\)BXBR�BP�BK�BC�B=qB;dB:^B>wB?}B>wBA�BD�BJ�BM�BL�BK�BL�BO�BW
BXBXBVBO�BI�BD�BD�BC�BA�BA�BA�B?}B>wB=qB<jB<jB<jBA�BB�BC�BC�BC�BF�BH�BI�BJ�BI�BI�BM�BO�BP�BVB]/Be`B�=BƨB��B��B��B��B��B��BƨBBBƨB��B��B�B�sB�sB�sB�mB�`B�mB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B��B	%B	
=B	DB	
=B	DB	PB	\B	bB	oB	�B	{B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	%�B	&�B	&�B	&�B	'�B	'�B	+B	+B	2-B	49B	5?B	6FB	8RB	;dB	<jB	@�B	G�B	J�B	L�B	Q�B	VB	VB	W
B	XB	YB	YB	ZB	[#B	[#B	]/B	_;B	aHB	cTB	ffB	hsB	iyB	hsB	gmB	e`B	hsB	iyB	jB	k�B	k�B	l�B	m�B	n�B	o�B	q�B	s�B	x�B	y�B	{�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�7B	�=B	�=B	�DB	�VB	�\B	�bB	�oB	�oB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�-B	�RB	�dB	�qB	��B	��B	B	B	ŢB	ŢB	ǮB	ȴB	ȴB	ɺB	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��BĜBǮB��B��B�B�BB�`B�fB�B�B��B��B��BB+B
=BDBBB��B��B��B�B�B�B�B�ZB�TB�B�B�B�B�B�sB�fB�fB�mB�`B�NB�/B�B��B��BƨB�dB�!B��B��B��B�uB�=B�1B�%B�B~�Bs�Bn�BjBffB^5BYBXBT�BS�BQ�BO�BO�BO�BH�B@�B?}B>wB9XB33B.B'�B%�B�B
=B��B��B�FB�B��Bu�BhsBO�B-BhB1BB
��B
�fB
�B
��B
ȴB
�?B
��B
~�B
o�B
YB
J�B
B�B
8RB
.B
�B
hB
DB
B	��B	�B	�)B	�B	�B	��B	��B	��B	��B	��B	��B	�bB	�JB	�B	� B	p�B	[#B	Q�B	G�B	33B	-B	+B	49B	8RB	8RB	7LB	7LB	7LB	7LB	8RB	49B	+B	'�B	'�B	'�B	$�B	�B	�B	{B	PB	DB		7B	1B	%B	B	B	B��B��B��B�B�B�B�B�sB�)B�
B�B��BĜB�wB��B�jB�XB�?B�LB�LB�RB�^B�XB�^B�XB�?B�?B�'B�B��B��B��B��B��B��B��B��B�{B�oB�VB�PB�=B�=B�=B�7B�%B�%B�B�B~�B}�B|�Bz�Bz�Bw�Bv�Bt�Br�Bp�Bl�BjBiyBffBbNBaHB_;B]/B]/B\)BYBVBW
BVBS�BS�BS�BR�BR�BR�BR�BO�BO�BO�BP�BQ�BS�BW
BXBXB\)B_;B^5B`BB^5B^5B[#BW
BR�BK�BE�B>wB=qB=qB?}BA�B?}BB�BE�BJ�BO�BN�BK�BL�BN�BYBZB[#BYBR�BN�BE�BE�BE�BB�BC�BB�B@�B?}B?}B=qB=qB?}BB�BC�BC�BD�BE�BH�BI�BI�BJ�BJ�BK�BN�BP�BS�BXB\)B]/By�BŢB��B��B��B��B��B��BɺBĜBĜBĜB��B��B�B�yB�yB�sB�yB�`B�mB�yB�B�B�B�B�B��B��B��B��B��B	  B��B��B	B	+B	
=B	PB	
=B	JB	PB	bB	bB	oB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	&�B	'�B	'�B	'�B	(�B	(�B	,B	-B	33B	5?B	6FB	7LB	9XB	<jB	>wB	@�B	G�B	J�B	M�B	R�B	VB	VB	W
B	XB	YB	YB	ZB	[#B	[#B	^5B	_;B	bNB	dZB	gmB	hsB	iyB	iyB	hsB	ffB	iyB	jB	jB	k�B	l�B	l�B	n�B	o�B	o�B	q�B	t�B	x�B	z�B	|�B	}�B	~�B	� B	�B	�B	�B	�B	�%B	�+B	�7B	�=B	�=B	�JB	�VB	�\B	�bB	�oB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�9B	�XB	�jB	�wB	��B	��B	B	ÖB	ŢB	ƨB	ȴB	ɺB	ȴB	ɺB	��B	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446572012010314465720120103144657  AO  ARGQ                                                                        20111130140634  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140634  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144657  IP                  G�O�G�O�G�O�                