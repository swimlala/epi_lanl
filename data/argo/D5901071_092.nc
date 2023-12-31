CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:16Z UW 3.1 conversion   
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
_FillValue                 �  A@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ex   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g`   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  o    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �\   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �l   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �p   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               \A   AO  20111130141238  20190522121826  1727_5046_092                   2C  D   APEX                            2143                            040306                          846 @Ԍ�A� 	1   @Ԍ��$ @7G-�c蛥�S�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dq��Dr� Ds  Dss3Dy��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�  @�  A   A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd�fDe  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Djy�Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq� Dq��Dr� Ds  Dss3Dy��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�+A�-A�(�A�+A�-A�33A�1'A�1'A�1'A��A��Aѣ�A�t�A�l�A�\)A�M�A�C�A�9XA�33A�+A� �A�A���A�`BA�bNA�5?Aɥ�A��;A�E�A�z�A�9XA���AōPAĴ9Aá�A�ffA�dZA��FA� �A�n�A�
=A���A��
A��/A�JA��7A��A���A�7LA���A��DA��-A�~�A�v�A�7LA��A���A��9A��A��A�"�A�hsA��A�A�n�A��7A��yA�XA�?}A���A�A�A�v�A�E�A���A���A�x�A���A��FA��A���A�C�A�A���A�{A�bNA�n�A�|�A�A��wA���A��DA�?}A�/A�O�A��TA��A���A��A��A���A�dZA�ZA�\)A�ZA��HA�`BA�;dA��A���A��`A�VA�ffA���A��A��A���A��!A�dZA���A��A���A�^5A��-AC�A|��AzffAyoAw|�Avv�Au�As��Aq�Aq��AnI�Aj{Ag�Ae�Ae+AchsA_�A]&�AYVAW�7AUƨAT{ASXAR�AR �AQ��AO��AN��AM�#AK7LAJz�AI�AI�#AIXAF=qACVA@�/A@bA?�A?p�A?O�A?/A>�A>�A> �A=�A<�\A;"�A9�A7��A6�yA6�DA6Q�A5�A4ZA3ƨA1��A133A0=qA.��A-l�A,Q�A+�;A*�HA*-A)��A)C�A(�uA'�A&=qA$��A$ȴA$jA#�A!7LA 5?A|�A^5A
=A�uAI�AA��A(�A{A�FA��A-A�Ap�A��A�!A1'AC�A��A1AXA�uA`BA��AA�A&�Av�A��A��AhsA%A�+A1A+A
VA	�7AZA��A;dA�HAI�A��A
=Az�AA�A��AK�A�yAn�A;d@��;@�V@�t�@�A�@��@�I�@��/@�\)@�-@�X@�ƨ@��@�5?@�x�@��@蛦@�Q�@��;@�R@�X@��@���@�~�@��@�(�@�;d@ݺ^@ܴ9@ۮ@�x�@��T@� �@��y@�hs@Ͼw@Ο�@͉7@̼j@��;@ʗ�@��@�b@�l�@���@�{@��/@�|�@��7@�b@���@�;d@���@��7@���@�|�@���@�G�@��@�1'@�S�@�C�@�C�@�^5@���@��@�`B@�G�@�1'@�l�@���@���@��/@�Z@��;@���@�+@�v�@�{@�S�@�dZ@���@��@���@��-@��@�I�@�  @��;@�t�@��@��#@��-@�/@���@��@��P@�\)@�;d@�;d@�ȴ@�O�@���@�C�@��!@�M�@��@��;@���@��@�M�@�@��@���@���@�Ĝ@���@�z�@�  @���@�A�@��@��H@��R@�M�@�V@���@�&�@��@��F@�C�@��@�~�@�@���@�`B@�/@��j@���@���@�l�@�"�@�ȴ@�~�@��@���@���@���@�Ĝ@�(�@�1@�1@�  @�  @��w@�|�@�t�@�+@��@��@�b@���@�dZ@��\@�M�@�5?@�{@�@��^@��@�z�@�1@�ƨ@��F@��m@�
=@���@��@��#@���@���@��^@�O�@�7L@��@�V@��`@���@��/@���@���@���@�z�@�I�@��@�ƨ@��@��@��#@�@���@���@�M�@��@��#@���@�O�@��@��@�r�@�Z@�1'@��@�1@��;@���@�33@�"�@��@�
=@��@��y@��R@�^5@��^@�/@��@��@��@��/@�j@�1'@�(�@��@�b@��F@�
=@���@��+@�&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�+A�-A�(�A�+A�-A�33A�1'A�1'A�1'A��A��Aѣ�A�t�A�l�A�\)A�M�A�C�A�9XA�33A�+A� �A�A���A�`BA�bNA�5?Aɥ�A��;A�E�A�z�A�9XA���AōPAĴ9Aá�A�ffA�dZA��FA� �A�n�A�
=A���A��
A��/A�JA��7A��A���A�7LA���A��DA��-A�~�A�v�A�7LA��A���A��9A��A��A�"�A�hsA��A�A�n�A��7A��yA�XA�?}A���A�A�A�v�A�E�A���A���A�x�A���A��FA��A���A�C�A�A���A�{A�bNA�n�A�|�A�A��wA���A��DA�?}A�/A�O�A��TA��A���A��A��A���A�dZA�ZA�\)A�ZA��HA�`BA�;dA��A���A��`A�VA�ffA���A��A��A���A��!A�dZA���A��A���A�^5A��-AC�A|��AzffAyoAw|�Avv�Au�As��Aq�Aq��AnI�Aj{Ag�Ae�Ae+AchsA_�A]&�AYVAW�7AUƨAT{ASXAR�AR �AQ��AO��AN��AM�#AK7LAJz�AI�AI�#AIXAF=qACVA@�/A@bA?�A?p�A?O�A?/A>�A>�A> �A=�A<�\A;"�A9�A7��A6�yA6�DA6Q�A5�A4ZA3ƨA1��A133A0=qA.��A-l�A,Q�A+�;A*�HA*-A)��A)C�A(�uA'�A&=qA$��A$ȴA$jA#�A!7LA 5?A|�A^5A
=A�uAI�AA��A(�A{A�FA��A-A�Ap�A��A�!A1'AC�A��A1AXA�uA`BA��AA�A&�Av�A��A��AhsA%A�+A1A+A
VA	�7AZA��A;dA�HAI�A��A
=Az�AA�A��AK�A�yAn�A;d@��;@�V@�t�@�A�@��@�I�@��/@�\)@�-@�X@�ƨ@��@�5?@�x�@��@蛦@�Q�@��;@�R@�X@��@���@�~�@��@�(�@�;d@ݺ^@ܴ9@ۮ@�x�@��T@� �@��y@�hs@Ͼw@Ο�@͉7@̼j@��;@ʗ�@��@�b@�l�@���@�{@��/@�|�@��7@�b@���@�;d@���@��7@���@�|�@���@�G�@��@�1'@�S�@�C�@�C�@�^5@���@��@�`B@�G�@�1'@�l�@���@���@��/@�Z@��;@���@�+@�v�@�{@�S�@�dZ@���@��@���@��-@��@�I�@�  @��;@�t�@��@��#@��-@�/@���@��@��P@�\)@�;d@�;d@�ȴ@�O�@���@�C�@��!@�M�@��@��;@���@��@�M�@�@��@���@���@�Ĝ@���@�z�@�  @���@�A�@��@��H@��R@�M�@�V@���@�&�@��@��F@�C�@��@�~�@�@���@�`B@�/@��j@���@���@�l�@�"�@�ȴ@�~�@��@���@���@���@�Ĝ@�(�@�1@�1@�  @�  @��w@�|�@�t�@�+@��@��@�b@���@�dZ@��\@�M�@�5?@�{@�@��^@��@�z�@�1@�ƨ@��F@��m@�
=@���@��@��#@���@���@��^@�O�@�7L@��@�V@��`@���@��/@���@���@���@�z�@�I�@��@�ƨ@��@��@��#@�@���@���@�M�@��@��#@���@�O�@��@��@�r�@�Z@�1'@��@�1@��;@���@�33@�"�@��@�
=@��@��y@��R@�^5@��^@�/@��@��@��@��/@�j@�1'@�(�@��@�b@��F@�
=@���@��+@�&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�HB�HB�HB�HB�HB�HB�HB�HB�BB�NB�fB�B��B��B��B��B��B��B��B��B��B��B��B�B�BƨB��B�B�FB��B�jB�#BɺB�?B��B~�B� B�7B��B�B�'BB/BDB'�Br�B�BffBz�B|�B�B��B��B��B�bB��B��B�B�qBɺBŢBB��B�
B��B��BȴB��B��BŢB�LB�B��B��B��B�bB�\B�uB�hB�DB�DB�DB�DB�DB�Bt�BhsBe`Be`BcTBaHB\)BO�BD�B;dB(�B�BoBB�B�/B��B��B�!B��B�hB{�BiyB\)B?}B�B1B
��B
�B
�)B
ȴB
�RB
��B
�oB
� B
t�B
hsB
P�B
6FB
�B
bB
%B	��B	�B	�B	�5B	�B	��B	�^B	��B	�B	z�B	t�B	e`B	M�B	6FB	�B	\B	B	B	B	uB	{B	oB		7B	%B��B�B�sB�ZB�NB�BƨB�LB�B��B��B��B��B��B��B��B��B��B��B��B�hB�VB�PB�DB�7B�+B�B~�B|�Bz�Bx�Bx�Bw�Bw�Bv�Bx�Bx�Bx�Bw�Bw�Bu�Bx�B|�B}�B|�B|�B~�Bz�Bx�Bv�Bu�Bv�Bu�Bt�Bt�Bu�Bt�Br�Bs�Bs�Bs�Bs�Br�Br�Bq�Bt�Bq�Bo�Bl�BiyBhsBffBe`BbNB`BB`BB_;B^5B]/B\)BZBYBYBW
BT�BT�BT�BS�BS�BR�BR�BS�BS�BR�BVBT�BS�BO�BK�BI�BI�BG�BG�BF�BB�B<jB:^B9XB8RB7LB6FB7LB8RB9XB:^B9XB:^B:^B:^B:^B=qB<jB;dB:^B<jB<jB;dB9XB:^B:^B9XB8RB7LB7LB8RB8RB9XB;dB<jB?}BB�BD�BD�BE�BF�BI�BJ�BJ�BJ�BI�BJ�BK�BL�BP�BQ�BQ�BR�BVBT�BT�BXB\)B_;B`BBdZBiyBk�Bl�Bn�Bq�Bu�By�B|�B~�B� B�%B�\B�bB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�LB�dB�jB�^B�?B�3B�'B�3B�FB�LB�jB��BÖBÖBBÖBĜBƨBɺB��B��B�B�B�B�B�B�)B�NB�TB�mB�fB�mB�B�B�B�B�B�B�B�B��B��B��B��B��B	  B	B	B	B	
=B	DB	hB	uB	{B	{B	{B	�B	�B	�B	 �B	$�B	(�B	33B	49B	5?B	6FB	8RB	;dB	?}B	A�B	A�B	A�B	C�B	F�B	H�B	K�B	N�B	M�B	L�B	O�B	R�B	R�B	R�B	S�B	W
B	XB	YB	ZB	\)B	_;B	bNB	dZB	ffB	hsB	iyB	iyB	iyB	iyB	k�B	w�B	}�B	�B	�+B	�DB	�JB	�PB	�VB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�-B	�FB	�RB	�XB	�^B	�^B	�^B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�HB�HB�HB�HB�HB�HB�HB�HB�HB�TB�sB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�XB��B�qB�BB��B�XB��B�B�B�JB��B�B�BB49BPB&�Bx�B�JBhsB|�B}�B�B��B�B��B�uB��B��B�B�wB��BɺBŢB�B�B�
B��B��B��B��BǮB�^B�-B��B��B��B�uB�\B��B�uB�JB�JB�PB�VB�\B�+By�Bk�BffBffBdZBcTBbNBT�BG�BB�B-B!�B�BDB��B�NB��BŢB�-B�B�{B� Bm�B`BBH�B�B
=BB
�B
�HB
��B
�wB
��B
��B
�B
w�B
p�B
\)B
=qB
%�B
{B

=B	��B	��B	�B	�NB	�B	�)B	ĜB	��B	�+B	|�B	y�B	o�B	T�B	A�B	�B	{B	1B	B	B	{B	�B	�B	VB		7B	+B�B�yB�ZB�TB�HB��B�qB�'B�B��B��B��B��B��B��B��B��B��B��B��B�bB�VB�JB�JB�7B�B�B~�B}�B|�B|�Bz�By�By�Bz�By�Bz�By�By�Bz�B|�B}�B� B�B�B�B}�B|�B{�Bw�Bw�Bw�Bx�Bv�Bv�Bv�Bu�Bu�Bt�Bu�Bt�Bs�Bs�Bt�Bv�Bs�Bq�Bn�Bl�BjBgmBhsBdZBbNBaHB`BB_;B_;B^5B]/B\)B\)B[#BXBVBVBVBVBT�BT�BT�BT�BT�BXBW
BXBS�BN�BN�BN�BK�BJ�BL�BE�B>wB<jB<jB:^B8RB7LB8RB9XB:^B;dB;dB<jB<jB<jB;dB>wB>wB<jB<jB=qB=qB>wB=qB<jB<jB;dB:^B9XB9XB9XB9XB;dB=qB>wB@�BC�BE�BF�BG�BI�BK�BK�BK�BK�BK�BL�BM�BO�BQ�BR�BR�BT�BVBT�BT�BYB]/B_;BaHBffBjBl�Bn�Bp�Br�Bv�Bz�B}�B� B�B�B�\B�oB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�3B�LB�jB�wB�jB�FB�9B�-B�?B�RB�XB�jBBĜBÖBÖBĜBĜBƨBɺB��B��B�
B�B�B�B�B�)B�TB�ZB�sB�sB�sB�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	B	%B	DB	JB	hB	uB	{B	{B	{B	�B	�B	�B	!�B	$�B	&�B	33B	5?B	6FB	6FB	8RB	;dB	?}B	B�B	B�B	B�B	D�B	F�B	H�B	K�B	O�B	O�B	L�B	O�B	R�B	R�B	R�B	T�B	W
B	XB	YB	ZB	\)B	_;B	bNB	dZB	ffB	iyB	iyB	jB	iyB	iyB	iyB	v�B	{�B	�B	�+B	�JB	�PB	�PB	�\B	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�-B	�FB	�XB	�XB	�dB	�^B	�^B	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447062012010314470620120103144706  AO  ARGQ                                                                        20111130141238  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141238  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144706  IP                  G�O�G�O�G�O�                