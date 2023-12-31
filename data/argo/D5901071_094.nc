CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:17Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ^A   AO  20111130141306  20190522121826  1727_5046_094                   2C  D   APEX                            2143                            040306                          846 @ԏ)[�?�1   @ԏ*�@@6-V�c�1&�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @9��@y��@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#fD#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A֏\A֝�A֟�A֡�A֡�A֣�A֡�A֣�A֥�A֥�A֧�A֧�A֩�A֩�A֬A֮A֮Aְ!Aְ!Aֲ-A֮A֡�A֛�A֛�A֗�A֛�A֛�A֛�A�z�A�hsA���AԶFA��A��A�A�A�
=AǸRA��A�jA��mA���A��/A��uA��
A�ffA���A��#A�A�$�A�ƨA�-A�  A��A��A�ZA�A�A���A���A��DA�A�A��A���A�$�A��RA�x�A�"�A���A�E�A�I�A�-A���A�1A�l�A��A��hA�=qA���A��DA���A��A��mA��jA�r�A��A���A��hA� �A��A�"�A�p�A��yA�^5A�/A�bA��TA�1'A��TA�VA��\A�K�A���A�M�A�VA��A�/A�1'A���A���A��A{�AzĜAy�#Ax�AwdZAu��Aq`BAk
=AhQ�AgoAe��Ab��A^jA\(�AZI�AY�^AX�\AW%AV��AU|�ASdZARA�AQ|�AP�uAOAN��AM+AK��AH~�AF��AE��AD�AD(�ABZA@I�A?C�A>��A>~�A>5?A=S�A<A:��A:=qA:$�A:bA:JA9�A8��A81'A7�7A5�A41A2�`A2v�A1��A0��A0^5A/;dA-�
A-`BA,�A+��A*�`A*jA)A(�A&��A&I�A%�A%�FA%�A$�!A$1A"ĜA"^5A!�PA��AZA�wA�AC�AffA��A �AC�A�A�A{A��A��A��A��A=qA�A&�A�;A�AG�A�yA1'A��A�HAƨAK�A�AZA�A
�A	�mA�jA`BA��AffA �Ar�AĜAhsA7LA r�A b@�E�@�1@�dZ@��@��
@��@��`@��m@�@�7@�Ĝ@�C�@���@��@��m@��;@�w@�33@��@�$�@�@陚@�`B@�&�@���@�bN@�n�@��@�J@�X@�bN@ޟ�@ݡ�@�K�@�ff@��@ى7@�|�@�hs@�r�@��
@ӕ�@��y@ёh@��m@�ff@�J@���@�|�@��@�@ź^@�z�@�ƨ@�33@�~�@���@�I�@�|�@�"�@�@�G�@��@��@��@��@�hs@���@� �@�ƨ@���@�S�@��@���@�5?@�`B@���@� �@�K�@��H@��h@�A�@��@���@��#@�&�@���@��9@��@���@�=q@��@�J@�J@�J@���@��@���@���@��!@�t�@��@��P@�|�@�+@���@��@���@���@�ȴ@���@���@���@��9@��w@�ȴ@��@�
=@�@���@�p�@�Q�@��!@�"�@�ƨ@�+@��+@�=q@�{@���@�&�@�I�@���@�+@��@�n�@�5?@��@���@�A�@�I�@�Q�@� �@���@��;@��w@���@�dZ@�
=@�o@�S�@�;d@�o@�o@�"�@�
=@�E�@��#@��;@�o@�C�@��@�  @�b@�ƨ@�|�@�
=@���@�~�@�{@��@��#@��^@��h@�O�@���@�1@��w@�t�@�
=@��H@�ȴ@���@�E�@���@���@�/@���@��@���@�t�@�+@��!@�ff@�=q@���@��7@�hs@�/@�%@�%@�p�@���@���@���@�1@��F@��F@�dZ@�|�@��@���@�  @�1'@�Z@�(�@�  @��
@�1@�1@��;@��@�\)@�o@�@��y@��!@���@�ff@��@���@���@���@���@�x�@��@�b@�r�@�r�@�9X@�b@�1@�  @�w@;d@}�@|��@|z�@|�@{��@{��@{��@{ƨ@{�m@|(�@|1@{S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A֏\A֝�A֟�A֡�A֡�A֣�A֡�A֣�A֥�A֥�A֧�A֧�A֩�A֩�A֬A֮A֮Aְ!Aְ!Aֲ-A֮A֡�A֛�A֛�A֗�A֛�A֛�A֛�A�z�A�hsA���AԶFA��A��A�A�A�
=AǸRA��A�jA��mA���A��/A��uA��
A�ffA���A��#A�A�$�A�ƨA�-A�  A��A��A�ZA�A�A���A���A��DA�A�A��A���A�$�A��RA�x�A�"�A���A�E�A�I�A�-A���A�1A�l�A��A��hA�=qA���A��DA���A��A��mA��jA�r�A��A���A��hA� �A��A�"�A�p�A��yA�^5A�/A�bA��TA�1'A��TA�VA��\A�K�A���A�M�A�VA��A�/A�1'A���A���A��A{�AzĜAy�#Ax�AwdZAu��Aq`BAk
=AhQ�AgoAe��Ab��A^jA\(�AZI�AY�^AX�\AW%AV��AU|�ASdZARA�AQ|�AP�uAOAN��AM+AK��AH~�AF��AE��AD�AD(�ABZA@I�A?C�A>��A>~�A>5?A=S�A<A:��A:=qA:$�A:bA:JA9�A8��A81'A7�7A5�A41A2�`A2v�A1��A0��A0^5A/;dA-�
A-`BA,�A+��A*�`A*jA)A(�A&��A&I�A%�A%�FA%�A$�!A$1A"ĜA"^5A!�PA��AZA�wA�AC�AffA��A �AC�A�A�A{A��A��A��A��A=qA�A&�A�;A�AG�A�yA1'A��A�HAƨAK�A�AZA�A
�A	�mA�jA`BA��AffA �Ar�AĜAhsA7LA r�A b@�E�@�1@�dZ@��@��
@��@��`@��m@�@�7@�Ĝ@�C�@���@��@��m@��;@�w@�33@��@�$�@�@陚@�`B@�&�@���@�bN@�n�@��@�J@�X@�bN@ޟ�@ݡ�@�K�@�ff@��@ى7@�|�@�hs@�r�@��
@ӕ�@��y@ёh@��m@�ff@�J@���@�|�@��@�@ź^@�z�@�ƨ@�33@�~�@���@�I�@�|�@�"�@�@�G�@��@��@��@��@�hs@���@� �@�ƨ@���@�S�@��@���@�5?@�`B@���@� �@�K�@��H@��h@�A�@��@���@��#@�&�@���@��9@��@���@�=q@��@�J@�J@�J@���@��@���@���@��!@�t�@��@��P@�|�@�+@���@��@���@���@�ȴ@���@���@���@��9@��w@�ȴ@��@�
=@�@���@�p�@�Q�@��!@�"�@�ƨ@�+@��+@�=q@�{@���@�&�@�I�@���@�+@��@�n�@�5?@��@���@�A�@�I�@�Q�@� �@���@��;@��w@���@�dZ@�
=@�o@�S�@�;d@�o@�o@�"�@�
=@�E�@��#@��;@�o@�C�@��@�  @�b@�ƨ@�|�@�
=@���@�~�@�{@��@��#@��^@��h@�O�@���@�1@��w@�t�@�
=@��H@�ȴ@���@�E�@���@���@�/@���@��@���@�t�@�+@��!@�ff@�=q@���@��7@�hs@�/@�%@�%@�p�@���@���@���@�1@��F@��F@�dZ@�|�@��@���@�  @�1'@�Z@�(�@�  @��
@�1@�1@��;@��@�\)@�o@�@��y@��!@���@�ff@��@���@���@���@���@�x�@��@�b@�r�@�r�@�9X@�b@�1@�  @�w@;d@}�@|��@|z�@|�@{��@{��@{��@{ƨ@{�m@|(�@|1@{S�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�B�#B�#B�#B�#B�#B�#B�#B�B�#B�)B�)B�)B�/B�)B�)B�#B�5B�;B�mB��BDB�BN�B�VB��BŢB��B��B��BB�LB��B�7B�B�B�B� B~�B|�Bz�Bw�Bx�B�B��B��B�B�B�B��B��B��B��B��B��B��B��B��B�{B�PB�BZBF�B6FB1'B%�BPB  B��B�B�B�sB�TB�#BB�-B��B��B�\B�%Bs�BbNBR�BC�B+B
��B
ŢB
�-B
�B
��B
��B
�B
`BB
W
B
G�B
33B
�B	��B	�`B	�/B	�B	��B	ĜB	�!B	�VB	e`B	R�B	H�B	=qB	,B	�B	oB	1B	B	  B��B��B�B�sB�TB�BB�#B�B��B��BÖB�XB�3B�!B�B��B��B��B��B��B��B��B��B�{B�uB�uB�{B�uB�uB�hB�bB�VB�DB�=B�B�B�B�B�B~�B~�B�B�B� B� B�B� B~�B~�B�B�B�B� B~�B}�B{�By�Bv�Bs�Bo�Bl�Bm�Bl�Bk�Bk�BjBl�Bl�Bl�Bk�Bl�Bk�Bk�BjBiyBiyBhsBgmBiyBjBiyBhsBgmBe`BcTBbNBbNBaHB`BB^5B\)B[#BXBXBW
BT�BQ�BN�BM�BK�BJ�BI�BH�BE�BE�BD�BB�BB�BB�BA�BA�B@�B@�B?}B>wB>wB>wB>wB>wB>wB>wB=qB>wB>wB=qB=qB=qB<jB;dB<jB=qB<jB;dB9XB<jB<jB?}B>wB=qB<jB<jB?}B?}B?}B>wB=qB;dB=qB>wB=qB>wBA�BD�BH�BJ�BF�BG�BI�BL�BO�BT�BW
BW
BXBYBXBZB\)B^5B^5B_;B_;B_;B_;B_;B_;B_;B_;BaHBcTBdZBe`Be`Be`Be`Be`BdZBe`BffBffBffBe`BffBk�Bm�Bp�Br�Br�Bu�Bz�B�B�oB��B�-B�LB�LB�RB�^B�qB��BBÖBȴB��B�ZB�B�B�B�B�B��B�B�B�B�fB�)BɺBȴBɺB��B��B��B��B��B��B�#B�;B�BB�HB�NB�ZB�B��B��B��B	%B	
=B	PB	VB	\B	{B	�B	 �B	$�B	'�B	(�B	)�B	+B	+B	)�B	(�B	%�B	'�B	,B	/B	5?B	6FB	8RB	;dB	@�B	B�B	D�B	F�B	G�B	H�B	H�B	J�B	M�B	O�B	R�B	R�B	T�B	W
B	XB	XB	YB	ZB	[#B	[#B	\)B	_;B	cTB	ffB	gmB	iyB	l�B	n�B	p�B	s�B	w�B	y�B	|�B	}�B	� B	�B	�+B	�+B	�B	�%B	�JB	�VB	�bB	�{B	��B	��B	��B	��B	��B	�B	�'B	�?B	�RB	�^B	�dB	�qB	�wB	��B	��B	��B	B	ÖB	ĜB	ƨB	ǮB	ǮB	ǮB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�#B�B�#B�#B�#B�#B�#B�#B�#B�B�#B�)B�)B�)B�/B�)B�)B�#B�5B�HB�B��BVB�BXB��B��BǮB��B�B��BȴB�}B��B�PB�1B�1B�B�B�B}�B}�B~�B|�B�%B��B��B�B�B�B�B�B��B��B��B��B��B��B��B��B�\B�PB_;BK�B7LB33B.BhBB��B�B�B�B�`B�`BɺB�?B�B��B�oB�VBy�BhsBZBM�B8RBDB
ɺB
�9B
�B
��B
��B
�{B
cTB
]/B
M�B
<jB
$�B

=B	�sB	�;B	�B	��B	ȴB	�^B	��B	l�B	VB	K�B	E�B	7LB	!�B	�B	
=B		7B	B��B��B��B�B�fB�TB�5B�)B�
B��B��B�}B�LB�3B�B�B�B��B��B��B��B��B��B��B�{B�uB�{B�uB�{B�uB�oB�bB�\B�\B�1B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B� B� B{�By�By�Bs�Bn�Bn�Bm�Bn�Bn�Bo�Bo�Bm�Bn�Bm�Bm�Bl�Bl�Bl�Bl�BjBk�Bk�BjBk�BjBk�Bk�BjBffBcTBcTBcTBbNBaHB^5B^5B[#BYBYBVBW
BS�BQ�BL�BL�BJ�BK�BI�BF�BF�BF�BE�BD�BC�BC�BB�BB�BB�BA�B@�B?}B>wB>wB?}B?}B>wB?}B>wB>wB>wB>wB=qB?}BB�B?}B>wB=qB<jB>wB@�BA�B?}B>wB?}B?}B@�B@�B@�B?}B?}B=qB?}B?}B?}B@�BC�BH�BJ�BL�BG�BH�BJ�BM�BQ�BVBXBYBYBZBZB\)B^5B_;B_;B`BB`BB_;B`BB`BB`BB`BBaHBbNBdZBffBffBgmBgmBffBgmBffBffBgmBffBffBgmBiyBk�Bm�Bp�Br�Br�Bu�Bz�B�B�hB��B�-B�LB�LB�XB�^B�qB��BBÖBǮB��B�ZB�B�B�B�B�B��B��B�B�B�B�HB��BɺB��B��B��B��B��B��B��B�)B�BB�HB�NB�TB�fB�B��B��B��B	%B	
=B	PB	VB	bB	�B	�B	 �B	$�B	'�B	(�B	)�B	+B	,B	+B	-B	&�B	'�B	,B	.B	5?B	7LB	9XB	<jB	A�B	C�B	D�B	F�B	G�B	H�B	H�B	J�B	N�B	P�B	R�B	R�B	VB	W
B	XB	XB	ZB	[#B	[#B	\)B	]/B	`BB	dZB	ffB	hsB	jB	m�B	n�B	q�B	t�B	w�B	y�B	|�B	}�B	~�B	�B	�1B	�1B	�%B	�+B	�JB	�\B	�bB	��B	�{B	��B	��B	��B	��B	�B	�'B	�?B	�RB	�^B	�dB	�wB	�}B	��B	��B	��B	B	ÖB	ŢB	ǮB	ǮB	ǮB	ǮB	ǮB	ɺB	ɺB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�/B	�/111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<e`B<#�
<#�
<#�
<#�
<#�
<�o<#�
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
<u<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447072012010314470720120103144707  AO  ARGQ                                                                        20111130141306  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141306  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144707  IP                  G�O�G�O�G�O�                