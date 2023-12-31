CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:19Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               dA   AO  20111130141429  20190522121826  1727_5046_100                   2C  D   APEX                            2143                            040306                          846 @Ԗ�)��1   @Ԗ��}?�@6��-V�c�E����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�33A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  Dy�D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Di��Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�n�A�n�A�p�A�t�A�n�A�n�A�p�A�p�A�r�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�hsA�VA��;A���A�/A���A��A�r�A���A�x�A��TA�n�A�;dA�oA�\)A�G�A�~�A�E�A�{A�  A���A��9A���A��A�dZA� �A��A��9A��/A�jA�
=A��A��A�+A��A��A�XA��+A�5?A��A��DA��A�A���A�\)A���A�"�A���A�l�A��FA��yA�A�VA�"�A�7LA��A�5?A�VA�n�A��yA�dZA��!A�VA�/A��A�VA�1A��A�A�A��yA���A��A�A��7A�n�A��yA��-A�bNA�1A���A�`BA�VA�jA�^5A���A�oA�S�A��+A�hsA�v�A��RA�;dA�p�A��A�$�A�  A��A}��A|  Ay�#Ax{Av�/Av�uAu��AuhsAt~�AsoAqVAo��An�AmAm
=Akt�Ai�-Agp�AfQ�Ae�7Ad��Ab��A`5?A^I�A]`BA\ �AZAX�9AV�\AU%ASVAR^5AP�yAO�AN�`AM��AL��AK|�AI"�AG��AGAF��AFQ�AE��AEƨAE;dACC�ABĜAB�A@~�A>�A=��A<bA:bA8A5ƨA4r�A2��A2E�A1\)A0�A/�A-��A,�\A)C�A(9XA&�yA$��A#�^A"��A"bA!�PA!C�A �9A VA �Al�Az�A�^Ar�A|�A�AJA��A�;A&�A|�AjA �A�AbA�AC�A5?A��A��A��AA�A�;A��A/A�RAn�AM�A�A�A��A�DA^5A �AƨA
��A��A��AZA�A�yA^5A��A?}A
=AjA�hA �+@���@�bN@�o@��^@��F@��!@��^@��9@�K�@��@�"�@�G�@�j@��y@��@畁@�@�!@�9@�@�"�@ۥ�@�@ش9@թ�@ԓu@Ӆ@�j@��@Χ�@��@�I�@���@ˍP@ɉ7@�S�@���@�^5@���@��`@î@��@�J@��T@���@�G�@�t�@�n�@��@�ƨ@�S�@�v�@��u@�C�@�n�@���@�V@�z�@�Q�@���@��@��#@�hs@�/@���@�A�@�b@�  @��@��;@�ƨ@��w@��@�dZ@�+@��@���@��/@�p�@���@�J@��@�@��@��@���@��P@�S�@�
=@��y@��R@�~�@�M�@��@��T@��#@��T@�@�X@�/@��@��@��D@�A�@�1@��+@��@��@���@���@�K�@�J@���@��F@��\@��^@��9@�Z@�j@�9X@�A�@�1'@�1@��@�+@�ff@�5?@��@��@���@�I�@�  @�  @��@�(�@�9X@�9X@�b@�t�@�K�@�K�@�@�ȴ@��\@�V@�-@���@�X@��9@�Z@�Q�@�1'@�1@��m@���@�%@�z�@�b@���@�C�@�+@�+@�o@��@��+@��^@�p�@��@���@��j@���@�z�@� �@��;@��w@�l�@�+@���@�-@���@��@�hs@�O�@���@�r�@�b@��@� �@��m@��@�S�@�;d@�+@��y@���@�V@�-@�J@��@��-@�x�@�V@���@�%@�%@�%@�%@��@�?}@�hs@��7@�p�@�X@�O�@�X@�O�@�G�@�&�@��`@���@�Ĝ@��9@��u@�j@�Q�@�Q�@�9X@�(�@�b@��;@��F@��@��P@�dZ@�+@�@��y@�ȴ@��!@���@�v�@�ff@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�n�A�n�A�p�A�t�A�n�A�n�A�p�A�p�A�r�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�t�A�v�A�v�A�v�A�v�A�v�A�v�A�v�A�hsA�VA��;A���A�/A���A��A�r�A���A�x�A��TA�n�A�;dA�oA�\)A�G�A�~�A�E�A�{A�  A���A��9A���A��A�dZA� �A��A��9A��/A�jA�
=A��A��A�+A��A��A�XA��+A�5?A��A��DA��A�A���A�\)A���A�"�A���A�l�A��FA��yA�A�VA�"�A�7LA��A�5?A�VA�n�A��yA�dZA��!A�VA�/A��A�VA�1A��A�A�A��yA���A��A�A��7A�n�A��yA��-A�bNA�1A���A�`BA�VA�jA�^5A���A�oA�S�A��+A�hsA�v�A��RA�;dA�p�A��A�$�A�  A��A}��A|  Ay�#Ax{Av�/Av�uAu��AuhsAt~�AsoAqVAo��An�AmAm
=Akt�Ai�-Agp�AfQ�Ae�7Ad��Ab��A`5?A^I�A]`BA\ �AZAX�9AV�\AU%ASVAR^5AP�yAO�AN�`AM��AL��AK|�AI"�AG��AGAF��AFQ�AE��AEƨAE;dACC�ABĜAB�A@~�A>�A=��A<bA:bA8A5ƨA4r�A2��A2E�A1\)A0�A/�A-��A,�\A)C�A(9XA&�yA$��A#�^A"��A"bA!�PA!C�A �9A VA �Al�Az�A�^Ar�A|�A�AJA��A�;A&�A|�AjA �A�AbA�AC�A5?A��A��A��AA�A�;A��A/A�RAn�AM�A�A�A��A�DA^5A �AƨA
��A��A��AZA�A�yA^5A��A?}A
=AjA�hA �+@���@�bN@�o@��^@��F@��!@��^@��9@�K�@��@�"�@�G�@�j@��y@��@畁@�@�!@�9@�@�"�@ۥ�@�@ش9@թ�@ԓu@Ӆ@�j@��@Χ�@��@�I�@���@ˍP@ɉ7@�S�@���@�^5@���@��`@î@��@�J@��T@���@�G�@�t�@�n�@��@�ƨ@�S�@�v�@��u@�C�@�n�@���@�V@�z�@�Q�@���@��@��#@�hs@�/@���@�A�@�b@�  @��@��;@�ƨ@��w@��@�dZ@�+@��@���@��/@�p�@���@�J@��@�@��@��@���@��P@�S�@�
=@��y@��R@�~�@�M�@��@��T@��#@��T@�@�X@�/@��@��@��D@�A�@�1@��+@��@��@���@���@�K�@�J@���@��F@��\@��^@��9@�Z@�j@�9X@�A�@�1'@�1@��@�+@�ff@�5?@��@��@���@�I�@�  @�  @��@�(�@�9X@�9X@�b@�t�@�K�@�K�@�@�ȴ@��\@�V@�-@���@�X@��9@�Z@�Q�@�1'@�1@��m@���@�%@�z�@�b@���@�C�@�+@�+@�o@��@��+@��^@�p�@��@���@��j@���@�z�@� �@��;@��w@�l�@�+@���@�-@���@��@�hs@�O�@���@�r�@�b@��@� �@��m@��@�S�@�;d@�+@��y@���@�V@�-@�J@��@��-@�x�@�V@���@�%@�%@�%@�%@��@�?}@�hs@��7@�p�@�X@�O�@�X@�O�@�G�@�&�@��`@���@�Ĝ@��9@��u@�j@�Q�@�Q�@�9X@�(�@�b@��;@��F@��@��P@�dZ@�+@�@��y@�ȴ@��!@���@�v�@�ff@�j11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB%B�B9XBQ�B[#B[#B[#B[#B[#BVBQ�BN�BJ�BB�B8RB5?B5?B33B?}BS�BO�BYB]/B`BBhsBx�B�1B�VB�\B�hB��B��B��B��B�B��B��B��B��B�RB�LB�FB�!B�B��B��B�'B�B��B��B��B��B�bB�B�By�B_;BP�BH�BA�B7LB&�B�BJB%BB  B��B��B�B�mB��B�3B��B��B�+B~�Bv�Bp�BjB\)BM�BA�B6FB+B�BB
�B
�B
�NB
ɺB
�'B
��B
�oB
�1B
v�B
\)B
R�B
B�B
:^B
7LB
5?B
0!B
,B
%�B
�B
bB
1B
  B	��B	��B	�B	�NB	��B	��B	ŢB	�dB	�B	��B	�\B	�=B	�%B	}�B	t�B	s�B	l�B	XB	O�B	H�B	I�B	D�B	H�B	>wB	:^B	7LB	33B	49B	49B	2-B	0!B	.B	+B	#�B	�B	�B	1B	B��B�B�ZB�#B�
B��B��BƨB��B�^B�?B�-B��B��B�JB�+B~�Bz�By�Bx�Bx�Bw�Bw�Bv�Bu�Bu�Bu�Br�Bp�Bo�Bo�Bo�Bo�Bm�Bl�Bm�Bm�Bm�Bm�Bl�Bk�BjBjBjBiyBiyBiyBk�Bk�BjBiyBiyBhsBffBbNBcTBcTBbNBaHB`BB\)BYBXBT�BO�BN�BM�BN�BT�BT�BR�BO�BI�BB�B@�B=qB<jB;dB;dB:^B:^B9XB8RB6FB7LB7LB9XB9XB7LB5?B2-B.B-B,B.B.B-B1'B1'B1'B8RB>wB>wB?}BB�BB�BB�BC�BF�BG�BG�BG�BH�BJ�BK�BM�BN�BM�BN�BP�BQ�BVB\)B\)B]/B^5B^5B`BB`BB`BBaHBaHBaHBcTBffBgmBhsBl�Bp�Bs�Bv�Bx�Bz�B{�B|�B|�B}�B}�B�1B�hB��B��B��B��B��B�B�B�B�B�3B�?B�RB�^B�dB�wB��BƨB��B��B��B��B�B�#B�)B�/B�HB�TB�`B�mB�sB�sB�mB�sB�sB�yB�B�yB�mB�`B�ZB�ZB�fB�mB�B�B�B�B�B�B�B�B�B�B��B	B	%B	DB	JB	PB	VB	hB	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	#�B	'�B	)�B	)�B	)�B	+B	+B	(�B	&�B	&�B	'�B	(�B	/B	2-B	2-B	2-B	33B	5?B	=qB	?}B	B�B	C�B	C�B	D�B	D�B	E�B	F�B	G�B	H�B	J�B	L�B	O�B	R�B	T�B	T�B	VB	[#B	_;B	bNB	e`B	ffB	hsB	k�B	n�B	p�B	q�B	q�B	r�B	s�B	u�B	w�B	z�B	|�B	}�B	�B	�B	�%B	�1B	�1B	�=B	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�-B	�3B	�9B	�?B	�?B	�FB	�LB	�XB	�^B	�dB	�jB	�qB	�wB	��B	��B	��B	B	B	ĜB	ĜB	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BB+B�B?}B\)BcTBcTB^5B^5B^5BXBR�BO�BM�BG�B;dB6FB6FB8RBF�BXBR�B[#BbNBe`Bk�B|�B�DB�bB�bB��B��B��B��B��B�!B�B�!B��B�B�dB�wB�qB�9B�!B��B�B�9B�3B�B��B��B��B��B�DB�B�BcTBR�BJ�BD�B=qB,B�B\B1B+BB��B��B��B�B��B�XB��B��B�7B�Bx�Br�Bp�BaHBS�BD�B8RB.B �B+B
��B
�B
�sB
��B
�XB
��B
��B
�PB
�B
`BB
YB
G�B
=qB
8RB
7LB
2-B
/B
)�B
!�B
uB
JB
B	��B	��B	�B	�sB	�
B	��B	ȴB	��B	�3B	��B	�oB	�VB	�JB	�B	x�B	v�B	p�B	ZB	R�B	J�B	L�B	H�B	J�B	B�B	@�B	:^B	6FB	5?B	5?B	33B	1'B	0!B	0!B	$�B	!�B	�B	JB	B	B��B�yB�HB�#B�B��BɺBĜB�qB�XB�FB�-B��B�bB�PB�B}�B|�Bz�By�By�Bx�Bw�Bw�Bx�Bx�Bw�Bt�Bs�Bq�Bo�Bp�Bp�Br�Bq�Bn�Bm�Bm�Bm�Bn�Bn�Bl�Bk�Bl�Bl�BjBl�Bm�Bk�BjBjBiyBiyBhsBdZBdZBcTBbNBcTB`BBZBYB]/BQ�BP�BO�BO�BVBW
BVBS�BN�BE�BB�B@�B?}B=qB=qB<jB=qB=qB<jB9XB9XB:^B9XB<jB7LB6FB6FB49B2-B2-B2-B0!B2-B33B33B7LB;dB?}B@�BB�BB�BC�BF�BF�BG�BH�BH�BH�BJ�BK�BL�BM�BN�BN�BQ�BR�BS�BXB]/B^5B`BB`BB`BBaHBaHBaHBbNBcTBdZBdZBffBhsBiyBm�Bp�Bs�Bv�Bx�Bz�B{�B|�B}�B~�B�B�7B�hB��B��B��B��B��B�B�B�B�!B�9B�FB�RB�dB�jB�}B��BƨB��B��B��B�B�B�#B�)B�5B�NB�TB�mB�yB�B�yB�mB�yB�B�B�B�B�sB�mB�`B�ZB�fB�mB�B�B�B�B�B�B�B�B�B��B��B	B	%B	DB	JB	PB	VB	oB	�B	�B	�B	�B	 �B	 �B	 �B	"�B	$�B	$�B	(�B	)�B	)�B	)�B	,B	/B	+B	'�B	'�B	(�B	)�B	/B	2-B	2-B	2-B	49B	6FB	=qB	@�B	B�B	C�B	C�B	D�B	E�B	E�B	F�B	H�B	H�B	K�B	M�B	P�B	R�B	T�B	T�B	W
B	\)B	`BB	bNB	e`B	ffB	iyB	k�B	n�B	p�B	q�B	r�B	s�B	s�B	u�B	w�B	z�B	|�B	~�B	�B	�B	�%B	�1B	�1B	�=B	�PB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�'B	�'B	�-B	�3B	�9B	�?B	�?B	�FB	�LB	�XB	�^B	�dB	�jB	�wB	�wB	��B	��B	��B	B	B	ĜB	ĜB	��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447092012010314470920120103144709  AO  ARGQ                                                                        20111130141429  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141429  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144709  IP                  G�O�G�O�G�O�                