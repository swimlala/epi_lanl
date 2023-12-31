CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:06Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               8A   AO  20111130140344  20190522121826  1727_5046_056                   2C  D   APEX                            2143                            040306                          846 @�^h	1   @�^��@76E�����c��l�C�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B?��BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Dy` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B?��BH  BPffBX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C-�fC0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DWy�DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp�fDq  Dq� Dr  Dr� Ds  Dy` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA���A���A��A��FA��A�S�A�A�A���A��;A���A���A���A�ȴA��jA��9A��!A���A���A���A���A���A���A���A��A�t�A�`BA�^5A�ZA�XA�O�A�K�A�E�A�C�A�1'A��A�VA�
=A�A��`A��RA��
A���A�bA�S�A�/A�M�A���A��TA�{A��A�ƨA�oA���A�A�hsA�{A��A�bA���A�`BA���A���A�bNA�A�A�jA�A���A�O�A���A��A�C�A��A��FA��DA�^5A�A�A�(�A�
=A��A���A��/A�v�A�XA�z�A��!A��-A���A�dZA���A��;A�K�A��PA���A�C�A��RA�(�A���A��+A~��Ayx�AvȴAv(�Au�^At�As�-ArȴArVArAq�ApJAnI�Al�Ah�`Af9XAel�AdbNAb�Ab��Ab�Aa�A`v�A^��A^M�A]�mA]�A]C�A\ĜAZ(�AY;dAV�AT�/AS�AO&�AKt�AJAG�PAF �AD~�AC�^ABM�A@n�A=
=A;
=A:9XA9�#A9XA8��A7`BA5
=A4��A4A2��A1��A1�PA1
=A/�A.�A-�7A,�A+C�A)O�A(�A(��A(�RA(�DA(  A&��A&Q�A%��A$�yA$r�A#�PA"Q�A 5?A�FA"�A�RAn�A�TAĜA�uA^5AE�A��A33A��AffA-A�A�
A�A�RA=qAt�A�A�\A1At�AhsA?}A�yA=qA$�A�^AĜA5?A�;A�A�DAQ�A �A��A"�A �AhsAVA��A��AZA��A
�`A	�FA��A�uA�;AhsA7LA&�A�DA�7A~�A�A�FA��A�`A�;A ��@�5?@�^5@��+@�{@��u@��`@�+@�1'@�b@�l�@���@�Z@�G�@�/@��D@��@�$�@�Ĝ@���@ڟ�@�X@؛�@���@���@�$�@�p�@�r�@�bN@�Q�@�I�@�9X@��@�&�@ϕ�@�S�@��@�@��@͉7@ˍP@�~�@��/@�j@���@��@�x�@�O�@��@�Ĝ@å�@�ȴ@�?}@�+@�M�@���@�O�@�V@���@��u@�1@��!@��^@���@�I�@�1'@�M�@���@��^@��h@�%@��9@��;@��+@�V@���@�`B@�&�@��`@��D@�-@��@�
=@��T@�@�x�@�@� �@�
=@�+@�S�@� �@�Q�@��u@��`@�V@���@��@�r�@���@�O�@��@�O�@��^@���@�|�@� �@���@�Z@��u@�V@�1'@�{@��P@�ff@�M�@�&�@��@�/@��7@���@�5?@���@�I�@���@�\)@�S�@��@��m@���@���@�+@���@��!@�J@�Ĝ@��9@�O�@�`B@�`B@���@���@�5?@�&�@�t�@��H@�
=@��H@���@��@�~�@�%@��j@��@�z�@�Z@���@��
@�b@��9@�r�@���@�  @���@�v�@�E�@�G�@�1@���@��@��`@��j@��D@�bN@�I�@�9X@�1@��w@�\)@�33@�K�@�ȴ@�E�@���@���@��7@�7L@�V@���@��u@��D@�z�@�1'@�1@��m@���@��P@��@�|�@�dZ@�K�@��@��!@���@�v�@�E�@�$�@�{@�@��@��#@���@��^@��-@���@���@���@��h@�V@��@�Z@�I�@�9X@� �@���@���@�l�@�
=@��@���@���@��\@�~�@�v�@�v�@�n�@�M�@�-@�{@��T@��T@��#@��#@���@���@��@�hs@�?}@~�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��TA���A���A��A��FA��A�S�A�A�A���A��;A���A���A���A�ȴA��jA��9A��!A���A���A���A���A���A���A���A��A�t�A�`BA�^5A�ZA�XA�O�A�K�A�E�A�C�A�1'A��A�VA�
=A�A��`A��RA��
A���A�bA�S�A�/A�M�A���A��TA�{A��A�ƨA�oA���A�A�hsA�{A��A�bA���A�`BA���A���A�bNA�A�A�jA�A���A�O�A���A��A�C�A��A��FA��DA�^5A�A�A�(�A�
=A��A���A��/A�v�A�XA�z�A��!A��-A���A�dZA���A��;A�K�A��PA���A�C�A��RA�(�A���A��+A~��Ayx�AvȴAv(�Au�^At�As�-ArȴArVArAq�ApJAnI�Al�Ah�`Af9XAel�AdbNAb�Ab��Ab�Aa�A`v�A^��A^M�A]�mA]�A]C�A\ĜAZ(�AY;dAV�AT�/AS�AO&�AKt�AJAG�PAF �AD~�AC�^ABM�A@n�A=
=A;
=A:9XA9�#A9XA8��A7`BA5
=A4��A4A2��A1��A1�PA1
=A/�A.�A-�7A,�A+C�A)O�A(�A(��A(�RA(�DA(  A&��A&Q�A%��A$�yA$r�A#�PA"Q�A 5?A�FA"�A�RAn�A�TAĜA�uA^5AE�A��A33A��AffA-A�A�
A�A�RA=qAt�A�A�\A1At�AhsA?}A�yA=qA$�A�^AĜA5?A�;A�A�DAQ�A �A��A"�A �AhsAVA��A��AZA��A
�`A	�FA��A�uA�;AhsA7LA&�A�DA�7A~�A�A�FA��A�`A�;A ��@�5?@�^5@��+@�{@��u@��`@�+@�1'@�b@�l�@���@�Z@�G�@�/@��D@��@�$�@�Ĝ@���@ڟ�@�X@؛�@���@���@�$�@�p�@�r�@�bN@�Q�@�I�@�9X@��@�&�@ϕ�@�S�@��@�@��@͉7@ˍP@�~�@��/@�j@���@��@�x�@�O�@��@�Ĝ@å�@�ȴ@�?}@�+@�M�@���@�O�@�V@���@��u@�1@��!@��^@���@�I�@�1'@�M�@���@��^@��h@�%@��9@��;@��+@�V@���@�`B@�&�@��`@��D@�-@��@�
=@��T@�@�x�@�@� �@�
=@�+@�S�@� �@�Q�@��u@��`@�V@���@��@�r�@���@�O�@��@�O�@��^@���@�|�@� �@���@�Z@��u@�V@�1'@�{@��P@�ff@�M�@�&�@��@�/@��7@���@�5?@���@�I�@���@�\)@�S�@��@��m@���@���@�+@���@��!@�J@�Ĝ@��9@�O�@�`B@�`B@���@���@�5?@�&�@�t�@��H@�
=@��H@���@��@�~�@�%@��j@��@�z�@�Z@���@��
@�b@��9@�r�@���@�  @���@�v�@�E�@�G�@�1@���@��@��`@��j@��D@�bN@�I�@�9X@�1@��w@�\)@�33@�K�@�ȴ@�E�@���@���@��7@�7L@�V@���@��u@��D@�z�@�1'@�1@��m@���@��P@��@�|�@�dZ@�K�@��@��!@���@�v�@�E�@�$�@�{@�@��@��#@���@��^@��-@���@���@���@��h@�V@��@�Z@�I�@�9X@� �@���@���@�l�@�
=@��@���@���@��\@�~�@�v�@�v�@�n�@�M�@�-@�{@��T@��T@��#@��#@���@���@��@�hs@�?}@~�+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB{�B|�B{�B{�B{�Bz�Bz�By�Bx�Bx�Bw�Bw�Bu�Bm�By�By�By�By�Bx�Bx�Bx�Bx�Bx�Bw�Bw�Bv�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bu�Bt�Bs�Bs�Br�Bo�Bk�BcTBk�By�B{�B}�B�%B�1B�hB�{B�uB�hB�\B�hB�\B�VB�JB�+B�B|�Bt�Bk�BbNB[#BN�BC�B=qB7LB%�B!�B�B�BhBVBDB1B%BBB  B��B�B��B�B�7BffB2-B�BB
�TB
��B
��B
w�B
jB
cTB
ZB
N�B
E�B
2-B	��B	��B	�}B	�dB	�FB	�!B	��B	��B	��B	��B	�{B	�1B	z�B	o�B	\)B	N�B	H�B	B�B	@�B	?}B	=qB	;dB	:^B	6FB	49B	33B	2-B	33B	33B	$�B	�B	{B	%B��B�B�;B��B��BȴBŢB��B�qB�RB�-B�'B�'B�!B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�hB�VB�JB�=B�7B�DB�DB��B��B��B��B��B�3B�dB�wB�wB�jB�^B�LB�?B�9B�LB�^B�qB�wB�jB�^B�jB�qBŢB��B�XB�'B��B��B��B��B�oB�+Bx�Br�Br�Bo�Bk�Be`B[#BP�BN�BK�BF�BC�BB�BC�BA�BD�BE�BE�BC�BA�B@�B@�B@�B@�B?}BB�BB�BG�BI�BJ�BJ�BI�BG�BG�BG�BK�BM�BN�BO�BP�BP�BP�BO�BM�BM�BN�BP�BP�BS�BT�BZB^5BbNBffBjBp�Bz�B� B�1B�{B��B��B��B��B��B��B��B��B��B�-B�FB�LB�FB�B��B�B�LB�XB�^B�wB��BŢBɺB��B�;B�TB�fB�sB�B�yB�sB�NB��B��B��B�B�#B�ZB�B��B��B	B	+B	uB	�B	,B	9XB	8RB	?}B	A�B	C�B	F�B	J�B	O�B	R�B	XB	bNB	aHB	_;B	_;B	\)B	ZB	ZB	YB	]/B	`BB	_;B	]/B	[#B	^5B	cTB	ffB	gmB	ffB	iyB	gmB	aHB	_;B	cTB	hsB	gmB	e`B	_;B	XB	S�B	VB	YB	[#B	\)B	]/B	dZB	jB	n�B	s�B	z�B	}�B	u�B	m�B	l�B	k�B	gmB	gmB	jB	k�B	l�B	l�B	l�B	l�B	l�B	l�B	l�B	n�B	p�B	w�B	z�B	~�B	� B	�B	�B	�1B	�7B	�VB	�\B	�\B	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�?B	�?B	�?B	�9B	�LB	�wB	B	B	ÖB	ÖB	ĜB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B{�B|�B{�B{�B|�B{�Bz�Bz�By�Bx�Bw�Bw�Bu�Bm�By�By�By�By�Bx�Bx�Bx�Bx�Bx�Bw�Bw�Bv�Bw�Bw�Bw�Bw�Bw�Bv�Bv�Bv�Bu�Bt�Bs�Bs�Br�Bp�Bn�Bk�Bv�B|�B�B�B�=B�bB��B��B�{B��B��B�{B�oB�bB�oB�JB�B�Bw�Bq�Be`BbNBS�BF�B@�B?}B'�B#�B�B�BoB\BJB	7B%BBBB��B�B�B�9B�uBu�B7LB&�BDB
�sB
�;B
�-B
|�B
l�B
ffB
]/B
R�B
M�B
H�B
B	�B	��B	�jB	�RB	�3B	��B	��B	��B	��B	��B	�JB	~�B	x�B	cTB	P�B	K�B	F�B	A�B	A�B	@�B	=qB	>wB	8RB	5?B	49B	33B	5?B	:^B	'�B	&�B	�B	DB	1B��B�`B�)B��B��BȴBƨBĜBÖB�RB�3B�-B�'B�!B�!B�B�B�B�B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�PB�PB�=B�JB�=B��B��B�B�B��B�3B�jB��B��B�}B�jB�RB�LB�?B�RB�^B�}B��B�}B�dB�qB�qBɺBĜB�jB�LB��B��B��B��B��B�bB}�Br�Bs�Br�Bn�Bk�BbNBQ�BO�BN�BH�BE�BD�BE�BB�BE�BG�BF�BD�BB�B@�B@�B@�B@�BA�BE�BD�BH�BI�BJ�BJ�BK�BJ�BI�BI�BL�BO�BP�BP�BP�BQ�BQ�BQ�BO�BP�BQ�BQ�BQ�BT�BVB[#B_;BcTBhsBl�Br�B{�B�B�DB��B��B��B��B��B��B��B��B��B��B�-B�FB�RB�XB�!B�B�!B�LB�XB�^B��BBŢBɺB��B�;B�TB�fB�sB�B�yB�yB�B�B��B��B��B�B�TB�B��B	  B	  B	B	bB	�B	)�B	;dB	8RB	A�B	A�B	C�B	E�B	I�B	O�B	Q�B	T�B	cTB	bNB	_;B	cTB	_;B	[#B	ZB	ZB	]/B	bNB	`BB	_;B	[#B	]/B	cTB	ffB	hsB	iyB	l�B	iyB	dZB	`BB	cTB	hsB	hsB	hsB	dZB	[#B	T�B	VB	YB	[#B	]/B	]/B	dZB	iyB	n�B	s�B	{�B	�B	y�B	m�B	m�B	m�B	jB	hsB	jB	k�B	l�B	l�B	l�B	l�B	l�B	m�B	m�B	n�B	p�B	x�B	{�B	� B	� B	�B	�%B	�1B	�=B	�VB	�\B	�\B	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�3B	�9B	�?B	�?B	�?B	�?B	�RB	�wB	B	B	ÖB	ÖB	ŢB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�Z111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<#�
<#�
<#�
<#�
<e`B<u<#�
<#�
<#�
<#�
<#�
<#�
<�9X<e`B<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446532012010314465320120103144653  AO  ARGQ                                                                        20111130140344  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140344  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144653  IP                  G�O�G�O�G�O�                