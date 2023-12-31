CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:03Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               +A   AO  20111130140034  20190522121825  1727_5046_043                   2C  D   APEX                            2143                            040306                          846 @�M?W� 1   @�M?�5 @7h�9Xb�c�����1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dy� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@9��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CI�fCL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dqy�Dr  Dr� Ds  Ds� Dy� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A˥�A˝�AˋDA�jA�?}A�/A�(�A�&�A�$�A�"�A��A��A��A��A��A��A��A��A��A�bA�JA�JA�JA�1A�%A�A�A�A�A�A���A���A���A��A��A��A��`A��
Aʺ^AǗ�A�(�A��hA��A��A�"�A��A�E�A���A���A��RA��HA���A���A�M�A��^A���A�XA�dZA��A�=qA���A�~�A��+A���A���A��wA��A�x�A�I�A�JA��\A�=qA�ffA���A���A�A��^A�|�A�~�A��
A�z�A�dZA��9A��9A���A�1'A���A��HA�^5A�ĜA���A��\A��A�VA��!A���A�"�A���A���A�ZA��TA��RA���A�E�A�
=A�M�A�G�A���A��jA�r�A���A��A}p�Az��Ax�AvE�Ar-Ao��Ak��AiAg|�AedZAb�Aa�7A`��A`A�A_�7A]��A\�jA\ �AZ�jAW��AW/AVM�AU�#AU��AU�ATz�AS�AR�+APz�AN�/ANZAM33AL�AKS�AJjAI�TAHjAG%AF�+AE��ADȴADbNACO�AB��AB{A@�A?��A=�hA<1'A;\)A:E�A8��A8{A7�hA7`BA7�A5�#A4�DA3C�A2��A1��A0z�A/dZA.=qA-�wA,�RA*�HA*n�A)33A(��A(�A(M�A'ƨA'"�A&-A%t�A$�A#��A"��A!�wA!G�A ĜA �A��A��A�HA�A��AXA��AJAx�A?}A�A-A&�A�\A9XAl�AZA��AE�A�A��AG�A�!AJA�wA��A%A
�!A
bNA
9XA
  A	O�AbA+A�A�A�PAVA �AXA 1@�E�@���@�S�@��-@��@�o@�n�@���@�ff@�O�@���@��@�5?@��T@���@�|�@�hs@�hs@�  @�?}@�!@�@��@�K�@ݙ�@ܣ�@��m@�33@��@�j@��y@ְ!@թ�@�b@�t�@ҟ�@�{@�p�@��/@�ff@��@�(�@�+@�ff@ɑh@�9X@��y@�5?@�O�@���@��@�@�p�@�Q�@��P@��!@��j@�o@�n�@�=q@�Ĝ@�o@�`B@���@�|�@�33@�o@���@�V@��@���@��j@���@�K�@���@�-@���@�?}@���@��w@���@��`@��@�l�@�33@�-@��@�/@�I�@�1@��@�@���@��!@�v�@��@��^@��7@��@��u@�9X@���@�E�@���@�p�@�G�@���@��`@��j@��D@�I�@�1'@�(�@��@�1@���@�ƨ@��@��@��y@�^5@��^@�?}@��j@�z�@�r�@�  @��@���@�dZ@��@���@���@���@��7@�`B@��@�V@���@��@�9X@��;@��@�S�@��@���@�E�@�@�@���@��7@��h@��-@��@�J@��@�{@�v�@�$�@��#@��h@�/@���@���@���@�Ĝ@��9@��u@��j@���@�r�@�I�@��@��@��
@��@�dZ@�K�@��y@��\@�M�@��@���@��h@�7L@��@��D@�j@�bN@�bN@�Z@�I�@�A�@�1'@�(�@�b@��F@��@�t�@�t�@�C�@�"�@���@���@��y@���@���@��R@���@�M�@�-@�$�@���@��h@�&�@�%@�Q�@��
@���@��@�l�@�K�@�@���@���@�V@�=q@��T@��^@��h@�hs@�?}@�&�@�Ĝ@�r�@�Q�@�A�@�(�@�  @���@�|�@�S�@���@�~�@�M�@�=q@�$�@�{@�@���@��T@}��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A˥�A˝�AˋDA�jA�?}A�/A�(�A�&�A�$�A�"�A��A��A��A��A��A��A��A��A��A�bA�JA�JA�JA�1A�%A�A�A�A�A�A���A���A���A��A��A��A��`A��
Aʺ^AǗ�A�(�A��hA��A��A�"�A��A�E�A���A���A��RA��HA���A���A�M�A��^A���A�XA�dZA��A�=qA���A�~�A��+A���A���A��wA��A�x�A�I�A�JA��\A�=qA�ffA���A���A�A��^A�|�A�~�A��
A�z�A�dZA��9A��9A���A�1'A���A��HA�^5A�ĜA���A��\A��A�VA��!A���A�"�A���A���A�ZA��TA��RA���A�E�A�
=A�M�A�G�A���A��jA�r�A���A��A}p�Az��Ax�AvE�Ar-Ao��Ak��AiAg|�AedZAb�Aa�7A`��A`A�A_�7A]��A\�jA\ �AZ�jAW��AW/AVM�AU�#AU��AU�ATz�AS�AR�+APz�AN�/ANZAM33AL�AKS�AJjAI�TAHjAG%AF�+AE��ADȴADbNACO�AB��AB{A@�A?��A=�hA<1'A;\)A:E�A8��A8{A7�hA7`BA7�A5�#A4�DA3C�A2��A1��A0z�A/dZA.=qA-�wA,�RA*�HA*n�A)33A(��A(�A(M�A'ƨA'"�A&-A%t�A$�A#��A"��A!�wA!G�A ĜA �A��A��A�HA�A��AXA��AJAx�A?}A�A-A&�A�\A9XAl�AZA��AE�A�A��AG�A�!AJA�wA��A%A
�!A
bNA
9XA
  A	O�AbA+A�A�A�PAVA �AXA 1@�E�@���@�S�@��-@��@�o@�n�@���@�ff@�O�@���@��@�5?@��T@���@�|�@�hs@�hs@�  @�?}@�!@�@��@�K�@ݙ�@ܣ�@��m@�33@��@�j@��y@ְ!@թ�@�b@�t�@ҟ�@�{@�p�@��/@�ff@��@�(�@�+@�ff@ɑh@�9X@��y@�5?@�O�@���@��@�@�p�@�Q�@��P@��!@��j@�o@�n�@�=q@�Ĝ@�o@�`B@���@�|�@�33@�o@���@�V@��@���@��j@���@�K�@���@�-@���@�?}@���@��w@���@��`@��@�l�@�33@�-@��@�/@�I�@�1@��@�@���@��!@�v�@��@��^@��7@��@��u@�9X@���@�E�@���@�p�@�G�@���@��`@��j@��D@�I�@�1'@�(�@��@�1@���@�ƨ@��@��@��y@�^5@��^@�?}@��j@�z�@�r�@�  @��@���@�dZ@��@���@���@���@��7@�`B@��@�V@���@��@�9X@��;@��@�S�@��@���@�E�@�@�@���@��7@��h@��-@��@�J@��@�{@�v�@�$�@��#@��h@�/@���@���@���@�Ĝ@��9@��u@��j@���@�r�@�I�@��@��@��
@��@�dZ@�K�@��y@��\@�M�@��@���@��h@�7L@��@��D@�j@�bN@�bN@�Z@�I�@�A�@�1'@�(�@�b@��F@��@�t�@�t�@�C�@�"�@���@���@��y@���@���@��R@���@�M�@�-@�$�@���@��h@�&�@�%@�Q�@��
@���@��@�l�@�K�@�@���@���@�V@�=q@��T@��^@��h@�hs@�?}@�&�@�Ĝ@�r�@�Q�@�A�@�(�@�  @���@�|�@�S�@���@�~�@�M�@�=q@�$�@�{@�@���@��T@}��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�yB�yB�sB�sB�yB�sB�mB�`B�
B�/B�#B�B�B�B��BǮB�qB�XB�RB�?B�RB�XB�LB�3B�-B�B��B��B�B�B�B�B��B�{B�=B�+B�B� By�Br�Bm�BdZB^5B^5B[#BVBQ�BF�B@�B:^B7LB+BbBB��B��B�B�fB��B�LB��Bq�BaHBYBVBH�B)�B
��B
�B
�mB
��B
��B
�9B
��B
�1B
x�B
t�B
n�B
iyB
[#B
E�B
5?B
$�B
{B
B	�B	�#B	�qB	�!B	��B	��B	�7B	�B	~�B	{�B	v�B	o�B	jB	gmB	`BB	XB	VB	Q�B	O�B	N�B	K�B	H�B	D�B	>wB	7LB	1'B	0!B	/B	-B	+B	)�B	&�B	�B	�B	�B	�B	{B	uB	{B	�B	{B	\B	1B	B��B��B�B�yB�fB�TB�NB�BB�B�B��B��B��BǮBĜBÖB��B�dB�FB�?B�3B�-B�!B�B�B�B��B��B��B��B��B��B��B��B��B�oB�\B�PB�JB�DB�=B�1B�1B�B�B�B� B|�B{�By�Bv�Br�Bs�Bp�Bo�Bo�Bm�Bk�BjBhsBdZB_;B]/B\)B[#BYBVBR�BP�BM�BJ�BH�BF�BD�BA�B?}B>wB<jB:^B8RB7LB6FB5?B2-B1'B1'B0!B0!B1'B1'B0!B1'B7LB8RB8RB2-B/B+B(�B&�B'�B'�B'�B(�B(�B'�B(�B(�B&�B'�B'�B)�B)�B)�B(�B+B-B-B.B/B/B0!B2-B33B49B49B49B5?B:^B<jB>wB>wB?}BD�BE�BE�BD�BE�BJ�BL�BL�BM�BL�BL�BM�BM�BM�BM�BQ�BR�BS�BXBZBZB\)B_;B`BBbNBdZBk�Bm�Bn�Bq�Br�Bv�Bw�By�B|�B}�B}�B~�B�B�B�B�=B�7B�=B�uB��B��B��B��B��B��B��B��B�B�-B�?B�^B��BBĜBŢBŢBǮB��B��B�B�/B�BB�HB�ZB�mB�sB�B�B�B�B��B��B��B��B��B��B	B	B	+B	
=B	PB	\B	oB	�B	�B	�B	�B	#�B	$�B	'�B	0!B	2-B	49B	:^B	A�B	A�B	D�B	I�B	K�B	N�B	T�B	XB	YB	[#B	]/B	aHB	cTB	dZB	e`B	gmB	iyB	jB	l�B	m�B	n�B	p�B	r�B	s�B	t�B	u�B	x�B	{�B	~�B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�+B	�=B	�DB	�DB	�PB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�9B	�FB	�LB	�XB	�dB	�dB	�qB	�qB	�wB	�}B	�}B	��B	B	ĜB	ŢB	ŢB	ǮB	ȴB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�yB�yB�yB�sB�sB�yB�sB�mB�B�HB�NB�ZB�)B�#B�/B��B��BB�qB�dB�RB�dB�^B�XB�3B�3B�-B�B�B�'B�-B�3B�3B��B��B�PB�=B�B�B|�Bt�Br�BffB`BBaHB]/BXBW
BI�BB�B;dB;dB6FB�B+B��B��B��B�B��B�wB�Bv�BcTBZBZBQ�B:^BB
��B
�B
�
B
ŢB
�^B
��B
�PB
z�B
u�B
p�B
m�B
bNB
L�B
;dB
)�B
�B
JB	�B	�`B	ĜB	�9B	�B	��B	�JB	�B	�B	}�B	z�B	s�B	l�B	k�B	hsB	ZB	YB	R�B	P�B	P�B	M�B	K�B	H�B	E�B	<jB	33B	49B	2-B	1'B	.B	+B	+B	"�B	�B	�B	�B	�B	�B	�B	�B	�B	oB	PB	B��B��B�B�B�mB�ZB�TB�ZB�5B�B��B��B��B��BǮBŢBÖB��B�RB�XB�9B�3B�'B�'B�B�B�B��B��B��B��B��B��B��B��B��B�oB�bB�PB�JB�DB�DB�7B�%B�B�B�B~�B|�B{�By�Bv�Bt�Bq�Bp�Bp�Bo�Bm�Bk�BjBiyBe`B^5B]/B\)B[#BZBVBR�BQ�BO�BJ�BI�BG�BE�BB�BA�B>wB=qB;dB9XB7LB8RB6FB33B49B2-B1'B2-B33B33B5?B7LB;dB=qB7LB/B-B,B)�B)�B)�B)�B+B,B+B)�B+B)�B(�B)�B+B+B+B-B-B.B/B/B0!B1'B2-B33B49B5?B6FB6FB7LB<jB=qB@�BA�BB�BE�BF�BG�BG�BH�BL�BM�BM�BM�BM�BM�BN�BN�BO�BO�BR�BS�BT�BYB[#B[#B^5BcTBaHBdZBe`Bl�Bo�Bo�Br�Bt�Bw�Bx�Bz�B}�B}�B~�B� B�B�B�B�=B�=B�JB�{B��B��B��B��B��B��B��B��B�B�-B�?B�^B��BBĜBŢBƨBȴB��B��B�B�5B�BB�NB�`B�mB�yB�B�B�B�B��B��B��B��B��B��B	B	B	1B	
=B	VB	bB	uB	�B	�B	�B	�B	#�B	$�B	'�B	0!B	2-B	49B	:^B	B�B	A�B	E�B	J�B	K�B	N�B	T�B	XB	YB	[#B	]/B	aHB	cTB	dZB	e`B	gmB	iyB	k�B	l�B	m�B	o�B	q�B	r�B	s�B	u�B	u�B	y�B	|�B	� B	�B	�B	�B	�B	�B	�B	�B	�B	�%B	�1B	�=B	�DB	�DB	�PB	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�?B	�LB	�LB	�^B	�dB	�jB	�qB	�qB	�wB	�}B	�}B	��B	ÖB	ĜB	ŢB	ŢB	ǮB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�`11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446492012010314464920120103144649  AO  ARGQ                                                                        20111130140034  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140034  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144649  IP                  G�O�G�O�G�O�                