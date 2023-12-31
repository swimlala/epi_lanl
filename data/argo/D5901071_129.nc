CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:27Z UW 3.1 conversion   
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
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Kl   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mh   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  UP   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _4   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  g   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  x�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  z�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142115  20190522121827  1727_5046_129                   2C  D   APEX                            2143                            040306                          846 @Լg�I��1   @ԼhK� 	@7t9XbN�d�j~��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$�fD%fD%� D%��D&y�D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�3D�6fD�S3D�� D��D�#3D�ffD��3D��fD��D�\�D�� D���D�#3D�c3D�\�D��3D�  D�\�D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&�C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$fD$�fD%fD%� D%��D&y�D'  D'� D(  D(� D)  D)� D)��D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6y�D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�3D�6fD�S3D�� D��D�#3D�ffD��3D��fD��D�\�D�� D���D�#3D�c3D�\�D��3D�  D�\�D� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A��
A���A���A��#A��;A��#A��#A��mA��;A��
A�ƨA��PA�\)A�7LA�-A�-A�-A�+A�(�A�$�A��A�
=A���A��`A�A��A��9A��jA�A�ƨA��jA��RA�A���A��/A��;A��#A��A���A�ƨA�ĜA��wA��9A���A��DA�r�A�bNA�=qA��mA���A���A���A��A�ffA�5?A�&�A�&�A�+A�&�A� �A�A���A��\A��A�`BA���A�;dA�|�A��A��A�ƨA��A�?}A���A�
=A�C�A��PA��/A���A�jA�(�A�x�A���A��jA��`A�ffA�9XA�%A���A�dZA�7LA���A�t�A�hsA�Q�A�=qA�"�A�JA�%A��FA�l�A�VA�"�A�%A��A���A�p�A�\)A�I�A�?}A�9XA�1'A�+A��A�JA���A��A��mA���A���A�;dA���A���A�G�A���A���A�M�A��A�ZA��+A�{A�&�A���A��jA�ƨA���A�"�A�\)A���A�33A��`A��\A�%A�jA���A�  A��^A�bA��A��TA�ȴA�n�A��9A���A��A|��Ay�#Aw�Av-As��Ar�yArM�Aq|�Ap��Apv�Ap �Ao�An�HAm"�Ah�Ae��AahsA^ZAZ�/AY��AW��ASK�AN��AH�AG�AFȴAD�jADE�ACt�AB(�A@=qA>��A<�jA:1'A81'A7VA6�`A6�RA6�\A6ZA6-A5x�A41A1��A17LA0��A.��A+��A+p�A*ȴA*E�A)��A)+A&5?A#�A"$�A -A��A^5A�HA�mA�yA(�A�#A+A�`AĜA��A=qAK�A1'A�^AC�A�A��AffA&�AI�A�A|�A��A{A
��A	��A�+A��A��A�uA(�A�7AG�A�A�yAȴA�A+A��A �+A ^5A 9X@�C�@��@���@�Z@�(�@��F@���@���@�7L@��@���@��9@��@�M�@�J@�p�@��m@@�O�@�@�I�@�K�@�1@��@��@��@�@�F@���@�9X@�@���@���@٩�@�S�@�hs@�Q�@�V@���@��@̓u@�1'@�+@�x�@���@Ƨ�@�x�@�dZ@¸R@�v�@�$�@���@�j@��F@���@���@�dZ@���@���@�v�@�-@�@�@��@�-@�5?@�-@��h@��@�(�@�1@��P@��@��\@��T@���@�r�@� �@��m@�;d@��H@��+@�n�@�M�@��@�=q@��@�&�@��j@���@���@�I�@�\)@��y@���@�~�@�n�@�M�@�-@�J@�-@�n�@��+@�5?@�G�@�9X@��@�l�@�33@�+@��@���@�&�@�I�@���@��@�t�@�dZ@��@���@��@�V@�9X@���@�\)@�ff@�{@��@�X@�r�@��m@��F@�"�@��y@��!@�~�@�n�@�^5@��@���@��7@��@���@��@�l�@��y@�~�@�=q@���@�x�@��@��@���@�z�@�(�@��@���@�33@�@��H@���@�v�@�$�@�@�`B@�7L@�Ĝ@�z�@�1@��;@��w@�l�@�K�@�C�@��y@���@�v�@�V@�-@�{@��-@�p�@�V@�Ĝ@�9X@��@�|�@�+@�+@��H@��!@�v�@�5?@��@�@��@��h@�&�@��@���@��@��@���@���@�n�@�v�@�v�@��@���@��7@�hs@�`B@�X@�/@�&�@�V@���@���@���@��/@�bN@���@�ƨ@��@���@�dZ@�C�@��@��R@���@�Q�@~��@w�@o\)@b-@Y�@Q�#@J��@E�@@�u@:-@1�@-��@)��@%�h@!��@33@�@�!@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A��
A���A���A��#A��;A��#A��#A��mA��;A��
A�ƨA��PA�\)A�7LA�-A�-A�-A�+A�(�A�$�A��A�
=A���A��`A�A��A��9A��jA�A�ƨA��jA��RA�A���A��/A��;A��#A��A���A�ƨA�ĜA��wA��9A���A��DA�r�A�bNA�=qA��mA���A���A���A��A�ffA�5?A�&�A�&�A�+A�&�A� �A�A���A��\A��A�`BA���A�;dA�|�A��A��A�ƨA��A�?}A���A�
=A�C�A��PA��/A���A�jA�(�A�x�A���A��jA��`A�ffA�9XA�%A���A�dZA�7LA���A�t�A�hsA�Q�A�=qA�"�A�JA�%A��FA�l�A�VA�"�A�%A��A���A�p�A�\)A�I�A�?}A�9XA�1'A�+A��A�JA���A��A��mA���A���A�;dA���A���A�G�A���A���A�M�A��A�ZA��+A�{A�&�A���A��jA�ƨA���A�"�A�\)A���A�33A��`A��\A�%A�jA���A�  A��^A�bA��A��TA�ȴA�n�A��9A���A��A|��Ay�#Aw�Av-As��Ar�yArM�Aq|�Ap��Apv�Ap �Ao�An�HAm"�Ah�Ae��AahsA^ZAZ�/AY��AW��ASK�AN��AH�AG�AFȴAD�jADE�ACt�AB(�A@=qA>��A<�jA:1'A81'A7VA6�`A6�RA6�\A6ZA6-A5x�A41A1��A17LA0��A.��A+��A+p�A*ȴA*E�A)��A)+A&5?A#�A"$�A -A��A^5A�HA�mA�yA(�A�#A+A�`AĜA��A=qAK�A1'A�^AC�A�A��AffA&�AI�A�A|�A��A{A
��A	��A�+A��A��A�uA(�A�7AG�A�A�yAȴA�A+A��A �+A ^5A 9X@�C�@��@���@�Z@�(�@��F@���@���@�7L@��@���@��9@��@�M�@�J@�p�@��m@@�O�@�@�I�@�K�@�1@��@��@��@�@�F@���@�9X@�@���@���@٩�@�S�@�hs@�Q�@�V@���@��@̓u@�1'@�+@�x�@���@Ƨ�@�x�@�dZ@¸R@�v�@�$�@���@�j@��F@���@���@�dZ@���@���@�v�@�-@�@�@��@�-@�5?@�-@��h@��@�(�@�1@��P@��@��\@��T@���@�r�@� �@��m@�;d@��H@��+@�n�@�M�@��@�=q@��@�&�@��j@���@���@�I�@�\)@��y@���@�~�@�n�@�M�@�-@�J@�-@�n�@��+@�5?@�G�@�9X@��@�l�@�33@�+@��@���@�&�@�I�@���@��@�t�@�dZ@��@���@��@�V@�9X@���@�\)@�ff@�{@��@�X@�r�@��m@��F@�"�@��y@��!@�~�@�n�@�^5@��@���@��7@��@���@��@�l�@��y@�~�@�=q@���@�x�@��@��@���@�z�@�(�@��@���@�33@�@��H@���@�v�@�$�@�@�`B@�7L@�Ĝ@�z�@�1@��;@��w@�l�@�K�@�C�@��y@���@�v�@�V@�-@�{@��-@�p�@�V@�Ĝ@�9X@��@�|�@�+@�+@��H@��!@�v�@�5?@��@�@��@��h@�&�@��@���@��@��@���@���@�n�@�v�@�v�@��@���@��7@�hs@�`B@�X@�/@�&�@�V@���@���@���@��/@�bN@���@�ƨ@��@���@�dZ@�C�@��@��R@���@�Q�@~��@w�@o\)@b-@Y�@Q�#@J��@E�@@�u@:-@1�@-��@)��@%�h@!��@33@�@�!@�R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B� B~�Bz�Bx�Bw�Bw�Bw�Bw�Bw�Bw�Bw�By�By�By�Bw�Bs�Bs�Bv�By�B{�B}�B{�B{�B}�B�B�+B�1B�1B�7B�=B�DB�DB�JB�PB�PB�VB�VB�VB�PB�7B�1B�1B�1B�+B�+B�=B��B��B��B��B��B��B��B��B��B��B��B��B�PB�7B�+B�+B�bB��B��B��B�B�B�9B�LB�RB�RB�^B�wB�}B��B�wB�dB�XB�9B�-B�!B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�hB�7B� BiyBT�BG�B33B{B��B�ZB��B��B��Bz�BbNB]/BW
BK�B>wB0!B$�B�BVB
�B
��B
��B
z�B
aHB
N�B
9XB
 �B
DB	��B	�B	�yB	�HB	�/B	�B	��B	��B	��B	ɺB	ÖB	�-B	��B	�B	cTB	O�B	;dB	5?B	%�B	PB��B�ZB�;B�B��B��B��BǮB��B�jB�RB�^B�!B�-B�?B�XB�qB�wB�qB�dB�LB�?B�-B�B��B��B��B��B��B��B��B�\B�JB�+B�B}�B}�Bt�Br�Bp�Bo�Bn�Bm�Bl�Bl�BjBiyBgmBffBe`BcTBaHB_;B^5B]/B\)B\)BZBYBW
BW
BVBT�BQ�BQ�BP�BP�BO�BO�BN�BN�BL�BK�BI�BH�BH�BH�BG�BF�BF�BF�BF�BE�BD�BD�BC�BB�B@�B>wB;dB7LB6FB5?B33B1'B1'B33B33B33B2-B7LB8RB9XB:^B9XB9XB6FB;dB=qB:^BA�BO�BS�BP�BZB]/BaHBaHBcTBdZBffBhsBk�Bm�Bo�Bq�Bu�Bv�Bw�Bx�Bz�B|�B~�B�B�+B�VB�{B��B��B��B��B��B��B��B��B�B�!B�!B�B�B��B�B�-B�3B�^B�dB�^B�RB�RB�^B�^B�qBÖBɺB��B��B��B��B��B��B�B�B�#B�#B�#B�)B�)B�5B�BB�NB�NB�B�B�B�B�B�B�B��B��B	  B	+B	JB	PB	\B	\B	bB	�B	�B	�B	�B	�B	�B	�B	"�B	$�B	)�B	.B	2-B	33B	5?B	7LB	:^B	;dB	<jB	<jB	?}B	@�B	B�B	D�B	F�B	I�B	M�B	P�B	R�B	T�B	XB	[#B	_;B	`BB	`BB	dZB	gmB	iyB	k�B	m�B	o�B	o�B	q�B	r�B	t�B	w�B	z�B	{�B	~�B	�B	�B	�B	�%B	�1B	�7B	�=B	�DB	�PB	�VB	�\B	�bB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�'B	�'B	�'B	�3B	�FB	�jB	�jB	�}B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	��B
B

=B
�B
�B
#�B
-B
49B
<jB
D�B
J�B
O�B
R�B
ZB
_;B
dZB
jB
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B� B� B{�By�Bw�Bw�Bw�Bw�Bw�Bw�Bw�By�By�By�Bx�Bs�Bs�Bv�By�B{�B}�B{�B{�B}�B�B�+B�1B�1B�7B�=B�DB�DB�JB�VB�PB�\B�VB�\B�\B�=B�7B�1B�1B�1B�1B�=B��B��B��B��B��B��B��B��B��B�'B��B��B�\B�JB�JB�VB��B��B��B��B�B�-B�?B�RB�XB�^B�jB�}BBB�}B�jB�dB�?B�3B�3B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�B�B�'B�!B�B��B��B��B�uB�VB�+Bn�BXBM�B;dB�BB�yB�B�B��B�BdZB_;BZBN�BC�B33B&�B!�B�B
��B
�B
��B
�B
e`B
VB
@�B
&�B
hB
B	��B	�B	�TB	�;B	�#B	�B	��B	��B	��B	ȴB	��B	��B	�\B	m�B	ZB	?}B	;dB	49B	�B	%B�mB�HB�5B�B��B��B��BŢB��B�wB�}B�3B�3B�FB�^B�wB�}B�}B�}B�qB�LB�9B�?B�B��B��B��B��B��B��B��B�hB�PB�+B�%B�Bx�Bv�Bs�Bp�Bq�Bn�Bm�Bm�Bl�Bm�Bk�BhsBgmBffBe`BdZBcTB`BB^5B^5B\)B\)B[#BYBZBW
BT�BR�BQ�BR�BP�BP�BO�BO�BN�BN�BM�BL�BI�BI�BI�BH�BH�BG�BG�BF�BF�BF�BE�BD�BB�BB�B>wB8RB7LB6FB6FB33B33B49B33B5?B8RB:^B:^B:^B;dB;dB;dB:^B=qBA�B?}BB�BS�BW
BR�B]/BaHBdZBaHBdZBe`BhsBjBm�Bo�Br�Br�Bv�Bw�Bx�Bz�B{�B~�B�B�1B�1B�VB�{B��B��B��B��B��B��B��B��B�B�'B�!B�B�B��B�B�-B�3B�dB�jB�dB�XB�XB�^B�^B�jBĜB��B��B��B��B��B��B��B�B�#B�#B�#B�#B�)B�)B�5B�BB�NB�TB�B�B�B�B�B�B��B��B��B	B	1B	JB	PB	\B	bB	oB	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	+B	/B	2-B	49B	5?B	7LB	:^B	;dB	<jB	=qB	@�B	@�B	C�B	E�B	G�B	J�B	N�B	Q�B	S�B	VB	YB	\)B	_;B	`BB	aHB	e`B	hsB	jB	l�B	m�B	o�B	p�B	q�B	s�B	u�B	x�B	z�B	|�B	� B	�B	�B	�B	�+B	�1B	�7B	�DB	�JB	�PB	�VB	�\B	�bB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�-B	�3B	�'B	�3B	�FB	�qB	�qB	�}B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ƨB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B
B

=B
�B
�B
$�B
-B
49B
<jB
D�B
J�B
O�B
R�B
ZB
_;B
dZB
jB
o�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<D��<#�
<#�
<#�
<#�
<e`B<T��<u<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447192012010314471920120103144719  AO  ARGQ                                                                        20111130142115  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142115  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144719  IP                  G�O�G�O�G�O�                