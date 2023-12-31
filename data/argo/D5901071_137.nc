CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:29Z UW 3.1 conversion   
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
_FillValue                 �  A4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  T,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  e<   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  g$   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  vL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  x4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �D   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �t   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �t   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �t   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �t   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142301  20190522121827  1727_5046_137                   2C  D   APEX                            2143                            040306                          846 @�ƽ��@1   @�ƾ�Y@@7��1'�d�Q�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dg��Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�33A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D>��D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� DgfDg� Dg��Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A���A��A��A���A���A��`A�Aď\AîADA���A�E�A�(�A�bA�  A��
A��jA��A���A��\A��DA��A�n�A�^5A�VA�+A��A�ĜA�1A�|�A�dZA�JA�ƨA�dZA�&�A�{A�JA���A��A�hsA�-A��A���A��;A���A��-A�t�A�bNA�VA�33A��/A�K�A��A���A�?}A��A���A���A��A�`BA�&�A��A��A���A�E�A��A��FA��A�ƨA���A�bNA�JA��A�
=A��A�VA��A���A��HA�;dA�oA�p�A��`A�{A��TA��HA��A���A�hsA��wA�S�A���A�`BA���A�`BA�1A�ZA��A��A��mA��mA��A��A�dZA���A��HA���A���A��DA���A���A�9XA�r�A�l�A��A�bA�-A���A���A�n�A��TA��-A�n�A� �A���A�{A~ZA{�Ay�hAx�!Aw�FAvffAtI�Ap��Anz�Al��Ak�Ajr�Ah�Ag�Af�yAeS�Ad�Acp�AaS�A_dZA]�A[�
A[?}AZ�AX �AVjAT��AR�AR5?AQ��AQG�AO\)AM�AL��AK��AJ��AI��AIp�AIC�AHn�AGG�AF�AF  AD~�AC��ACoABn�A@�RA>�A=
=A:�\A9��A8�yA7|�A6��A69XA5G�A3\)A0-A.VA-��A-O�A,��A+�
A+�A*�yA*bNA)�#A(��A&��A%�A$��A#�
A#oA"E�A ��A�A�hA�A  Al�A33Av�AA��A�Ax�A��AM�A�At�AoAffA�^At�A�A��A�AoA�jAVA �A��AVAM�A�
A�AhsA �A
��A
�DA�`Az�A5?Al�A �A+A��A;dA��A�A"�@���@�X@���@��T@���@�A�@���@�1'@�+@�7L@�D@��@�\@蛦@�ff@���@�@�^@�-@��@�/@�9X@�v�@�j@�;d@�E�@ݡ�@��`@�33@�^5@ى7@ى7@ف@�x�@�x�@�O�@�1'@���@�G�@�ff@�?}@�A�@�1@υ@�^5@��
@ˮ@ˍP@�l�@���@���@�x�@ȴ9@Ǯ@��@��m@�-@��@��`@��F@�J@��h@���@�+@�`B@��@�b@��@�@���@��-@���@�Z@�I�@��@�\)@���@�r�@��@�|�@�K�@�^5@�x�@�&�@��u@��@�Z@�t�@��y@�V@�@�p�@��9@�C�@�~�@�hs@���@�9X@��
@��F@��P@�l�@���@�~�@���@�?}@�/@��@���@�n�@�-@��@��@��-@�7L@��`@�A�@�ƨ@��@�\)@�S�@�S�@�33@�ȴ@��@���@�X@�G�@�&�@��/@��u@�9X@���@��w@��@�dZ@��@���@�M�@��^@�/@�%@���@��@�z�@�Z@�j@�j@�Z@�  @��;@��@��@�@�l�@�1@��j@��@���@�^5@�E�@�=q@���@�?}@��@��`@��u@��@� �@��@���@��w@�dZ@�S�@�;d@��R@��\@�n�@�^5@�M�@��@��T@��#@�/@�%@��@��`@���@��9@��u@�I�@���@��;@�ƨ@�dZ@��@��@�5?@��@���@���@��h@��h@��h@�?}@��/@���@�j@�I�@�b@��w@���@�S�@�dZ@�\)@�+@��H@�V@�M�@�n�@���@��\@�V@��@���@��@�x�@�O�@�G�@�O�@��@��-@��h@�hs@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�A�A���A��A��A���A���A��`A�Aď\AîADA���A�E�A�(�A�bA�  A��
A��jA��A���A��\A��DA��A�n�A�^5A�VA�+A��A�ĜA�1A�|�A�dZA�JA�ƨA�dZA�&�A�{A�JA���A��A�hsA�-A��A���A��;A���A��-A�t�A�bNA�VA�33A��/A�K�A��A���A�?}A��A���A���A��A�`BA�&�A��A��A���A�E�A��A��FA��A�ƨA���A�bNA�JA��A�
=A��A�VA��A���A��HA�;dA�oA�p�A��`A�{A��TA��HA��A���A�hsA��wA�S�A���A�`BA���A�`BA�1A�ZA��A��A��mA��mA��A��A�dZA���A��HA���A���A��DA���A���A�9XA�r�A�l�A��A�bA�-A���A���A�n�A��TA��-A�n�A� �A���A�{A~ZA{�Ay�hAx�!Aw�FAvffAtI�Ap��Anz�Al��Ak�Ajr�Ah�Ag�Af�yAeS�Ad�Acp�AaS�A_dZA]�A[�
A[?}AZ�AX �AVjAT��AR�AR5?AQ��AQG�AO\)AM�AL��AK��AJ��AI��AIp�AIC�AHn�AGG�AF�AF  AD~�AC��ACoABn�A@�RA>�A=
=A:�\A9��A8�yA7|�A6��A69XA5G�A3\)A0-A.VA-��A-O�A,��A+�
A+�A*�yA*bNA)�#A(��A&��A%�A$��A#�
A#oA"E�A ��A�A�hA�A  Al�A33Av�AA��A�Ax�A��AM�A�At�AoAffA�^At�A�A��A�AoA�jAVA �A��AVAM�A�
A�AhsA �A
��A
�DA�`Az�A5?Al�A �A+A��A;dA��A�A"�@���@�X@���@��T@���@�A�@���@�1'@�+@�7L@�D@��@�\@蛦@�ff@���@�@�^@�-@��@�/@�9X@�v�@�j@�;d@�E�@ݡ�@��`@�33@�^5@ى7@ى7@ف@�x�@�x�@�O�@�1'@���@�G�@�ff@�?}@�A�@�1@υ@�^5@��
@ˮ@ˍP@�l�@���@���@�x�@ȴ9@Ǯ@��@��m@�-@��@��`@��F@�J@��h@���@�+@�`B@��@�b@��@�@���@��-@���@�Z@�I�@��@�\)@���@�r�@��@�|�@�K�@�^5@�x�@�&�@��u@��@�Z@�t�@��y@�V@�@�p�@��9@�C�@�~�@�hs@���@�9X@��
@��F@��P@�l�@���@�~�@���@�?}@�/@��@���@�n�@�-@��@��@��-@�7L@��`@�A�@�ƨ@��@�\)@�S�@�S�@�33@�ȴ@��@���@�X@�G�@�&�@��/@��u@�9X@���@��w@��@�dZ@��@���@�M�@��^@�/@�%@���@��@�z�@�Z@�j@�j@�Z@�  @��;@��@��@�@�l�@�1@��j@��@���@�^5@�E�@�=q@���@�?}@��@��`@��u@��@� �@��@���@��w@�dZ@�S�@�;d@��R@��\@�n�@�^5@�M�@��@��T@��#@�/@�%@��@��`@���@��9@��u@�I�@���@��;@�ƨ@�dZ@��@��@�5?@��@���@���@��h@��h@��h@�?}@��/@���@�j@�I�@�b@��w@���@�S�@�dZ@�\)@�+@��H@�V@�M�@�n�@���@��\@�V@��@���@��@�x�@�O�@�G�@�O�@��@��-@��h@�hs@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B��B�
B�B�B�B�B�
B�#B�BBB;dB��B�B�B�/B�5B�BB�NB�TB�ZB�`B�`B�`B�fB�mB�mB�mB�fB�NB�#B��BǮBŢBB��B��B��B��B�}B�}B��BBBÖBĜBĜBǮB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĜBB��B�jB�-B��B��B�{B�oB�\B�JB�B|�Bz�Bw�Br�Bn�Bk�BcTBN�B>wB5?B0!B-B!�BhB	7BB��B�B�B�5BŢB�B��B�VB�Bv�BjBffB`BBVBJ�B=qB.B�BVB+B
��B
�B
�HB
��B
�XB
��B
�=B
{�B
v�B
gmB
e`B
e`B
^5B
ZB
R�B
H�B
6FB
0!B
-B
%�B
�B
PB	��B	�B	�B	�ZB	�)B	��B	��B	ǮB	��B	�qB	�LB	�B	��B	��B	��B	��B	�oB	�JB	�B	~�B	t�B	s�B	q�B	l�B	cTB	_;B	XB	O�B	J�B	F�B	E�B	C�B	>wB	:^B	7LB	1'B	,B	'�B	$�B	�B	�B	PB	B��B��B�B�B�`B�HB�#B��BB�XB�FB�9B�!B�B��B��B��B��B��B��B��B��B�oB�hB�JB�=B�DB�DB�=B�DB�PB�oB�hB�\B�\B�PB�JB�=B�7B�1B�%B�B�B� B~�B}�B|�B{�Bz�Bx�Bv�Bt�Bo�Bl�Bk�BjBiyBgmBcTBaHB_;B^5B]/B[#BYBW
BR�BP�BO�BO�BM�BK�BK�BI�BH�BH�BF�BG�BG�BG�BG�BG�BH�BG�BE�BF�BG�BH�BH�BH�BH�BG�BF�BE�BE�BG�BI�BI�BK�BL�BN�BO�BQ�BQ�BQ�BQ�BP�BP�BP�BQ�BP�BVBXBYBYBYBYB\)B\)B\)B[#B[#B\)B\)B]/B]/B`BBcTBdZBgmBgmBjBn�Bn�Bp�Bs�Bw�Bz�Bz�B�B�7B�JB�VB�bB�VB�\B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�9B�FB�LB�XB�^B�dB�dB�qB��BÖBǮBɺB��B�B�)B�5B�;B�NB�TB�mB�B�B�B�B��B��B��B��B��B	  B	B	%B	+B	1B	
=B	DB	uB	�B	�B	�B	�B	!�B	"�B	"�B	#�B	)�B	,B	/B	0!B	1'B	49B	6FB	7LB	8RB	;dB	<jB	=qB	?}B	B�B	I�B	M�B	R�B	Q�B	O�B	P�B	R�B	T�B	VB	VB	VB	W
B	[#B	`BB	e`B	hsB	m�B	p�B	r�B	s�B	t�B	v�B	v�B	v�B	v�B	w�B	x�B	y�B	z�B	{�B	}�B	� B	�B	�B	�B	�B	�+B	�1B	�7B	�=B	�JB	�VB	�VB	�hB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�!B	�-B	�9B	�9B	�FB	�LB	�XB	�wB	�}B	B	ŢB	ƨB	ǮB	ǮB	Ǯ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B��B�
B�B�B�B�B�
B�)B�TB+BH�B��B�
B�B�/B�;B�HB�NB�TB�ZB�`B�`B�`B�fB�mB�sB�sB�mB�fB�NB��BȴBƨBÖBB��B��B��B��B��BBBBĜBĜBŢBȴB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BƨBÖBB��B�jB��B��B��B�{B�bB�hB�B~�B|�B{�Bu�Bo�Bn�Bn�BXBD�B:^B1'B1'B)�B�BDB1B��B��B�B�fB��B�!B��B�oB�JB|�Bl�BhsBe`B[#BP�BC�B6FB�B\B	7B  B
�B
�fB
��B
�jB
�B
�\B
�B
x�B
hsB
ffB
gmB
`BB
^5B
XB
Q�B
:^B
33B
0!B
)�B
"�B
�B
B	��B	�B	�sB	�HB	�B	��B	��B	ĜB	��B	�wB	�FB	�'B	��B	��B	��B	��B	�bB	�7B	�B	u�B	t�B	s�B	p�B	ffB	aHB	[#B	R�B	L�B	G�B	F�B	E�B	A�B	;dB	:^B	5?B	.B	)�B	&�B	$�B	�B	uB	JB��B��B��B�B�fB�ZB�HB�BǮB�dB�RB�FB�3B�!B�B�B��B�B�B��B��B��B��B�{B�hB�VB�JB�VB�PB�PB�VB�{B�uB�hB�hB�\B�PB�JB�=B�7B�+B�%B�B�B� B� B~�B~�B{�By�Bw�Bx�Bt�Bn�Bm�Bk�BjBk�BgmBcTBdZB_;B^5B^5B]/BZBW
BS�BQ�BR�BP�BQ�BN�BL�BL�BL�BL�BK�BJ�BJ�BI�BH�BI�BJ�BI�BI�BH�BH�BH�BH�BH�BH�BG�BG�BH�BI�BJ�BJ�BL�BN�BO�BP�BQ�BQ�BQ�BQ�BP�BR�BR�BS�BT�BXBZBZBZB[#B]/B\)B\)B\)B\)B\)B]/B]/B_;B`BBcTBcTBffBgmBiyBm�Bo�Bp�Bs�Bv�Bx�B|�B|�B�B�7B�JB�\B�hB�VB�bB�bB��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�3B�?B�LB�RB�XB�^B�dB�jB�wB��BĜBǮB��B��B�#B�/B�5B�;B�TB�ZB�sB�B�B�B�B��B��B��B��B��B	B	B	%B	+B		7B	DB	DB	{B	�B	�B	�B	 �B	!�B	#�B	#�B	$�B	)�B	,B	/B	0!B	1'B	49B	6FB	7LB	9XB	;dB	=qB	>wB	?}B	A�B	H�B	L�B	T�B	R�B	P�B	P�B	R�B	VB	W
B	VB	VB	XB	[#B	aHB	e`B	iyB	m�B	q�B	r�B	s�B	u�B	v�B	v�B	v�B	v�B	w�B	x�B	y�B	{�B	{�B	}�B	� B	�B	�B	�B	�%B	�1B	�1B	�7B	�DB	�PB	�\B	�\B	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�3B	�?B	�?B	�FB	�LB	�XB	�wB	�}B	B	ŢB	ƨB	ǮB	ȴB	Ǯ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447222012010314472220120103144722  AO  ARGQ                                                                        20111130142301  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142301  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144722  IP                  G�O�G�O�G�O�                