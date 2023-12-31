CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:25Z UW 3.1 conversion   
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               zA   AO  20111130141940  20190522121827  1727_5046_122                   2C  D   APEX                            2143                            040306                          846 @ԳY�_�1   @ԳZ���@7g-�d��E�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @333@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D�fD  D� D  Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dgy�Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@333@�  @�  A   A   A@  A`  A���A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��3C�  C�  C�  C�  C��C�  D   D � D  D� D  D� D  D�fD  D� D  Dy�D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9�fD:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR�fDS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Day�Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Df��Dgy�Dg��Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dy�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�?}A�9XA�5?A�?}A�G�A�M�A�O�A�M�A�I�A�A�A�A�A�Q�A�XA�`BA�S�A�G�A��A�{A�G�A�K�A�I�A�Q�A�\)A�bNA�Q�A�+A���A�/A���A���A��
A�~�A��A���A���A��uA��7A�z�A�t�A�p�A�~�A��9A�O�A�VA��FA�t�A�E�A��TA�E�A�v�A�{A���A��#A�JA���A��FA�{A��^A�hsA���A��PA���A�K�A�r�A���A��RA�E�A�ƨA�5?A���A���A�Q�A���A��A���A��yA�A��PA���A�ƨA��9A�VA��DA���A�l�A��A��
A���A�  A�ȴA�5?A�A�A�M�A�VA�5?A�^5A��A�ƨA���A�A��uA�p�A�$�A���A�^5A��mA��A�{A���A�/A�  A�ffA�v�A�A�+A�bAl�A~��A}�AxffAt�Aq�Ap~�Ao��An�jAlQ�Aj�!Ai�^Ag��AgAfbNAe�TAe|�AdĜAc+A`~�A[�^AY�AW|�AV�`AV��AV��AVM�AU�7ATA�AS&�AR=qAPĜAM�AL-AL{AK�;AK33AI�7AFjAD(�AB~�AAVA>��A=
=A;�A:��A9G�A8~�A7��A6�A5��A4��A4�A2�DA2$�A1�A1�A0�RA0A�A.�yA-�A-��A-A,�!A,�+A+�
A*��A*�HA*��A*z�A)l�A(~�A(�A'��A&$�A%t�A$�/A$1A#+A"��A"�A"VA!�AO�AĜA�TA�uAbNA(�A�mA��A33A��A��AM�A?}A=qAn�A  A��A��AA5?A�AA�AG�A�A��A�9A�A
E�A	/A�yA�uA��A��A��A��A��Al�A Q�@��y@�n�@�(�@��7@�Ĝ@���@�ȴ@�z�@�b@�@�Z@��@�E�@���@�-@�1@�K�@�ff@�K�@�|�@�-@�j@�r�@��@��@�?}@�X@��T@�@�Z@ް!@���@�O�@ܛ�@�  @�S�@��@�O�@�  @׶F@���@ա�@�9X@ӝ�@Ұ!@�/@� �@υ@Ώ\@��@�hs@�A�@�+@�@���@�%@�\)@�V@�C�@�E�@��h@��@��@��P@���@�J@�/@��@��@�r�@��@��@�5?@�=q@�M�@�ff@�ȴ@���@���@�I�@��@��@��!@�-@�O�@��j@�Ĝ@�V@��/@�bN@��F@�\)@�|�@�J@��9@�I�@�dZ@���@�~�@��@��^@�hs@��9@��;@��@�l�@��H@���@�O�@��D@�b@�|�@�ȴ@�J@��^@���@�O�@�V@���@� �@�S�@�
=@��H@�ȴ@��\@��#@��-@���@�x�@�7L@��`@��D@��@��y@�V@�p�@�Ĝ@��`@��@�Q�@�Q�@�(�@�  @��m@��@�@��+@�v�@��@���@���@��P@�v�@��@�hs@�V@��@�z�@�bN@�Z@�A�@�1@���@���@��m@�|�@��y@�M�@��-@�x�@�hs@�`B@�X@�O�@�G�@�&�@��@��@�Ĝ@� �@��;@�|�@�C�@�"�@�@���@���@��R@���@�^5@�=q@�J@��@�@��@�X@�/@��@�V@�%@���@��`@��j@��9@��u@��u@��D@��@�j@�I�@�(�@�b@��@�ƨ@��w@��F@�C�@���@��\@���@���@��+@�M�@��#@��h@��@�X@���@��/@���@��9@�Z@��@�1@���@�|�@���@���@�v�@�ff@�V@�5?@�-@���@��-@���@��h@�hs@�%@�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�?}A�9XA�5?A�?}A�G�A�M�A�O�A�M�A�I�A�A�A�A�A�Q�A�XA�`BA�S�A�G�A��A�{A�G�A�K�A�I�A�Q�A�\)A�bNA�Q�A�+A���A�/A���A���A��
A�~�A��A���A���A��uA��7A�z�A�t�A�p�A�~�A��9A�O�A�VA��FA�t�A�E�A��TA�E�A�v�A�{A���A��#A�JA���A��FA�{A��^A�hsA���A��PA���A�K�A�r�A���A��RA�E�A�ƨA�5?A���A���A�Q�A���A��A���A��yA�A��PA���A�ƨA��9A�VA��DA���A�l�A��A��
A���A�  A�ȴA�5?A�A�A�M�A�VA�5?A�^5A��A�ƨA���A�A��uA�p�A�$�A���A�^5A��mA��A�{A���A�/A�  A�ffA�v�A�A�+A�bAl�A~��A}�AxffAt�Aq�Ap~�Ao��An�jAlQ�Aj�!Ai�^Ag��AgAfbNAe�TAe|�AdĜAc+A`~�A[�^AY�AW|�AV�`AV��AV��AVM�AU�7ATA�AS&�AR=qAPĜAM�AL-AL{AK�;AK33AI�7AFjAD(�AB~�AAVA>��A=
=A;�A:��A9G�A8~�A7��A6�A5��A4��A4�A2�DA2$�A1�A1�A0�RA0A�A.�yA-�A-��A-A,�!A,�+A+�
A*��A*�HA*��A*z�A)l�A(~�A(�A'��A&$�A%t�A$�/A$1A#+A"��A"�A"VA!�AO�AĜA�TA�uAbNA(�A�mA��A33A��A��AM�A?}A=qAn�A  A��A��AA5?A�AA�AG�A�A��A�9A�A
E�A	/A�yA�uA��A��A��A��A��Al�A Q�@��y@�n�@�(�@��7@�Ĝ@���@�ȴ@�z�@�b@�@�Z@��@�E�@���@�-@�1@�K�@�ff@�K�@�|�@�-@�j@�r�@��@��@�?}@�X@��T@�@�Z@ް!@���@�O�@ܛ�@�  @�S�@��@�O�@�  @׶F@���@ա�@�9X@ӝ�@Ұ!@�/@� �@υ@Ώ\@��@�hs@�A�@�+@�@���@�%@�\)@�V@�C�@�E�@��h@��@��@��P@���@�J@�/@��@��@�r�@��@��@�5?@�=q@�M�@�ff@�ȴ@���@���@�I�@��@��@��!@�-@�O�@��j@�Ĝ@�V@��/@�bN@��F@�\)@�|�@�J@��9@�I�@�dZ@���@�~�@��@��^@�hs@��9@��;@��@�l�@��H@���@�O�@��D@�b@�|�@�ȴ@�J@��^@���@�O�@�V@���@� �@�S�@�
=@��H@�ȴ@��\@��#@��-@���@�x�@�7L@��`@��D@��@��y@�V@�p�@�Ĝ@��`@��@�Q�@�Q�@�(�@�  @��m@��@�@��+@�v�@��@���@���@��P@�v�@��@�hs@�V@��@�z�@�bN@�Z@�A�@�1@���@���@��m@�|�@��y@�M�@��-@�x�@�hs@�`B@�X@�O�@�G�@�&�@��@��@�Ĝ@� �@��;@�|�@�C�@�"�@�@���@���@��R@���@�^5@�=q@�J@��@�@��@�X@�/@��@�V@�%@���@��`@��j@��9@��u@��u@��D@��@�j@�I�@�(�@�b@��@�ƨ@��w@��F@�C�@���@��\@���@���@��+@�M�@��#@��h@��@�X@���@��/@���@��9@�Z@��@�1@���@�|�@���@���@�v�@�ff@�V@�5?@�-@���@��-@���@��h@�hs@�%@�J11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B`BB_;B_;B^5BZBZB]/B]/B]/B^5B_;B^5B]/BZBP�BJ�BG�BE�BC�BE�BF�BO�BS�BS�BQ�BN�BN�BN�BW
Bl�B`BBZB\)B`BBcTBdZBaHBZB[#BbNBgmBjBn�Bq�Bu�Bu�Bt�Bs�Bv�Bw�Bx�Bz�B�%B�B�Bx�Bz�B� Bz�Bu�Bl�BbNBdZB[#BP�BI�BJ�BW
B\)BXBN�BD�B>wB0!B#�B�BB�BĜB��B��B~�Bn�B`BBS�BG�B5?B&�B�B
=B
=BB
��B
�NB
��B
ÖB
�?B
��B
��B
�B
q�B
hsB
S�B
G�B
@�B
;dB
/B
+B	�B	�HB	�B	�
B	��B	��B	��B	�XB	�B	��B	��B	��B	��B	��B	�hB	�B	p�B	e`B	_;B	]/B	\)B	[#B	XB	S�B	N�B	J�B	D�B	@�B	/B	+B	)�B	'�B	"�B	�B		7B	B��B�B�fB�;B�#B�B��B��B��B��B��B��BȴBƨBǮBŢBÖB��B�}B�jB�dB�RB�LB�FB�9B�-B�!B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�hB�bB�bB�\B�\B�VB�PB�DB�7B�1B�+B� Bz�Bw�Bn�BjBaHB\)B\)B[#B[#B\)B[#B[#BZBYBZB\)B[#BYBXBW
BR�BP�BM�BW
BT�BR�BQ�BQ�BQ�BP�BP�BO�BYB^5BdZBe`BdZBe`BcTB`BB\)BYBS�BK�BI�BJ�BL�BQ�BT�BXBYBZBaHBdZBm�Bq�Br�Br�Bs�Bt�Bu�Bt�Bt�Bs�Bs�Br�Br�Bs�Bs�Bs�Bu�Bu�Bv�B{�B|�B~�B�B�B�B�B�%B�B�B}�B|�B{�B{�Bz�Bx�Bw�Bv�Bv�Bw�Bz�B~�B� B}�B{�B~�B�1B�JB��B��B��B��B��B��B��B�B�-B�RBƨB��B��B��B��B��B��B��B��B��B��B�
B�B�)B�5B�;B�HB�TB�ZB�`B�mB�B�B�B��B��B��B��B	  B	B	%B	
=B	PB	VB	hB	uB	{B	�B	�B	�B	%�B	+B	-B	/B	1'B	2-B	9XB	>wB	A�B	E�B	K�B	Q�B	XB	YB	YB	ZB	\)B	]/B	_;B	aHB	bNB	bNB	bNB	cTB	e`B	e`B	hsB	k�B	n�B	p�B	s�B	u�B	w�B	w�B	y�B	~�B	� B	�B	�B	�+B	�DB	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�-B	�?B	�LB	�LB	�RB	�RB	�XB	�^B	�jB	�jB	�qB	�wB	�wB	�wB	�}B	��B	��B	��B	B	B	B	B	ÖB	ĜB	ŢB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�
B	�
B	�
B	�B	�B	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B_;B`BB_;B_;B_;BZBYB]/B]/B]/B^5B_;B^5B^5B]/BS�BK�BG�BF�BE�BE�BF�BO�BS�BS�BQ�BN�BN�BN�BVBn�BbNB\)B]/BaHBe`BgmBe`B\)BaHBgmBk�Bp�Bu�Bv�Bv�Bv�Bv�Bx�By�Bz�B|�B�B�1B�JB�=B{�B}�B�B|�Bw�Bo�BcTBhsB_;BR�BM�BJ�BW
B^5B\)BQ�BG�BB�B5?B(�B�BJB��B��B�B��B�Bs�BdZBW
BK�B8RB+B�BDBJBB  B
�mB
��B
ȴB
�XB
�!B
��B
�%B
u�B
p�B
YB
I�B
A�B
>wB
<jB
hB	��B	�`B	�#B	�B	�)B	��B	ĜB	�wB	�'B	�B	��B	��B	��B	��B	��B	�hB	w�B	k�B	aHB	^5B	]/B	\)B	[#B	YB	R�B	N�B	J�B	L�B	2-B	,B	+B	)�B	&�B	�B	VB	B	B��B�B�NB�5B�)B�
B��B��B��B��B��B��BǮBɺBƨBĜBÖBĜB�}B�jB�^B�RB�LB�LB�?B�'B�!B�'B�'B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�hB�hB�bB�bB�\B�VB�VB�PB�DB�=B�B{�Bz�Bs�Bp�BcTB]/B]/B\)B\)B]/B\)B\)B\)B_;B]/B]/B\)B[#B\)BZBW
BT�BT�B[#BXBS�BVBVBS�BR�BR�BS�BZB]/BdZBffBgmBhsBhsBdZB^5B[#BZBR�BL�BL�BL�BQ�BS�BXBYBYBaHBdZBp�Bs�Bs�Bs�Bt�Bu�Bv�Bv�Bv�Bt�Bt�Bt�Bt�Bt�Bu�Bu�Bw�Bv�Bx�B|�B}�B�B�B�%B�%B�+B�7B�7B�B� B}�B|�B|�B|�By�By�Bx�Bw�Bx�B{�B� B�B� B{�B~�B�1B�JB��B��B��B��B��B��B��B�B�3B�RBƨB��B��B��B��B��B��B��B��B��B�B�B�#B�/B�;B�BB�NB�TB�`B�fB�yB�B�B��B��B��B��B��B	  B	B	%B	DB	VB	\B	oB	uB	{B	�B	�B	�B	%�B	+B	.B	0!B	2-B	49B	9XB	?}B	C�B	F�B	K�B	R�B	XB	YB	YB	ZB	\)B	^5B	`BB	bNB	bNB	cTB	cTB	dZB	gmB	gmB	iyB	l�B	o�B	q�B	s�B	u�B	w�B	w�B	y�B	~�B	� B	�B	�B	�1B	�JB	�bB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�'B	�-B	�?B	�LB	�LB	�RB	�RB	�XB	�^B	�jB	�jB	�qB	�wB	�wB	�wB	�}B	��B	��B	��B	B	B	B	ÖB	ĜB	ĜB	ŢB	ŢB	ŢB	ǮB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�
B	�
B	�
B	�B	�B	�Z11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447162012010314471620120103144716  AO  ARGQ                                                                        20111130141940  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141940  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144717  IP                  G�O�G�O�G�O�                