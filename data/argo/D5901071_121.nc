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
_FillValue                 �  A<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  J�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  L�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  TD   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ]�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ed   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  gL   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  n�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  v�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  xl   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �8   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �H   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �L   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �\   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �`   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �d   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               yA   AO  20111130141924  20190522121827  1727_5046_121                   2C  D   APEX                            2143                            040306                          846 @Բ�N�1   @Բ@y`@7$Z�1�d$�/1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @9��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Dfy�Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Dyff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @9��@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DU��DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Dfy�Df��Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� DqfDq� Dr  Dr� Ds  Dyff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aź^A�33Aĝ�A�z�A�hsA�ZA�Q�A�C�A�33A��mA��
A���AþwA�ƨA���AþwAò-AîAá�AÕ�A�|�A�O�A�{A��#A¶FA²-A²-A²-A²-A°!A¬A�z�A�p�A�jA�=qA��A��!A�M�A��7A�ȴA��\A�ĜA���A�XA�+A��A��;A�XA���A�(�A�/A�%A��RA���A�ƨA�JA��A�hsA�M�A�E�A��A�/A���A�`BA�?}A�/A��-A�A�A���A��`A���A��A�VA�VA���A�{A�ȴA���A��wA��hA�v�A�ZA��A��TA� �A���A���A�^5A�JA��jA��A�dZA���A�M�A�oA�VA�%A�;dA��`A��A���A�|�A��TA�x�A��TA��#A���A�
=A�bNA��A���A�;dA�VA��RA��A���A�(�A���A�t�A�A�A~�+A|��Azr�Av1'As�-Ao�-Al��Ak�PAi&�Ag�mAf��Ae�Ae�
Ac��Aat�A\ffAZ�9AY��AX�9AWhsAU�wAR~�AK��AJ�!AJ��AH��AE�;AE"�AD��AD��AE&�AD�RAC�-AA�TA@�\A?��A?\)A?%A>�\A=��A=/A<A�A;K�A9�A7�#A6��A5S�A3�;A1�FA/�wA,$�A)p�A(v�A'��A%�#A$^5A#+A!��A�#A�`A��A$�A�;A�A=qA�A��A�-AXA�`AVAXA�;A;dAJA�7AdZA7LAA��A�A��A\)A�jA�A�FA�A
v�A
E�A
(�A
 �A
�A	�A��A��AVA�7A`BA;dAoA��A1'A��A��A�FA?}AK�A33A��An�A E�@��@�1'@�+@��@�=q@���@�ff@�dZ@���@�|�@���@�{@�x�@�9@�I�@�(�@�F@�;d@�R@�^5@�-@�l�@�Ĝ@��@�S�@���@��`@�b@��;@߅@�v�@�Z@�;d@�"�@���@��H@��@��@��@�M�@���@�x�@�t�@�ff@�E�@��T@�7L@�t�@͉7@�=q@���@�A�@Ǿw@��m@�ƨ@��@ŉ7@��@ēu@��@Ý�@�n�@�{@�&�@�\)@��@��y@���@��!@�-@�?}@�Ĝ@�9X@�o@���@���@�J@�V@���@�/@���@�dZ@�\)@��@�G�@���@���@�C�@�33@�+@�
=@��@��R@��!@��+@�ȴ@�@��@���@�v�@�ff@�^5@�V@�V@�-@���@���@��@�?}@���@��H@���@��#@�x�@�O�@�7L@�V@��j@�A�@��;@��@��@�-@�`B@�z�@�t�@�x�@��j@��/@���@��@��R@���@�^5@���@��h@��7@�p�@�G�@�?}@�X@�x�@���@��-@�O�@�%@��@�Z@�Z@�Q�@�+@��\@�@��@��@���@��@��9@��D@�I�@��@�1@�l�@���@���@��!@���@��+@�v�@�^5@�M�@�=q@��@��@�`B@���@�j@�Q�@���@��@�S�@��@���@��@��^@���@��@�O�@�/@��@�V@���@��/@��j@��u@��@�j@�9X@��m@��w@��@�;d@��@��H@���@�$�@��@��7@�Ĝ@�bN@��
@���@���@���@��@��P@�t�@�33@��@���@���@���@�^5@�$�@�p�@�&�@��@��@�r�@�1'@��@�+@�@��@��@�ȴ@��R@���@�~�@�v�@�V@��h@���@��@��/@�Ĝ@��j@���@�r�@�(�@��;@���@��@�l�@�33@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Aź^A�33Aĝ�A�z�A�hsA�ZA�Q�A�C�A�33A��mA��
A���AþwA�ƨA���AþwAò-AîAá�AÕ�A�|�A�O�A�{A��#A¶FA²-A²-A²-A²-A°!A¬A�z�A�p�A�jA�=qA��A��!A�M�A��7A�ȴA��\A�ĜA���A�XA�+A��A��;A�XA���A�(�A�/A�%A��RA���A�ƨA�JA��A�hsA�M�A�E�A��A�/A���A�`BA�?}A�/A��-A�A�A���A��`A���A��A�VA�VA���A�{A�ȴA���A��wA��hA�v�A�ZA��A��TA� �A���A���A�^5A�JA��jA��A�dZA���A�M�A�oA�VA�%A�;dA��`A��A���A�|�A��TA�x�A��TA��#A���A�
=A�bNA��A���A�;dA�VA��RA��A���A�(�A���A�t�A�A�A~�+A|��Azr�Av1'As�-Ao�-Al��Ak�PAi&�Ag�mAf��Ae�Ae�
Ac��Aat�A\ffAZ�9AY��AX�9AWhsAU�wAR~�AK��AJ�!AJ��AH��AE�;AE"�AD��AD��AE&�AD�RAC�-AA�TA@�\A?��A?\)A?%A>�\A=��A=/A<A�A;K�A9�A7�#A6��A5S�A3�;A1�FA/�wA,$�A)p�A(v�A'��A%�#A$^5A#+A!��A�#A�`A��A$�A�;A�A=qA�A��A�-AXA�`AVAXA�;A;dAJA�7AdZA7LAA��A�A��A\)A�jA�A�FA�A
v�A
E�A
(�A
 �A
�A	�A��A��AVA�7A`BA;dAoA��A1'A��A��A�FA?}AK�A33A��An�A E�@��@�1'@�+@��@�=q@���@�ff@�dZ@���@�|�@���@�{@�x�@�9@�I�@�(�@�F@�;d@�R@�^5@�-@�l�@�Ĝ@��@�S�@���@��`@�b@��;@߅@�v�@�Z@�;d@�"�@���@��H@��@��@��@�M�@���@�x�@�t�@�ff@�E�@��T@�7L@�t�@͉7@�=q@���@�A�@Ǿw@��m@�ƨ@��@ŉ7@��@ēu@��@Ý�@�n�@�{@�&�@�\)@��@��y@���@��!@�-@�?}@�Ĝ@�9X@�o@���@���@�J@�V@���@�/@���@�dZ@�\)@��@�G�@���@���@�C�@�33@�+@�
=@��@��R@��!@��+@�ȴ@�@��@���@�v�@�ff@�^5@�V@�V@�-@���@���@��@�?}@���@��H@���@��#@�x�@�O�@�7L@�V@��j@�A�@��;@��@��@�-@�`B@�z�@�t�@�x�@��j@��/@���@��@��R@���@�^5@���@��h@��7@�p�@�G�@�?}@�X@�x�@���@��-@�O�@�%@��@�Z@�Z@�Q�@�+@��\@�@��@��@���@��@��9@��D@�I�@��@�1@�l�@���@���@��!@���@��+@�v�@�^5@�M�@�=q@��@��@�`B@���@�j@�Q�@���@��@�S�@��@���@��@��^@���@��@�O�@�/@��@�V@���@��/@��j@��u@��@�j@�9X@��m@��w@��@�;d@��@��H@���@�$�@��@��7@�Ĝ@�bN@��
@���@���@���@��@��P@�t�@�33@��@���@���@���@�^5@�$�@�p�@�&�@��@��@�r�@�1'@��@�+@�@��@��@�ȴ@��R@���@�~�@�v�@�V@��h@���@��@��/@�Ĝ@��j@���@�r�@�(�@��;@���@��@�l�@�33@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBS�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BQ�B`BBk�Bk�BjBl�Bo�Bw�B}�B~�B�B�B�B� B}�B|�B{�B|�B|�B|�B|�B|�B|�Bz�Bz�B{�B|�By�B}�B}�Bn�B~�B�+B�1B�B�B�=B�VB�PB�hB�hB�bB�PB�VB�JB�=B�+B�%B�B�B� B}�By�Bv�Bs�Br�Bq�Bp�Bo�Bm�Bl�Be`BbNB^5BW
BJ�BB�B9XB(�B(�B'�B%�B$�B"�B�B��B�B�yB�ZB�HB�B��BƨB�'B��B�uB�BiyB[#BM�B=qB9XB.B'�B �B�BhBB
�B
�
B
ȴB
�^B
�B
��B
��B
��B
�\B
|�B
_;B
T�B
L�B
9XB
�B
{B
%B	�B	�)B	ȴB	ĜB	�?B	��B	��B	�\B	�VB	��B	��B	w�B	B�B	5?B	.B	"�B	hB��B�)BB�`B�B�`B�B�fB��B��B��B	B��B��B�B�B�B�B�sB�ZB�BB��B��BɺBĜB�qB�9B��B��B��B��B�B{�By�By�By�Bx�Bw�Bv�Bs�Bp�Bn�Bm�Bm�Bn�Bt�B|�B}�B}�B}�B|�Bx�Bu�Bt�Bt�Bu�Bu�Bu�Bt�Bt�Bt�Br�Bp�Bp�Bq�Bq�Bq�Bq�Bq�Br�Br�Bq�Bp�Bo�Bo�Bn�Bp�Bo�Bo�Bm�Bl�Bk�BhsBgmBgmBo�Br�Br�Bq�Bm�Bl�Bl�Bl�Bm�Bl�Bk�BhsBe`BdZBffBffBgmBgmBhsBiyBk�Bk�Bk�Bk�Bk�Bk�BjBk�Bo�Bo�Bn�Bn�Bo�Bp�Bp�Bp�Bq�Br�Br�Bq�Bq�Bq�Br�Bq�Bq�Br�Br�Br�Bl�BjBiyBgmBe`Be`BbNBe`BffBiyBk�Bn�Bo�Bo�Bo�Bo�Bq�Bs�Bv�Bx�By�B|�B�B�=B�DB�JB�JB�\B�{B��B��B��B��B�B�B�B�B�B�LB�XB�qB�jB�^B�XB�wBŢBɺB��B��B��B��B��B��B�BB�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	JB	hB	hB	uB	{B	{B	{B	�B	�B	�B	�B	�B	!�B	&�B	(�B	%�B	"�B	$�B	)�B	+B	.B	0!B	2-B	49B	9XB	;dB	<jB	<jB	>wB	@�B	A�B	C�B	E�B	E�B	H�B	I�B	K�B	L�B	L�B	N�B	S�B	T�B	XB	YB	ZB	[#B	_;B	`BB	dZB	dZB	ffB	ffB	k�B	o�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	v�B	w�B	x�B	|�B	�B	�B	�B	�%B	�+B	�7B	�=B	�DB	�\B	�oB	�oB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�LB	�XB	�dB	�jB	�qB	�wB	�}B	��B	��B	��B	ÖB	ÖB	ȴB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�
B	�B	�#B	�)B	�)B	�/B	�;B	�Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B\)BT�BR�BQ�BQ�BQ�BQ�BQ�BS�B`BBk�Bk�BjBl�Bo�Bw�B}�B~�B�B�B�B�B~�B}�B{�B|�B|�B|�B|�B|�B}�Bz�Bz�B|�B}�Bz�B� B�Bq�B�B�=B�=B�B�B�VB�oB�uB�{B�uB�{B�oB�\B�PB�uB�bB�1B�B�B� B�B|�Bx�Bu�Bs�Bq�Br�Bq�Bo�Bn�BffBdZBdZB]/BM�BE�BC�B-B)�B(�B&�B%�B$�B&�B��B�B�B�`B�TB�)B�B��B�RB��B��B�VBo�B`BBT�B>wB>wB0!B+B"�B�B�BB
��B
�B
��B
�wB
�!B
��B
��B
��B
��B
�B
bNB
VB
R�B
C�B
#�B
�B
hB	��B	�mB	��B	ȴB	�dB	��B	��B	�hB	�\B	��B	��B	�B	F�B	7LB	2-B	&�B	�B	B�BŢB�`B��B�B�B�mB��B��B	B	1B	B��B��B�B�B�B�B�mB�ZB�B��B��BȴB��B�RB�B��B�B��B�B}�B~�B}�B|�B|�B|�By�By�Bw�Bt�Bp�Bo�Bo�Bt�B}�B~�B� B� B� B{�Bw�Bw�Bu�Bv�Bv�Bv�Bu�Bv�Bv�Bs�Br�Br�Br�Bs�Bs�Br�Bq�Br�Br�Br�Bs�Bp�Bp�Bp�Bq�Bp�Bp�Bn�Bm�Bm�Bk�BjBhsBo�Br�Bs�Bs�Bt�Bp�Bo�Bn�Bn�Bm�Bn�Bl�BjBhsBhsBgmBhsBhsBiyBjBk�Bl�Bl�Bl�Bl�Bm�Bn�Bp�Bp�Bq�Bq�Bp�Bq�Bq�Bq�Br�Bt�Bt�Br�Bq�Bq�Bq�Br�Bq�Br�Bt�Bw�Bu�Bn�BjBjBhsBhsBhsBgmBgmBgmBjBk�Bn�Bp�Bq�Bp�Bp�Br�Bt�Bx�By�B{�B� B�%B�=B�DB�JB�PB�hB��B��B��B��B��B�B�!B�-B�B�!B�XB�XB�wB�}B�jB�^B�wBŢBɺB��B��B��B��B��B��B�BB�B�B�B�B�B�B�B��B��B��B��B	B	B	%B	PB	hB	oB	uB	{B	{B	�B	�B	�B	�B	�B	�B	"�B	(�B	+B	(�B	#�B	$�B	,B	-B	.B	0!B	33B	5?B	:^B	;dB	<jB	<jB	>wB	@�B	A�B	C�B	E�B	F�B	I�B	J�B	L�B	L�B	L�B	P�B	T�B	VB	XB	YB	ZB	\)B	`BB	`BB	dZB	dZB	ffB	gmB	l�B	o�B	q�B	r�B	s�B	t�B	u�B	u�B	v�B	v�B	w�B	y�B	}�B	�B	�B	�B	�%B	�1B	�7B	�DB	�JB	�bB	�oB	�oB	�uB	�uB	�{B	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�?B	�LB	�XB	�dB	�jB	�wB	�}B	�}B	��B	��B	B	ĜB	ĜB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�;B	�Z1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<49X<#�
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
<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447162012010314471620120103144716  AO  ARGQ                                                                        20111130141924  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141924  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144716  IP                  G�O�G�O�G�O�                