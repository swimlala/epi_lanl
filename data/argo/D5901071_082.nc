CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:14Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               RA   AO  20111130141015  20190522121826  1727_5046_082                   2C  D   APEX                            2143                            040306                          846 @���?�1   @��q� @8�`A�7L�dbM��1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCA�fCC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
�C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCA�fCC�fCF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.y�D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� De��Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsl�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ffA�r�A�t�A�x�A�x�A�x�A�z�A�z�A�z�A�x�A�x�A�x�A�z�A�x�A�v�A�x�A�l�A�bNA�bNA�`BA�Q�A��Aɥ�A�
=A�VA���A�\)A�(�A��A�~�A�hsA��jA�{A��A��wA�Q�A��HA�=qA�  A�JA���A�A��jA�?}A��A�oA��FA��wA�;dA��A�ƨA��#A���A�JA�x�A�-A���A�v�A��A��+A��A���A��A�-A��A�A�?}A� �A�XA�?}A���A�\)A���A�E�A�`BA�
=A��!A��TA���A�;dA�XA���A��A�\)A�^5A��\A�C�A�+A��mA��A��#A�|�A�G�A�"�A��A��!A���A�|�A�x�A�ZA���A�?}A��^A�`BA��FA�=qA��^A�?}A��9A�-A���A�z�A�"�A�x�A�~�A�^5A��wA�t�A��PA�G�A��-A}��A{��AxJAt�Aq%An�yAk�wAi;dAgO�Af1'Ac�Ac�Ac+Ab�!Ab�A`^5A_l�A^��A]|�A[�mA[p�AZ��AYAY+AX(�AV�uAT�AT�+AT��AS�PAR^5AQ��AQ/AO��AN�9AM��AK�7AJ�AI�^AI|�AH�HAG�AFjAEƨAEC�AD��AC�^AC
=AB��ABr�AB-A?x�A=�PA=VA<ȴA;�hA:I�A9�A8��A7�TA7K�A6�9A5�hA4��A4�DA4(�A3�-A2^5A0��A/t�A.A-;dA,bNA+��A*��A)�A)l�A(��A'XA&z�A%��A%;dA$��A$�jA$M�A#/A"ZA!�FA!C�A �A ĜA E�A�7A�HA9XAO�A�RAn�A�^AVAVA9XAVA�A$�A��Ax�A��AJA�/A��A=qA7LA
��A	�TAz�A+AoA
=A��A��A  A/AE�A?}AJA 1@��H@�V@��h@�  @�dZ@���@�Q�@�n�@��@�S�@�G�@�bN@�33@���@�z�@�@�j@�F@�\)@�
=@�~�@ᙚ@��H@�G�@�/@�I�@��@��T@أ�@��@�E�@��@���@�C�@��@�x�@�1'@�ƨ@ύP@�/@���@��H@��@�Ĝ@�bN@�I�@�t�@�n�@�@��@š�@ļj@��
@å�@��@��^@��7@�p�@���@�bN@��m@�J@�x�@�z�@�|�@���@�=q@���@��7@�V@��/@��`@���@�\)@�v�@��/@���@� �@���@�M�@��@��^@��@�ȴ@��-@���@�r�@�I�@��9@��@�G�@�7L@���@��m@��@�7L@�bN@�?}@���@��R@��H@�n�@��@�@�V@�z�@�
=@���@��H@���@�ȴ@��+@�V@�@���@���@��-@��@���@�|�@�A�@�I�@�(�@��;@�ƨ@��
@���@�Z@���@�(�@���@��+@�t�@�o@�@���@���@�V@�~�@�@�C�@�|�@�r�@�/@�hs@��h@��@�hs@�`B@�&�@�Ĝ@���@��`@�Z@�ƨ@�
=@���@�n�@��@�x�@��P@���@�=q@��-@���@�{@�X@���@��@�X@���@�9X@��\@��!@���@��@�-@�x�@�p�@�`B@�7L@���@�(�@�S�@�@��H@�ȴ@�v�@��^@���@��9@��@�Q�@���@�
=@���@���@�v�@�^5@�^5@�J@��^@���@�x�@�`B@��/@���@��@�bN@�A�@�  @��F@��P@�t�@�v�@��@���@��-@��@��D@��m@�K�@�"�@�ȴ@��!@�V@��@���@�`B@���@��9@�r�@� �@��m@��w@�l�@�K�@�;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�ffA�r�A�t�A�x�A�x�A�x�A�z�A�z�A�z�A�x�A�x�A�x�A�z�A�x�A�v�A�x�A�l�A�bNA�bNA�`BA�Q�A��Aɥ�A�
=A�VA���A�\)A�(�A��A�~�A�hsA��jA�{A��A��wA�Q�A��HA�=qA�  A�JA���A�A��jA�?}A��A�oA��FA��wA�;dA��A�ƨA��#A���A�JA�x�A�-A���A�v�A��A��+A��A���A��A�-A��A�A�?}A� �A�XA�?}A���A�\)A���A�E�A�`BA�
=A��!A��TA���A�;dA�XA���A��A�\)A�^5A��\A�C�A�+A��mA��A��#A�|�A�G�A�"�A��A��!A���A�|�A�x�A�ZA���A�?}A��^A�`BA��FA�=qA��^A�?}A��9A�-A���A�z�A�"�A�x�A�~�A�^5A��wA�t�A��PA�G�A��-A}��A{��AxJAt�Aq%An�yAk�wAi;dAgO�Af1'Ac�Ac�Ac+Ab�!Ab�A`^5A_l�A^��A]|�A[�mA[p�AZ��AYAY+AX(�AV�uAT�AT�+AT��AS�PAR^5AQ��AQ/AO��AN�9AM��AK�7AJ�AI�^AI|�AH�HAG�AFjAEƨAEC�AD��AC�^AC
=AB��ABr�AB-A?x�A=�PA=VA<ȴA;�hA:I�A9�A8��A7�TA7K�A6�9A5�hA4��A4�DA4(�A3�-A2^5A0��A/t�A.A-;dA,bNA+��A*��A)�A)l�A(��A'XA&z�A%��A%;dA$��A$�jA$M�A#/A"ZA!�FA!C�A �A ĜA E�A�7A�HA9XAO�A�RAn�A�^AVAVA9XAVA�A$�A��Ax�A��AJA�/A��A=qA7LA
��A	�TAz�A+AoA
=A��A��A  A/AE�A?}AJA 1@��H@�V@��h@�  @�dZ@���@�Q�@�n�@��@�S�@�G�@�bN@�33@���@�z�@�@�j@�F@�\)@�
=@�~�@ᙚ@��H@�G�@�/@�I�@��@��T@أ�@��@�E�@��@���@�C�@��@�x�@�1'@�ƨ@ύP@�/@���@��H@��@�Ĝ@�bN@�I�@�t�@�n�@�@��@š�@ļj@��
@å�@��@��^@��7@�p�@���@�bN@��m@�J@�x�@�z�@�|�@���@�=q@���@��7@�V@��/@��`@���@�\)@�v�@��/@���@� �@���@�M�@��@��^@��@�ȴ@��-@���@�r�@�I�@��9@��@�G�@�7L@���@��m@��@�7L@�bN@�?}@���@��R@��H@�n�@��@�@�V@�z�@�
=@���@��H@���@�ȴ@��+@�V@�@���@���@��-@��@���@�|�@�A�@�I�@�(�@��;@�ƨ@��
@���@�Z@���@�(�@���@��+@�t�@�o@�@���@���@�V@�~�@�@�C�@�|�@�r�@�/@�hs@��h@��@�hs@�`B@�&�@�Ĝ@���@��`@�Z@�ƨ@�
=@���@�n�@��@�x�@��P@���@�=q@��-@���@�{@�X@���@��@�X@���@�9X@��\@��!@���@��@�-@�x�@�p�@�`B@�7L@���@�(�@�S�@�@��H@�ȴ@�v�@��^@���@��9@��@�Q�@���@�
=@���@���@�v�@�^5@�^5@�J@��^@���@�x�@�`B@��/@���@��@�bN@�A�@�  @��F@��P@�t�@�v�@��@���@��-@��@��D@��m@�K�@�"�@�ȴ@��!@�V@��@���@�`B@���@��9@�r�@� �@��m@��w@�l�@�K�@�;d111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�%B�%B�%B�+B�+B�+B�%B�+B�+B�+B�+B�+B�+B�1B�=B�DB�PB�\B�hB��B��B�5B�B(�B#�B�B{B�B�B�B{BbBJB+BBB  B��B�B�B�B�B�B��B��B��B�sB�B��B��B��B�B�B�B�
B�B��B��B��BŢBĜBB�}B�wB�dB�^B�FB�3B��B��B�uB�PB�=B�Bv�BjBe`B\)BQ�BH�B<jB2-B(�B�BB��B�yB�BȴB�XB�B��B��B��B�hB�PB�B|�B|�Bx�Bp�BhsB_;BM�BE�B=qB7LB0!B'�B!�B�B�BhB%B
��B
�#B
ŢB
�3B
��B
��B
� B
iyB
ZB
C�B
0!B
�B
VB	��B	�B	�B	�B	��B	ǮB	ĜB	��B	�dB	�3B	�B	��B	��B	�{B	�hB	�VB	�=B	�DB	�%B	x�B	u�B	|�B	�B	�B	�B	~�B	x�B	o�B	dZB	]/B	S�B	N�B	L�B	K�B	F�B	C�B	>wB	;dB	8RB	49B	/B	.B	,B	)�B	%�B	�B	JB		7B	%B��B��B��B�B�B�B�yB�`B�TB�BB�5B�B��B��B��B��BǮBB�}B�^B�LB�dB�LB�!B��B��B��B�B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B�{B�\B�JB�7B�B�B~�Bz�Bx�Bw�Bt�Br�Bo�Bk�BiyBhsBffBdZBffBffBffBe`BcTBcTBbNB_;B\)BVBR�BR�BQ�BO�BN�BL�BK�BI�BF�BG�BF�BG�BF�BE�BE�BE�BF�BE�BF�BE�BE�BE�BC�BB�BD�BE�BE�BE�BF�BF�BE�BF�BI�BI�BI�BH�BH�BI�BI�BG�BI�BM�BM�BO�BS�BYBYBZB\)B\)B]/B^5BcTBdZBcTBdZBn�Bp�Bq�Bs�Bt�Bs�Bu�Bu�Bv�Bz�B|�B}�B~�B� B�B�%B�%B�B�B�B�B�DB�DB�JB�PB�PB�DB�+B� B~�B� B� B�B�JB��B��B��B��B��B��B��B��B�-B�dBŢBɺB��B��B��B��B��B��B��B��B�B�B�5B�BB�NB�`B�fB�fB�mB�mB�`B�B�B�B�B�B��B��B��B��B	  B	B	B	VB	oB	VB	DB	DB	\B	uB	�B	�B	#�B	/B	7LB	;dB	@�B	A�B	@�B	@�B	@�B	A�B	C�B	F�B	L�B	N�B	R�B	W
B	XB	YB	YB	[#B	\)B	\)B	[#B	aHB	dZB	cTB	hsB	jB	jB	iyB	iyB	gmB	hsB	jB	p�B	t�B	w�B	y�B	y�B	{�B	z�B	x�B	w�B	v�B	v�B	v�B	v�B	v�B	x�B	z�B	z�B	|�B	}�B	� B	�B	�B	�B	�%B	�B	�1B	�=B	�JB	�VB	�VB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�-B	�9B	�?B	�FB	�XB	�jB	�jB	�w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�%B�%B�%B�+B�+B�+B�%B�+B�+B�+B�+B�+B�+B�1B�=B�DB�PB�\B�hB��B�B�fB�B,B+B�B�B�B�B�B�B�BVB
=BBBB��B�B�B��B�B�B��B��B��B�B�#B��B��B��B�)B�#B�B�B�B��B��B��BȴBƨBĜB��B��B��B�qB�LB�LB�B��B�{B�\B�PB�B|�Bl�BiyB`BBVBM�B@�B5?B-B%�B	7B  B�B�BB��B��B�3B��B��B��B�uB�uB�B|�B}�B{�Br�BjBe`BP�BG�B?}B9XB2-B)�B#�B�B�B{B
=B  B
�TB
��B
�RB
�B
��B
�7B
o�B
e`B
M�B
<jB
!�B
�B
B	�B	�5B	�BB	��B	ȴB	ƨB	ÖB	��B	�FB	�B	�B	��B	��B	�{B	�hB	�JB	�VB	�=B	|�B	v�B	|�B	�B	�%B	�B	�B	{�B	r�B	gmB	bNB	XB	O�B	M�B	M�B	I�B	G�B	@�B	=qB	:^B	7LB	1'B	/B	-B	+B	.B	�B	VB	
=B	
=B	B��B��B��B�B�B�B�mB�`B�HB�BB�5B�B�B��B��B��BŢBÖB�wB�RB�wB�dB�-B�B��B��B�B�-B�3B�'B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�\B�JB�7B�%B�B~�B{�By�Bx�Bv�Bs�Bn�Bk�Bk�Bk�BhsBffBffBffBffBe`BffBe`BcTB`BB\)BT�BS�BR�BR�BO�BO�BM�BL�BM�BL�BI�BI�BH�BG�BH�BI�BH�BG�BG�BF�BF�BF�BF�BD�BD�BF�BG�BG�BH�BG�BG�BH�BK�BJ�BJ�BJ�BJ�BJ�BJ�BK�BK�BO�BO�BQ�BT�BYB[#B\)B]/B\)B^5B`BBe`Be`BdZBffBn�Bp�Br�Bt�Bu�Bv�Bv�Bw�Bx�B{�B}�B~�B~�B�B�B�%B�1B�B�B�B�B�JB�VB�PB�VB�PB�JB�=B�B� B�B� B�B�JB��B��B��B��B��B��B��B��B�'B�^BŢB��B��B��B��B��B��B��B��B��B�B�B�5B�HB�NB�`B�fB�mB�yB�yB�ZB�B�B�B�B�B��B��B��B��B	B	+B	B	\B	{B	oB	PB	JB	\B	oB	�B	�B	"�B	.B	7LB	;dB	@�B	A�B	@�B	@�B	A�B	A�B	C�B	G�B	M�B	O�B	S�B	XB	YB	ZB	\)B	\)B	]/B	]/B	[#B	aHB	e`B	cTB	hsB	jB	k�B	jB	l�B	gmB	hsB	jB	r�B	u�B	w�B	y�B	y�B	|�B	{�B	y�B	x�B	v�B	v�B	w�B	w�B	w�B	y�B	z�B	z�B	}�B	~�B	�B	�B	�B	�B	�%B	�%B	�7B	�=B	�JB	�VB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�!B	�-B	�3B	�?B	�FB	�FB	�^B	�jB	�jB	�w111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447022012010314470220120103144702  AO  ARGQ                                                                        20111130141015  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141015  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144702  IP                  G�O�G�O�G�O�                