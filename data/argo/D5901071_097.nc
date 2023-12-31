CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:18Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               aA   AO  20111130141349  20190522121826  1727_5046_097                   2C  D   APEX                            2143                            040306                          846 @ԓÂ1   @ԓm�/�@6�
=p���c�t�j~�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq�fDrfDr� Dr��Dy3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D ��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��Dy�D��D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DCy�DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL�fDM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^y�D^��D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk�fDl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq�fDrfDr� Dr��Dy3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��`A��`A���Aԩ�A�oAӗ�AӇ+A�z�A�r�A�l�A�dZA�`BA�ZA�S�A�K�A�G�A�E�A�A�A�;dA�;dA�7LA�7LA�5?A�&�A��A��A�bA�A���A�z�A��#A���A��Aɕ�AȮA�A�I�A�O�A���A�ƨA��HA�E�A��A��TA��RA�jA�G�A��A��hA��
A���A���A�G�A�9XA�7LA�1'A�$�A��HA��A�C�A�r�A�jA�I�A���A��RA�+A��A��A�5?A�1A��wA�/A���A�A�v�A��wA�O�A�+A���A�bNA��A��!A�bA���A�=qA��/A�l�A��A��yA�oA��HA��A���A���A�?}A��DA��mA�t�A��A��A�33A���A�ZA��9A�p�A��A��uA��`A���A���A�A���A�&�A��`A�Q�A��
A��A�dZA�&�A���A�7LA��A��9A��uA�
=A���A���A��A��HA���A��uA~�A{�AydZAx�AvM�Au/Ar��Aq�hAp��An�jAl�yAl(�Ak�hAk"�AiC�Af��Ae�AdQ�Acl�A`JA^  A\��A[��AZ�!AX�AV��AU7LAT�ASVAQ\)APJAO��AOt�ANA�AL�AL�AL��AK�mAK
=AJ��AI�AIVAF��ADĜAC��AB�DAA�A@v�A>{A=��A<�9A;+A81A5oA3S�A2��A1��A0ȴA/��A-S�A,$�A+�A*-A'�A't�A&�A%hsA$ZA"Q�A!C�A z�A�
A�A|�A�RA|�A�A+A%A��A�DAE�A�A��A��A �A"�A�^A�HAffAbA�A�uA�A�\A�A��AXA9XA&�A
bNAZA�`A��A��A��A33A��A1AdZA?}A �@��y@��h@���@���@�1@�33@���@��@���@�K�@�$�@�bN@�l�@�\@���@�X@���@�P@�hs@�z�@�M�@��@�R@���@��y@�{@߅@�hs@���@�  @ڟ�@ؼj@���@ՙ�@���@�Q�@��T@���@���@�-@��@�o@ˍP@��m@˾w@�;d@�-@ȼj@Ə\@�p�@�t�@\@���@��F@�Q�@�K�@�I�@�?}@�Z@�I�@��@�ȴ@�@�`B@�V@��j@�j@��@��P@�S�@�C�@��y@���@��`@� �@�Z@�ȴ@���@�1'@�$�@�E�@�~�@�ff@��R@���@��R@��!@���@�J@���@�7L@�|�@��#@��u@�7L@�7L@�S�@�X@�r�@���@���@�V@�E�@��+@���@�1'@��w@���@��@�G�@�"�@�@�x�@�V@��@��D@�I�@��@��@�\)@��@��\@�^5@�V@�M�@�5?@���@��T@���@��#@���@�@���@���@��^@�G�@��@��/@��@�Ĝ@�j@�j@�r�@�z�@�Z@�9X@��@��
@��F@��\@�E�@�@��@���@���@�O�@��`@�z�@�Z@� �@�b@���@�  @�  @���@��P@�+@�ȴ@���@�n�@�M�@�J@���@��^@���@�x�@�G�@�Z@��@�l�@�"�@�-@��@��-@�x�@�O�@�V@���@���@�r�@�j@�z�@��j@�Ĝ@�1@�  @�  @�1@��@�Q�@��@��@�E�@���@��^@�@���@��7@�p�@�hs@�hs@��@��/@��@��
@�ƨ@��@�33@�
=@�;d@��@�@�"�@�"�@�+@�C�@�S�@�l�@��@��m@��
@��w@��@���@�t�@�C�@�33@��@���@��H@���@��H@��h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��`A��`A���Aԩ�A�oAӗ�AӇ+A�z�A�r�A�l�A�dZA�`BA�ZA�S�A�K�A�G�A�E�A�A�A�;dA�;dA�7LA�7LA�5?A�&�A��A��A�bA�A���A�z�A��#A���A��Aɕ�AȮA�A�I�A�O�A���A�ƨA��HA�E�A��A��TA��RA�jA�G�A��A��hA��
A���A���A�G�A�9XA�7LA�1'A�$�A��HA��A�C�A�r�A�jA�I�A���A��RA�+A��A��A�5?A�1A��wA�/A���A�A�v�A��wA�O�A�+A���A�bNA��A��!A�bA���A�=qA��/A�l�A��A��yA�oA��HA��A���A���A�?}A��DA��mA�t�A��A��A�33A���A�ZA��9A�p�A��A��uA��`A���A���A�A���A�&�A��`A�Q�A��
A��A�dZA�&�A���A�7LA��A��9A��uA�
=A���A���A��A��HA���A��uA~�A{�AydZAx�AvM�Au/Ar��Aq�hAp��An�jAl�yAl(�Ak�hAk"�AiC�Af��Ae�AdQ�Acl�A`JA^  A\��A[��AZ�!AX�AV��AU7LAT�ASVAQ\)APJAO��AOt�ANA�AL�AL�AL��AK�mAK
=AJ��AI�AIVAF��ADĜAC��AB�DAA�A@v�A>{A=��A<�9A;+A81A5oA3S�A2��A1��A0ȴA/��A-S�A,$�A+�A*-A'�A't�A&�A%hsA$ZA"Q�A!C�A z�A�
A�A|�A�RA|�A�A+A%A��A�DAE�A�A��A��A �A"�A�^A�HAffAbA�A�uA�A�\A�A��AXA9XA&�A
bNAZA�`A��A��A��A33A��A1AdZA?}A �@��y@��h@���@���@�1@�33@���@��@���@�K�@�$�@�bN@�l�@�\@���@�X@���@�P@�hs@�z�@�M�@��@�R@���@��y@�{@߅@�hs@���@�  @ڟ�@ؼj@���@ՙ�@���@�Q�@��T@���@���@�-@��@�o@ˍP@��m@˾w@�;d@�-@ȼj@Ə\@�p�@�t�@\@���@��F@�Q�@�K�@�I�@�?}@�Z@�I�@��@�ȴ@�@�`B@�V@��j@�j@��@��P@�S�@�C�@��y@���@��`@� �@�Z@�ȴ@���@�1'@�$�@�E�@�~�@�ff@��R@���@��R@��!@���@�J@���@�7L@�|�@��#@��u@�7L@�7L@�S�@�X@�r�@���@���@�V@�E�@��+@���@�1'@��w@���@��@�G�@�"�@�@�x�@�V@��@��D@�I�@��@��@�\)@��@��\@�^5@�V@�M�@�5?@���@��T@���@��#@���@�@���@���@��^@�G�@��@��/@��@�Ĝ@�j@�j@�r�@�z�@�Z@�9X@��@��
@��F@��\@�E�@�@��@���@���@�O�@��`@�z�@�Z@� �@�b@���@�  @�  @���@��P@�+@�ȴ@���@�n�@�M�@�J@���@��^@���@�x�@�G�@�Z@��@�l�@�"�@�-@��@��-@�x�@�O�@�V@���@���@�r�@�j@�z�@��j@�Ĝ@�1@�  @�  @�1@��@�Q�@��@��@�E�@���@��^@�@���@��7@�p�@�hs@�hs@��@��/@��@��
@�ƨ@��@�33@�
=@�;d@��@�@�"�@�"�@�+@�C�@�S�@�l�@��@��m@��
@��w@��@���@�t�@�C�@�33@��@���@��H@���@��H@��h111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�ZB�ZB�TB�HB�/B�/B�/B�)B�/B�/B�)B�)B�)B�)B�)B�#B�#B�#B�#B�#B�#B�)B�)B�B�B�B�
B��B��B�'Bt�BQ�B,B2-B/B�B%�BA�BQ�B^5BgmBl�BhsBp�BȴB��BB.BA�BVBffBl�Bt�Bu�Bu�Bu�Bu�Bu�Bu�Bv�Bz�B� B�B�+B�=B�JB�JB�DB�\B�\B�hB��B��B��B��B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�B��B��B��B��B�uB�7Bs�B`BBW
BN�BF�B:^B+B�BPBJB��B��BJBB�B�HB��B��B�Bo�BcTBM�B;dB!�BVB
�yB
��B
�}B
�XB
�'B
��B
�hB
t�B
XB
;dB
"�B
�B
VB
B	��B	�B	�mB	�`B	�/B	�B	��B	��B	ŢB	�jB	�'B	��B	��B	��B	�PB	�VB	�+B	�B	{�B	l�B	ffB	`BB	[#B	T�B	N�B	M�B	N�B	O�B	N�B	F�B	N�B	ZB	T�B	S�B	[#B	_;B	YB	F�B	1'B	%�B	�B	�B	PB��B��B�B�B�qB��B��B��B��B��B��B�hB�bB�\B�PB�PB�JB�7B�%B�B�B�B�B�B�=B�DB�=B�+B�7B�1B�+B�%B�B�B~�B|�By�Bv�Br�Bq�Bo�Bo�Bn�Bl�BiyBgmBe`BcTBbNB`BB`BBdZB[#BW
BS�BR�BP�BO�BN�BM�BL�BK�BJ�BH�BG�BG�BG�BF�BE�BE�BD�BC�BC�BA�B?}B@�B@�BE�BH�BJ�BI�BI�BJ�BI�BE�B?}B<jB:^B8RB7LB7LB:^B;dB?}B>wB<jB=qB=qB>wBC�BG�BD�BD�BD�BH�BK�BP�BR�BT�BVBW
BXBYB^5B_;BaHBe`BgmBn�Bt�B�%B�+B�1B�hB�{B�uB�{B�{B��B��B�{B��B��B�{B�{B�oB�\B�VB�bB�oB�hB�uB�VB�DB�PB��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�!B�B�'B�!B�!B�'B�-B�3B�FB�RB�dB�dB�jB�jB��BÖBĜBǮBɺB��B��B��B��B�B�B�;B�TB�sB�yB�yB�sB�yB�B�B�B��B��B��B��B	B	+B	
=B	DB	PB	\B	uB	{B	�B	�B	�B	�B	�B	�B	�B	#�B	%�B	'�B	)�B	+B	,B	-B	.B	/B	0!B	2-B	49B	49B	49B	5?B	6FB	<jB	@�B	C�B	D�B	E�B	F�B	G�B	I�B	I�B	I�B	I�B	H�B	I�B	J�B	K�B	L�B	S�B	VB	W
B	ZB	[#B	]/B	_;B	aHB	dZB	e`B	gmB	k�B	m�B	m�B	r�B	t�B	v�B	y�B	� B	�B	�B	� B	~�B	� B	�B	�B	�B	�B	�B	�1B	�=B	�JB	�VB	�oB	�uB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�?B	�FB	�^B	�qB	�wB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�ZB�ZB�ZB�ZB�;B�/B�/B�)B�/B�/B�)B�)B�)B�)B�)B�#B�#B�#B�#B�#B�#B�)B�)B�B�B�B�
B�B��B��B|�BZB.B6FB33B$�B(�BB�BR�BaHBiyBm�Bl�By�B��B��B+B0!BC�BYBgmBm�Bt�Bu�Bu�Bu�Bv�Bv�Bw�By�B~�B�B�1B�JB�PB�JB�PB�\B�bB�hB�{B��B��B��B��B��B��B��B��B�B�B�B�B�B��B��B��B��B��B�!B�?B�B��B��B��B��B�\By�BbNBYBP�BH�B>wB2-B�BuBbB  B��BbBDB�B�B�#B��B�=Bt�Bk�BQ�BD�B)�B�B
��B
�
B
��B
�jB
�?B
�B
��B
}�B
bNB
D�B
&�B
�B
uB
B
B	�B	�B	�B	�NB	�B	��B	��B	��B	ĜB	�FB	�B	��B	��B	�{B	�oB	�=B	�B	�B	p�B	iyB	cTB	^5B	YB	Q�B	N�B	O�B	R�B	R�B	G�B	N�B	\)B	W
B	T�B	]/B	bNB	_;B	L�B	49B	(�B	�B	�B	{B��B��B�B�TBŢB�B��B��B��B��B��B��B�uB�oB�uB�\B�\B�PB�7B�=B�B�B�B�B�DB�VB�VB�PB�=B�1B�1B�+B�%B�%B�B� B{�By�Bv�Bs�Bp�Bp�Bp�Bo�Bl�BjBgmBdZBcTBcTBcTBgmBaHB[#BXBVBT�BQ�BP�BO�BN�BL�BM�BL�BJ�BI�BG�BG�BG�BF�BF�BF�BD�BC�BB�BB�BB�BE�BI�BK�BL�BM�BL�BM�BH�BC�B@�B=qB:^B;dB;dB;dB=qBB�BA�B?}B=qB?}B?}BG�BI�BG�BE�BE�BK�BK�BP�BR�BVBXBYB[#B[#BaHB`BBcTBhsBffBp�Bs�B�DB�7B�1B�oB��B�{B��B��B��B��B��B��B��B�{B��B�{B�hB�\B�bB��B�uB��B�oB�DB�PB��B��B��B��B��B��B��B��B��B�B��B��B��B�!B�3B�-B�-B�'B�'B�-B�-B�3B�LB�^B�jB�dB�qB�}BÖBŢBŢBȴB��B��B��B��B��B�B�B�BB�TB�sB�yB�yB�sB�yB�B�B�B��B��B��B��B	B	1B	
=B	DB	PB	bB	uB	{B	�B	�B	�B	�B	�B	�B	�B	$�B	&�B	'�B	)�B	+B	-B	.B	/B	/B	0!B	2-B	49B	49B	49B	5?B	6FB	=qB	A�B	C�B	D�B	E�B	F�B	G�B	I�B	I�B	I�B	I�B	I�B	J�B	J�B	L�B	M�B	S�B	VB	W
B	ZB	[#B	]/B	_;B	aHB	dZB	e`B	gmB	k�B	n�B	m�B	r�B	t�B	v�B	y�B	�B	�B	�B	�B	~�B	� B	�B	�B	�B	�B	�B	�7B	�DB	�PB	�\B	�oB	�{B	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�?B	�FB	�^B	�qB	�wB	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<u<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447082012010314470820120103144708  AO  ARGQ                                                                        20111130141349  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141349  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144708  IP                  G�O�G�O�G�O�                