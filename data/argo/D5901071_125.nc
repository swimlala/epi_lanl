CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:26Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               }A   AO  20111130142020  20190522121827  1727_5046_125                   2C  D   APEX                            2143                            040306                          846 @Է2˩�1   @Է3@@7�$�/��d	?|�h1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,y�D-  D-� D.  D.�fD/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDqfDq�fDrfDr� Ds  Dy� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BW��B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�33B�33B�33B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D#��D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D*��D+� D,  D,y�D-  D-� D.  D.�fD/  D/� D0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DEy�DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh�fDi  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp�fDqfDq�fDrfDr� Ds  Dy� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�v�A��\A��hA��hA��uA��uA��uA��uA��uA��uA��hA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A��PA�jA�{A��RA���A�n�A�ZA�S�A�O�A�A�A�1'A�/A�1'A�/A�33A�7LA�=qA�;dA�"�A�oA�A�A�A�A�%A�JA�VA�VA��A�-A�C�A�(�A�t�A�C�A�I�A�M�A�bNA�JA�K�A�z�A�ȴA���A��RA�?}A�JA��^A��A�(�A�ƨA�ffA��FA�?}A��yA��A���A�/A��/A�G�A��HA�S�A��A��A��A�9XA���A�XA��hA��A��wA�;dA��TA��#A�E�A���A�O�A�Q�A��A���A�M�A�C�A��A�l�A�(�A��
A�~�A��A�A���AdZA~(�A}7LA|��Az�RAtE�Aq�PApv�Al��AjZAh �AfbNAe?}Ac"�AY�hAT�RAS�AQ�TAP�yAPA�AO��ANI�AMoAJ{AFAC|�AA�A@��A?��A>^5A<�/A;�A: �A9p�A8�9A8(�A7?}A6ZA5+A3�
A3�A2z�A1�
A1�PA0jA0A/�A/��A/��A.�HA.M�A-�A-��A-l�A,(�A*�HA*n�A)��A)�A'�mA&{A#XA"�RA"ZA!��A�A��A�hAXAI�At�A��A$�A��Ax�A?}A�A��A\)A��A�A��An�A=qA�PAVA��A�+A5?A�wA33A~�A�hAt�A�jA^5A�mA�hA`BA7LA
��A�/AQ�A �A�mA��AhsA��AZA�^Ar�AA��A%A =qA {@�ȴ@���@��w@�|�@�=q@���@��@�@�@�+@�Ĝ@�"�@�+@�+@�D@��@��@�Z@��T@��@�G�@�z�@��
@�|�@��@�5?@�@���@�I�@�=q@�@���@�1@�o@ղ-@�&�@���@���@�9X@�dZ@�o@ҧ�@�@��@�ƨ@�ff@�{@ͺ^@���@ˍP@���@�r�@�1@ǥ�@�33@���@Ų-@ř�@���@�|�@�J@��/@��D@�1@���@��@�`B@��7@��@�(�@���@�C�@��R@���@���@��@�G�@���@�1'@�
=@�J@�hs@��@���@��9@�
=@�5?@��@�C�@�
=@���@��@��@�1'@��m@��w@���@���@�1@�b@��;@�t�@��y@�~�@�^5@��@��`@�I�@��;@�l�@�o@��@�7L@��D@�9X@�ƨ@��@�K�@�
=@���@�ff@�^5@�5?@��@��@��#@�hs@�O�@�/@��@��u@�"�@���@���@���@�Ĝ@��9@��@��@���@�z�@�9X@��@�1@���@���@���@�t�@�K�@��@��+@�M�@�-@�hs@�&�@��@��/@��9@���@��@�Q�@��;@��@���@�t�@�
=@�n�@���@��^@���@�`B@��@��j@���@��D@�Z@��@��;@���@�|�@�\)@���@�ff@�=q@��@��@��@��#@�x�@�&�@���@��u@� �@���@�t�@�C�@�@��y@���@���@��\@��+@�v�@�n�@�V@�5?@�-@�{@��#@�O�@��@�V@��@���@��j@��9@��@�Q�@��@��
@��w@���@�t�@�l�@�dZ@�+@�
=@���@���@�ȴ@�ȴ@��!@�V@��T@�@���@�G�@�&�@��T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�v�A�v�A��\A��hA��hA��uA��uA��uA��uA��uA��uA��hA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A��A���A��PA�jA�{A��RA���A�n�A�ZA�S�A�O�A�A�A�1'A�/A�1'A�/A�33A�7LA�=qA�;dA�"�A�oA�A�A�A�A�%A�JA�VA�VA��A�-A�C�A�(�A�t�A�C�A�I�A�M�A�bNA�JA�K�A�z�A�ȴA���A��RA�?}A�JA��^A��A�(�A�ƨA�ffA��FA�?}A��yA��A���A�/A��/A�G�A��HA�S�A��A��A��A�9XA���A�XA��hA��A��wA�;dA��TA��#A�E�A���A�O�A�Q�A��A���A�M�A�C�A��A�l�A�(�A��
A�~�A��A�A���AdZA~(�A}7LA|��Az�RAtE�Aq�PApv�Al��AjZAh �AfbNAe?}Ac"�AY�hAT�RAS�AQ�TAP�yAPA�AO��ANI�AMoAJ{AFAC|�AA�A@��A?��A>^5A<�/A;�A: �A9p�A8�9A8(�A7?}A6ZA5+A3�
A3�A2z�A1�
A1�PA0jA0A/�A/��A/��A.�HA.M�A-�A-��A-l�A,(�A*�HA*n�A)��A)�A'�mA&{A#XA"�RA"ZA!��A�A��A�hAXAI�At�A��A$�A��Ax�A?}A�A��A\)A��A�A��An�A=qA�PAVA��A�+A5?A�wA33A~�A�hAt�A�jA^5A�mA�hA`BA7LA
��A�/AQ�A �A�mA��AhsA��AZA�^Ar�AA��A%A =qA {@�ȴ@���@��w@�|�@�=q@���@��@�@�@�+@�Ĝ@�"�@�+@�+@�D@��@��@�Z@��T@��@�G�@�z�@��
@�|�@��@�5?@�@���@�I�@�=q@�@���@�1@�o@ղ-@�&�@���@���@�9X@�dZ@�o@ҧ�@�@��@�ƨ@�ff@�{@ͺ^@���@ˍP@���@�r�@�1@ǥ�@�33@���@Ų-@ř�@���@�|�@�J@��/@��D@�1@���@��@�`B@��7@��@�(�@���@�C�@��R@���@���@��@�G�@���@�1'@�
=@�J@�hs@��@���@��9@�
=@�5?@��@�C�@�
=@���@��@��@�1'@��m@��w@���@���@�1@�b@��;@�t�@��y@�~�@�^5@��@��`@�I�@��;@�l�@�o@��@�7L@��D@�9X@�ƨ@��@�K�@�
=@���@�ff@�^5@�5?@��@��@��#@�hs@�O�@�/@��@��u@�"�@���@���@���@�Ĝ@��9@��@��@���@�z�@�9X@��@�1@���@���@���@�t�@�K�@��@��+@�M�@�-@�hs@�&�@��@��/@��9@���@��@�Q�@��;@��@���@�t�@�
=@�n�@���@��^@���@�`B@��@��j@���@��D@�Z@��@��;@���@�|�@�\)@���@�ff@�=q@��@��@��@��#@�x�@�&�@���@��u@� �@���@�t�@�C�@�@��y@���@���@��\@��+@�v�@�n�@�V@�5?@�-@�{@��#@�O�@��@�V@��@���@��j@��9@��@�Q�@��@��
@��w@���@�t�@�l�@�dZ@�+@�
=@���@���@�ȴ@�ȴ@��!@�V@��T@�@���@�G�@�&�@��T111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B��B�B��BBB  B  BBBBBB%B1B
=BJBJB
=B1B1B	7B	7B
=BJBPBJBbB�B�B�B	7B�B�B�}B��B�=B�B�JB��B��B�JBt�Bn�Bk�BiyBgmBffBe`BbNBZBO�B:^B�B��B�NB��B�wB�RB�?B��B��B�\B�+Bz�Bo�BXBI�B;dB2-B!�B1B
�B
�#B
��B
ÖB
�B
�=B
{�B
u�B
n�B
iyB
dZB
^5B
W
B
S�B
@�B
2-B
'�B
 �B
�B
	7B	�#B	ǮB	�dB	��B	�{B	�B	{�B	w�B	k�B	;dB	&�B	!�B	�B	�B	{B	VB	+B��B�B�)B��B��B��B��B��B�jB�9B�'B�3B�-B�-B�-B�!B�-B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�uB�bB�PB�1B�B}�Bz�Bz�Bw�By�By�By�Bx�Bw�Bx�By�Bz�By�Bx�Bx�Bw�Bu�Bs�Br�Bq�Bn�BjBhsBgmBffBe`BdZBcTBaHB`BB^5B[#BZBYBXBXBW
BVBT�BQ�BQ�BP�BP�BO�BO�BN�BL�BL�BK�BK�BI�BE�B@�B?}B>wB=qB;dB:^B9XB7LB6FB6FB6FB;dB:^B7LB5?B8RBA�BC�B;dB5?B.B,B-B.B1'B5?B6FB8RB<jB?}B?}B@�BF�BG�BG�BH�BI�BL�BM�BN�BM�BN�BP�BQ�BQ�BT�BYBdZBhsBk�Bm�Bq�Bw�Bz�Bz�By�Bz�B|�Bz�B{�B� B� B� B�B�JB�PB�bB�hB�bB��B��B��B��B��B��B�B�!B�-B�-B�-B�9B�?B�dB�wBBÖBĜBÖB��B��B��B��B�B�/B�`B�fB�B�B��B��B��B��B��B	  B	B	B	%B	B	B		7B	JB	\B	oB	uB	�B	�B	�B	�B	"�B	#�B	%�B	&�B	'�B	(�B	(�B	)�B	)�B	)�B	+B	-B	-B	-B	,B	-B	49B	=qB	A�B	A�B	B�B	B�B	C�B	C�B	D�B	D�B	F�B	F�B	F�B	F�B	G�B	H�B	H�B	I�B	K�B	N�B	O�B	P�B	ZB	]/B	]/B	_;B	aHB	bNB	cTB	cTB	gmB	hsB	hsB	iyB	k�B	n�B	p�B	r�B	t�B	v�B	w�B	y�B	z�B	z�B	z�B	|�B	}�B	~�B	~�B	~�B	�B	�B	�DB	�PB	�bB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�'B	�?B	�LB	�RB	�XB	�XB	�XB	�XB	�^B	�^B	�jB	�wB	�}B	��B	��B	��B	��B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ĜB	ƨB	ȴB	ȴB	ɺB	��B	��B	�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�'B��B�B��BBB  B  BBBBBB%B1B
=BPBJB
=B1B1B	7B	7B
=BJBPBJBbB�B�B �B\B�B�5B��B��B�VB�7B�\B��B�'B��Bz�Bo�Bl�BjBhsBhsBhsBdZB\)BS�B@�B'�BB�B�
B��B�^B�jB�'B��B��B�DB� By�B_;BR�B>wB8RB+BbB
��B
�;B
��B
��B
�FB
�\B
}�B
x�B
o�B
k�B
ffB
`BB
W
B
ZB
D�B
5?B
)�B
!�B
 �B
�B	�NB	��B	ŢB	�B	��B	�=B	� B	� B	�+B	I�B	+B	&�B	�B	�B	�B	oB	DB	%B��B�ZB�#B��B��B��BŢB�}B�RB�3B�?B�9B�?B�?B�9B�FB�'B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�uB�VB�JB� B|�B|�B}�Bz�Bz�Bz�B|�Bz�Bz�B{�B{�Bz�By�Bx�Bx�Bx�Bu�Bt�Br�Bq�Bp�BjBhsBgmBffBe`Be`BcTBbNBaHBaHB\)BZBZBYBXBW
BW
BXBS�BQ�BQ�BP�BP�BP�BN�BO�BO�BN�BM�BK�BC�B@�BA�BA�B=qB;dB<jB;dB9XB9XB8RB=qB=qB:^B5?B:^BE�BI�B?}B:^B2-B-B.B0!B2-B6FB7LB:^B=qBA�BA�BC�BG�BH�BH�BI�BK�BM�BM�BN�BN�BO�BQ�BR�BR�BVB[#BffBiyBl�Bo�Bs�Bz�B|�B{�Bz�B{�B}�B|�B{�B�B�B�B�+B�PB�VB�oB�uB�hB��B��B��B��B��B��B�B�'B�-B�3B�3B�FB�RB�dB�}BÖBÖBŢBƨB��B��B�B�B�B�;B�mB�sB�B�B��B��B��B��B��B	  B	B	B	%B	%B	%B	
=B	PB	bB	uB	�B	�B	�B	�B	 �B	"�B	#�B	%�B	'�B	'�B	(�B	(�B	)�B	)�B	)�B	,B	-B	-B	.B	-B	/B	7LB	>wB	A�B	A�B	B�B	B�B	C�B	C�B	D�B	D�B	F�B	F�B	F�B	F�B	G�B	H�B	H�B	J�B	L�B	N�B	O�B	Q�B	ZB	]/B	]/B	_;B	aHB	bNB	cTB	dZB	gmB	hsB	hsB	jB	l�B	o�B	q�B	r�B	t�B	w�B	x�B	y�B	z�B	z�B	{�B	|�B	}�B	~�B	~�B	� B	�B	�B	�DB	�PB	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�-B	�?B	�LB	�RB	�XB	�XB	�XB	�XB	�^B	�dB	�qB	�wB	�}B	��B	��B	��B	B	B	ÖB	ÖB	ĜB	ĜB	ĜB	ŢB	ƨB	ȴB	ȴB	ɺB	��B	��B	�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<e`B<�C�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�/<e`B<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447182012010314471820120103144718  AO  ARGQ                                                                        20111130142020  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142020  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144718  IP                  G�O�G�O�G�O�                