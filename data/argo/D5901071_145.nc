CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:31Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130142448  20190522121827  1727_5046_145                   2C  D   APEX                            2143                            040306                          846 @���k�1   @��s��@6}p��
=�c�-V1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  Dy�D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9�fD:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD�fDE  DEy�DF  DF� DG  DG� DH  DH� DIfDI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp�fDqfDq� Dq��Dr� Ds  Dsl�Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  Dy�D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3fD3� D4  D4y�D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9�fD:  D:y�D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DAy�DB  DB� DC  DC� DD  DD�fDE  DEy�DF  DF� DG  DG� DH  DH� DIfDI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Doy�Dp  Dp�fDqfDq� Dq��Dr� Ds  Dsl�Dy�f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A���A���AľwA�Aĺ^Aę�Ać+AēuA�5?A�A�ȴAÉ7A�ZA�JA��#A�A�JA���A��^A��RA���A�`BA�I�A�ƨA��A�ZA�  A��A��HA���A��A��HA��9A�v�A���A�ȴA���A�bNA�&�A���A��;A���A��A���A�`BA��wA��A�{A�1A�%A���A�v�A��RA��HA��A�7LA��A���A��A��;A��A���A���A�^5A���A�7LA��jA��+A���A�(�A�Q�A��9A�?}A�JA�+A�VA��7A�;dA���A�XA�VA��A�v�A�7LA��\A�~�A�z�A�`BA��A�n�A�K�A�VA�(�A�A�dZA�M�A�5?A�v�A���A�/A��#A��jA�Q�A�ZA���A��RA��A�ĜA��A��/A��!A��A��/A���A�/A�x�A���A��
A�?}A��^A�hsA�C�A�&�A��A�mA|�Ay��Av��At^5Ar-Aqt�Ap��Am33AiO�Ae�FAb$�A`n�A_�A^^5A[�AZ-AX��AW��AU��AU?}AS�#ARn�ARbAQ�
AQ�AQVAOl�AM/AM/AMoALE�AK�AK�PAI�
AH�\AG�AF�AEAE&�AD�+ACXAB5?A@bA>{A=x�A<�A<1'A:��A9A7A65?A6JA5�TA5�PA41A3`BA3/A2��A1�A2JA1XA.��A-VA+�wA(��A'+A&��A&�A%A#�A!K�A�
A��A��A�FA?}AĜA �A�yA�AbA$�A�A�;AK�A�yA�Az�A�PA�`AA�A+A=qA
�RA
^5A
VA
=qA	t�A�yA�A��A�uA�+AbNA-A�#A�\A��A5?A �HA I�@�;d@��@���@��@��^@�G�@��D@�I�@���@�b@�@��/@�V@�%@�
=@ꗍ@�bN@�h@�Q�@㝲@�C�@�E�@�hs@�  @���@ڰ!@�X@�z�@׍P@�t�@�K�@��@�ȴ@֟�@��@�hs@���@�z�@�r�@�I�@�C�@җ�@Ѻ^@�dZ@�hs@���@�r�@�r�@�bN@�|�@�~�@�/@�1@ǝ�@�33@Ƨ�@���@ģ�@���@���@��^@���@��7@��7@�?}@��j@�1@���@�;d@��@+@�V@�x�@�J@�5?@�ff@�?}@��@��@��@���@���@���@��;@��H@���@��P@�?}@���@��w@��@��H@��+@��@�@��@�z�@���@��@�n�@���@���@��^@��T@�-@���@�Ĝ@���@�t�@��R@��+@�M�@�@�X@�`B@��@�  @�ƨ@�"�@���@��T@���@�x�@�j@��
@��@��m@� �@��@�;d@�@�7L@��@��F@��!@��#@�p�@��/@�r�@�z�@�z�@��D@�&�@���@�j@��`@�&�@�V@��/@��;@�^5@���@�v�@�~�@�E�@�$�@�@�@��T@���@�@��7@���@���@�j@�b@���@���@�@�p�@�?}@�&�@���@��@��m@�ƨ@�ƨ@��F@��P@�|�@�33@�@��R@�5?@��h@��@���@��@�K�@�+@�"�@�"�@��@�M�@��T@�@�@��^@��-@��-@�x�@�/@��@�V@�V@���@���@�bN@�9X@��@�dZ@�;d@�o@�o@��@��@��@��H@��y@���@�o@�o@���@�$�@�Ĝ@��m@���@�|�@�t�@�S�@�S�@�\)@�dZ@��@�t�@�33@�;d@��!@�E�@���@�p�@�/@�%@��@�j@� �@�  @���@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A���A���AľwA�Aĺ^Aę�Ać+AēuA�5?A�A�ȴAÉ7A�ZA�JA��#A�A�JA���A��^A��RA���A�`BA�I�A�ƨA��A�ZA�  A��A��HA���A��A��HA��9A�v�A���A�ȴA���A�bNA�&�A���A��;A���A��A���A�`BA��wA��A�{A�1A�%A���A�v�A��RA��HA��A�7LA��A���A��A��;A��A���A���A�^5A���A�7LA��jA��+A���A�(�A�Q�A��9A�?}A�JA�+A�VA��7A�;dA���A�XA�VA��A�v�A�7LA��\A�~�A�z�A�`BA��A�n�A�K�A�VA�(�A�A�dZA�M�A�5?A�v�A���A�/A��#A��jA�Q�A�ZA���A��RA��A�ĜA��A��/A��!A��A��/A���A�/A�x�A���A��
A�?}A��^A�hsA�C�A�&�A��A�mA|�Ay��Av��At^5Ar-Aqt�Ap��Am33AiO�Ae�FAb$�A`n�A_�A^^5A[�AZ-AX��AW��AU��AU?}AS�#ARn�ARbAQ�
AQ�AQVAOl�AM/AM/AMoALE�AK�AK�PAI�
AH�\AG�AF�AEAE&�AD�+ACXAB5?A@bA>{A=x�A<�A<1'A:��A9A7A65?A6JA5�TA5�PA41A3`BA3/A2��A1�A2JA1XA.��A-VA+�wA(��A'+A&��A&�A%A#�A!K�A�
A��A��A�FA?}AĜA �A�yA�AbA$�A�A�;AK�A�yA�Az�A�PA�`AA�A+A=qA
�RA
^5A
VA
=qA	t�A�yA�A��A�uA�+AbNA-A�#A�\A��A5?A �HA I�@�;d@��@���@��@��^@�G�@��D@�I�@���@�b@�@��/@�V@�%@�
=@ꗍ@�bN@�h@�Q�@㝲@�C�@�E�@�hs@�  @���@ڰ!@�X@�z�@׍P@�t�@�K�@��@�ȴ@֟�@��@�hs@���@�z�@�r�@�I�@�C�@җ�@Ѻ^@�dZ@�hs@���@�r�@�r�@�bN@�|�@�~�@�/@�1@ǝ�@�33@Ƨ�@���@ģ�@���@���@��^@���@��7@��7@�?}@��j@�1@���@�;d@��@+@�V@�x�@�J@�5?@�ff@�?}@��@��@��@���@���@���@��;@��H@���@��P@�?}@���@��w@��@��H@��+@��@�@��@�z�@���@��@�n�@���@���@��^@��T@�-@���@�Ĝ@���@�t�@��R@��+@�M�@�@�X@�`B@��@�  @�ƨ@�"�@���@��T@���@�x�@�j@��
@��@��m@� �@��@�;d@�@�7L@��@��F@��!@��#@�p�@��/@�r�@�z�@�z�@��D@�&�@���@�j@��`@�&�@�V@��/@��;@�^5@���@�v�@�~�@�E�@�$�@�@�@��T@���@�@��7@���@���@�j@�b@���@���@�@�p�@�?}@�&�@���@��@��m@�ƨ@�ƨ@��F@��P@�|�@�33@�@��R@�5?@��h@��@���@��@�K�@�+@�"�@�"�@��@�M�@��T@�@�@��^@��-@��-@�x�@�/@��@�V@�V@���@���@�bN@�9X@��@�dZ@�;d@�o@�o@��@��@��@��H@��y@���@�o@�o@���@�$�@�Ĝ@��m@���@�|�@�t�@�S�@�S�@�\)@�dZ@��@�t�@�33@�;d@��!@�E�@���@�p�@�/@�%@��@�j@� �@�  @���@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBBÖBÖBBÖBÖBB��B��B��B�wB�}BBĜBÖBÖBBĜBȴBǮBȴB�
B�BɺB��BĜB�jB�?B�B�'B�FB�jB��B��B��B��BĜB�}BBŢB��B��B��B�B�#B�;B�B�TB��B�TB��BDB�B �B�B�B�BuBhBbBJBDB%B��B��B�B��B�B�yB�mB�BB�
B��BB�B��B�hB�VBz�BhsBW
BE�B?}B33B(�B�BB�B�`B�
B��B��BɺBŢB�^B�B��B��B��B�\Bx�BcTB\)BM�B;dB(�B�BoB+B
��B
��B
�B
�BB
��B
ƨB
�dB
�-B
��B
��B
�1B
~�B
v�B
r�B
p�B
n�B
n�B
jB
R�B
=qB
#�B
{B
%B
  B	��B	�BB	��B	�FB	��B	��B	��B	�oB	�B	{�B	v�B	n�B	ffB	aHB	YB	R�B	P�B	N�B	L�B	H�B	B�B	G�B	R�B	VB	M�B	O�B	Q�B	W
B	S�B	O�B	I�B	D�B	>wB	;dB	7LB	0!B	$�B	�B	bB	
=B	B��B��B�B�fB�ZB�TB�B�B�B�B�mB�B�B�sB�)B��BɺB��B�wB�dB�LB�'B�B��B��B��B��B��B��B�{B�hB�bB�VB�DB�1B�B�B�B~�Bz�Bw�Bs�Bq�Bn�Bl�BjBiyBiyBiyBhsBgmBgmBgmBgmBgmBffBffBdZBaHB\)BZBW
BW
BVBVBT�BT�BS�BS�BQ�BM�BM�BM�BL�BJ�BI�BI�BH�BH�BG�BG�BI�BJ�BJ�BI�BI�BI�BH�BI�BK�BL�BL�BN�BM�BM�BM�BM�BM�BM�BM�BN�BN�BN�BM�BN�BN�BM�BO�BR�BS�BT�BS�BR�BS�BW
BW
B[#B[#B\)B\)B]/B_;BffBiyBiyBk�Bl�Bn�Bu�Bz�B�B�B�=B��B�B�'B�RB�wB��BÖB��B��BɺB�qB�'B�9B�LB�FB�LB�^BB��B�B�B�B�B�B�B�B�B�)B�5B�5B�NB�fB�mB�sB�B�B��B��B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B	B	%B		7B	DB	DB	
=B	VB	\B	oB	�B	�B	�B	#�B	'�B	'�B	'�B	(�B	1'B	49B	8RB	?}B	E�B	K�B	J�B	H�B	L�B	Q�B	VB	YB	ZB	`BB	cTB	e`B	ffB	gmB	gmB	gmB	ffB	ffB	dZB	bNB	_;B	_;B	^5B	^5B	^5B	^5B	^5B	_;B	dZB	e`B	e`B	e`B	ffB	ffB	gmB	hsB	iyB	k�B	m�B	o�B	r�B	r�B	u�B	u�B	u�B	v�B	v�B	y�B	{�B	|�B	|�B	}�B	� B	�B	�+B	�PB	�\B	�bB	�bB	�hB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�9B	�9B	�9B	�FB	�LB	�LB	�LB	�RB	�dB	�jB	�jB	�wB	�wB	��B	ÖB	ȴB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BBÖBÖBBÖBÖBB��B��BB�}B��BÖBŢBĜBĜBÖBǮB��B��B��B�#B�B��B��BƨB�qB�LB�B�'B�FB�dB��B��B��B��BŢB��BÖBƨB��B��B��B�B�)B�HB�B�mB�
B�TB��BPB�B#�B�B�B�B{BoB{BPBPBDBB��B��B��B�B�B�yB�NB�B��BƨB�!B��B�hB�uB� Bm�B]/BF�BC�B6FB/B�B1B��B�B�#B��B��B��B��B��B�!B��B��B��B��B�Be`BbNBVB@�B,B!�B�BDB
��B
��B
�B
�ZB
�B
��B
�}B
�FB
�B
��B
�JB
�B
x�B
s�B
q�B
n�B
p�B
s�B
[#B
F�B
)�B
�B
1B
B
B	�B	��B	�}B	��B	��B	��B	��B	�1B	� B	z�B	s�B	iyB	ffB	^5B	S�B	Q�B	O�B	O�B	N�B	J�B	G�B	S�B	YB	N�B	Q�B	XB	\)B	XB	S�B	L�B	F�B	@�B	>wB	:^B	6FB	)�B	�B	oB	JB	1B��B��B�B�mB�`B�ZB�B�B�B�B�yB�B�B�B�HB�
B��BƨB��B�qB�dB�FB�LB�B��B��B��B��B��B��B��B�hB�bB�hB�=B�1B�B�B�B~�Bz�Bu�Bs�Bq�Bo�Bn�BjBiyBjBk�BiyBhsBgmBgmBgmBgmBgmBffBffBffB_;B\)BYBYBW
BVBVBT�BT�BS�BT�BR�BP�BN�BN�BM�BK�BK�BI�BK�BK�BK�BK�BK�BK�BJ�BK�BL�BM�BM�BM�BM�BN�BM�BM�BN�BM�BN�BN�BN�BO�BN�BN�BO�BO�BO�BQ�BR�BS�BT�BT�BS�BT�BVBYBYB[#B\)B]/B^5B_;B_;BhsBiyBiyBk�Bl�Bo�Bv�Bz�B�B�B�%B��B�B�!B�LB�wB��BŢB��B��B��BĜB�-B�9B�XB�RB�LB�XB�}B��B�B�B�B�B�B�#B�#B�#B�/B�BB�;B�TB�fB�mB�sB�B�B��B��B�B��B�B��B��B��B��B��B	B��B��B��B��B��B��B	  B��B��B	B	%B	
=B	JB	DB	JB	VB	hB	oB	�B	�B	 �B	$�B	'�B	'�B	'�B	'�B	2-B	5?B	7LB	?}B	E�B	K�B	L�B	K�B	L�B	Q�B	VB	YB	ZB	`BB	cTB	e`B	ffB	gmB	hsB	hsB	ffB	ffB	e`B	dZB	`BB	`BB	_;B	^5B	^5B	^5B	_;B	`BB	dZB	e`B	e`B	e`B	ffB	gmB	gmB	iyB	jB	l�B	n�B	p�B	s�B	s�B	u�B	u�B	u�B	v�B	w�B	z�B	{�B	|�B	|�B	}�B	� B	�B	�1B	�PB	�\B	�bB	�bB	�oB	�hB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�FB	�FB	�LB	�LB	�LB	�LB	�RB	�dB	�jB	�jB	�wB	�}B	��B	ĜB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447252012010314472520120103144725  AO  ARGQ                                                                        20111130142448  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130142448  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144725  IP                  G�O�G�O�G�O�                