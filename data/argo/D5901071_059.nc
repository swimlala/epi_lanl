CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:07Z UW 3.1 conversion   
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
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ;A   AO  20111130140427  20190522121826  1727_5046_059                   2C  D   APEX                            2143                            040306                          846 @�a�RL�1   @�a���@77
=p���c�ȴ9X1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A^ffA�  A�  A�  A���A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  BpffBw��B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>fD>�fD?fD?� D?��D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�ff@�  A   A   A@  A^ffA�  A�  A�  A���A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@ffBH  BP  BX  B`  Bh  BpffBw��B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D��Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%fD%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>fD>�fD?fD?� D?��D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DKy�DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]�fD^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��-A��!A��!A��!A��A��A��RA��^A��jA��^A��jA��wA��wA���A�ƨA�A�A�A�ĜA�ȴA�ƨA�ĜA�A�ĜA�A���A���A���A���A�A�ĜA�ĜA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��FA��jA�S�A�A��A���A���A�ȴA��-A���A�t�A�O�A��A���A���A�t�A�?}A�+A���A�ƨA�Q�A�33A�C�A�  A�M�A���A�oA��DA��yA�/A��!A��^A��HA��/A�ffA�hsA�r�A�G�A���A�M�A��A�oA�
=A��A��A��PA��`A��mA��A���A�33A��/A��#A�&�A�?}A��A�dZA��
A���A�O�A���A��A
=Az�\AxE�Av��At�Ar1Ap�An�Al�/Aj1'Af�/Af�Ad{Aa��A`�!A_��A^n�A\�!A[�AZ��AX�uAU��ATn�ASAQK�AN��AMO�AI�^AE�#AD�AC��AA�A@n�A?oA=��A;ƨA:��A:I�A9��A8��A8��A8�A8bNA8$�A7��A7A69XA3�TA1O�A0-A.�A-�A,n�A*��A)\)A(1'A&9XA%�7A%p�A%`BA%XA%K�A%C�A%+A%oA$ĜA#��A"9XA!ƨA!S�A (�A��AdZAS�A
=A�A��A~�AoAE�Ax�A��A$�A�A�A~�A9XAAXA?}A33A&�A��A�A+A\)A�HA�uAI�A1A�
A�A�`AbA
�A
��A
�9A
��A
{A	��A	�A�FA�wA
=AM�A�A$�@�5?@�Q�@�{@���@���@�z�@�(�@�1@��;@��@�C�@���@���@�@�Ĝ@���@⟾@⟾@�V@�@�O�@�A�@�dZ@�=q@�ƨ@�1'@���@Ԭ@��@ӥ�@�ƨ@ӥ�@��@�X@�%@�Z@���@�G�@�S�@ʧ�@��T@Ǯ@Ƈ+@���@ũ�@��@��/@�Q�@Õ�@�@�Ĝ@���@��@�X@�{@�r�@���@���@�{@�hs@�1@�V@��T@��H@�Z@ēu@þw@��j@�l�@��y@�"�@�S�@�|�@��
@���@��F@�dZ@�l�@�|�@��w@�t�@��!@���@��@�^5@�hs@��@��@��9@�K�@�hs@��@�$�@���@���@���@��-@�z�@���@�A�@���@���@�9X@�K�@�t�@�1'@��@��m@�\)@��@�v�@��H@�+@�+@�5?@�r�@�v�@�?}@��`@��@��-@�x�@��-@���@���@�=q@�~�@��R@��R@�~�@�@�|�@�@���@��h@�x�@��P@�^5@�@�@���@��7@�hs@��@��w@���@���@�|�@�S�@�
=@���@���@�M�@��h@���@��9@��F@��+@�J@���@��@�&�@�1@��F@��P@���@�t�@��F@��F@��F@���@�+@��@��@�V@�@���@��h@�G�@��@�z�@�(�@�(�@�1@��w@�\)@�o@�v�@��\@���@��+@��\@�@�O�@��@��9@��u@�z�@�1'@��w@�t�@�
=@�
=@�
=@��@��\@���@�~�@�=q@��@�@��T@��7@�%@�j@���@���@��@��!@�ff@�5?@�$�@�{@��@���@���@�p�@�?}@�%@���@��@��D@�Z@� �@��m@��F@���@�\)@���@�^5@�J@���@���@��@�`B@��@���@��D@��D@��D@�Q�@�w@+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��-A��!A��!A��!A��A��A��RA��^A��jA��^A��jA��wA��wA���A�ƨA�A�A�A�ĜA�ȴA�ƨA�ĜA�A�ĜA�A���A���A���A���A�A�ĜA�ĜA�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��FA��jA�S�A�A��A���A���A�ȴA��-A���A�t�A�O�A��A���A���A�t�A�?}A�+A���A�ƨA�Q�A�33A�C�A�  A�M�A���A�oA��DA��yA�/A��!A��^A��HA��/A�ffA�hsA�r�A�G�A���A�M�A��A�oA�
=A��A��A��PA��`A��mA��A���A�33A��/A��#A�&�A�?}A��A�dZA��
A���A�O�A���A��A
=Az�\AxE�Av��At�Ar1Ap�An�Al�/Aj1'Af�/Af�Ad{Aa��A`�!A_��A^n�A\�!A[�AZ��AX�uAU��ATn�ASAQK�AN��AMO�AI�^AE�#AD�AC��AA�A@n�A?oA=��A;ƨA:��A:I�A9��A8��A8��A8�A8bNA8$�A7��A7A69XA3�TA1O�A0-A.�A-�A,n�A*��A)\)A(1'A&9XA%�7A%p�A%`BA%XA%K�A%C�A%+A%oA$ĜA#��A"9XA!ƨA!S�A (�A��AdZAS�A
=A�A��A~�AoAE�Ax�A��A$�A�A�A~�A9XAAXA?}A33A&�A��A�A+A\)A�HA�uAI�A1A�
A�A�`AbA
�A
��A
�9A
��A
{A	��A	�A�FA�wA
=AM�A�A$�@�5?@�Q�@�{@���@���@�z�@�(�@�1@��;@��@�C�@���@���@�@�Ĝ@���@⟾@⟾@�V@�@�O�@�A�@�dZ@�=q@�ƨ@�1'@���@Ԭ@��@ӥ�@�ƨ@ӥ�@��@�X@�%@�Z@���@�G�@�S�@ʧ�@��T@Ǯ@Ƈ+@���@ũ�@��@��/@�Q�@Õ�@�@�Ĝ@���@��@�X@�{@�r�@���@���@�{@�hs@�1@�V@��T@��H@�Z@ēu@þw@��j@�l�@��y@�"�@�S�@�|�@��
@���@��F@�dZ@�l�@�|�@��w@�t�@��!@���@��@�^5@�hs@��@��@��9@�K�@�hs@��@�$�@���@���@���@��-@�z�@���@�A�@���@���@�9X@�K�@�t�@�1'@��@��m@�\)@��@�v�@��H@�+@�+@�5?@�r�@�v�@�?}@��`@��@��-@�x�@��-@���@���@�=q@�~�@��R@��R@�~�@�@�|�@�@���@��h@�x�@��P@�^5@�@�@���@��7@�hs@��@��w@���@���@�|�@�S�@�
=@���@���@�M�@��h@���@��9@��F@��+@�J@���@��@�&�@�1@��F@��P@���@�t�@��F@��F@��F@���@�+@��@��@�V@�@���@��h@�G�@��@�z�@�(�@�(�@�1@��w@�\)@�o@�v�@��\@���@��+@��\@�@�O�@��@��9@��u@�z�@�1'@��w@�t�@�
=@�
=@�
=@��@��\@���@�~�@�=q@��@�@��T@��7@�%@�j@���@���@��@��!@�ff@�5?@�$�@�{@��@���@���@�p�@�?}@�%@���@��@��D@�Z@� �@��m@��F@���@�\)@���@�^5@�J@���@���@��@�`B@��@���@��D@��D@��D@�Q�@�w@+111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�BƨB��B��B��B��B��B�B�B�B�/B�;B�TB�`B�mB�B�B�B�/BƨB�?B��B�1B�B�Bz�BhsBM�BH�B:^B"�BB��B�B�`B��B�}B�B��B��B�{B�uB�oB�PB�B{�Bn�B_;BO�BB�B6FB%�B�BDB
�B
��B
�wB
��B
e`B
F�B
A�B
<jB
,B
	7B	��B	�B	�5B	��B	��B	�B	��B	�B	p�B	jB	\)B	O�B	J�B	E�B	>wB	33B	,B	$�B	�B	PB	B��B�B�yB�5B��BǮBĜB�wB�^B�RB�9B�-B�!B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�oB�bB�\B�oB��B��B��B��B��B��B��B��B��B�B�B�B�B�B�B�B�B�FB�RB�XB�^B�dB�dB�dB�dB�LB�?BBŢBĜBƨBƨBĜB�}B�XB�FB�!B��B��B�Bs�Bl�Be`BaHB`BB`BB`BB_;B^5B^5B\)BVBH�BC�BA�BB�BB�BC�BC�BD�BC�BB�B?}B;dB6FB49B6FB7LB8RB9XB;dB:^B9XB8RB8RB9XB9XB9XB8RB6FB5?B5?B5?B49B5?B49B49B5?B33B6FB9XB@�BG�BP�Bl�Bw�B~�B�B�7B��B�B�LB��B��B��B��BǮBƨB��B��B��B�
B�`B�yB�B�B�B�B�B��B��B��B��B��B	B	B	B	B��B��B��B�B�B��B��B��B��B	B	B	B	B��B��B	B	VB	hB	�B	uB	JB	VB	uB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	+B	-B	/B	49B	8RB	:^B	;dB	;dB	:^B	5?B	33B	1'B	1'B	/B	.B	.B	-B	,B	,B	,B	-B	/B	49B	5?B	6FB	7LB	8RB	:^B	;dB	;dB	=qB	@�B	C�B	B�B	F�B	I�B	K�B	L�B	K�B	I�B	H�B	I�B	J�B	K�B	L�B	P�B	Q�B	S�B	T�B	W
B	YB	ZB	ZB	ZB	ZB	ZB	ZB	YB	ZB	[#B	[#B	\)B	]/B	`BB	bNB	hsB	m�B	r�B	y�B	y�B	x�B	w�B	v�B	w�B	w�B	x�B	y�B	|�B	}�B	�B	�B	�B	�B	�%B	�=B	�VB	�bB	�bB	�bB	�bB	�bB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�3B	�FB	�^B	�dB	�jB	�wB	��B	ÖB	ŢB	ŢB	ƨB	ɺB	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�#B	�#B	�)B	�/B	�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�-BȴB��B��B��B��B��B�B�B�#B�5B�BB�TB�fB�sB�B�B�B�sB��B�qB��B�JB�B�B� Bq�BO�BK�B@�B/BVB  B�B�B�;BƨB�FB��B��B�{B�uB�{B�bB�1B� Bt�Be`BT�BF�B=qB,B�B�B
��B
�B
ǮB
��B
o�B
H�B
B�B
@�B
5?B
VB	��B	��B	�TB	��B	ƨB	�-B	��B	�JB	s�B	p�B	dZB	R�B	M�B	J�B	D�B	7LB	/B	,B	�B	oB		7B	B��B�B�ZB�
BɺBƨBÖB�dB�dB�LB�FB�3B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�'B�!B�'B�LB�XB�^B�dB�jB�jB�qB�qB�^B�FBBŢBƨBǮBȴBȴBŢB�dB�RB�9B��B��B�DBw�Bo�BhsBcTBaHB`BB`BB`BB_;B_;B`BB`BBM�BF�BB�BB�BC�BD�BE�BF�BE�BE�BD�BB�B:^B6FB8RB8RB8RB:^B>wB<jB:^B:^B;dB<jB=qB:^B:^B:^B7LB6FB6FB5?B6FB5?B6FB6FB6FB9XB=qBC�BF�BM�BjBv�B� B�B�%B��B�B�FB�wB��B��B��BɺBǮB��B��B��B�
B�`B�B�B�B�B�B��B��B��B��B��B	B	B	B	B	%B	B��B��B��B�B��B��B��B��B	B	B	B	1B	  B��B	B	\B	hB	�B	�B	VB	VB	uB	�B	�B	�B	�B	�B	�B	�B	�B	"�B	%�B	+B	-B	/B	49B	8RB	:^B	<jB	=qB	>wB	8RB	49B	1'B	2-B	33B	0!B	/B	.B	,B	,B	,B	/B	1'B	49B	5?B	6FB	7LB	9XB	:^B	<jB	<jB	?}B	A�B	D�B	D�B	H�B	I�B	K�B	L�B	L�B	K�B	I�B	I�B	J�B	K�B	L�B	P�B	Q�B	S�B	VB	W
B	YB	[#B	[#B	ZB	ZB	[#B	ZB	ZB	[#B	[#B	[#B	]/B	^5B	aHB	cTB	hsB	m�B	r�B	y�B	z�B	y�B	x�B	v�B	w�B	w�B	y�B	z�B	}�B	~�B	�B	�B	�B	�B	�%B	�=B	�VB	�bB	�bB	�bB	�hB	�hB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�'B	�-B	�3B	�FB	�^B	�dB	�jB	�wB	��B	ÖB	ŢB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�#B	�)B	�/B	�5B	�;111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<T��<#�
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
<49X<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446542012010314465420120103144654  AO  ARGQ                                                                        20111130140427  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130140427  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144654  IP                  G�O�G�O�G�O�                