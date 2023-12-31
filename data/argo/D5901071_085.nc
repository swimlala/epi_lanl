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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               UA   AO  20111130141103  20190522121826  1727_5046_085                   2C  D   APEX                            2143                            040306                          846 @ԃ��/�1   @ԃ��@7��t�j�d�;dZ1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DG��DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dp��Dqy�Dr  Dr� Ds  DsffDy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  Dy�D  D� D  D� D  D� DfD� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)fD)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2y�D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8y�D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DDfDD� DE  DE� DF  DF� DG  DG� DG��DHy�DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn�fDo  Do� Dp  Dp� Dp��Dqy�Dr  Dr� Ds  DsffDy�31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�/A�-A�1'A�5?A���A��AͲ-A͍PA�+Ả7A�jA�bNA�^5A�ZA�VA�O�A�E�A�A�A�;dA�33A�1'A�5?A�5?A�/A�$�A��A��A˕�A�?}A�|�A�ffAȏ\Aũ�A�5?Aġ�A�(�A��AîAÅA�A��mA��A��DA���A�E�A��-A�{A��9A���A���A��7A���A�&�A��-A��hA��^A�9XA�ƨA��A���A��A�E�A��yA�Q�A��FA�A��#A��7A��TA�l�A�;dA��#A�=qA���A���A���A�x�A�l�A��A�A�A�E�A���A��A�A�VA�n�A�p�A��A��^A�%A�Q�A��A��\A��A���A�C�A�1'A���A��A���A�$�A�z�A�Q�A���A�`BA�VA�ƨA�ȴA���A��A�  A��;A��wA�A��A�5?A�
=A�oA���A��/A�l�A�+A��A���A��/A�5?A���A�p�A�`BA��A�z�A�1A��FA��hA��A{�Aw�wAu��As�hAodZAm�hAk�PAjn�Aix�Af��AcS�Ab~�Aa`BA\�!A[�AY�AU�ATr�AS��ARv�APE�AO�AMx�AK�AJȴAIC�AHA�AG�AF1'AEVAEK�AD�jAC��ABZAAƨAA��AA�7AA\)A@��A?`BA>{A<Q�A:��A:M�A9t�A8�HA6�uA5+A4^5A2��A1�7A0��A0�DA0-A/t�A.�HA.^5A-�wA-?}A,z�A+hsA+�A*��A)hsA(�DA(jA(�A&��A&�DA&1A%;dA$v�A#�#A#S�A"�yA"�+A!��A!�hA ��A�A\)AA�A�9Ax�AȴA�7AE�AoA��A��A��AG�Ar�Az�A?}A�\A1'A��Ap�A��An�A�A�wA\)A�/AbA
�DA	l�A��AbA
=AbNA9XA��A��A+A=qA ��A J@���@��@�%@��+@�G�@�r�@�
=@�$�@��^@��;@�@��@�  @�J@�A�@�|�@�M�@�r�@�K�@�hs@�D@�;d@�^5@���@�9X@�C�@�E�@���@��m@ۅ@�\)@���@�?}@�r�@���@ׅ@֧�@ղ-@�1'@��@��T@�ƨ@���@�%@�l�@ʰ!@�-@�/@�r�@�(�@�"�@�@Ĵ9@�j@�I�@��;@�ȴ@�E�@�z�@�o@�^5@�J@��7@���@�r�@�t�@�{@�7L@�Z@�S�@��R@�?}@�z�@��@���@��^@�Z@�l�@��@�n�@���@��@�Z@��@��@���@�\)@��R@�ff@��@��`@�1'@���@��F@�|�@�K�@��!@�V@�b@��@���@�C�@���@�~�@�$�@�O�@���@�Z@�b@�+@��H@��!@���@��@�`B@�V@���@�9X@��m@�j@��D@��9@��9@�A�@�
=@�@�+@�C�@��@��@��m@��P@�M�@���@�p�@���@��-@�hs@�I�@�j@�hs@�-@���@�hs@���@���@���@�;d@�v�@���@�&�@���@�j@�A�@��m@���@�ff@�ff@��@�o@�ff@��#@��@��@�{@��\@���@�"�@���@�@��h@�%@�1@��;@��;@�r�@���@�\)@�"�@�E�@�o@��@��m@�\)@�5?@�E�@���@�b@��@�1@��F@��@���@�M�@��@�J@���@���@���@�p�@�G�@��/@��@�1'@���@�l�@�;d@�+@�"�@�o@���@�^5@�V@�-@�J@�@��@��@�V@�%@�V@�V@��@���@��9@��@��w@��P@�"�@�v�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�/A�-A�1'A�5?A���A��AͲ-A͍PA�+Ả7A�jA�bNA�^5A�ZA�VA�O�A�E�A�A�A�;dA�33A�1'A�5?A�5?A�/A�$�A��A��A˕�A�?}A�|�A�ffAȏ\Aũ�A�5?Aġ�A�(�A��AîAÅA�A��mA��A��DA���A�E�A��-A�{A��9A���A���A��7A���A�&�A��-A��hA��^A�9XA�ƨA��A���A��A�E�A��yA�Q�A��FA�A��#A��7A��TA�l�A�;dA��#A�=qA���A���A���A�x�A�l�A��A�A�A�E�A���A��A�A�VA�n�A�p�A��A��^A�%A�Q�A��A��\A��A���A�C�A�1'A���A��A���A�$�A�z�A�Q�A���A�`BA�VA�ƨA�ȴA���A��A�  A��;A��wA�A��A�5?A�
=A�oA���A��/A�l�A�+A��A���A��/A�5?A���A�p�A�`BA��A�z�A�1A��FA��hA��A{�Aw�wAu��As�hAodZAm�hAk�PAjn�Aix�Af��AcS�Ab~�Aa`BA\�!A[�AY�AU�ATr�AS��ARv�APE�AO�AMx�AK�AJȴAIC�AHA�AG�AF1'AEVAEK�AD�jAC��ABZAAƨAA��AA�7AA\)A@��A?`BA>{A<Q�A:��A:M�A9t�A8�HA6�uA5+A4^5A2��A1�7A0��A0�DA0-A/t�A.�HA.^5A-�wA-?}A,z�A+hsA+�A*��A)hsA(�DA(jA(�A&��A&�DA&1A%;dA$v�A#�#A#S�A"�yA"�+A!��A!�hA ��A�A\)AA�A�9Ax�AȴA�7AE�AoA��A��A��AG�Ar�Az�A?}A�\A1'A��Ap�A��An�A�A�wA\)A�/AbA
�DA	l�A��AbA
=AbNA9XA��A��A+A=qA ��A J@���@��@�%@��+@�G�@�r�@�
=@�$�@��^@��;@�@��@�  @�J@�A�@�|�@�M�@�r�@�K�@�hs@�D@�;d@�^5@���@�9X@�C�@�E�@���@��m@ۅ@�\)@���@�?}@�r�@���@ׅ@֧�@ղ-@�1'@��@��T@�ƨ@���@�%@�l�@ʰ!@�-@�/@�r�@�(�@�"�@�@Ĵ9@�j@�I�@��;@�ȴ@�E�@�z�@�o@�^5@�J@��7@���@�r�@�t�@�{@�7L@�Z@�S�@��R@�?}@�z�@��@���@��^@�Z@�l�@��@�n�@���@��@�Z@��@��@���@�\)@��R@�ff@��@��`@�1'@���@��F@�|�@�K�@��!@�V@�b@��@���@�C�@���@�~�@�$�@�O�@���@�Z@�b@�+@��H@��!@���@��@�`B@�V@���@�9X@��m@�j@��D@��9@��9@�A�@�
=@�@�+@�C�@��@��@��m@��P@�M�@���@�p�@���@��-@�hs@�I�@�j@�hs@�-@���@�hs@���@���@���@�;d@�v�@���@�&�@���@�j@�A�@��m@���@�ff@�ff@��@�o@�ff@��#@��@��@�{@��\@���@�"�@���@�@��h@�%@�1@��;@��;@�r�@���@�\)@�"�@�E�@�o@��@��m@�\)@�5?@�E�@���@�b@��@�1@��F@��@���@�M�@��@�J@���@���@���@�p�@�G�@��/@��@�1'@���@�l�@�;d@�+@�"�@�o@���@�^5@�V@�-@�J@�@��@��@�V@�%@�V@�V@��@���@��9@��@��w@��P@�"�@�v�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB2-B1'B1'B1'B0!B0!B0!B/B-B'�B'�B'�B'�B'�B'�B'�B'�B'�B&�B&�B&�B'�B(�B'�B&�B#�B�BhB1B��B�mB�B�jB�9B�}B�B�`B��B��B��B��BPB�B'�B7LBJ�B[#Be`Bs�Bv�By�B�7B�\B��B�?B�^B�LB�FB�LB�^B�jB�wB�wB�}BÖBɺBɺBȴBƨBĜBÖB��B�wB��BƨBɺBɺB��B��B�B��B��B��B��B��B�wB�3B��B��B�?B�!B��B�B��B��B�Bl�BT�BE�B=qB/B�BB�B�sB�
B��B��B�dB�!B�B��B�VBp�B`BBM�B<jB,B�BuB%B
�B
�B
�B
ÖB
�RB
�B
��B
��B
��B
�VB
�B
w�B
bNB
N�B
@�B
!�B
+B	��B	�mB	��B	��B	�?B	�B	��B	�oB	�B	z�B	m�B	O�B	B�B	49B	�B	�B	hB	\B	�B	bB	B�B�B�`B�NB�NB�/B�/B�sB�B�B�B�B�B�B�sB�HB�)B�B��B��BŢB��B�qB�RB�B��B��B��B��B�uB�bB�VB�JB�=B�1B�%B�B�B�B�B� B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�%B�+B�7B�=B�%B�B�B� B|�B{�By�Bu�Bs�Bp�Bl�BjBiyBhsBgmBffBe`BdZBcTBbNB`BB^5B[#BXBVBT�BR�BP�BP�BO�BO�BN�BM�BK�BE�BA�B>wB<jB9XB9XB9XB8RB7LB6FB5?B7LB5?B49B49B49B5?B49B6FB8RB49B33B33B5?B5?B5?B49B5?B6FB7LB8RB8RB8RB7LB8RB9XB9XB9XB8RB8RB8RB8RB8RB:^B9XB;dB>wB?}B?}BA�BB�BA�BA�BB�BD�BE�BE�BE�BE�BD�BE�BG�BH�BH�BI�BI�BI�BK�BM�BO�BQ�BVBW
BZB\)B\)B`BBbNBffBiyBk�Bl�Bm�Bp�Bs�Bt�Bt�Bt�Bu�Bv�Bx�By�B|�B�B�B�B�B�B�B�VB��B��B��B��B��B��B��B��B��B��B��B�B�!B�'B�9B�LB�RB�XB�jB�wBB��B��B��B��B��B��B��B�B�NB�B�B�B�B�B�B��B��B��B	B	B	B	\B	�B	�B	�B	{B	�B	�B	�B	"�B	$�B	%�B	'�B	,B	-B	-B	,B	,B	-B	)�B	%�B	#�B	$�B	&�B	,B	2-B	7LB	;dB	?}B	B�B	A�B	B�B	D�B	I�B	K�B	N�B	S�B	^5B	hsB	hsB	gmB	p�B	x�B	x�B	u�B	t�B	w�B	� B	�B	�B	�%B	�+B	�1B	�=B	�PB	�\B	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B2-B1'B1'B2-B1'B1'B1'B1'B0!B(�B'�B'�B'�B'�B'�B'�B'�B'�B&�B&�B&�B'�B(�B'�B&�B$�B�BuBJB��B�B�sB�wB�FB��B�B�fB��B��B��BBbB�B+B:^BM�B]/BiyBu�Bx�B{�B�JB�hB��B�RB�jB�XB�LB�^B�dB�qB��B��BBƨB��B��B��BȴBŢBŢBĜB�}BBɺB��BɺB��B��B�B�B�B��B��B��BB�XB��B��B�RB�3B��B�!B�B��B�DBq�BZBG�BA�B6FB!�B+B��B�B�B��B��B�}B�-B�B��B��Bv�BgmBT�BB�B0!B �B�BJB
�B
�B
�5B
ǮB
�dB
�'B
��B
��B
��B
�uB
�B
}�B
hsB
R�B
J�B
+B
JB	��B	�B	��B	ƨB	�RB	�B	�B	��B	�B	~�B	z�B	T�B	G�B	?}B	#�B	�B	�B	�B	�B	{B	B��B�B�mB�ZB�`B�BB�/B�B��B�B�B�B�B�B�yB�fB�BB�5B�B��BȴBBĜB�jB�'B�B��B��B��B��B�oB�bB�VB�JB�=B�7B�1B�B�B�B�B�B�B�1B�B�B�B�B�B�B�B�B�B�B�%B�B�B�+B�+B�=B�PB�1B�+B�B�B� B|�B{�By�Bv�Bu�Bp�Bl�BjBiyBhsBhsBgmBe`BdZBcTBbNBaHB`BB\)BYBW
BVBR�BQ�BQ�BS�BS�BP�BP�BG�BB�BA�B>wB=qB;dB;dB8RB9XB7LB8RB9XB8RB6FB49B6FB6FB6FB8RB:^B7LB49B5?B6FB7LB6FB6FB7LB8RB9XB9XB8RB9XB9XB9XB:^B:^B;dB:^B:^B:^B:^B;dB<jB<jB>wB?}B@�BA�BA�BC�BC�BC�BD�BE�BE�BF�BG�BF�BG�BG�BH�BI�BI�BJ�BJ�BK�BM�BO�BQ�BS�BW
BYB[#B]/B^5BbNBdZBhsBjBl�Bm�Bm�Bq�Bs�Bt�Bt�Bu�Bv�Bw�By�B{�B}�B�B�B�B�B�B�1B�bB��B��B��B��B��B��B��B��B��B��B�B�!B�'B�'B�?B�LB�XB�^B�qB�}B��B��B��B��B��B��B��B��B�B�NB�yB�B��B�B�B�B��B��B��B	B	B	B	VB	�B	�B	�B	�B	�B	�B	�B	$�B	%�B	&�B	(�B	,B	.B	/B	-B	,B	.B	-B	&�B	$�B	$�B	&�B	,B	1'B	7LB	;dB	@�B	C�B	A�B	C�B	F�B	I�B	K�B	M�B	Q�B	\)B	iyB	iyB	ffB	n�B	x�B	y�B	w�B	t�B	u�B	~�B	�B	�B	�+B	�7B	�7B	�DB	�PB	�\B	�\B	�bB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<T��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447032012010314470320120103144703  AO  ARGQ                                                                        20111130141103  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130141103  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144703  IP                  G�O�G�O�G�O�                