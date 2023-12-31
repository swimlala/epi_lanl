CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:00Z UW 3.1 conversion   
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
_FillValue                    �hArgo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL                A   AO  20111130135759  20190522121825  1727_5046_032                   2C  D   APEX                            2143                            040306                          846 @�?�	�1   @�?�m��@7�ě��T�c��hr�!1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq�fDr  Dr� Ds  Ds� Dz331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�ff@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Cg�fCj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� DfD�fD  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dpy�Dq  Dq�fDr  Dr� Ds  Ds� Dz331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��AϼjAϾwAϴ9Aϧ�Aϗ�Aϡ�A�^5A�"�A�bA�JA�A���A�  A�  A���A���A���A���A��A��A��A���A���A��A��yA��mA��yA��A��yA��HA�O�A�G�A���A���A�\)A�jA�JA��A�A��A�  A�l�A�Q�A���A���A�  A�C�A�^5A���A�z�A�;dA���A�VA��A��+A���A��A���A�G�A���A��PA�p�A�jA�bNA�A�A�
=A�K�A�"�A�A�ȴA��uA�ZA��A���A���A�r�A�A�A�ƨA���A�S�A��yA�l�A�%A��
A��^A��hA�ffA�1'A�1A���A�A�A���A�ƨA���A�`BA�A�~�A�E�A�%A�ƨA��+A�A�A�|�A��RA�(�A���A�I�A�oA�v�A��A��jA�$�A�=qA���A��A���A�\)A�VA��A�XA�;dA�$�A���A��mA��mA��jA�XA�oA���A��HA�l�A��`A���A�ffA���A���A���A��;A��A�z�A�~�A�;A{|�Ax9XAu�PAs��Ar�\Aq�^Aq��Ao��Al��Ak7LAi��Ah��Af  Ac�TAa�hA]�AY�TAX�AW�hAV�`AVbAT��AS��AQ��AP^5AO��AN��ALĜAKp�AI�#AI&�AGXAD�!AC\)AAƨA@bNA?A>�A=&�A:��A9
=A7��A6��A6v�A5�FA5oA4�A4$�A3��A2�RA29XA1/A0r�A/��A.n�A-��A-oA,�jA,�uA,r�A,�A+|�A*��A)`BA'��A&��A&  A$�HA#�A"��A"{A!XA ��A�wAG�A��AbA�AdZA;dA1'A��A�DA�7AQ�AQ�A$�AO�A�RAn�A�A33AdZA�A��A��A�mA�^A�hAS�A
�HA	�FAZA��A1'A�Ap�A~�AA+A��A�!A$�A �@���@��\@�J@�I�@�I�@���@��@��j@�~�@�@���@��/@�u@�@�n�@�+@�t�@��@��y@�@��/@�bN@���@�K�@�33@�S�@�l�@�^5@�x�@�Ĝ@�1@��@�h@�?}@ޏ\@��@��`@܃@�1'@��
@۝�@�C�@�o@���@ڗ�@��@׍P@��#@ԋD@�S�@��@�&�@�z�@�C�@���@θR@�&�@���@�Q�@�ƨ@ˍP@�o@�J@���@Ɵ�@���@ċD@��@�Q�@��@�S�@�$�@���@�l�@��7@�Q�@�\)@�X@��
@���@�o@���@�J@�x�@�/@���@�r�@��@�33@�G�@��F@���@�|�@��\@���@��@� �@�1@���@�^5@�X@�O�@�%@���@�o@���@��+@�-@��#@���@��h@��h@�p�@�?}@���@��@�o@��H@��@���@��+@�hs@��@�&�@�/@���@���@�9X@�b@��m@�dZ@�
=@���@��@���@�hs@�X@�7L@�z�@���@�bN@�Q�@�  @�33@��H@��\@�E�@�hs@�/@���@���@��D@��@���@���@�t�@�"�@��\@�^5@�@���@�7L@��@���@��u@��@��@�r�@�A�@�1@�1@���@�ƨ@��@���@�l�@�ȴ@��\@��+@�~�@�n�@�M�@�$�@��#@��h@��@�hs@���@�Ĝ@��@�bN@�Z@�Q�@�I�@� �@��@���@�l�@�@��@���@���@�ff@�=q@�{@�@���@���@�`B@��@�%@��`@�Ĝ@�I�@� �@��@��m@�C�@�@��H@��R@���@�=q@�{@��@��-@�&�@���@��j@��9@�ȴ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AϼjAϾwAϴ9Aϧ�Aϗ�Aϡ�A�^5A�"�A�bA�JA�A���A�  A�  A���A���A���A���A��A��A��A���A���A��A��yA��mA��yA��A��yA��HA�O�A�G�A���A���A�\)A�jA�JA��A�A��A�  A�l�A�Q�A���A���A�  A�C�A�^5A���A�z�A�;dA���A�VA��A��+A���A��A���A�G�A���A��PA�p�A�jA�bNA�A�A�
=A�K�A�"�A�A�ȴA��uA�ZA��A���A���A�r�A�A�A�ƨA���A�S�A��yA�l�A�%A��
A��^A��hA�ffA�1'A�1A���A�A�A���A�ƨA���A�`BA�A�~�A�E�A�%A�ƨA��+A�A�A�|�A��RA�(�A���A�I�A�oA�v�A��A��jA�$�A�=qA���A��A���A�\)A�VA��A�XA�;dA�$�A���A��mA��mA��jA�XA�oA���A��HA�l�A��`A���A�ffA���A���A���A��;A��A�z�A�~�A�;A{|�Ax9XAu�PAs��Ar�\Aq�^Aq��Ao��Al��Ak7LAi��Ah��Af  Ac�TAa�hA]�AY�TAX�AW�hAV�`AVbAT��AS��AQ��AP^5AO��AN��ALĜAKp�AI�#AI&�AGXAD�!AC\)AAƨA@bNA?A>�A=&�A:��A9
=A7��A6��A6v�A5�FA5oA4�A4$�A3��A2�RA29XA1/A0r�A/��A.n�A-��A-oA,�jA,�uA,r�A,�A+|�A*��A)`BA'��A&��A&  A$�HA#�A"��A"{A!XA ��A�wAG�A��AbA�AdZA;dA1'A��A�DA�7AQ�AQ�A$�AO�A�RAn�A�A33AdZA�A��A��A�mA�^A�hAS�A
�HA	�FAZA��A1'A�Ap�A~�AA+A��A�!A$�A �@���@��\@�J@�I�@�I�@���@��@��j@�~�@�@���@��/@�u@�@�n�@�+@�t�@��@��y@�@��/@�bN@���@�K�@�33@�S�@�l�@�^5@�x�@�Ĝ@�1@��@�h@�?}@ޏ\@��@��`@܃@�1'@��
@۝�@�C�@�o@���@ڗ�@��@׍P@��#@ԋD@�S�@��@�&�@�z�@�C�@���@θR@�&�@���@�Q�@�ƨ@ˍP@�o@�J@���@Ɵ�@���@ċD@��@�Q�@��@�S�@�$�@���@�l�@��7@�Q�@�\)@�X@��
@���@�o@���@�J@�x�@�/@���@�r�@��@�33@�G�@��F@���@�|�@��\@���@��@� �@�1@���@�^5@�X@�O�@�%@���@�o@���@��+@�-@��#@���@��h@��h@�p�@�?}@���@��@�o@��H@��@���@��+@�hs@��@�&�@�/@���@���@�9X@�b@��m@�dZ@�
=@���@��@���@�hs@�X@�7L@�z�@���@�bN@�Q�@�  @�33@��H@��\@�E�@�hs@�/@���@���@��D@��@���@���@�t�@�"�@��\@�^5@�@���@�7L@��@���@��u@��@��@�r�@�A�@�1@�1@���@�ƨ@��@���@�l�@�ȴ@��\@��+@�~�@�n�@�M�@�$�@��#@��h@��@�hs@���@�Ĝ@��@�bN@�Z@�Q�@�I�@� �@��@���@�l�@�@��@���@���@�ff@�=q@�{@�@���@���@�`B@��@�%@��`@�Ĝ@�I�@� �@��@��m@�C�@�@��H@��R@���@�=q@�{@��@��-@�&�@���@��j@��9@�ȴ1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBw�Bv�Bx�Bz�B{�Bz�B~�B�B�B�B�B�B�B�%B�%B�B�%B�+B�%B�%B�%B�7B�=B�7B�7B�7B�=B�DB�DB�7B�1Bv�BhsBiyBv�B}�B�B�B�%B�DB��B��B��B��B�9B�dB��B��B��B��B�
B�B�HB�B��B��BB	7B
=BDBPBVBVB\BoB�B!�B!�B#�B$�B'�B(�B)�B+B+B,B+B,B-B.B/B0!B0!B0!B0!B1'B1'B0!B0!B.B+B#�B�BuB
=BB��B�B�B�fB�BB�BǮB�B��B��B��B�uBu�BYBQ�BF�B33B-B!�B+B��B�BBƨB�-B��B��B��B��B�%Bo�BffB^5B?}B!�B%B
�NB
��B
ɺB
�wB
�!B
��B
�VB
~�B
hsB
M�B
C�B
+B
�B
hB
+B	��B	��B	��B	��B	�B	�;B	�#B	��B	��B	�-B	��B	�1B	r�B	u�B	t�B	s�B	p�B	l�B	ffB	[#B	S�B	Q�B	I�B	8RB	-B	 �B	�B	
=B��B�B�/B��B��B��B��BǮBǮBǮBǮBǮBŢBŢBĜBÖBB��B�}B�qB�^B�LB�9B�3B�'B�!B�B�B�B��B��B��B��B��B�{B�hB�\B�JB�=B�1B�%B�B�B�B}�B{�Bz�Bx�Bu�Bs�Bq�Bn�Bk�BjBiyBhsBgmBe`BdZBaHB_;B^5B]/BZBYBXBW
BVBS�BP�BM�BK�BJ�BI�BG�BF�BE�BD�BC�BB�B@�B?}B<jB<jB:^B8RB8RB7LB6FB6FB:^BJ�BP�BJ�BB�BC�BD�BK�BJ�BJ�BM�BR�B^5BdZBcTBiyBiyBl�Bo�Bo�Bm�Bk�BjBq�Bp�Bm�BgmBgmBl�Bt�By�B|�B}�B}�B~�B~�B}�B�B�B�%B�=B�=B�7B�DB�PB�hB�hB�bB�oB��B��B��B��B��B��B��B��B��B��B�\B�bB��B��B��B�B�B�3B�LB�RB�qBBBÖBĜBŢBǮBȴBȴB��B��B��B��B�B�
B�B�
B�B�/B�HB�HB�BB�NB�`B�`B�`B�yB�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B	B	%B	+B	1B	
=B	JB	PB	VB	\B	oB	�B	�B	�B	 �B	#�B	&�B	(�B	-B	/B	49B	5?B	9XB	9XB	:^B	;dB	;dB	?}B	@�B	A�B	B�B	D�B	I�B	J�B	K�B	M�B	O�B	S�B	S�B	W
B	XB	[#B	[#B	\)B	_;B	aHB	bNB	cTB	dZB	gmB	hsB	o�B	p�B	q�B	s�B	v�B	|�B	~�B	� B	� B	� B	�B	�B	�B	�B	�%B	�+B	�=B	�DB	�JB	�VB	�VB	�\B	�\B	�bB	�uB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�3B	�3B	�9B	�?B	�^B	�dB	�jB	�qB	��B	ĜB	ĜB	ĜB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 Bw�Bv�Bx�Bz�B{�B{�B� B�B�B�B�B�B�B�%B�%B�B�%B�+B�%B�%B�%B�7B�=B�7B�7B�7B�=B�DB�DB�PB�uB�JBv�Bt�B{�B� B�B�B�1B�\B��B��B��B�B�LB�wBŢB��B��B��B�B�)B�TB�B��B��BBDBJBPBVBVBVBbB{B�B"�B"�B$�B%�B(�B)�B+B,B-B-B-B-B.B0!B1'B2-B1'B1'B1'B2-B2-B1'B2-B0!B/B(�B �B�BJB+B��B��B�B�mB�NB�/B��B�'B��B��B��B��B|�B[#BVBL�B5?B0!B+BDB��B�B��B�RB��B��B��B��B�JBq�BgmBdZBG�B(�BoB
�fB
�
B
��B
B
�LB
��B
�oB
�1B
s�B
Q�B
O�B
49B
"�B
�B
DB
B	��B
B
  B	�B	�TB	�;B	�B	ĜB	�FB	��B	�bB	v�B	v�B	u�B	u�B	r�B	o�B	jB	^5B	T�B	T�B	N�B	<jB	1'B	"�B	�B	hB��B�B�HB�
B��B��B��B��B��BɺBɺB��BǮBƨBƨBŢBŢBÖBB��B�jB�jB�LB�?B�-B�'B�!B�B�B�B��B��B��B��B��B��B�hB�PB�JB�=B�1B�%B�B�B�B}�B{�B{�By�Bt�Bt�Br�Bq�Bk�Bl�BjBhsBffBgmBgmBaHB_;BaHB`BBZBYBXBXBXBT�BR�BM�BK�BK�BJ�BH�BG�BE�BD�BD�BD�BB�B<jB=qB=qB>wB;dB8RB9XB:^B:^BK�BR�BO�BC�BE�BD�BO�BK�BJ�BL�BP�B_;BffBbNBiyBiyBl�Bq�Bq�Bn�Bl�Bl�Bs�Bq�Br�BiyBhsBl�Bu�Bz�B}�B~�B}�B~�B� B�B�B�B�1B�JB�JB�7B�DB�\B�oB�oB�oB�uB��B��B��B��B��B��B��B��B��B��B�oB�hB��B��B��B�B�'B�?B�XB�jB��BÖBÖBĜBŢBƨBȴBȴBɺB��B��B��B�B�B�
B�B�B�#B�;B�HB�NB�NB�ZB�`B�fB�mB�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B	B	%B	+B		7B	DB	PB	PB	VB	bB	uB	�B	�B	�B	!�B	#�B	&�B	)�B	-B	/B	49B	6FB	:^B	:^B	;dB	<jB	<jB	?}B	@�B	A�B	B�B	E�B	I�B	J�B	K�B	N�B	P�B	S�B	T�B	W
B	YB	[#B	[#B	]/B	_;B	aHB	bNB	cTB	dZB	gmB	hsB	o�B	p�B	q�B	s�B	w�B	|�B	~�B	� B	� B	� B	�B	�B	�B	�B	�%B	�1B	�=B	�JB	�JB	�VB	�VB	�\B	�\B	�hB	�uB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�3B	�3B	�9B	�FB	�^B	�dB	�qB	�qB	B	ĜB	ĜB	ĜB	��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�1<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0 dbar.                                                                                                                                                                                                                                                     none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031446452012010314464520120103144645  AO  ARGQ                                                                        20111130135759  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130135759  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144645  IP                  G�O�G�O�G�O�                