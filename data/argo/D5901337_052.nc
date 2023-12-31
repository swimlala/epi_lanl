CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:38Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �8   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �@   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �H   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               4A   AO  20111205113211  20190522121836  1901_5055_052                   2C  D   APEX                            2140                            040306                          846 @Ԫ��1   @Ԫ����@.D�t�j�cU�$�/1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�fC  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4�C5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D fD �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%y�D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Dn��Do� Dp  Dp� DqfDq�fDr  Dry�Ds  Ds� Ds��Dty�Du  Du� Dv  Dv� DwfDwy�D�fD�I�D���D���D���D�<�D�Y�D�ɚD��fD�@ D�p D���D��D�  D�S3D� D���D��D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @`  @�  @�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A���B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  Bf  BnffBv  B~  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� CffC� C	� C� C� C� CffC� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3��C5ffC7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� Cϳ3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C���C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D	` D	� D
ffD
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` DٚD` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� DffD� D` D� D` D�fD ffD � D!` D!� D"` D"� D#` D#� D$` D$� D%Y�D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` DnٚDo` Do� Dp` Dp�fDqffDq� DrY�Dr� Ds` DsٚDtY�Dt� Du` Du� Dv` Dv�fDwY�D��fD�9�D�y�D���D���D�,�D�I�D���D��fD�0 D�` DǼ�D�ٚD� D�C3D� D���D��D�P 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#A��HA��HA��/A��#A���A���A���A��
A�ȴAѸRAѬAѣ�A�E�AЗ�A�z�A�dZA�|�A�x�A�7LA���Aϣ�A�z�A�r�A�=qA��A��A��#Aκ^AΗ�A�{A͓uA��A��HAʝ�A�JA��A�bNA�K�A�=qA�I�A��HA�K�A�dZA���A��`A��mA���A�/A�XA�`BA�XA�ĜA���A�  A�ffA��yA��-A��A�1'A�9XA�r�A�I�A�ZA��A�Q�A�bNA���A��FA��yA�7LA���A�bNA�\)A�"�A��A�x�A��A�M�A��;A�dZA�5?A�+A��A&�A{VAu��As33An5?Ai�AdĜAa�A\�/AY�wAUC�AQ�;APE�AM�AIK�AFz�AE%AC�7AA�TA>�A;S�A:1'A9p�A8�A8(�A8M�A8(�A6��A5XA5A4�RA4ffA3��A1�A0(�A-7LA+|�A*VA*bA)�TA)��A)�;A*�A)l�A(�9A(�A'�A'��A&�/A&A�A%�FA%�A%;dA$ĜA$Q�A#t�A"�A"��A"�RA"bNA!��A!��A!7LA VAt�A�AM�A=qA(�A  A1A��A��A=qA��An�A/AQ�A��Ax�A��A�DAQ�A�Ap�A�/A~�A�-Ax�A�AG�A��AM�A��A�A�A��A�jA(�A��At�AK�A7LA�A�A��A$�A��A7LA�A�yA�A~�A�-Ax�A
��A
bNA
=qA
�A
A	�7A�`AA�A�TA|�A��A��A�DAVA$�AA�A�AĜAQ�A��A�PAl�AdZAO�A
=AȴAr�A5?A�A�wAG�A n�A -A {@�t�@�@���@�~�@�5?@�`B@��j@�b@�l�@�"�@���@�ff@��T@�hs@���@� �@�o@��+@�E�@�J@��T@��-@�G�@���@��u@��@�F@�K�@�n�@���@�|�@�ff@�V@��@�K�@�!@�E�@陚@���@�j@� �@�t�@�o@�E�@�7L@��`@�j@��m@�K�@�\@�V@�@�V@��/@��@ߝ�@���@�V@ݺ^@��`@�Q�@��;@�ƨ@�ƨ@۝�@�K�@��y@�ȴ@�V@�x�@�%@؋D@ץ�@և+@�@��T@ղ-@�`B@�V@��@Լj@�ƨ@�o@Ұ!@�5?@�G�@��@��@У�@�1'@ϥ�@�S�@�K�@���@�@�p�@̃@�S�@ʏ\@�J@��T@�`B@��/@�1@�ƨ@�l�@�C�@��@�J@��@�bN@å�@�\)@�"�@°!@�^5@�=q@�n�@�^5@��h@�7L@��/@�Q�@��;@�t�@��H@�~�@���@�%@��9@�Q�@�b@��@��@�+@�@���@�=q@��#@�@���@�7L@��j@��u@�z�@�9X@��m@�|�@�K�@�t�@�|�@�t�@���@���@��9@�b@���@�\)@�+@���@��#@��@�%@��@��@�S�@��y@�^5@�J@���@�/@�/@�7L@�7L@�G�@��9@�r�@�A�@�  @�t�@��P@��F@���@���@���@��^@�X@���@��D@�bN@�  @�|�@�"�@�ȴ@�ff@��h@�/@���@��/@��@��@��m@�ƨ@��@�K�@�
=@�ȴ@�ff@�=q@���@���@�X@���@��@�A�@��@��
@���@�t�@�\)@�;d@��H@�{@���@��h@�G�@�V@��`@���@�j@�b@��w@�t�@�33@��R@�M�@�{@���@��#@��^@��7@�/@��/@�bN@�9X@�1'@�b@��@���@�S�@���@���@��R@���@��\@�ff@�5?@��@��7@�?}@��`@�
=@�ƨ@�Z@v{@kƨ@c"�@X�9@O�w@Hb@A�7@9�#@2�!@+��@%�-@l�@&�@�@�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��#A��HA��HA��/A��#A���A���A���A��
A�ȴAѸRAѬAѣ�A�E�AЗ�A�z�A�dZA�|�A�x�A�7LA���Aϣ�A�z�A�r�A�=qA��A��A��#Aκ^AΗ�A�{A͓uA��A��HAʝ�A�JA��A�bNA�K�A�=qA�I�A��HA�K�A�dZA���A��`A��mA���A�/A�XA�`BA�XA�ĜA���A�  A�ffA��yA��-A��A�1'A�9XA�r�A�I�A�ZA��A�Q�A�bNA���A��FA��yA�7LA���A�bNA�\)A�"�A��A�x�A��A�M�A��;A�dZA�5?A�+A��A&�A{VAu��As33An5?Ai�AdĜAa�A\�/AY�wAUC�AQ�;APE�AM�AIK�AFz�AE%AC�7AA�TA>�A;S�A:1'A9p�A8�A8(�A8M�A8(�A6��A5XA5A4�RA4ffA3��A1�A0(�A-7LA+|�A*VA*bA)�TA)��A)�;A*�A)l�A(�9A(�A'�A'��A&�/A&A�A%�FA%�A%;dA$ĜA$Q�A#t�A"�A"��A"�RA"bNA!��A!��A!7LA VAt�A�AM�A=qA(�A  A1A��A��A=qA��An�A/AQ�A��Ax�A��A�DAQ�A�Ap�A�/A~�A�-Ax�A�AG�A��AM�A��A�A�A��A�jA(�A��At�AK�A7LA�A�A��A$�A��A7LA�A�yA�A~�A�-Ax�A
��A
bNA
=qA
�A
A	�7A�`AA�A�TA|�A��A��A�DAVA$�AA�A�AĜAQ�A��A�PAl�AdZAO�A
=AȴAr�A5?A�A�wAG�A n�A -A {@�t�@�@���@�~�@�5?@�`B@��j@�b@�l�@�"�@���@�ff@��T@�hs@���@� �@�o@��+@�E�@�J@��T@��-@�G�@���@��u@��@�F@�K�@�n�@���@�|�@�ff@�V@��@�K�@�!@�E�@陚@���@�j@� �@�t�@�o@�E�@�7L@��`@�j@��m@�K�@�\@�V@�@�V@��/@��@ߝ�@���@�V@ݺ^@��`@�Q�@��;@�ƨ@�ƨ@۝�@�K�@��y@�ȴ@�V@�x�@�%@؋D@ץ�@և+@�@��T@ղ-@�`B@�V@��@Լj@�ƨ@�o@Ұ!@�5?@�G�@��@��@У�@�1'@ϥ�@�S�@�K�@���@�@�p�@̃@�S�@ʏ\@�J@��T@�`B@��/@�1@�ƨ@�l�@�C�@��@�J@��@�bN@å�@�\)@�"�@°!@�^5@�=q@�n�@�^5@��h@�7L@��/@�Q�@��;@�t�@��H@�~�@���@�%@��9@�Q�@�b@��@��@�+@�@���@�=q@��#@�@���@�7L@��j@��u@�z�@�9X@��m@�|�@�K�@�t�@�|�@�t�@���@���@��9@�b@���@�\)@�+@���@��#@��@�%@��@��@�S�@��y@�^5@�J@���@�/@�/@�7L@�7L@�G�@��9@�r�@�A�@�  @�t�@��P@��F@���@���@���@��^@�X@���@��D@�bN@�  @�|�@�"�@�ȴ@�ff@��h@�/@���@��/@��@��@��m@�ƨ@��@�K�@�
=@�ȴ@�ff@�=q@���@���@�X@���@��@�A�@��@��
@���@�t�@�\)@�;d@��H@�{@���@��h@�G�@�V@��`@���@�j@�b@��w@�t�@�33@��R@�M�@�{@���@��#@��^@��7@�/@��/@�bN@�9X@�1'@�b@��@���@�S�@���@���@��R@���@��\@�ff@�5?@��@��7@�?}@��`@�
=@�ƨ@�Z@v{@kƨ@c"�@X�9@O�w@Hb@A�7@9�#@2�!@+��@%�-@l�@&�@�@�#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	I�B	I�B	I�B	H�B	I�B	H�B	H�B	H�B	H�B	G�B	G�B	K�B	R�B	��B
O�B
N�B
O�B
[#B
bNB
cTB
�jB
�BoB-B?}BB�BD�BD�BC�BE�BT�BjB�7B��B��B�\B�=Bu�B[#B^5Bk�B�uB�PBs�BiyB�?B��BB�B'�B&�BB.BF�BL�BM�BL�BP�BO�BJ�BG�BF�B>wB49B.B#�B�B��B�`B��B�LB��B�PBp�B]/BB�B/BbB
��B
�#B
�RB
��B
jB
{B	�HB	��B	��B	�VB	q�B	VB	9XB	$�B	\B	  B�B�B�B�sB�TB�BB�ZB�mB�NB�HB�fB�B�B��B	B	�B	O�B	_;B	dZB	e`B	ffB	gmB	hsB	o�B	o�B	ffB	k�B	u�B	� B	�+B	�7B	�DB	��B	��B	�9B	�XB	�wB	ǮB	��B	��B	��B	��B	��B	��B	�B	�TB	�`B	�mB	�yB	�B	�B	�B	�B	��B	��B	��B	��B
B
%B
	7B
PB
hB
DB
+B
JB
1B
B
  B	��B	��B	��B
B
B
B
  B
  B	��B	��B
B
JB
hB
{B
hB
hB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
hB
hB
oB
hB
oB
hB
hB
hB
VB
PB
DB

=B
PB
VB
VB
VB
JB
JB
\B
bB
\B
\B
PB
JB
DB
JB
PB
VB
VB
PB
PB
PB
JB
DB
DB
DB
JB
DB
DB

=B

=B
	7B
	7B

=B
	7B
	7B
	7B
	7B
	7B

=B

=B
	7B
1B
+B
B
B
B
B
B
B
B
  B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
%B
B
%B
B
B
B
B
B
B
B
B
B
+B
1B
1B
%B
B
  B
  B
  B
  B
  B	��B	��B	��B
B
B
B
  B
  B
  B
  B
  B
B
B
B
B
+B
%B
%B
%B
B
B
B
%B
B
%B
%B
%B
%B
%B
%B
+B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B
	7B

=B
DB
DB
DB
JB
JB
JB
PB
PB
PB
JB
JB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
hB
hB
hB
oB
oB
hB
oB
oB
oB
uB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
#�B
'�B
-B
0!B
6FB
=qB
B�B
I�B
N�B
S�B
ZB
_;B
dZB
gmB
l�B
q�B
v�B
y�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	I�B	I�B	I�B	H�B	I�B	H�B	H�B	H�B	I�B	H�B	G�B	K�B	T�B	��B
P�B
O�B
O�B
[#B
cTB
e`B
�qB
�BoB.B@�BC�BE�BE�BC�BF�BVBl�B�PB��B��B�uB�hB�+B_;B`BBo�B��B�uB~�BjB�?B��B	7B�B,B.B%B33BI�BO�BO�BQ�BS�BR�BN�BJ�BK�BB�B6FB1'B(�B�B1B�B�
B��B�B��By�BhsBI�B9XB�B
��B
�`B
�qB
��B
|�B
 �B	�yB	��B	��B	��B	x�B	\)B	>wB	-B	�B	
=B��B��B�B��B�B�`B�yB�B�B�B�B�B��B��B	B	�B	VB	cTB	e`B	ffB	gmB	iyB	l�B	s�B	u�B	jB	n�B	v�B	�B	�+B	�7B	�DB	��B	��B	�?B	�dB	�}B	ɺB	��B	��B	��B	��B	��B	�B	�/B	�`B	�`B	�sB	�B	�B	�B	�B	��B	��B	��B	��B	��B
B
+B
	7B
VB
�B
VB

=B
bB
PB
+B
B	��B
B	��B
B
B
B
B
B
B
  B
B
PB
oB
�B
uB
uB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
uB
oB
uB
oB
uB
uB
oB
uB
bB
\B
PB
DB
VB
VB
\B
\B
PB
VB
bB
hB
hB
hB
bB
PB
JB
VB
VB
\B
\B
VB
\B
\B
VB
PB
JB
JB
PB
JB
JB
JB
DB
DB

=B
DB

=B

=B

=B

=B

=B
DB
DB

=B
	7B
	7B
1B
+B
%B
%B
B
B
B
B
B
B
B
B
B
B
B
B
  B
  B
  B	��B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B	��B
B
B
  B
  B	��B	��B
  B
  B
  B	��B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
+B
+B
B
+B
%B
%B
B
B
%B
%B
%B
%B
B
+B
1B

=B
1B
B
B
B
B
B
B
B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
1B
+B
+B
+B
%B
B
B
1B
%B
%B
%B
+B
+B
+B
+B
1B
+B
1B
1B
	7B

=B

=B

=B
	7B
DB

=B
DB
JB
JB
JB
PB
PB
PB
VB
VB
VB
PB
PB
VB
\B
\B
\B
bB
bB
\B
bB
bB
hB
hB
oB
oB
oB
uB
oB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
$�B
'�B
-B
0!B
7LB
=qB
B�B
I�B
O�B
S�B
ZB
_;B
dZB
hsB
m�B
r�B
v�B
y�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�C�<#�
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
<T��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�t�<D��<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250252012011312502520120113125025  AO  ARGQ                                                                        20111205113211  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113211  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125025  IP                  G�O�G�O�G�O�                