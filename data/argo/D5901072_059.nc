CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:55Z UW 3.1 conversion   
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
_FillValue        G�O�   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�       M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   z   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       |   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                   �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�       �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �$   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �T   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �T   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �T   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �T   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ;A   AO  20111130144113  20190522121829  1728_5048_059                   2C  D   APEX                            2142                            040306                          846 @Խiy\��1   @Խj� @4˅�Q��b��Q�1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
fD
� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,�fD-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq�fDr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dws3Dy� D��fD�6fD�c3D��fD��3D�  D�|�D�� D�� D�#3D�ffD��3D�fD�)�DچfD�3D���D�fD�FfD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @y��@�  @�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  Bf  Bn  Bv  B~  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_��Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D Y�D � D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D	` D	�fD
` D
� D` D� D` D� D` D� D` D� DY�D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+�fD,ffD,� D-` D-� D.` D.� D/` D/� D0` D0� D1ffD1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� DqffDq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� Dv` Dv� DwS3Dy� D��fD�&fD�S3D��fD��3D� D�l�D�� D�� D�3D�VfDǳ3D��fD��D�vfD�3D���D�fD�6fD�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�ĜA��wA���A�dZA��-A�-A��A�p�A�;dA�A��A���A�z�A���A�r�A�M�A�(�A�%A���A�XA��A�A���A��\A�~�A�\)A�?}A�1'A�"�A�A��A��A�XA�z�A���A��FA��wA�ĜA��/A�  A�bA�9XA�ffA�(�A��A�M�A�|�A���A�(�A�\)A�;dA��A���A�n�A�bNA�1'A���A���A��hA�~�A�hsA�VA�I�A�A�A�S�A�XA�G�A�{A��/A���A��FA���A�l�A���A�ZA��
A��A���A��
A�/A���A���A���A��\A��A�ZA�{A��A�A��FA�x�A�bA��-A�n�A���A�{A�ffA��HA��yA��TA��RA�jA��A�K�A�XA�;dA���A�K�A�A�C�A�(�A�ƨA��7A���A�x�A�A~�A|Ay��Axr�Avr�Ap�yAl��AkoAh��Ah5?Ag�Ae�^Ac/A_�-A_?}A_"�A_"�A^��A^�A]��AXv�ATI�AT�yAS�wAR1'AQ?}AP(�AN9XAL�!AK��AJ�/AI�;AH-AF^5AD��AC�hABA�AAoA@ �A>�jA=�mA<�A;�FA:M�A9`BA8�HA7oA5��A3�wA2�!A1�A1�A0z�A/�^A-�A,��A+�FA*�A(��A'��A&(�A$�RA$�A#�-A"�9A!�A�Ax�AVA��AjAx�A�/Ar�A�hA�`A�A�#A�A^5AO�A�wAM�Ax�A��AAĜAO�A
$�A	�A1Al�AĜA�7AĜAQ�A�FA�`A9XAp�@���@�G�@�r�@���@��@���@�P@�v�@��#@�7L@��D@��@���@��@�7@�Ĝ@�r�@��@�S�@�O�@�E�@��@�"�@��@��
@�\)@��@�5?@�Z@ו�@�K�@���@�$�@��@ҸR@ѡ�@�j@�;d@���@�j@˾w@���@ɡ�@���@�dZ@�V@�7L@ě�@�b@�+@+@�hs@��@��@��
@�
=@��#@��`@���@���@�/@��@�$�@�&�@��9@��m@�dZ@��P@��@�"�@�~�@�$�@�@��@���@��^@��@�V@��u@��w@���@�ff@���@���@�  @���@�\)@��@��R@�=q@��h@���@�(�@��P@��P@�"�@��@�n�@��-@�hs@���@�r�@�1@�|�@��@��@��\@�J@���@�`B@��@��`@��m@�;d@�
=@��y@��+@�{@���@�/@���@���@��@�A�@�  @��m@���@��w@��@���@��P@�|�@�t�@�dZ@�l�@�t�@�t�@�\)@�+@��@���@�v�@�V@�M�@�=q@�=q@�E�@�E�@�=q@�-@�@��T@��T@��#@���@���@��-@��^@���@���@�p�@�`B@���@��T@��@�?}@�O�@�hs@��@��h@��7@��@�`B@�O�@�O�@�X@�7L@���@��@��j@��j@�z�@�9X@��@�1@�  @�ƨ@��P@�dZ@�;d@�@��H@�ȴ@���@�~�@�^5@�-@���@��#@��^@���@�X@�%@���@�r�@�1'@��
@��P@�dZ@�;d@��H@��\@�^5@�5?@�J@��T@���@�O�@�&�@���@��@�(�@��@��
@���@���@��@�S�@�@��R@�^5@�J@��@��#@���@�`B@��@�V@��`@�z�@�(�@��
@��@���@�dZ@�C�@�33@�"�@��y@���@�v�@�E�@���@��@�@���@�`B@�/@���@��`@���@��@�Z@� �@�  @��;@���@�dZ@�"�@���@�~�@�^5@�5?@�$�@�J@��@��^@�X@��u@w\)@pr�@f��@]�-@U�-@O��@I�^@D9X@>�y@9X@4�@-�-@(��@"�\@I�@ �@��@ȴ@
��@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�ĜA��wA���A�dZA��-A�-A��A�p�A�;dA�A��A���A�z�A���A�r�A�M�A�(�A�%A���A�XA��A�A���A��\A�~�A�\)A�?}A�1'A�"�A�A��A��A�XA�z�A���A��FA��wA�ĜA��/A�  A�bA�9XA�ffA�(�A��A�M�A�|�A���A�(�A�\)A�;dA��A���A�n�A�bNA�1'A���A���A��hA�~�A�hsA�VA�I�A�A�A�S�A�XA�G�A�{A��/A���A��FA���A�l�A���A�ZA��
A��A���A��
A�/A���A���A���A��\A��A�ZA�{A��A�A��FA�x�A�bA��-A�n�A���A�{A�ffA��HA��yA��TA��RA�jA��A�K�A�XA�;dA���A�K�A�A�C�A�(�A�ƨA��7A���A�x�A�A~�A|Ay��Axr�Avr�Ap�yAl��AkoAh��Ah5?Ag�Ae�^Ac/A_�-A_?}A_"�A_"�A^��A^�A]��AXv�ATI�AT�yAS�wAR1'AQ?}AP(�AN9XAL�!AK��AJ�/AI�;AH-AF^5AD��AC�hABA�AAoA@ �A>�jA=�mA<�A;�FA:M�A9`BA8�HA7oA5��A3�wA2�!A1�A1�A0z�A/�^A-�A,��A+�FA*�A(��A'��A&(�A$�RA$�A#�-A"�9A!�A�Ax�AVA��AjAx�A�/Ar�A�hA�`A�A�#A�A^5AO�A�wAM�Ax�A��AAĜAO�A
$�A	�A1Al�AĜA�7AĜAQ�A�FA�`A9XAp�@���@�G�@�r�@���@��@���@�P@�v�@��#@�7L@��D@��@���@��@�7@�Ĝ@�r�@��@�S�@�O�@�E�@��@�"�@��@��
@�\)@��@�5?@�Z@ו�@�K�@���@�$�@��@ҸR@ѡ�@�j@�;d@���@�j@˾w@���@ɡ�@���@�dZ@�V@�7L@ě�@�b@�+@+@�hs@��@��@��
@�
=@��#@��`@���@���@�/@��@�$�@�&�@��9@��m@�dZ@��P@��@�"�@�~�@�$�@�@��@���@��^@��@�V@��u@��w@���@�ff@���@���@�  @���@�\)@��@��R@�=q@��h@���@�(�@��P@��P@�"�@��@�n�@��-@�hs@���@�r�@�1@�|�@��@��@��\@�J@���@�`B@��@��`@��m@�;d@�
=@��y@��+@�{@���@�/@���@���@��@�A�@�  @��m@���@��w@��@���@��P@�|�@�t�@�dZ@�l�@�t�@�t�@�\)@�+@��@���@�v�@�V@�M�@�=q@�=q@�E�@�E�@�=q@�-@�@��T@��T@��#@���@���@��-@��^@���@���@�p�@�`B@���@��T@��@�?}@�O�@�hs@��@��h@��7@��@�`B@�O�@�O�@�X@�7L@���@��@��j@��j@�z�@�9X@��@�1@�  @�ƨ@��P@�dZ@�;d@�@��H@�ȴ@���@�~�@�^5@�-@���@��#@��^@���@�X@�%@���@�r�@�1'@��
@��P@�dZ@�;d@��H@��\@�^5@�5?@�J@��T@���@�O�@�&�@���@��@�(�@��@��
@���@���@��@�S�@�@��R@�^5@�J@��@��#@���@�`B@��@�V@��`@�z�@�(�@��
@��@���@�dZ@�C�@�33@�"�@��y@���@�v�@�E�@���@��@�@���@�`B@�/@���@��`@���@��@�Z@� �@�  @��;@���@�dZ@�"�@���@�~�@�^5@�5?@�$�@�J@��@��^@�X@��u@w\)@pr�@f��@]�-@U�-@O��@I�^@D9X@>�y@9X@4�@-�-@(��@"�\@I�@ �@��@ȴ@
��@�w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
@�B
@�B
E�B
O�B
^5B
�bB
�BhB�B!�B9XB� B�-B��B��B��B��B��BĜB�3B��B��B��B��B��B��B��B��B��B��B��B��B�RB��B��B��B�B�B�5B�yB�B
=B(�BN�BP�BS�BjB�B��B��B��B��B��B��B��B�'B�'B�RB�qB�}B�}BÖBȴB��B��B�#B�)B�B��B��B��B��B��B��B�oB}�Bt�Bp�BdZBK�BD�B<jBN�BD�B!�B�B
=B�yB��B�B��B��B�BjB\)B@�B6FB1'B%�B�B�B�BoBJBB
�B
�#B
ŢB
��B
��B
��B
��B
�7B
v�B
iyB
M�B
6FB
$�B
{B
	7B
%B
�B	�RB	��B	��B	�{B	�B	}�B	p�B	^5B	dZB	ffB	jB	l�B	o�B	iyB	9XB	#�B	F�B	R�B	YB	\)B	XB	O�B	J�B	E�B	A�B	;dB	49B	-B	%�B	%�B	�B	PB	
=B	JB	+B	  B��B��B�B�B	B�B�BB�B��BȴBȴB�wBǮB�^B�9B��B�B�-B��B�hB�bB�oB�1B�%Bx�Bw�Bq�Bq�Bx�Bp�Bo�Bn�Bl�BhsBffBe`BffBaHB_;B\)BYBW
BT�BP�BN�BK�BM�BI�BE�BD�BE�BE�B@�B>wB>wB<jB;dB:^B;dB<jB33B0!B5?B:^B49B2-B49B9XB;dBG�BF�B=qB6FB5?B5?B6FB9XB2-BB�B/B/B,B,B/B5?B49B:^B:^B8RB7LB7LB6FB<jB<jB<jB;dB;dB?}B?}B@�BA�BA�BE�BE�BE�BF�BE�BG�BI�BK�BJ�BL�BO�BQ�BT�BXB\)B^5Bm�Bs�B�B{�B|�B�7B�1B�\B��B��B��B��B��B�B�!B�3B�LB�dB�jB��B��B�}BB��B��B��B��BBÖBŢB��B��B��BBƨBŢBǮBɺB��B��B��B��B��B��B��B�B�B�BB�TB�ZB�`B�mB�B�B�B�B��B��B	  B	%B	PB	VB	hB	{B	�B	�B	�B	�B	!�B	$�B	%�B	(�B	)�B	-B	.B	0!B	33B	5?B	8RB	;dB	<jB	=qB	@�B	A�B	C�B	D�B	C�B	C�B	C�B	E�B	H�B	J�B	O�B	P�B	S�B	XB	]/B	`BB	cTB	ffB	gmB	hsB	l�B	r�B	u�B	v�B	y�B	{�B	}�B	~�B	�B	�B	�7B	�DB	�PB	�hB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�-B	�3B	�9B	�FB	�LB	�XB	�RB	�RB	�RB	�XB	�RB	�XB	�jB	�}B	�}B	�wB	�}B	�}B	�}B	��B	B	ÖB	ÖB	ĜB	ŢB	ȴB	ȴB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�#B	�)B	�/B	�5B	�5B	�BB	�BB	�BB	�HB	�NB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�fB	�mB	�mB	�sB	�yB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
{B
!�B
+B
2-B
7LB
;dB
A�B
I�B
N�B
T�B
YB
aHB
ffB
jB
n�B
s�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
@�B
A�B
F�B
R�B
`BB
�uB
�BoB�B!�B6FB}�B�-B��B��B��B��B��BƨB�9B�B��B��B��B��B��B��B��B��B��B��B��B�RB�}B��B��B�B�B�5B�yB�B	7B)�BO�BT�B]/Bm�B�B�{B��B�B��B��B��B��B�9B�'B�XB�wB��B��BÖBȴB��B��B�)B�/B�#B��B��B��B��B�B��B��B�Bz�Bu�Bl�BS�BN�B=qBT�BO�B(�B#�B�B��B�B�'B��B��B�\Bl�BdZBH�B9XB6FB+B�B�B�B�BoBJB
��B
�TB
��B
�B
��B
��B
��B
�\B
~�B
s�B
XB
>wB
,B
�B
VB
uB
 �B	�jB	��B	��B	��B	�%B	�B	y�B	_;B	e`B	ffB	k�B	n�B	r�B	w�B	D�B	"�B	J�B	XB	\)B	`BB	^5B	T�B	M�B	H�B	D�B	A�B	:^B	2-B	)�B	)�B	�B	bB	\B	\B	JB	B��B��B��B��B	%B�B�TB�B��B��B��BĜB��B�qB�LB�B�-B�FB��B�uB�oB��B�JB�JB~�B{�Bt�Bu�B{�Br�Bq�Bq�Bn�BiyBiyBhsBhsBe`BdZBaHB\)BYBXBT�BS�BO�BQ�BM�BG�BG�BJ�BH�BB�BA�BA�B?}B?}B@�B?}B>wB8RB49B7LB=qB6FB33B5?B:^B>wBJ�BI�B?}B7LB6FB6FB7LB<jB6FBC�B33B2-B.B-B0!B6FB7LB<jB;dB9XB9XB9XB:^B>wB>wB>wB>wB>wB?}BA�BB�BC�BD�BG�BG�BG�BG�BG�BI�BK�BL�BK�BN�BQ�BT�BW
BZB\)BaHBo�Bv�B�B|�B}�B�=B�1B�\B��B��B��B��B��B�B�'B�9B�RB�jB�wBB��B��BĜBÖB��B��BBÖBĜBƨB��BBBBǮBɺB��B��B��B��B��B��B��B��B��B�
B�#B�HB�ZB�`B�fB�yB�B�B�B��B��B��B	B	+B	PB	\B	oB	�B	�B	�B	�B	�B	"�B	$�B	%�B	(�B	)�B	-B	.B	0!B	49B	5?B	9XB	<jB	=qB	>wB	@�B	A�B	C�B	D�B	C�B	C�B	C�B	F�B	I�B	J�B	O�B	P�B	T�B	XB	]/B	`BB	dZB	gmB	gmB	hsB	l�B	s�B	v�B	v�B	y�B	{�B	}�B	~�B	�B	�B	�7B	�DB	�PB	�oB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�-B	�-B	�9B	�?B	�LB	�RB	�^B	�XB	�XB	�RB	�^B	�XB	�^B	�qB	��B	��B	�}B	��B	��B	��B	B	ÖB	ĜB	ĜB	ŢB	ƨB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�#B	�/B	�5B	�;B	�;B	�HB	�BB	�HB	�NB	�NB	�NB	�TB	�ZB	�`B	�`B	�fB	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B

=B
{B
!�B
+B
2-B
7LB
;dB
B�B
J�B
O�B
VB
YB
aHB
ffB
k�B
o�B
t�B
x�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<49X<#�
<#�
<T��<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452132012011014521320120110145213  AO  ARGQ                                                                        20111130144113  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130144113  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145213  IP                  G�O�G�O�G�O�                