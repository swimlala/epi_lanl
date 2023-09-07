CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               3A   AO  20111205113204  20190522121836  1901_5055_051                   2C  D   APEX                            2140                            040306                          846 @Ԩ+��1   @Ԩβ @.�\(��cK"��`B1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cq�fCs�fCv  Cx  Cz  C|�C~�C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D$��D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4y�D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DHfDH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dc��Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dwy�Dy��D�  D�@ D�� D��fD�  D�,�D�l�D���D���D�,�D�C3D�� D���D� D�|�D�fD���D�#3D�VfD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @`  @�  @�  A  A8  AX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B^  Bf  Bn  BvffB~  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� CqffCsffCu� Cw� Cy� C{��C}��C��C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C��3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D	` D	� D
` D
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$ٚD%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3ٚD4Y�D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@Y�D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG�fDH` DH� DI` DI�fDJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` DcٚDd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� Dv` Dv� DwY�Dyy�D�� D�0 D�p D��fD�� D��D�\�D���D��D��D�33Dǰ D��D�  D�l�D�fD���D�3D�FfD���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A�  A�  A���A���A���A���A���A��`A�ƨA�`BAϧ�Aδ9AΧ�AΟ�AΕ�AΏ\AΓuAΕ�AΝ�AΥ�AΣ�AΛ�AΣ�AζFAκ^AΟ�AΑhA΃A�bNA�"�A͇+A�E�A�v�A��/Aʙ�A�VA�n�A�C�A��A��mA��yA���A���A�(�A���A��
A�~�A��A�%A���A��A�5?A�^5A��#A��A�;dA��FA�7LA���A���A�A�A�ƨA�jA�bA��RA���A�A�A���A��A��A��!A��/A�ffA��A���A�{A���A�
=A�
=A�G�A�$�A���A�A�  A���A�(�A�$�At�AzVAv��At�uAq��AnA�Ak��Ad�HAa��A]�A[/AY�PAX��AW�FARVAO�AM�ALVAKO�AI��AGXAE|�AB��A@�!A=��A:jA9K�A8��A8��A9�A7�-A4��A3��A4jA6ZA6r�A5p�A3A4�A4 �A4 �A3hsA1��A1S�A09XA/G�A-�A,��A+\)A)33A(5?A'��A'?}A&��A&��A&ZA&5?A%��A%VA$ȴA$I�A$1A#�A#�#A#�^A#`BA#33A"�A"A�A!�^A!;dA =qAG�A~�A�DA5?A�TA��A�hAx�A�An�A�#AXA�!Ax�A�A�TA�-A��A�mA��A��A`BAdZAƨAv�AĜA�DAbNA1A�TA�A+An�A�AZA5?A�
A�/A=qA�A��A�\A��A�7A;dA��A{A�A��AXA
ffA	�A	oA�/A��AjA-A\)AA��A�A9XA��A�AZA{A�PA��A�uA �A�mA"�A �RA z�A Z@��@���@�/@��@�^5@�$�@��@��@��@��@��@���@��@��^@�hs@���@�(�@�ƨ@��@�E�@�$�@���@�@�j@�@���@��@�j@�I�@�l�@�ȴ@�-@��@�`B@�@��@畁@�C�@���@�M�@�M�@��T@�%@�@� �@��@��@�E�@�@�hs@�7L@��@�Q�@��@��@�v�@���@ݺ^@ݙ�@�X@�l�@ڏ\@�$�@ّh@���@�r�@�t�@֗�@�J@Ցh@���@ԣ�@ԋD@Ԭ@�b@�ƨ@�t�@�;d@ҧ�@�@ѡ�@�G�@мj@�I�@Ͼw@�33@Ο�@�p�@��@ˍP@�t�@�K�@ʏ\@���@�p�@�?}@���@�Ĝ@ȓu@ȃ@�A�@Ǿw@�\)@Ə\@�ff@�ff@�@�x�@Ĵ9@�(�@Ý�@�l�@�S�@�;d@¸R@�n�@�{@���@�&�@��
@��;@��@��F@�C�@�@�x�@���@�A�@���@�;d@��+@�M�@�-@�$�@�x�@�Z@�1@�ƨ@��@�=q@��@�/@�Ĝ@�I�@��w@�l�@�33@���@�-@���@��-@��h@�`B@��@��@�(�@�l�@�;d@��@�@���@�-@��T@��h@�/@��@�%@���@��j@�bN@���@���@�\)@�+@��R@�v�@�E�@�J@��^@���@�j@�b@��;@���@�;d@�;d@��@��H@�^5@�5?@��@�x�@��@���@�(�@��;@��
@���@�l�@���@�~�@�$�@��@���@��h@�p�@�`B@�O�@�G�@�&�@�%@���@���@�bN@�1'@�  @���@���@�\)@���@���@���@�^5@�E�@�5?@���@���@���@�hs@�/@��u@�1'@��
@��P@�;d@�o@��!@�M�@�J@��#@���@�X@�G�@�7L@�%@���@��@�Q�@��@���@��@�C�@�o@��@���@��+@��@���@�G�@�Ĝ@��F@�E�@�  @|1@r-@h��@_\)@Xb@NV@FE�@@bN@9��@3S�@.v�@(��@#@��@1'@t�@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�  A�  A���A���A���A���A���A��`A�ƨA�`BAϧ�Aδ9AΧ�AΟ�AΕ�AΏ\AΓuAΕ�AΝ�AΥ�AΣ�AΛ�AΣ�AζFAκ^AΟ�AΑhA΃A�bNA�"�A͇+A�E�A�v�A��/Aʙ�A�VA�n�A�C�A��A��mA��yA���A���A�(�A���A��
A�~�A��A�%A���A��A�5?A�^5A��#A��A�;dA��FA�7LA���A���A�A�A�ƨA�jA�bA��RA���A�A�A���A��A��A��!A��/A�ffA��A���A�{A���A�
=A�
=A�G�A�$�A���A�A�  A���A�(�A�$�At�AzVAv��At�uAq��AnA�Ak��Ad�HAa��A]�A[/AY�PAX��AW�FARVAO�AM�ALVAKO�AI��AGXAE|�AB��A@�!A=��A:jA9K�A8��A8��A9�A7�-A4��A3��A4jA6ZA6r�A5p�A3A4�A4 �A4 �A3hsA1��A1S�A09XA/G�A-�A,��A+\)A)33A(5?A'��A'?}A&��A&��A&ZA&5?A%��A%VA$ȴA$I�A$1A#�A#�#A#�^A#`BA#33A"�A"A�A!�^A!;dA =qAG�A~�A�DA5?A�TA��A�hAx�A�An�A�#AXA�!Ax�A�A�TA�-A��A�mA��A��A`BAdZAƨAv�AĜA�DAbNA1A�TA�A+An�A�AZA5?A�
A�/A=qA�A��A�\A��A�7A;dA��A{A�A��AXA
ffA	�A	oA�/A��AjA-A\)AA��A�A9XA��A�AZA{A�PA��A�uA �A�mA"�A �RA z�A Z@��@���@�/@��@�^5@�$�@��@��@��@��@��@���@��@��^@�hs@���@�(�@�ƨ@��@�E�@�$�@���@�@�j@�@���@��@�j@�I�@�l�@�ȴ@�-@��@�`B@�@��@畁@�C�@���@�M�@�M�@��T@�%@�@� �@��@��@�E�@�@�hs@�7L@��@�Q�@��@��@�v�@���@ݺ^@ݙ�@�X@�l�@ڏ\@�$�@ّh@���@�r�@�t�@֗�@�J@Ցh@���@ԣ�@ԋD@Ԭ@�b@�ƨ@�t�@�;d@ҧ�@�@ѡ�@�G�@мj@�I�@Ͼw@�33@Ο�@�p�@��@ˍP@�t�@�K�@ʏ\@���@�p�@�?}@���@�Ĝ@ȓu@ȃ@�A�@Ǿw@�\)@Ə\@�ff@�ff@�@�x�@Ĵ9@�(�@Ý�@�l�@�S�@�;d@¸R@�n�@�{@���@�&�@��
@��;@��@��F@�C�@�@�x�@���@�A�@���@�;d@��+@�M�@�-@�$�@�x�@�Z@�1@�ƨ@��@�=q@��@�/@�Ĝ@�I�@��w@�l�@�33@���@�-@���@��-@��h@�`B@��@��@�(�@�l�@�;d@��@�@���@�-@��T@��h@�/@��@�%@���@��j@�bN@���@���@�\)@�+@��R@�v�@�E�@�J@��^@���@�j@�b@��;@���@�;d@�;d@��@��H@�^5@�5?@��@�x�@��@���@�(�@��;@��
@���@�l�@���@�~�@�$�@��@���@��h@�p�@�`B@�O�@�G�@�&�@�%@���@���@�bN@�1'@�  @���@���@�\)@���@���@���@�^5@�E�@�5?@���@���@���@�hs@�/@��u@�1'@��
@��P@�;d@�o@��!@�M�@�J@��#@���@�X@�G�@�7L@�%@���@��@�Q�@��@���@��@�C�@�o@��@���@��+@��@���@�G�@�Ĝ@��F@�E�@�  @|1@r-@h��@_\)@Xb@NV@FE�@@bN@9��@3S�@.v�@(��@#@��@1'@t�@+11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	:^B	:^B	:^B	:^B	:^B	:^B	:^B	:^B	<jB	?}B	T�B	�B
�B
�bB
�uB
��B
��B
��B
��B
�B
�9B
�^B
B
��B
�B
�B
��B
��B
��B	7B!�BE�Bl�B�bB�B� Bl�B^5BcTBiyB_;B�B�RB��B��B�BJB33B2-B33B>wB?}B=qBC�BG�BS�BYB\)B[#B[#B[#BXBVBP�BO�BO�BJ�BK�BI�BG�BE�B@�B9XB33B!�BbB1B�BŢB��Br�BXB8RB
�B
��B
l�B
:^B
�B	�B	��B	�XB	��B	�uB	z�B	e`B	B�B	-B	oB	+B��B��B�B�)B��B��B��B��B��B�HB�HB�)B�HB�;B�yB�B��B	+B	"�B	�B	+B	%B	�B	P�B	cTB	dZB	�%B	��B	��B	��B	�B	�B	�3B	�qB	B	�wB	�qB	�FB	�?B	�XB	�XB	��B	��B	�B	�/B	�BB	�BB	�yB	�B	��B
  B
%B
JB
{B
�B
�B
�B
�B
!�B
!�B
�B
�B
�B
(�B
)�B
'�B
.B
1'B
0!B
1'B
49B
7LB
8RB
5?B
.B
-B
'�B
.B
(�B
 �B
{B
DB
JB
hB
�B
$�B
)�B
,B
-B
.B
,B
(�B
'�B
#�B
$�B
.B
/B
-B
(�B
+B
,B
)�B
,B
)�B
(�B
&�B
"�B
#�B
%�B
!�B
�B
�B
hB
\B
�B
�B
�B
�B
{B
oB
uB
�B
�B
hB
\B
PB
PB
DB
DB
	7B
1B
+B
+B
+B
+B
+B
%B
B
B
  B
B
  B
  B	��B	��B
  B
  B
  B	��B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
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
+B
+B
1B
1B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
	7B

=B

=B

=B
	7B
	7B

=B

=B
DB
DB
DB

=B
JB
VB
VB
PB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
hB
hB
hB
hB
oB
oB
uB
uB
uB
uB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
!�B
%�B
)�B
33B
8RB
>wB
A�B
F�B
N�B
S�B
YB
^5B
cTB
e`B
iyB
m�B
q�B
w�B
{�B
� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	:^B	:^B	:^B	:^B	:^B	:^B	:^B	;dB	=qB	A�B	XB	�B
�B
�bB
�uB
��B
��B
��B
��B
�B
�9B
�^B
B
��B
�B
��B
��B
��B  B
=B#�BF�Bn�B��B�B�Bu�B_;BhsBk�Bn�B�PB��B��B�B�BoB8RB7LB8RB@�BC�BA�BF�BL�BXB\)B_;B]/B\)B]/B[#BXBR�BR�BVBM�BN�BM�BK�BJ�BE�B<jB:^B+BuB\B��B�
B��By�B`BBK�B+B
��B
w�B
C�B
$�B	��B	�B	�wB	�B	��B	�B	x�B	L�B	:^B	�B	DB	B��B��B�`B��B��B�
B�B�B�fB�sB�TB�B�sB�B�B��B	%B	(�B	%�B	
=B	B	�B	P�B	ffB	iyB	�B	��B	��B	�B	�B	�B	�FB	��B	ƨB	��B	��B	�jB	�RB	�dB	�^B	��B	��B	�
B	�5B	�NB	�NB	�B	��B	��B
B
%B
PB
�B
�B
�B
�B
 �B
#�B
$�B
�B
�B
�B
)�B
+B
(�B
/B
2-B
1'B
33B
6FB
8RB
:^B
8RB
0!B
0!B
(�B
1'B
+B
#�B
�B
JB
JB
bB
�B
#�B
+B
-B
.B
/B
-B
)�B
+B
$�B
$�B
/B
0!B
0!B
+B
-B
.B
,B
.B
+B
)�B
(�B
$�B
#�B
'�B
"�B
"�B
�B
uB
bB
�B
�B
�B
�B
�B
uB
{B
�B
�B
uB
hB
VB
\B
PB
JB
DB
	7B
	7B
1B
1B
1B
1B
+B
+B
B
B
B
B
B
  B	��B
  B
B
B
  B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
%B
+B
1B
1B
1B
1B
1B
1B
1B
	7B
	7B
1B
1B
	7B
	7B
	7B
	7B
	7B

=B

=B

=B
DB

=B

=B

=B

=B
DB
DB
JB
JB
JB

=B
JB
VB
VB
PB
VB
VB
\B
\B
\B
bB
bB
bB
bB
bB
bB
hB
hB
hB
hB
oB
oB
oB
oB
oB
oB
uB
uB
{B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
!�B
&�B
+B
49B
9XB
?}B
A�B
G�B
N�B
T�B
YB
^5B
cTB
e`B
iyB
m�B
r�B
w�B
{�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�C�<#�
<#�
<#�
<���<�j<#�
<49X<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<���<#�
<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250252012011312502520120113125025  AO  ARGQ                                                                        20111205113204  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113204  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125025  IP                  G�O�G�O�G�O�                