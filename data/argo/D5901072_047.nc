CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS     N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-28T23:01:52Z UW 3.1 conversion   
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
_FillValue                    � Argo profile    3.1 1.2 19500101000000  5901072 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               /A   AO  20111130143955  20190522121829  1728_5048_047                   2C  D   APEX                            2142                            040306                          846 @ԟ�&�1   @ԟ�β@ @6%�Q��c�z�H1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   B   @�ff@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  BffB  B   B(  B0  B8  B@  BH  BP  BX  B_��Bh  Bp  BxffB�  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C�C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dy��D��D�I�D�s3D��3D���D�  D��3D��3D��fD�0 D�l�D��fD�3D�#3D�` D��fD�ٚD��D�Y�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @l��@�  @�  A  A8  AX  Ax  A�  A���A�  A�  A�  A�  A�  A�  B  BffB  B  B&  B.  B6  B>  BF  BN  BV  B]��Bf  Bn  BvffB~  B�  B�  B�  B�  B�  B�  B�  B���B�  B���B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C� C� C	� C� C� C� C� C� C� C� C��C��C� C� C!� C#� C%� C'� C)� C+� C-� C/� C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO��CQ��CS� CU� CW� CY� C[� C]� C_� Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{� C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C���C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� Cǳ3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�3C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D	` D	� D
` D
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� DffD� D` D� D` D� D` D� D` D� D` D� D` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1` D1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;` D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL� DM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DTffDT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� Dj` Dj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� Dv` Dv� Dw` Dy��D���D�9�D�c3D��3D���D� D��3D��3D��fD�  D�\�DǶfD��3D�3D�P D�fD�ɚD��D�I�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A���A���A��/A��mA��yA��mA��`A��A��yA��mA��A��yA��yA��A��A��A��A��A��A��mA��A��A��A���A���A���A���A���A���A���A���A���A��A��A��A��/A�A�r�AʾwA�z�A��A�&�A���A���A��^A���A�ĜA��
A�  A��A�ZA��A��A�ƨA��#A�&�A�r�A�ȴA�+A���A�ƨA�VA�1'A��A��-A�1'A�C�A�G�A�-A���A��A�bNA���A��A���A��A��^A���A�ffA�;dA�x�A���A�ZA�C�A�-A��A���A�ƨA�I�A�;dA��A�%A��A�5?A�l�A��wA��A�G�A��A�$�A��9A�\)A��A��A�A�A�n�A�^A%A~A�A|I�Ay�hAx�+Av�`Au�At��Ar�Ao|�An�9Am"�Aj�!Ait�Ai%AhA�Af~�Ae�Ae%AdjAc��Ab~�Aa?}A_�A]��AZ �AY33AVĜAU��AT$�ARĜAQ�AQ�PAQ33APbAM�AI�7AE��AA+A>�DA<VA:r�A9�wA8�9A7|�A6��A7S�A6�A5|�A1��A0~�A0JA/\)A.�9A,ȴA+K�A*r�A)�hA(=qA&��A&-A%|�A$��A#�A#A" �A!��A n�AXAVA��A7LA�yA��A�A�HAVA�A�A�At�A"�A��AS�A�RA��Ar�A��AbNA"�A�A��A��A�TA
�jA	\)AĜAVA�A��AJAXA��AĜA-A�A��A�wA/A �D@�dZ@��@�dZ@��R@���@��`@���@�5?@��-@�@�K�@�n�@��@�h@�z�@�"�@�-@���@�9@�V@�?}@�P@��@��@㕁@�$�@�Q�@߶F@�@�{@�`B@�/@�Ĝ@ە�@���@�v�@��#@�V@���@؛�@׶F@�1@�b@���@˅@��@ɉ7@�Ĝ@�I�@�1@��@�"�@�J@��/@�Ĝ@�x�@��@�-@��#@ũ�@���@Ĭ@ě�@��/@�%@ċD@��;@�dZ@�o@�ȴ@�@�^5@���@�7L@�z�@�Z@�(�@��
@��F@�C�@��@�^5@��-@�?}@���@�/@�j@�ȴ@��@��T@�7L@��j@�I�@��@�l�@�
=@�v�@�$�@��@�`B@��@���@��/@��u@��@�;d@���@�M�@���@�`B@�G�@�/@�O�@��h@��7@�p�@�hs@�7L@���@��9@���@��u@�j@�(�@��m@���@�l�@�S�@�"�@��@���@���@�v�@�V@�$�@��#@�7L@��/@��u@�I�@��
@�l�@�K�@��@�ff@���@��@�Ĝ@��u@��/@�bN@�dZ@��H@�{@�G�@���@���@���@�|�@�+@���@��@���@���@�(�@��@���@���@�;d@��@��@�n�@�=q@�{@��-@�?}@��@�Q�@�+@��@��@�~�@�/@���@�1@��w@�+@��+@�5?@��@���@�x�@�V@��@�bN@�r�@�j@�j@��@� �@���@�K�@�@��H@���@�dZ@�A�@��@��D@�z�@�j@� �@��@�S�@��y@�=q@��@��^@�x�@��@��9@�j@�I�@��@��m@��F@�t�@�+@�@���@��H@���@���@�V@�{@���@��#@��-@��h@�x�@�X@�/@��@���@��`@���@���@� �@���@��;@��
@��
@���@��@�|�@�C�@�
=@���@�ff@�=q@�$�@��T@���@�hs@���@��@�A�@� �@���@���@�ƨ@��@�|�@�
=@�n�@�@��@�7L@|(�@vv�@n�+@dz�@\I�@U�@Q&�@J�H@E�@>�R@8b@2~�@,�@'�;@#��@(�@�9@�@�9@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A���A��/A��mA��yA��mA��`A��A��yA��mA��A��yA��yA��A��A��A��A��A��A��mA��A��A��A���A���A���A���A���A���A���A���A���A��A��A��A��/A�A�r�AʾwA�z�A��A�&�A���A���A��^A���A�ĜA��
A�  A��A�ZA��A��A�ƨA��#A�&�A�r�A�ȴA�+A���A�ƨA�VA�1'A��A��-A�1'A�C�A�G�A�-A���A��A�bNA���A��A���A��A��^A���A�ffA�;dA�x�A���A�ZA�C�A�-A��A���A�ƨA�I�A�;dA��A�%A��A�5?A�l�A��wA��A�G�A��A�$�A��9A�\)A��A��A�A�A�n�A�^A%A~A�A|I�Ay�hAx�+Av�`Au�At��Ar�Ao|�An�9Am"�Aj�!Ait�Ai%AhA�Af~�Ae�Ae%AdjAc��Ab~�Aa?}A_�A]��AZ �AY33AVĜAU��AT$�ARĜAQ�AQ�PAQ33APbAM�AI�7AE��AA+A>�DA<VA:r�A9�wA8�9A7|�A6��A7S�A6�A5|�A1��A0~�A0JA/\)A.�9A,ȴA+K�A*r�A)�hA(=qA&��A&-A%|�A$��A#�A#A" �A!��A n�AXAVA��A7LA�yA��A�A�HAVA�A�A�At�A"�A��AS�A�RA��Ar�A��AbNA"�A�A��A��A�TA
�jA	\)AĜAVA�A��AJAXA��AĜA-A�A��A�wA/A �D@�dZ@��@�dZ@��R@���@��`@���@�5?@��-@�@�K�@�n�@��@�h@�z�@�"�@�-@���@�9@�V@�?}@�P@��@��@㕁@�$�@�Q�@߶F@�@�{@�`B@�/@�Ĝ@ە�@���@�v�@��#@�V@���@؛�@׶F@�1@�b@���@˅@��@ɉ7@�Ĝ@�I�@�1@��@�"�@�J@��/@�Ĝ@�x�@��@�-@��#@ũ�@���@Ĭ@ě�@��/@�%@ċD@��;@�dZ@�o@�ȴ@�@�^5@���@�7L@�z�@�Z@�(�@��
@��F@�C�@��@�^5@��-@�?}@���@�/@�j@�ȴ@��@��T@�7L@��j@�I�@��@�l�@�
=@�v�@�$�@��@�`B@��@���@��/@��u@��@�;d@���@�M�@���@�`B@�G�@�/@�O�@��h@��7@�p�@�hs@�7L@���@��9@���@��u@�j@�(�@��m@���@�l�@�S�@�"�@��@���@���@�v�@�V@�$�@��#@�7L@��/@��u@�I�@��
@�l�@�K�@��@�ff@���@��@�Ĝ@��u@��/@�bN@�dZ@��H@�{@�G�@���@���@���@�|�@�+@���@��@���@���@�(�@��@���@���@�;d@��@��@�n�@�=q@�{@��-@�?}@��@�Q�@�+@��@��@�~�@�/@���@�1@��w@�+@��+@�5?@��@���@�x�@�V@��@�bN@�r�@�j@�j@��@� �@���@�K�@�@��H@���@�dZ@�A�@��@��D@�z�@�j@� �@��@�S�@��y@�=q@��@��^@�x�@��@��9@�j@�I�@��@��m@��F@�t�@�+@�@���@��H@���@���@�V@�{@���@��#@��-@��h@�x�@�X@�/@��@���@��`@���@���@� �@���@��;@��
@��
@���@��@�|�@�C�@�
=@���@�ff@�=q@�$�@��T@���@�hs@���@��@�A�@� �@���@���@�ƨ@��@�|�@�
=@�n�@�@��@�7L@|(�@vv�@n�+@dz�@\I�@U�@Q&�@J�H@E�@>�R@8b@2~�@,�@'�;@#��@(�@�9@�@�9@z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B�B�B�mB�HB�/B�)B�B�B�B�B��B��BɺBƨBƨB�}B�?B�'B�B��B��B��B�\B�JB�B{�Bt�BiyBdZB_;B\)BT�BS�BQ�BK�BI�BP�BB�B0!B!�B�B��B�ZB��B�FB��B�=Br�BbNBM�B9XB%�B �B\B
��B
�yB
�B
��B
�XB
��B
�hB
�B
~�B
t�B
m�B
iyB
bNB
iyB
P�B
H�B
B�B
9XB
:^B
'�B
�B
PB
+B
B	�B	�B	�B	�`B	�#B	��B	��B	ǮB	�}B	�!B	��B	�{B	�B	n�B	bNB	VB	P�B	H�B	G�B	G�B	E�B	D�B	5?B	!�B��B�
B��B�1B{�Bv�Bx�B{�By�B�1B��B��B��B� Bv�Bt�Bu�Bu�Bl�BhsBbNBiyBYBR�BS�BQ�BW
BP�BO�BYBT�BYBYB`BBaHB`BB`BBbNBbNBbNBbNBbNBe`BdZBdZBdZBdZBdZBdZBdZBbNBcTBcTBaHB`BB]/B[#BYBXBW
BVBVBW
BT�BXBXBXBVBW
BYB_;B`BBT�BW
B_;BYBZBT�BT�BP�BT�BM�BK�BK�BN�BQ�BS�B]/BYBVBYBVBP�BO�BM�BI�BI�BF�BE�BD�BA�B@�B@�BA�BB�BC�BD�BH�BL�BR�B]/B_;B_;B^5B_;BXBH�BC�B@�BA�BD�BH�BK�BM�BM�BN�BP�BT�BVBaHBhsBiyBn�Bs�By�B~�B�B�%B�bB��B��B��B��B��B��B��B��B��B�B�B�B�!B�-B�3B�FB�RB�XB�}BǮBǮBŢBǮB��B��B��B��B�
B�
B�B�#B�BB�`B�mB�B�B�B�B�B�B��B��B��B��B��B��B��B	B	
=B	VB	�B	�B	�B	$�B	&�B	'�B	(�B	-B	2-B	49B	7LB	7LB	9XB	9XB	;dB	=qB	>wB	@�B	B�B	E�B	I�B	P�B	S�B	VB	XB	YB	ZB	[#B	\)B	`BB	aHB	bNB	bNB	cTB	e`B	iyB	e`B	cTB	bNB	bNB	cTB	dZB	dZB	dZB	e`B	e`B	e`B	e`B	e`B	hsB	jB	m�B	q�B	t�B	v�B	x�B	{�B	~�B	�B	�B	�B	�B	�B	~�B	� B	�B	�1B	�B	�B	�B	�%B	�%B	�B	�B	�%B	�+B	�7B	�=B	�7B	�=B	�JB	�VB	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�9B	�FB	�?B	�FB	�FB	�FB	�RB	�^B	�}B	��B	B	ÖB	ÖB	ŢB	ƨB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�)B	�/B	�5B	�;B	�BB	�NB	�TB	�TB	�ZB	�`B	�`B	�mB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
VB
{B
!�B
(�B
/B
49B
9XB
>wB
D�B
J�B
O�B
VB
[#B
_;B
ffB
jB
l�B
p�B
s�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�G�O�BB��B�B�B�B�`B�`B�BB�5B�)B�B�#B��B��B��B��B��BÖB�XB�RB�9B��B��B��B�oB�\B�DB�B{�Bm�BgmBdZBaHBYBVBVBM�BL�BS�BG�B33B%�B!�BB�yB�B�RB�'B�hBw�BhsBS�B?}B)�B%�B�B%B
�B
�#B
��B
B
�-B
��B
�B
�B
x�B
p�B
k�B
dZB
m�B
W
B
K�B
F�B
=qB
;dB
.B
�B
\B
DB
DB	��B	�B	�B	�B	�/B	��B	��B	ɺB	B	�9B	�B	��B	�JB	q�B	iyB	ZB	VB	L�B	J�B	I�B	G�B	H�B	>wB	-B	B�ZB�B�\B�By�B{�B� B{�B�+B��B��B��B�Bw�Bv�Bw�By�Bo�BjBdZBl�B]/BT�BVBT�BYBS�BR�B[#BYB\)B\)BbNBcTBaHBaHBffBe`BdZBdZBe`BiyBffBe`BffBhsBffBe`Be`Be`BgmBgmBe`Be`BcTB_;B]/B]/BYBXBYB[#BW
B[#BZBYBXBYB[#BbNBbNBW
BZBbNB\)B[#BW
BVBR�BW
BN�BM�BM�BP�BR�BT�B_;B[#BXBZBXBT�BQ�BP�BL�BK�BH�BH�BG�BB�BB�BB�BC�BC�BD�BF�BJ�BM�BS�B_;B`BB`BB`BBe`B_;BL�BG�BC�BB�BF�BI�BL�BN�BO�BP�BR�BT�BT�B`BBhsBjBo�Bu�Bz�B~�B�B�%B�hB��B��B��B��B��B��B��B��B�B�B�B�B�'B�3B�9B�LB�XB�^B�}BȴBɺBȴBȴB��B��B��B��B�B�B�B�)B�HB�fB�sB�B�B�B�B�B�B��B��B��B��B��B��B��B	B	
=B	\B	�B	�B	 �B	%�B	'�B	'�B	)�B	.B	33B	5?B	8RB	7LB	:^B	:^B	<jB	>wB	?}B	A�B	C�B	F�B	J�B	Q�B	T�B	W
B	YB	ZB	[#B	\)B	]/B	aHB	bNB	cTB	cTB	cTB	ffB	k�B	ffB	e`B	dZB	cTB	dZB	ffB	e`B	e`B	ffB	ffB	ffB	ffB	gmB	iyB	jB	m�B	r�B	u�B	w�B	y�B	|�B	� B	�B	�B	�B	�B	�%B	� B	� B	�B	�=B	�B	�B	�B	�+B	�+B	�B	�%B	�+B	�1B	�=B	�DB	�=B	�=B	�JB	�VB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�'B	�-B	�3B	�?B	�LB	�FB	�LB	�LB	�LB	�XB	�dB	��B	��B	B	ÖB	ĜB	ƨB	ǮB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�B	�
B	�B	�B	�#B	�/B	�5B	�;B	�BB	�HB	�TB	�TB	�ZB	�`B	�fB	�fB	�sB	�yB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B
VB
�B
"�B
)�B
/B
49B
:^B
>wB
D�B
J�B
P�B
W
B
[#B
_;B
gmB
jB
m�B
p�B
t�11111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<D��<D��<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201101452092012011014520920120110145209  AO  ARGQ                                                                        20111130143955  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143955  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120110145209  IP                  G�O�G�O�G�O�                